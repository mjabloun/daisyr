## dai_dataframe.R -- convert R data.frames into the list-of-records shape
## the rest of yaml_to_dai()'s renderers expect (dai_helpers.R,
## dai_horizons.R, dai_columns.R, dai_crops.R, dai_management.R,
## dai_programs.R), so a table of soil/crop/site data can drive
## generate_dai() directly -- no YAML file, and no hand-built R list.
##
## This is what makes running Daisy across many sites practical to
## automate: build a `config` per site from rows of a table you already
## have (soil horizon data, site coordinates, weather file names, ...),
## and generate a `.dai` file per site with a plain loop -- see
## generate_dai()'s own documentation (dai_generate.R) for that pattern.
##
## Depends only on base R -- no other dai_*.R file required.
##
## ---------------------------------------------------------------------
## The problem this solves
## ---------------------------------------------------------------------
## `config$horizons` (and similarly `columns[[i]]$Soil$horizons`,
## `programs[[i]]$output`, `programs[[i]]$manager`, `crops`) is a *list*,
## one named list per record -- exactly what `yaml::read_yaml()` produces
## from a YAML sequence of mappings. A data.frame is structurally a list
## of *columns*, not row-records, so it can't be handed to `generate_dai()`
## directly in place of that list -- each renderer would silently receive
## a whole column vector instead of one row. `records_from_df()` bridges
## the two: one row in, one record (a plain named list) out.
##
## ---------------------------------------------------------------------
## The `_value`/`_unit` column-pair convention
## ---------------------------------------------------------------------
## The generic nested-block mechanism (dai_helpers.R) recognises a
## `{value: X, unit: "u"}` pair for any field that needs a unit bracket,
## e.g. `(dry_bulk_density 1.46 [g/cm^3])`. A data.frame naturally splits
## that into two columns instead of one nested value, so `records_from_df()`
## looks for exactly-paired `"<field>_value"` + `"<field>_unit"` columns
## and folds each pair back into the `{value, unit}` shape automatically:
##
##   dry_bulk_density_value | dry_bulk_density_unit
##   ------------------------|------------------------
##   1.46                    | "g/cm^3"
##
##       -> dry_bulk_density: {value: 1.46, unit: "g/cm^3"}
##
## A column with no matching pair (e.g. `name`, `texture_class`) passes
## through unchanged as a plain scalar field. Only *paired* columns are
## folded -- a lone `*_value` column with no matching `*_unit` column is
## left exactly as named (`dry_bulk_density_value`), never guessed at, so
## behaviour never depends on a silent judgement call.
##
## `NA` values are dropped from a record entirely (both plain scalars and
## the value half of a pair -- a row with `dry_bulk_density_value = NA`
## produces no `dry_bulk_density` field at all), matching how an absent
## key in YAML behaves: "this field wasn't given for this row", not
## "this field is the string NA".
##
## A column can also hold a list per row (e.g. a `Devel` column whose
## entries are each `list("original", list(DSRate2 = 0.025))`, matching
## the positional-method-pair shape dai_helpers.R already understands) --
## `records_from_df()` passes those through unchanged, so a data.frame and
## a plain R list can be mixed freely within the same table.
##
## ---------------------------------------------------------------------
## What this is -- and isn't -- a good fit for
## ---------------------------------------------------------------------
## Good: any naturally tabular, one-row-per-item, fixed-column-set list --
## `horizons`, a column's `Soil$horizons` references, a program's `output`
## or `manager` list, `crops` for simple flat overrides (no nested
## `Devel`/`Partit` sub-blocks).
##
## Not a good fit: an activity's `field_operations` -- `fertilise` needs
## `product`/`weight`, `harvest` needs `stub`/`stem`/`leaf`, and so on, a
## different shape per row, which one column set can't cover without an
## enormous, mostly-empty table. Build those as an ordinary R list.
## ---------------------------------------------------------------------

## -- helpers ---------------------------------------------------------------

#' Extract one row of a data.frame as a plain named list.
#'
#' Unlike `as.list(df[i, ])`, this indexes each column with `[[i]]`, which
#' does the right thing for both ordinary atomic columns (returns the
#' scalar) and list-columns (returns that row's list element, unwrapped --
#' e.g. an already-built PLF table or `[method, {overrides}]` pair stored
#' one per row).
#' @keywords internal
.row_as_list <- function(df, i) {
  setNames(lapply(names(df), function(nm) df[[nm]][[i]]), names(df))
}

#' @keywords internal
.is_na_scalar <- function(x) {
  length(x) == 1 && !is.list(x) && is.na(x)
}

#' Fold `<field>_value` / `<field>_unit` column pairs in one row-list back
#' into `<field> = list(value = ..., unit = ...)`, matching the shape
#' `render_generic_value()` (dai_helpers.R) expects. Only exact pairs are
#' folded; an unpaired `*_value` or `*_unit` column is left as-is. If the
#' `_value` half is `NA`, the whole pair is dropped (see file header).
#' @keywords internal
.fold_value_unit_pairs <- function(row) {
  nm <- names(row)
  value_cols <- grep("_value$", nm, value = TRUE)
  bases <- sub("_value$", "", value_cols)
  paired <- bases[paste0(bases, "_unit") %in% nm]

  for (base in paired) {
    vcol <- paste0(base, "_value")
    ucol <- paste0(base, "_unit")
    if (.is_na_scalar(row[[vcol]])) {
      row[[vcol]] <- NULL
      row[[ucol]] <- NULL
      next
    }
    unit <- row[[ucol]]
    row[[base]] <- list(value = row[[vcol]],
                         unit = if (.is_na_scalar(unit)) NULL else as.character(unit))
    row[[vcol]] <- NULL
    row[[ucol]] <- NULL
  }
  row
}

## -- public entry point -----------------------------------------------------

#' Convert a data.frame into a list of records
#'
#' Turns a "flat" data.frame into the list-of-records shape `generate_dai()`
#' and its renderers expect: one named list per row, `<field>_value`/
#' `<field>_unit` column pairs folded into a single
#' `<field> = list(value = ..., unit = ...)` entry (see this file's header
#' comment for the full convention), and `NA` fields dropped entirely (an
#' absent field, not a literal `"NA"`).
#'
#' Use this for any naturally tabular section -- most commonly top-level
#' `horizons`, a column's `Soil$horizons` (depth/unit/horizon-name
#' references), a program's `output` or `manager` list, or `crops` for
#' simple flat overrides (no nested sub-blocks). It is not a good fit for
#' an activity's `field_operations`, which vary too much in shape row to
#' row for one column set to cover -- build those as an ordinary R list.
#'
#' @param df A data.frame, one row per record.
#'
#' @return A list of one-row named lists (length `nrow(df)`), in row order.
#'   `records_from_df(df[0, ])` (or any zero-row data.frame) returns
#'   `list()`.
#'
#' @examples
#' horizons_df <- data.frame(
#'   name = c("Hor_25", "Hor_50"),
#'   texture_class = c("FAO3", "FAO3"),
#'   dry_bulk_density_value = c(1.46, 1.62),
#'   dry_bulk_density_unit = c("g/cm^3", "g/cm^3"),
#'   clay_value = c(0.22, 0.30),
#'   clay_unit = c("", ""),
#'   hydraulic = c("Cosby_et_al", "Cosby_et_al"),
#'   stringsAsFactors = FALSE
#' )
#' config <- list(horizons = records_from_df(horizons_df))
#' cat(generate_dai(config))
#'
#' ## Soil.horizons references (a column's own sub-list) are just as
#' ## tabular -- depth/unit/horizon-name triples:
#' refs_df <- data.frame(
#'   depth = c(-25, -50), unit = c("cm", "cm"),
#'   horizon = c("Hor_25", "Hor_50"), stringsAsFactors = FALSE
#' )
#' column_cfg <- list(
#'   name = "MyColumn",
#'   Soil = list(MaxRootingDepth = list(value = 100, unit = "cm"),
#'               horizons = records_from_df(refs_df)),
#'   Movement = list(type = "vertical", zplus = c(-25, -50, -100)),
#'   Groundwater = "deep"
#' )
#'
#' @export
records_from_df <- function(df) {
  if (!is.data.frame(df)) stop("records_from_df() requires a data.frame, got: ", class(df)[1])
  if (nrow(df) == 0) return(list())

  lapply(seq_len(nrow(df)), function(i) {
    row <- .row_as_list(df, i)
    row <- .fold_value_unit_pairs(row)
    row[!vapply(row, .is_na_scalar, logical(1))]
  })
}

## dai_dataframe.R ends here.
