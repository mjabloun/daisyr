## dai_helpers.R -- shared low-level helpers used by every dai_*.R file.
##
## Nothing in this file knows about any particular Daisy construct
## (defaction, defcrop, defhorizon, ...) -- it's pure formatting/parsing
## plumbing: string quoting and number formatting, the date/wait-condition
## mini-language, comment rendering, and the generic nested-block mechanism
## used by dai_crops.R / dai_horizons.R / dai_columns.R to pass Daisy's own
## parameter vocabulary straight through from YAML without a bespoke schema
## per construct.
##
## Every other dai_*.R file (and the dai_generate.R orchestrator) sources
## this one first.
##
## ---------------------------------------------------------------------

#' @keywords internal
`%||%` <- function(a, b) if (is.null(a)) b else a

## -- low-level formatting helpers --------------------------------------

#' Escape a string for use inside a Daisy double-quoted literal.
#' @keywords internal
.escape_dai_string <- function(x) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("\"", "\\\"", x, fixed = TRUE)
  x
}

#' Quote a name/string as a Daisy string literal, e.g. `"Spring Barley"`.
#'
#' Daisy accepts both bare symbols (`KAS`) and quoted strings (`"KAS"`) for
#' referencing named objects (both forms appear interchangeably in this
#' project's own .dai files for the very same fertilizer). Always quoting
#' is therefore always valid, and sidesteps having to guess whether a name
#' is a legal bare symbol (it may contain spaces, semicolons, digits, etc).
#'
#' Used for the NAME an object is defined under (`defcrop "X" ...`) and for
#' references to another already-named object (a parent crop/program, a
#' horizon inside a column, a fertilizer/crop/activity inside a management
#' operation). NOT used for Daisy's own fixed vocabulary (type keywords,
#' method/submodel selectors, enum-like values such as `none`/`deep`/
#' `vertical`) -- see `.bare()` below for those.
#' @keywords internal
.q <- function(x) paste0("\"", .escape_dai_string(as.character(x)), "\"")

#' Render a value as a bare (unquoted) Daisy token.
#'
#' Every real .dai script seen in this project writes Daisy's own fixed
#' vocabulary -- submodel/method selectors (`Devel default`, `hydraulic
#' Cosby_et_al`), type keywords (`Movement vertical`, `defprogram ... Daisy`),
#' and crop-parameter enum values (`water_stress_effect none`) -- as bare
#' symbols, never quoted strings. Numbers and booleans always render bare
#' regardless of caller. This is the counterpart to `.q()`: use `.q()` for
#' a name/reference, `.bare()` for a keyword/selector/enum value.
#' @keywords internal
.bare <- function(x) {
  if (is.logical(x)) return(if (isTRUE(x)) "true" else "false")
  if (is.numeric(x)) return(.fmt_num(x))
  as.character(x)
}

#' Format a plain number: integers print bare, non-integers keep their
#' decimals (e.g. `100` -> "100", `12.73` -> "12.73", `-1` -> "-1").
#' @keywords internal
.fmt_num <- function(x) {
  if (is.null(x)) return("")
  if (is.numeric(x) && isTRUE(x == round(x))) {
    as.character(as.integer(round(x)))
  } else {
    format(x, trim = TRUE, scientific = FALSE)
  }
}

#' Format a fractional quantity (harvest partition fractions, development
#' stage thresholds, stub heights) with at least one decimal place, e.g.
#' `1` -> "1.0", `0.85` -> "0.85", matching the style used throughout
#' dk-management.dai / dk-veg-man.dai.
#' @keywords internal
.fmt_frac <- function(x) {
  format(round(as.numeric(x), 10), nsmall = 1, trim = TRUE, scientific = FALSE)
}

#' Parse an `mm_dd` value into a list(month=, day=).
#'
#' Accepts `"MM-DD"`, `"YYYY-MM-DD"` (year is dropped -- Daisy management
#' activities describe a repeatable calendar, not a specific year), or a
#' 2-element list/vector `[M, D]`.
#' @keywords internal
parse_mm_dd <- function(x) {
  if (is.character(x)) {
    parts <- as.integer(strsplit(x, "-", fixed = TRUE)[[1]])
    if (length(parts) == 3) parts <- parts[2:3]
    if (length(parts) != 2 || anyNA(parts))
      stop("mm_dd must look like 'MM-DD' or 'YYYY-MM-DD', got: '", x, "'")
    return(list(month = parts[1], day = parts[2]))
  }
  if (is.list(x) || length(x) == 2) {
    return(list(month = as.integer(x[[1]]), day = as.integer(x[[2]])))
  }
  stop("Unrecognised mm_dd value: ", paste(x, collapse = ","))
}

## -- comments ------------------------------------------------------------

#' Render a `comment:` field (attached to almost any block throughout the
#' schema -- a field operation, an activity, a horizon, a column, a crop, a
#' program, ...) as `;;`-prefixed Daisy comment line(s), or `NULL` if no
#' comment was given. A multi-line string produces one `;;` line per line.
#' @keywords internal
render_comment_lines <- function(comment, indent = "") {
  if (is.null(comment) || !nzchar(comment)) return(NULL)
  lines <- strsplit(comment, "\n", fixed = TRUE)[[1]]
  paste(paste0(indent, ";; ", lines), collapse = "\n")
}

#' Prepend a `fields$comment` line (if present) to an already-rendered
#' block's lines. Used by every per-block renderer across the dai_*.R
#' files so `comment:` behaves identically everywhere it's allowed.
#' @keywords internal
.with_comment <- function(fields, indent, lines) {
  c_lines <- render_comment_lines(fields$comment, indent)
  if (is.null(c_lines)) lines else c(c_lines, lines)
}

#' Prepend a stored field/section comment to already-rendered text.
.dai_render_with_comment <- function(comment, indent, rendered) {
  c_lines <- render_comment_lines(comment, indent)
  if (is.null(c_lines)) return(rendered)
  if (is.null(rendered) || (is.character(rendered) && length(rendered) == 1 && !nzchar(rendered)))
    return(paste(c_lines, collapse = "\n"))
  paste(c(c_lines, rendered), collapse = "\n")
}

## -- date / wait-condition mini-language --------------------------------

#' Render a `date`/`condition` spec to its Daisy predicate text, e.g.
#' `(crop_ds_after "Spring Barley" 2.0 [])` or
#' `(or (crop_ds_after "X" 1.8 [])\n    (mm_dd 10 1))`.
#' Does NOT wrap in `(wait ...)` -- see `render_wait_from_date()` for that.
#'
#' Understands both the user-facing shorthand (a bare `"MM-DD"` string, or
#' `ds`/`mm_dd`/`days`/`any_of`/`all_of`/`not`/`crop_dm_over`/
#' `soil_water_pressure_above`/`raw`) and, recursively, its own expansion
#' of that shorthand (`crop_ds_after`) -- so `any_of`/`all_of`/`not` can
#' nest either form freely. `default_crop` (typically the enclosing
#' operation's own `crop` field) fills in `{ds: 1.8}` when it doesn't name
#' a crop explicitly, and is threaded through nested conditions too.
#' @keywords internal
render_condition <- function(spec, default_crop = NULL) {
  if (is.character(spec)) spec <- list(mm_dd = spec)

  if (!is.null(spec$raw)) return(spec$raw)

  if (!is.null(spec$crop_ds_after)) {
    c_ <- spec$crop_ds_after
    return(sprintf("(crop_ds_after %s %s [])", .q(c_$crop), .fmt_frac(c_$ds)))
  }
  if (!is.null(spec$crop_dm_over)) {
    c_ <- spec$crop_dm_over
    height_txt <- if (!is.null(c_$height))
      sprintf(" (height %s [%s])", .fmt_num(c_$height), c_$unit_height %||% "cm")
    else ""
    return(sprintf("(crop_dm_over %s %s [%s]%s)",
                    .q(c_$crop), .fmt_num(c_$amount), c_$unit %||% "kg DM/ha",
                    height_txt))
  }
  if (!is.null(spec$soil_water_pressure_above)) {
    c_ <- spec$soil_water_pressure_above
    return(sprintf(
      "(soil_water_pressure_above (height %s [%s]) (potential %s [%s]))",
      .fmt_num(c_$height), c_$unit_height %||% "cm",
      .fmt_num(c_$potential), c_$unit_potential %||% "cm"))
  }
  if (!is.null(spec$not)) {
    return(sprintf("(not %s)", render_condition(spec$not, default_crop)))
  }
  if (!is.null(spec$any_of)) {
    parts <- vapply(spec$any_of, render_condition, character(1), default_crop = default_crop)
    return(sprintf("(or %s)", paste(parts, collapse = "\n    ")))
  }
  if (!is.null(spec$all_of)) {
    parts <- vapply(spec$all_of, render_condition, character(1), default_crop = default_crop)
    return(sprintf("(and %s)", paste(parts, collapse = "\n    ")))
  }

  ## ds / mm_dd shorthand, possibly combined ("whichever comes first")
  ds <- spec$ds
  md <- spec$mm_dd
  crop <- spec$crop %||% default_crop
  if (!is.null(ds) && !is.null(md)) {
    if (is.null(crop))
      stop("A 'ds' date/condition needs a 'crop' -- set it on this field_operation ",
           "(e.g. sow/harvest's own 'crop'), or explicitly as date: {ds: .., crop: \"X\"}")
    ds_txt <- sprintf("(crop_ds_after %s %s [])", .q(crop), .fmt_frac(ds))
    md_parsed <- parse_mm_dd(md)
    md_txt <- sprintf("(mm_dd %d %d)", md_parsed$month, md_parsed$day)
    return(sprintf("(or %s)", paste(c(ds_txt, md_txt), collapse = "\n    ")))
  }
  if (!is.null(ds)) {
    if (is.null(crop))
      stop("A 'ds' date/condition needs a 'crop' -- set it on this field_operation ",
           "(e.g. sow/harvest's own 'crop'), or explicitly as date: {ds: .., crop: \"X\"}")
    return(sprintf("(crop_ds_after %s %s [])", .q(crop), .fmt_frac(ds)))
  }
  if (!is.null(md)) {
    md_parsed <- parse_mm_dd(md)
    return(sprintf("(mm_dd %d %d)", md_parsed$month, md_parsed$day))
  }
  stop("Unrecognised date/condition, expected one of mm_dd/ds/crop_ds_after/",
       "crop_dm_over/soil_water_pressure_above/not/any_of/all_of/raw, got: ",
       paste(names(spec), collapse = ", "))
}

#' Render the `(wait ...)` (or `(wait_mm_dd ..)` / `(wait_days ..)`) text
#' for a `date` field, or `NULL` if the field is absent (meaning: no wait,
#' run immediately after the previous field operation).
#' @keywords internal
render_wait_from_date <- function(date_spec, default_crop = NULL) {
  if (is.null(date_spec)) return(NULL)
  if (is.character(date_spec)) {
    md <- parse_mm_dd(date_spec)
    return(sprintf("(wait_mm_dd %d %d)", md$month, md$day))
  }
  other_keys <- setdiff(names(date_spec), c("mm_dd", "crop"))
  if (!is.null(date_spec$mm_dd) && length(other_keys) == 0) {
    md <- parse_mm_dd(date_spec$mm_dd)
    return(sprintf("(wait_mm_dd %d %d)", md$month, md$day))
  }
  if (!is.null(date_spec$days) && length(date_spec) == 1) {
    return(sprintf("(wait_days %s)", .fmt_num(date_spec$days)))
  }
  sprintf("(wait %s)", render_condition(date_spec, default_crop))
}

## -- generic nested-block mechanism --------------------------------------
##
## Used by dai_crops.R (defcrop overrides), dai_horizons.R (defhorizon
## parameters), and dai_columns.R (OrganicMatter and similar free-form
## sub-blocks) to pass Daisy's own parameter vocabulary straight through
## from YAML, instead of hardcoding a bespoke schema per construct.
##
## A YAML mapping's keys become Daisy parameter/sub-block names verbatim.
## Recognised value shapes:
##
##   NULL                      -> the key is skipped entirely
##   TRUE / FALSE               -> (key true) / (key false)
##   a bare number               -> (key 100)          -- no unit bracket
##   a bare string                -> (key none)          -- bare token, see
##                                    .bare() -- matches every real
##                                    defcrop/defhorizon parameter value
##                                    seen so far (never a quoted string)
##   {value: X, unit: "u"}       -> (key X [u])
##   {value: X, unit: ""}        -> (key X [])          -- explicitly
##                                    dimensionless, as distinct from a
##                                    bare number (no bracket at all)
##   a list of 2-item lists       -> (key (x1 y1)(x2 y2)...) -- a
##                                    piecewise-linear (PLF) table
##   [method, {overrides...}]     -> (key method (k1 v1)...) -- the
##                                    positional method/type keyword some
##                                    sub-blocks take right after their own
##                                    name (e.g. `Devel: [original, {DSRate2:
##                                    0.025}]` -> `(Devel original (DSRate2
##                                    0.025))`)
##   a nested mapping              -> (key (k1 v1)(k2 v2)...) -- recurses

#' @keywords internal
.is_plf_table <- function(value) {
  if (!is.list(value) || length(value) == 0) return(FALSE)
  if (!is.null(names(value)) && any(nzchar(names(value)))) return(FALSE)
  all(vapply(value, function(pt) is.null(names(pt)) && length(pt) == 2 &&
               is.numeric(unlist(pt)), logical(1)))
}

#' Render the parent/inheritance positional argument shared by `defcolumn`,
#' `defcrop`, and (for the inheriting-program case) `defprogram`: the bare
#' keyword `default` when no real parent is named (Daisy's own "use the
#' library default" convention, e.g. `(defcolumn "X" default ...)` /
#' `(defcrop "Spring Barley" default ...)`), or a quoted reference to
#' another named object otherwise (`(defprogram "west" "Common" ...)`).
#' @keywords internal
.render_parent_ref <- function(based_on) {
  if (is.null(based_on) || identical(based_on, "default")) "default" else .q(based_on)
}

#' @keywords internal
.is_method_pair <- function(value) {
  if (!is.list(value) || length(value) != 2) return(FALSE)
  if (!is.null(names(value)) && any(nzchar(names(value)))) return(FALSE)
  is.character(value[[1]]) && length(value[[1]]) == 1 &&
    is.list(value[[2]]) && !is.null(names(value[[2]]))
}

#' @keywords internal
.is_value_unit_pair <- function(value) {
  is.list(value) && !is.null(names(value)) &&
    setequal(names(value), c("value", "unit")) && !is.null(value$value)
}

#' Render a single `key: value` pair from a generic parameter block to its
#' Daisy text (one or more lines, un-indented at the left -- the caller is
#' responsible for placing it inside its enclosing block).
#' @keywords internal
render_generic_value <- function(key, value, indent = "") {
  if (is.null(value)) return(NULL)

  ## `description` is a recognised cross-cutting field (appears on both
  ## defprogram and defcrop) that is genuine free text, e.g.
  ## `(description "RS-Model Projekt")` -- always quoted, unlike ordinary
  ## generic parameter values (see .bare()'s doc comment).
  if (identical(key, "description") && is.character(value) && length(value) == 1) {
    return(sprintf("%s(%s %s)", indent, key, .q(value)))
  }

  if (is.logical(value) && length(value) == 1) {
    return(sprintf("%s(%s %s)", indent, key, .bare(value)))
  }

  if (.is_plf_table(value)) {
    ## .fmt_frac (not .fmt_num) so whole-number breakpoints still print
    ## with a decimal (e.g. "0.0 1.0", not "0 1"), matching the style of
    ## every real PLF table seen in this project's .dai files -- purely
    ## cosmetic (Daisy parses either form identically) but keeps generated
    ## crop tables looking like hand-written ones.
    pairs <- vapply(value, function(pt)
      sprintf("(%s %s)", .fmt_frac(pt[[1]]), .fmt_frac(pt[[2]])), character(1))
    return(sprintf("%s(%s %s)", indent, key, paste(pairs, collapse = "")))
  }

  if (.is_method_pair(value)) {
    method <- value[[1]]
    inner <- render_generic_block_body(value[[2]], "")
    return(sprintf("%s(%s %s%s)", indent, key, method,
                    if (nzchar(inner)) paste0(" ", inner) else ""))
  }

  if (.is_value_unit_pair(value)) {
    v_txt <- if (is.character(value$value)) .bare(value$value) else .fmt_num(value$value)
    unit <- value$unit
    if (is.null(unit)) return(sprintf("%s(%s %s)", indent, key, v_txt))
    return(sprintf("%s(%s %s [%s])", indent, key, v_txt, unit))
  }

  if (is.list(value) && length(value) > 0 && !is.null(names(value)) &&
      all(nzchar(names(value)))) {
    inner <- render_generic_block_body(value, "")
    return(sprintf("%s(%s%s)", indent, key,
                    if (nzchar(inner)) paste0(" ", inner) else ""))
  }

  ## bare scalar leaf (number, or a keyword-like string -- never quoted;
  ## see .bare()'s doc comment for why)
  sprintf("%s(%s %s)", indent, key, .bare(value))
}

#' Render every `key: value` pair of a generic parameter block (a named
#' list -- typically one nesting level of a `defcrop`/`defhorizon` entry,
#' or the contents of a `[method, {...}]` pair / nested mapping) to a
#' single space-joined string of `(key value)` forms, skipping the
#' reserved `comment` key (handled separately by callers via
#' `.with_comment()`, not passed through generically). Per-field comments
#' recovered by `parse_dai()` live on `field_comments` and are emitted
#' immediately before the matching `(key ...)` form.
#' @keywords internal
render_generic_block_body <- function(fields, indent = "") {
  comments <- fields$field_comments %||% list()
  trailing <- comments[[".trailing"]]
  comments <- comments[names(comments) != ".trailing"]
  fields <- fields[!names(fields) %in% c("comment", "field_comments")]
  if (length(fields) == 0) {
    return(if (is.null(trailing)) "" else paste(render_comment_lines(trailing, indent), collapse = "\n"))
  }
  parts <- Map(function(k, v) {
    body <- render_generic_value(k, v, indent)
    .dai_render_with_comment(comments[[k]], indent, body)
  }, names(fields), fields)
  parts <- parts[!vapply(parts, is.null, logical(1))]
  out <- paste(unlist(parts), collapse = if (nzchar(indent)) "\n" else "")
  if (!is.null(trailing))
    out <- paste(c(out, render_comment_lines(trailing, indent)), collapse = "\n")
  out
}

## dai_helpers.R ends here.
