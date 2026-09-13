## dai_columns.R -- Generate Daisy `defcolumn` (soil column) blocks from
## YAML.
##
## `Soil` (rooting depth + horizon references) and `Movement` (the 1D
## `vertical` or 2D `rectangle` geometry grid) have their own fixed shapes
## and get bespoke renderers below. Everything else a column can carry
## (`Groundwater`, `OrganicMatter`, and any other sub-block a future script
## turns out to need) is passed straight through via the generic
## nested-block mechanism in dai_helpers.R -- no separate schema per field.
##
## Requires dai_helpers.R to already be sourced (`.q`, `.bare`, `.fmt_num`,
## `%||%`, `render_generic_value`, `render_generic_block_body`,
## `.render_parent_ref`, `.with_comment`).
##
## ---------------------------------------------------------------------
## YAML schema
## ---------------------------------------------------------------------
## columns:                          # one or more soil columns
##   - name: "Soil_Column"
##     based_on: default             # optional, defaults to "default" (the
##                                    # library base column) -- name another
##                                    # column to derive from it instead
##     comment: "optional free text" # -> a ;; comment line before (defcolumn ...)
##
##     Soil:
##       MaxRootingDepth: {value: 100, unit: "cm"}
##       horizons:                   # references the `horizons` list --
##                                    # depth (negative, downward) + name
##         - {depth: -25, unit: "cm", horizon: "Hor_25"}
##         - {depth: -50, unit: "cm", horizon: "Hor_50"}
##         - {depth: -72, unit: "cm", horizon: "Hor_72"}
##         - {depth: -100, unit: "cm", horizon: "Hor_100"}
##
##     Movement:
##       type: vertical              # "vertical" (1D, zplus only) or
##                                    # "rectangle" (2D, zplus + xplus)
##       zplus: [-2.5, -5, -10, -15, -25, -40, -50, -60, -72, -80, -90, -100]
##       xplus: [0, 50, 100]         # only for type: rectangle
##
##     Groundwater: deep             # generic passthrough (see dai_helpers.R)
##
##     OrganicMatter:                # optional, generic passthrough --
##       ...                         # any Daisy OrganicMatter parameters
##
## Renders to, e.g.:
##   (defcolumn "Soil_Column" default
##     (Soil (MaxRootingDepth 100 [cm])
##       (horizons (-25 [cm] "Hor_25")(-50 [cm] "Hor_50")(-72 [cm] "Hor_72")(-100 [cm] "Hor_100")))
##     (Movement vertical
##       (Geometry (zplus -2.5 -5 -10 -15 -25 -40 -50 -60 -72 -80 -90 -100)))
##     (Groundwater deep)
##   )
## ---------------------------------------------------------------------

#' @keywords internal
render_soil_block <- function(soil, indent) {
  if (is.null(soil)) return(NULL)
  mrd_txt <- if (!is.null(soil$MaxRootingDepth))
    render_generic_value("MaxRootingDepth", soil$MaxRootingDepth, "")
  else
    stop("Soil requires 'MaxRootingDepth'")

  horizons <- soil$horizons %||% list()
  if (length(horizons) == 0) stop("Soil requires at least one entry under 'horizons'")
  hz_txt <- paste(vapply(horizons, function(h) {
    if (is.null(h$depth) || is.null(h$horizon))
      stop("Each Soil horizon reference needs 'depth' and 'horizon'")
    sprintf("(%s [%s] %s)", .fmt_num(h$depth), h$unit %||% "cm", .q(h$horizon))
  }, character(1)), collapse = "")

  ## Any other recognized Soil parameter (e.g. `border`, read back from a
  ## real hand-written .dai file by parse_soil_block()) is passed through
  ## generically -- not silently dropped -- via the same mechanism
  ## render_column() uses for a column's own extra fields.
  extra <- soil[!names(soil) %in% c("MaxRootingDepth", "horizons", "field_comments")]
  fc <- soil$field_comments %||% list()
  extra$field_comments <- fc[setdiff(names(fc), c("MaxRootingDepth", "horizons"))]
  extra_body <- render_generic_block_body(extra, paste0(indent, "  "))
  extra_txt <- if (nzchar(extra_body)) paste0("\n", extra_body) else ""

  mrd_txt <- .dai_render_with_comment(fc$MaxRootingDepth, paste0(indent, "  "), mrd_txt)
  hz_line <- .dai_render_with_comment(fc$horizons, paste0(indent, "  "),
                                     sprintf("%s  (horizons %s)", indent, hz_txt))

  sprintf("%s(Soil %s\n%s%s)", indent, mrd_txt, hz_line, extra_txt)
}

#' @keywords internal
render_movement_block <- function(mv, indent) {
  if (is.null(mv)) return(NULL)
  type <- mv$type
  if (is.null(type)) stop("Movement requires 'type' (vertical or rectangle)")

  geom_parts <- character(0)
  if (!is.null(mv$zplus))
    geom_parts <- c(geom_parts, sprintf("(zplus %s)",
      paste(vapply(mv$zplus, .fmt_num, character(1)), collapse = " ")))
  if (!is.null(mv$xplus))
    geom_parts <- c(geom_parts, sprintf("(xplus %s)",
      paste(vapply(mv$xplus, .fmt_num, character(1)), collapse = " ")))
  if (length(geom_parts) == 0)
    stop("Movement requires at least a 'zplus' grid")

  sprintf("%s(Movement %s\n%s  (Geometry %s))", indent, .bare(type), indent,
          paste(geom_parts, collapse = " "))
}

#' Render one `columns[[i]]` entry to a complete `(defcolumn ...)` block.
#' @keywords internal
render_column <- function(cfg) {
  if (is.null(cfg$name)) stop("A column is missing its 'name' field")
  parent <- .render_parent_ref(cfg$based_on)

  known <- c("name", "based_on", "comment", "Soil", "Movement", "field_comments")
  extra <- cfg[!names(cfg) %in% known]
  fc <- cfg$field_comments %||% list()
  extra$field_comments <- fc[setdiff(names(fc), c("Soil", "Movement"))]

  parts <- c(
    .dai_render_with_comment(fc$Soil, "  ", render_soil_block(cfg$Soil, "  ")),
    .dai_render_with_comment(fc$Movement, "  ", render_movement_block(cfg$Movement, "  "))
  )
  extra_body <- render_generic_block_body(extra, "  ")
  if (nzchar(extra_body)) parts <- c(parts, extra_body)
  parts <- parts[!vapply(parts, is.null, logical(1))]

  block <- sprintf("(defcolumn %s %s\n%s\n)", .q(cfg$name), parent, paste(parts, collapse = "\n"))
  paste(.with_comment(cfg, "", block), collapse = "\n")
}

## -- parsing (the inverse direction) --------------------------------------
##
## `Soil` and `Movement` have their own fixed shapes above (not the generic
## mechanism), so they get their own bespoke inverses here too, mirroring
## render_soil_block()/render_movement_block() one for one.

#' Parse one entry of a `horizons` reference list -- accepts both the
#' fixed `(depth [unit] "name")` shape `render_soil_block()` always
#' produces, and Daisy's own more flexible `(depth name)` shape (the unit
#' defaults to "cm" when omitted, matching `render_soil_block()`'s own
#' `%||% "cm"` default), with the horizon name as either a bare symbol or
#' a quoted string.
#' @keywords internal
.dai_parse_soil_horizon_ref <- function(ref) {
  if (ref$kind != "list" || length(ref$children) < 2 || ref$children[[1]]$kind != "atom" ||
      !.dai_looks_numeric(ref$children[[1]]$text))
    stop("dai parse error: each Soil horizon reference must be (depth [unit] horizon) or (depth horizon)")
  ch <- ref$children
  depth <- as.numeric(ch[[1]]$text)
  if (length(ch) == 3 && ch[[2]]$kind == "bracket" && ch[[3]]$kind %in% c("atom", "string"))
    return(list(depth = depth, unit = ch[[2]]$text, horizon = ch[[3]]$text))
  if (length(ch) == 2 && ch[[2]]$kind %in% c("atom", "string"))
    return(list(depth = depth, horizon = ch[[2]]$text))
  stop("dai parse error: unrecognized Soil horizon reference shape")
}

#' Parse a `(Soil ...)` form's children (i.e. everything after the leading
#' `Soil` keyword atom) back into `cfg$Soil` -- the inverse of
#' `render_soil_block()`, generalized to also accept Daisy's own more
#' flexible Soil shape as seen in real hand-written `.dai` files:
#' `MaxRootingDepth`/`horizons`/other sub-blocks in any order, an optional
#' per-point unit on `horizons` entries, and any other recognized Soil
#' parameter (e.g. `border`) passed through generically rather than
#' rejected.
#' @keywords internal
parse_soil_block <- function(children) {
  cfg <- list()
  extra <- list()
  split <- .dai_split_commented_items(children)
  field_comments <- list()
  for (it in split$items) {
    child <- it$node
    if (!.dai_is_field_form(child))
      stop("dai parse error: unrecognized entry inside Soil")
    key <- child$children[[1]]$text
    field_comments <- .dai_set_field_comment(field_comments, key, it$comment)
    if (identical(key, "MaxRootingDepth")) {
      cfg$MaxRootingDepth <- .dai_parse_generic_rest(child$children[-1])
    } else if (identical(key, "horizons")) {
      cfg$horizons <- lapply(child$children[-1], .dai_parse_soil_horizon_ref)
    } else {
      field <- .dai_parse_field_form(child)
      extra[[field$key]] <- field$value
    }
  }
  if (!is.null(split$trailing))
    field_comments <- .dai_set_field_comment(field_comments, ".trailing", split$trailing)
  if (is.null(cfg$MaxRootingDepth)) stop("dai parse error: Soil requires a MaxRootingDepth field")
  if (is.null(cfg$horizons)) stop("dai parse error: Soil requires a horizons field")
  if (length(field_comments) > 0) cfg$field_comments <- field_comments
  c(cfg, extra)
}

#' Parse a `(Movement type (Geometry (zplus ...)(xplus ...)))` form's
#' children (i.e. everything after the leading `Movement` keyword atom)
#' back into `cfg$Movement` -- the inverse of `render_movement_block()`.
#' @keywords internal
parse_movement_block <- function(children) {
  if (length(children) != 2 || children[[1]]$kind != "atom" || children[[2]]$kind != "list")
    stop("dai parse error: Movement requires a bare type and a Geometry form")

  type <- children[[1]]$text
  geom_node <- children[[2]]
  if (geom_node$children[[1]]$text != "Geometry")
    stop("dai parse error: Movement's sub-block must be Geometry")

  out <- list(type = type)
  for (g in .dai_without_comments(geom_node$children[-1])) {
    if (g$kind != "list" || g$children[[1]]$kind != "atom")
      stop("dai parse error: unrecognized entry inside Movement's Geometry")
    key <- g$children[[1]]$text
    out[[key]] <- as.numeric(vapply(g$children[-1], function(c2) c2$text, character(1)))
  }
  out
}

#' Parse a `(defcolumn name parent [description] field...)` form's body
#' children (i.e. everything after the leading `defcolumn` keyword atom)
#' back into one `columns[[i]]` entry -- the inverse of `render_column()`,
#' also accepting a bare (unquoted) name and/or Daisy's optional
#' positional description string (see dai_parse_helpers.R). The block's
#' own `comment` field, if any, is attached by the caller (`parse_dai()`,
#' dai_read.R), not here.
#' @keywords internal
parse_column <- function(body_children) {
  if (length(body_children) < 2 || !(body_children[[1]]$kind %in% c("atom", "string")))
    stop("dai parse error: a defcolumn needs a name and a parent reference")
  name <- .dai_parse_name_node(body_children[[1]])
  based_on <- .dai_parse_parent_ref(body_children[[2]])

  cfg <- list(name = name)
  if (!is.null(based_on)) cfg$based_on <- based_on

  desc <- .dai_consume_positional_description(body_children[-(1:2)])
  if (!is.null(desc$description)) cfg$description <- desc$description

  split <- .dai_split_commented_items(desc$rest)
  field_comments <- list()
  for (it in split$items) {
    nd <- it$node
    if (nd$kind != "list" || nd$children[[1]]$kind != "atom")
      stop("dai parse error: unrecognized entry inside defcolumn")
    key <- nd$children[[1]]$text
    field_comments <- .dai_set_field_comment(field_comments, key, it$comment)
    if (identical(key, "Soil")) {
      cfg$Soil <- parse_soil_block(nd$children[-1])
    } else if (identical(key, "Movement")) {
      cfg$Movement <- parse_movement_block(nd$children[-1])
    } else {
      field <- .dai_parse_field_form(nd)
      cfg[[field$key]] <- field$value
    }
  }
  if (!is.null(split$trailing))
    field_comments <- .dai_set_field_comment(field_comments, ".trailing", split$trailing)
  if (length(field_comments) > 0) cfg$field_comments <- field_comments
  cfg
}

## dai_columns.R ends here.
