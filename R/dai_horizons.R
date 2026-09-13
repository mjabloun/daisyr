## dai_horizons.R -- Generate Daisy `defhorizon` (soil horizon) blocks from
## YAML, via the generic nested-block mechanism in dai_helpers.R.
##
## Requires dai_helpers.R to already be sourced (`.q`, `.bare`,
## `render_generic_block_body`, `.with_comment`).
##
## ---------------------------------------------------------------------
## YAML schema
## ---------------------------------------------------------------------
## horizons:                         # one or more soil horizons
##   - name: "Hor_25"
##     texture_class: FAO3           # bare keyword, e.g. FAO3/ISSS4/USDA3
##     comment: "optional free text" # -> a ;; comment line before (defhorizon ...)
##
##     ## Everything else is passed straight through to Daisy's own
##     ## parameter vocabulary via the generic mechanism (dai_helpers.R):
##     ##   a bare number/string -> (key value), no unit bracket
##     ##   {value: X, unit: "u"} -> (key X [u])
##     ##   {value: X, unit: ""}  -> (key X [])  -- explicitly dimensionless
##     ##   [method, {overrides}] -> (key method (ov1 ..)...) -- e.g. a
##     ##                            `hydraulic` method that itself takes
##     ##                            parameters, not just a bare name
##     dry_bulk_density: {value: 1.46, unit: "g/cm^3"}
##     clay: {value: 0.22, unit: ""}
##     silt: {value: 0.27, unit: ""}
##     sand: {value: 0.51, unit: ""}
##     humus: {value: 0.0722, unit: ""}
##     hydraulic: Cosby_et_al         # bare method name, no parameters --
##                                    # or [MethodName, {param: value, ...}]
##                                    # if the method itself takes parameters
##
## Renders to, e.g.:
##   (defhorizon "Hor_25" FAO3
##     (dry_bulk_density 1.46 [g/cm^3])
##     (clay 0.22 [])
##     (silt 0.27 [])
##     (sand 0.51 [])
##     (humus 0.0722 [])
##     (hydraulic Cosby_et_al)
##   )
## ---------------------------------------------------------------------

#' Render one `horizons[[i]]` entry to a complete `(defhorizon ...)` block.
#' @keywords internal
render_horizon <- function(cfg) {
  if (is.null(cfg$name)) stop("A horizon is missing its 'name' field")
  texture <- cfg$texture_class
  if (is.null(texture))
    stop("Horizon '", cfg$name, "' is missing 'texture_class' (e.g. FAO3, ISSS4, USDA3)")

  params <- cfg[!names(cfg) %in% c("name", "texture_class", "comment")]
  body <- render_generic_block_body(params, "  ")

  block <- if (nzchar(body))
    sprintf("(defhorizon %s %s\n%s\n)", .q(cfg$name), .bare(texture), body)
  else
    sprintf("(defhorizon %s %s)", .q(cfg$name), .bare(texture))

  paste(.with_comment(cfg, "", block), collapse = "\n")
}

#' Parse a `(defhorizon name texture [description] field...)` form's body
#' children (i.e. everything after the leading `defhorizon` keyword atom)
#' back into one `horizons[[i]]` entry -- the inverse of `render_horizon()`,
#' also accepting a bare (unquoted) name and/or Daisy's optional positional
#' description string (see dai_parse_helpers.R). The block's own `comment`
#' field, if any, is attached by the caller (`parse_dai()`, dai_read.R),
#' not here -- see that file for why.
#' @keywords internal
parse_horizon <- function(body_children) {
  if (length(body_children) < 2 ||
      !(body_children[[1]]$kind %in% c("atom", "string")) || body_children[[2]]$kind != "atom")
    stop("dai parse error: a defhorizon needs a name and a bare texture_class")
  name <- .dai_parse_name_node(body_children[[1]])
  texture_class <- body_children[[2]]$text
  desc <- .dai_consume_positional_description(body_children[-(1:2)])
  fields <- .dai_parse_generic_block_body(desc$rest)
  out <- list(name = name, texture_class = texture_class)
  if (!is.null(desc$description)) out$description <- desc$description
  c(out, fields)
}

## dai_horizons.R ends here.
