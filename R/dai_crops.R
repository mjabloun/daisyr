## dai_crops.R -- Generate Daisy `defcrop` blocks from YAML, via the
## generic nested-block mechanism in dai_helpers.R.
##
## Only the *derived*-crop case is supported: a name, a parent crop to
## inherit from, and a set of overridden parameters (flat, or nested
## sub-blocks like `Devel`/`Partit`). A full from-scratch base crop
## definition (dozens of parameters across nine sub-blocks -- see
## sbarley.dai's base "Spring Barley") is out of scope; getting that wrong
## silently would be worse than not supporting it, and every real use case
## seen so far is a small override on top of an existing crop.
##
## Requires dai_helpers.R to already be sourced (`.q`,
## `render_generic_block_body`, `.render_parent_ref`, `.with_comment`).
##
## ---------------------------------------------------------------------
## YAML schema
## ---------------------------------------------------------------------
## crops:                            # one or more derived crops
##   - name: "Grass Scotland"
##     based_on: "Grass to grain"    # the parent crop to inherit from --
##                                    # or "default" (the library default)
##     comment: "optional free text" # -> a ;; comment line before (defcrop ...)
##
##     ## Everything else reuses Daisy's own parameter/sub-block names
##     ## verbatim (the generic mechanism in dai_helpers.R):
##     enable_N_stress: false
##     water_stress_effect: none
##
## A nested sub-block override, using the same mechanism recursively:
##   crops:
##     - name: "Scot Barley"
##       based_on: "Spring Barley"
##       Devel: [original, {DSRate2: 0.025}]   # positional method + overrides
##       Partit:
##         Leaf: [[0.00, 1.00], [0.25, 0.70], [0.51, 0.55], [0.60, 0.50],
##                [0.72, 0.23], [0.83, 0.01], [0.95, 0.00], [2.00, 0.00]]
##
## Renders to, e.g.:
##   (defcrop "Grass Scotland" "Grass to grain"
##     (enable_N_stress false)
##     (water_stress_effect none)
##   )
##
##   (defcrop "Scot Barley" "Spring Barley"
##     (Devel original (DSRate2 0.025))
##     (Partit (Leaf (0.00 1.00)(0.25 0.70)(0.51 0.55)(0.60 0.50)(0.72 0.23)(0.83 0.01)(0.95 0.00)(2.00 0.00)))
##   )
## ---------------------------------------------------------------------

#' Render one `crops[[i]]` entry to a complete `(defcrop ...)` block.
#' @keywords internal
render_crop <- function(cfg) {
  if (is.null(cfg$name)) stop("A crop is missing its 'name' field")
  parent <- .render_parent_ref(cfg$based_on)

  overrides <- cfg[!names(cfg) %in% c("name", "based_on", "comment")]
  body <- render_generic_block_body(overrides, "  ")

  block <- if (nzchar(body))
    sprintf("(defcrop %s %s\n%s\n)", .q(cfg$name), parent, body)
  else
    sprintf("(defcrop %s %s)", .q(cfg$name), parent)

  paste(.with_comment(cfg, "", block), collapse = "\n")
}

#' Parse a `(defcrop name parent [description] field...)` form's body
#' children (i.e. everything after the leading `defcrop` keyword atom)
#' back into one `crops[[i]]` entry -- the inverse of `render_crop()`,
#' also accepting a bare (unquoted) name and/or Daisy's optional
#' positional description string (see dai_parse_helpers.R). The block's
#' own `comment` field, if any, is attached by the caller (`parse_dai()`,
#' dai_read.R), not here.
#' @keywords internal
parse_crop <- function(body_children) {
  if (length(body_children) < 2 || !(body_children[[1]]$kind %in% c("atom", "string")))
    stop("dai parse error: a defcrop needs a name and a parent reference")
  name <- .dai_parse_name_node(body_children[[1]])
  based_on <- .dai_parse_parent_ref(body_children[[2]])
  desc <- .dai_consume_positional_description(body_children[-(1:2)])
  overrides <- .dai_parse_generic_block_body(desc$rest)

  cfg <- list(name = name)
  if (!is.null(based_on)) cfg$based_on <- based_on
  if (!is.null(desc$description)) cfg$description <- desc$description
  c(cfg, overrides)
}

## dai_crops.R ends here.
