#' Placeholder delimiters used by [render_one_template()]
#'
#' Double braces (`{{name}}`, Mustache-style) rather than single braces,
#' because Daisy's own `.dai` syntax uses single-brace-free `${...}` for
#' internal self-reference substitution (e.g. `(path column "${column}" ...)`
#' inside a `deflog`) - using single-brace `{name}` placeholders would
#' collide with that and corrupt such lines.
#' @keywords internal
.placeholder_open <- "{{"
#' @rdname dot-placeholder_open
#' @keywords internal
.placeholder_close <- "}}"

#' Render a single .dai template given a set of raw substitutions
#'
#' Thin wrapper around [glue::glue_data()], kept as its own function (mirrors
#' the original `f.update()` helper) so the substitution mechanism is defined
#' in exactly one place. Placeholders in the template must be written as
#' `{{name}}` (see [.placeholder_open]) so they don't collide with Daisy's own
#' `${...}` self-reference syntax.
#'
#' @param subs Named list/vector of placeholder -> replacement text.
#' @param from_file Character scalar. Template file to read.
#' @param to_file Character scalar. Destination file to write the rendered
#'   text to.
#' @return Invisibly, the rendered text.
#' @keywords internal
render_one_template <- function(subs, from_file, to_file) {
  txt <- readLines(from_file, warn = FALSE)
  rendered <- as.character(glue::glue_data(subs, paste(txt, collapse = "\n"),
                                            .open = .placeholder_open, .close = .placeholder_close))
  writeLines(rendered, to_file)
  invisible(rendered)
}

#' Format a PLF's (x y) sequence as Daisy syntax
#'
#' Exposed as a standalone helper (in addition to being used internally by
#' [render_templates()] for `plf_curves` entries) since it's useful on its
#' own when constructing/inspecting a PLF block manually, e.g. in
#' documentation or interactive exploration of a candidate curve.
#'
#' @param x Numeric vector of knots.
#' @param y Numeric vector of values, same length as `x`.
#' @param format `sprintf()` format string applied to each `y`.
#' @return Character scalar, e.g. `"(-0.3 0.000) (0.0 0.500) (1.0 4.870)"`.
#' @export
format_plf_block <- function(x, y, format = "%.4f") {
  paste(sprintf(paste0("(%g ", format, ")"), x, y), collapse = " ")
}

#' Render all substitutions implied by a config for a given parameter vector
#'
#' Given a full set of current values for every free parameter in `config`
#' ([param_config_names()]), computes: (a) the direct placeholder
#' substitutions, and (b) the rendered `(x y)` text block for every
#' `plf_curves` entry (by evaluating its curve at the fixed `x_values` using
#' the current values of its shape parameters).
#'
#' @param config A `daisyr_param_config` object.
#' @param values Named numeric vector covering every name in
#'   [param_config_names()].
#'
#' @return A list with one element per output file-pair (keyed by
#'   `"from_file -> to_file"`), each itself a named list of placeholder ->
#'   replacement text ready to hand to [render_one_template()].
#' @keywords internal
build_substitutions <- function(config, values) {
  needed <- param_config_names(config)
  missing <- setdiff(needed, names(values))
  if (length(missing) > 0)
    stop("Missing value(s) for parameter(s): ", paste(missing, collapse = ", "))

  groups <- list()
  add_sub <- function(from_file, to_file, placeholder, text) {
    key <- paste(from_file, "->", to_file)
    if (is.null(groups[[key]])) groups[[key]] <<- list(from_file = from_file, to_file = to_file, subs = list())
    groups[[key]]$subs[[placeholder]] <<- text
  }

  direct <- config$parameters[config$parameters$role == "direct", ]
  for (i in seq_len(nrow(direct))) {
    add_sub(direct$from_file[i], direct$to_file[i], direct$name[i], format(values[[direct$name[i]]]))
  }

  for (pc in config$plf_curves) {
    y <- render_plf_curve(pc$curve, pc$x_values, values[pc$params])
    block <- format_plf_block(pc$x_values, y, pc$format)
    add_sub(pc$from_file, pc$to_file, pc$placeholder, block)
  }

  groups
}

#' Render every `.dai` template implied by the parameters file
#'
#' For every `(from_file, to_file)` pair referenced in `config`, reads the
#' template, substitutes every declared placeholder (direct parameters
#' verbatim, `plf_curves` blocks computed from their shape parameters), and
#' writes the result. This is the calibration/SA equivalent of the original
#' `updateParameters()`/`f.update()` pair, generalised to support multiple
#' template/target file pairs and PLF curve generation in one call.
#'
#' @param config A `daisyr_param_config` object, as returned by
#'   [read_param_config()].
#' @param values Named numeric vector (or list) with one entry per name in
#'   [param_config_names()] - i.e. every direct parameter and every
#'   `plf_curves` shape parameter.
#' @param template_dir Character scalar. Directory `from_file` paths are
#'   relative to. Defaults to the current directory.
#' @param output_dir Character scalar. Directory `to_file` paths are written
#'   relative to. Defaults to the current directory.
#'
#' @return Invisibly, a character vector of the `to_file` paths written.
#' @export
render_templates <- function(config, values, template_dir = ".", output_dir = ".") {
  values <- unlist(values)
  groups <- build_substitutions(config, values)

  written <- vapply(groups, function(g) {
    from_path <- file.path(template_dir, g$from_file)
    to_path <- file.path(output_dir, g$to_file)
    render_one_template(g$subs, from_path, to_path)
    to_path
  }, character(1))

  invisible(unname(written))
}
