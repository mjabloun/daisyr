#' Format one scalar `parameters:` entry as YAML text
#' @keywords internal
.scaffold_scalar_entry <- function(name) {
  sprintf(
'  - name: %s
    from_file: REPLACE_ME.dai   # TODO: template file containing a {{%s}} placeholder
    to_file: REPLACE_ME.dai     # TODO: file Daisy will actually read
    default: 0                  # TODO: default/starting value
    min: 0                      # TODO: lower bound
    max: 1                      # TODO: upper bound
',
    name, name)
}

#' Format the `parameters:` entries for a plf curve's shape parameters
#' @keywords internal
.scaffold_plf_shape_entries <- function(name, curve) {
  arg_names <- .plf_curve_arg_names(curve)
  shape_names <- paste0(name, "_", arg_names)
  paste0(vapply(shape_names, function(sn) {
    sprintf(
'  - name: %s
    default: 1   # TODO: default/starting value
    min: 0       # TODO: lower bound
    max: 2       # TODO: upper bound
',
      sn)
  }, character(1)), collapse = "")
}

#' Format one `plf_curves:` entry as YAML text
#' @keywords internal
.scaffold_plf_curve_entry <- function(name, curve) {
  arg_names <- .plf_curve_arg_names(curve)
  shape_names <- paste0(name, "_", arg_names)
  sprintf(
'  - name: %s
    from_file: REPLACE_ME.dai   # TODO: template file containing a {{%s_BLOCK}} placeholder
    to_file: REPLACE_ME.dai     # TODO: file Daisy will actually read
    placeholder: %s_BLOCK
    x_values: [0, 1]             # TODO: fixed x-knots (strictly increasing), never calibrated
    curve: %s                    # one of: %s
    params: [%s]                 # order matches the curve; see ?render_plf_curve
',
    name, name, name, curve, paste(known_plf_curves(), collapse = ", "), paste(shape_names, collapse = ", "))
}

#' Create a starter parameters YAML file
#'
#' Scaffolds a YAML parameters file with one stub entry per name in
#' `parameters`, so you don't have to write the config schema from
#' scratch by hand - fill in the `REPLACE_ME.dai`/bounds placeholders it
#' leaves behind (marked `# TODO`), then load it with
#' [read_param_config()] and check it with [validate_param_config()]. See
#' `vignette("daisyr")` for the full schema.
#'
#' @param parameters Character vector of parameter (or PLF) names.
#' @param type Character scalar or vector, recycled to `length(parameters)`.
#'   Either `"scalar"` (a plain calibration/SA parameter, substituted
#'   directly into a `{{name}}` placeholder) or `"plf"` (a piecewise-linear
#'   function generated from a curve family's shape parameters, scaffolding
#'   both the shape parameters and the `plf_curves` entry that consumes
#'   them - see `curve`).
#' @param curve Character scalar or vector, recycled to `length(parameters)`.
#'   One of [known_plf_curves()]. Only used for entries with `type = "plf"`;
#'   determines which shape parameters get scaffolded (e.g. `L`/`k`/`x0` for
#'   `"logistic"`).
#' @param path Character scalar, optional. If given, writes the YAML text to
#'   this file and returns the path invisibly. Otherwise returns the YAML
#'   text as a character scalar (e.g. to `cat()` or edit further before
#'   writing).
#'
#' @return Either `path` (invisibly, if given) or the assembled YAML text.
#'
#' @examples
#' cat(create_param_config(c("Ap_clay", "LAIvsDS"), type = c("scalar", "plf")))
#' @export
create_param_config <- function(parameters, type = "scalar", curve = "logistic", path = NULL) {
  n <- length(parameters)
  if (n == 0) stop("`parameters` must have at least one name")
  if (anyDuplicated(parameters) > 0)
    stop("`parameters` must not contain duplicate names: ",
         paste(unique(parameters[duplicated(parameters)]), collapse = ", "))

  type <- rep(type, length.out = n)
  curve <- rep(curve, length.out = n)
  bad_type <- setdiff(type, c("scalar", "plf"))
  if (length(bad_type) > 0)
    stop("`type` must be 'scalar' or 'plf', got: ", paste(bad_type, collapse = ", "))

  scalar_entries <- character(0)
  plf_shape_entries <- character(0)
  plf_curve_entries <- character(0)

  for (i in seq_len(n)) {
    nm <- parameters[i]
    if (type[i] == "scalar") {
      scalar_entries <- c(scalar_entries, .scaffold_scalar_entry(nm))
    } else {
      plf_shape_entries <- c(plf_shape_entries, .scaffold_plf_shape_entries(nm, curve[i]))
      plf_curve_entries <- c(plf_curve_entries, .scaffold_plf_curve_entry(nm, curve[i]))
    }
  }

  yaml_txt <- paste0(
    "parameters:\n",
    paste0(c(scalar_entries, plf_shape_entries), collapse = ""),
    if (length(plf_curve_entries) > 0) paste0("\nplf_curves:\n", paste0(plf_curve_entries, collapse = "")) else ""
  )

  if (!is.null(path)) {
    writeLines(yaml_txt, path)
    return(invisible(path))
  }
  yaml_txt
}
