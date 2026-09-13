#' Read free parameters from a YAML file
#'
#' The file describes every free parameter used for calibration and/or
#' sensitivity analysis, including where it lives in the `.dai` templates and
#' (for piecewise linear function shape parameters) how it feeds into a
#' `plf_curves` entry (see the `curve` argument of [render_plf_curve()]).
#'
#' See `vignette("daisyr", package = "daisyr")` for a worked example.
#' In short, the YAML file has two top-level keys:
#'
#' \describe{
#'   \item{`parameters`}{A list of scalar parameters. Each entry has `name`,
#'     `default`, `min`, `max`, and either (a) `from_file`/`to_file` for a
#'     parameter substituted directly into a `{{name}}` placeholder in a
#'     template, optionally with `plf` metadata (`name`/`index`) documenting
#'     that it corresponds to one point of a PLF, or (b) no `from_file`, in
#'     which case it must be referenced by a `plf_curves` entry's `params`.}
#'   \item{`plf_curves`}{A list of piecewise-linear-function generators (see
#'     [render_plf_curve()]). Each
#'     entry has `name`, `from_file`, `to_file`, `placeholder`, `x_values`
#'     (fixed, never calibrated), `curve` (one of [known_plf_curves()]), and
#'     `params` (names of `parameters` entries feeding the curve, in the
#'     order the curve function expects them).}
#' }
#'
#' @param path Character scalar. Path to the YAML parameters file.
#'
#' @return An object of class `daisyr_param_config`, a list with elements
#'   `parameters` (a `data.table`, one row per scalar parameter, with an added
#'   `role` column: `"direct"` or `"curve_input"`) and `plf_curves` (a list
#'   of curve specifications, unchanged from the YAML other than validation).
#' @export
read_param_config <- function(path) {
  if (!file.exists(path)) stop(sprintf("Parameters file not found: %s", path))
  raw <- yaml::read_yaml(path)

  if (is.null(raw$parameters) || length(raw$parameters) == 0)
    stop("Parameters YAML must define at least one entry under `parameters`")

  params <- data.table::rbindlist(lapply(raw$parameters, function(p) {
    required <- c("name", "default", "min", "max")
    missing <- setdiff(required, names(p))
    if (length(missing) > 0)
      stop(sprintf("Parameter entry is missing required field(s): %s",
                    paste(missing, collapse = ", ")))
    data.table::data.table(
      name       = p$name,
      default    = as.numeric(p$default),
      min        = as.numeric(p$min),
      max        = as.numeric(p$max),
      from_file  = if (is.null(p$from_file)) NA_character_ else p$from_file,
      to_file    = if (is.null(p$to_file)) NA_character_ else p$to_file,
      plf_name   = if (is.null(p$plf$name)) NA_character_ else p$plf$name,
      plf_index  = if (is.null(p$plf$index)) NA_integer_ else as.integer(p$plf$index)
    )
  }), fill = TRUE)

  if (anyDuplicated(params$name) > 0)
    stop("Duplicate parameter name(s) in parameters file: ",
         paste(unique(params$name[duplicated(params$name)]), collapse = ", "))

  plf_curves <- raw$plf_curves
  if (is.null(plf_curves)) plf_curves <- list()
  plf_curves <- lapply(plf_curves, function(pc) {
    required <- c("name", "from_file", "to_file", "placeholder", "x_values", "curve", "params")
    missing <- setdiff(required, names(pc))
    if (length(missing) > 0)
      stop(sprintf("plf_curves entry '%s' is missing required field(s): %s",
                    if (is.null(pc$name)) "<unnamed>" else pc$name,
                    paste(missing, collapse = ", ")))
    pc$x_values <- as.numeric(pc$x_values)
    pc$params <- as.character(pc$params)
    if (is.null(pc$format)) pc$format <- "%.4f"
    pc
  })
  names(plf_curves) <- vapply(plf_curves, function(pc) pc$name, character(1))

  # Every parameter referenced by a plf_curves entry is a "curve_input";
  # everything else must be "direct" (i.e. have its own from_file/to_file).
  curve_input_names <- unique(unlist(lapply(plf_curves, function(pc) pc$params)))
  params[, role := ifelse(name %in% curve_input_names, "curve_input", "direct")]

  config <- list(parameters = params[], plf_curves = plf_curves)
  class(config) <- "daisyr_param_config"
  validate_param_config_structure(config)
  config
}

#' Curve families known to [render_plf_curve()]
#'
#' @param plot If `TRUE`, also draw an example curve for each built-in
#'   family (using representative default shape parameters) as a quick
#'   visual reference for what each family looks like - see
#'   [plot_plf_curves()] for control over the x-range and parameters used.
#' @return Character vector of built-in curve names, invisibly if `plot = TRUE`.
#' @export
known_plf_curves <- function(plot = FALSE) {
  curves <- c("logistic", "gompertz", "richards", "exponential")
  if (plot) {
    plot_plf_curves()
    return(invisible(curves))
  }
  curves
}

#' Example shape parameters used by [plot_plf_curves()]
#' @keywords internal
.default_plf_curve_params <- function() {
  list(
    logistic    = c(L = 1, k = 5, x0 = 0),
    gompertz    = c(L = 1, b = 3, k = 5),
    richards    = c(L = 1, k = 5, x0 = 0, v = 0.5),
    exponential = c(a = 0.2, b = 2)
  )
}

#' Plot example curves for the built-in PLF curve families
#'
#' A quick visual reference for what each family in [known_plf_curves()]
#' looks like, useful when picking a `curve` for a `plf_curves` config
#' entry. Uses base graphics so it works without any plotting package
#' beyond what R ships with.
#'
#' @param x Numeric vector of x-values to evaluate the curves at. Defaults
#'   to a fine grid over `[-1, 1]`.
#' @param params Named list of parameter vectors, one per curve family (see
#'   [render_plf_curve()] for the parameter order each family expects).
#'   Defaults to a representative example for each built-in family.
#' @param curves Character vector of curve names to plot; defaults to all
#'   built-in families with an entry in `params`.
#'
#' @return Invisibly, a `data.frame` with columns `curve`, `x`, `y` for the
#'   plotted points.
#' @examples
#' plot_plf_curves()
#' @export
plot_plf_curves <- function(x = seq(-1, 1, length.out = 200),
                             params = .default_plf_curve_params(),
                             curves = names(params)) {
  y_list <- lapply(curves, function(cv) render_plf_curve(cv, x, params[[cv]]))
  names(y_list) <- curves

  y_range <- range(unlist(y_list))
  palette <- grDevices::palette.colors(n = max(length(curves), 3))

  graphics::plot(NULL, xlim = range(x), ylim = y_range, xlab = "x", ylab = "y",
                 main = "Example PLF curve families")
  for (i in seq_along(curves)) {
    graphics::lines(x, y_list[[curves[i]]], col = palette[i], lwd = 2)
  }
  graphics::legend("topleft", legend = curves, col = palette[seq_along(curves)],
                    lwd = 2, bty = "n")

  invisible(do.call(rbind, lapply(curves, function(cv) {
    data.frame(curve = cv, x = x, y = y_list[[cv]])
  })))
}

#' Validate the internal consistency of a config (structure only)
#'
#' Checks performed independently of any `.dai` template files: every
#' `direct` parameter has both `from_file` and `to_file`; every `curve_input`
#' parameter has neither (it's only meaningful inside its `plf_curves` entry);
#' every name referenced by a `plf_curves` entry's `params` exists in
#' `parameters`; and every `curve` name is either a built-in
#' ([known_plf_curves()]) or has been registered via [register_plf_curve()].
#'
#' @param config A `daisyr_param_config` object, as returned by
#'   [read_param_config()].
#' @keywords internal
validate_param_config_structure <- function(config) {
  params <- config$parameters

  direct <- params[role == "direct"]
  bad_direct <- direct[is.na(from_file) | is.na(to_file)]
  if (nrow(bad_direct) > 0)
    stop("Parameter(s) with role 'direct' must have both from_file and to_file: ",
         paste(bad_direct$name, collapse = ", "))

  curve_input <- params[role == "curve_input"]
  bad_curve_input <- curve_input[!is.na(from_file) | !is.na(to_file)]
  if (nrow(bad_curve_input) > 0)
    stop("Parameter(s) consumed by a plf_curves entry should not also have ",
         "their own from_file/to_file (they are written only via the curve): ",
         paste(bad_curve_input$name, collapse = ", "))

  for (pc in config$plf_curves) {
    missing_params <- setdiff(pc$params, params$name)
    if (length(missing_params) > 0)
      stop(sprintf("plf_curves entry '%s' references unknown parameter(s): %s",
                    pc$name, paste(missing_params, collapse = ", ")))
    if (!(pc$curve %in% known_plf_curves()) && is.null(get_plf_curve(pc$curve, quiet = TRUE)))
      stop(sprintf("plf_curves entry '%s' uses unknown curve '%s' - register it with register_plf_curve() first",
                    pc$name, pc$curve))
  }
  invisible(TRUE)
}

#' Validate a config against the actual template files on disk
#'
#' In addition to the structural checks in [read_param_config()], this
#' confirms every declared placeholder actually exists - exactly once - in
#' its `from_file`, catching typos or drifted templates before any Daisy run
#' is attempted.
#'
#' @param config A `daisyr_param_config` object.
#' @param template_dir Character scalar. Directory that `from_file` paths in
#'   the config are relative to. Defaults to the current directory.
#'
#' @return Invisibly `TRUE` if every check passes; otherwise throws an error
#'   listing every problem found (not just the first one).
#' @export
validate_param_config <- function(config, template_dir = ".") {
  problems <- character(0)

  check_placeholder <- function(from_file, placeholder, label) {
    full_path <- file.path(template_dir, from_file)
    if (!file.exists(full_path)) {
      problems <<- c(problems, sprintf("%s: template file not found: %s", label, full_path))
      return(invisible())
    }
    txt <- paste(readLines(full_path, warn = FALSE), collapse = "\n")
    pattern <- paste0("\\{\\{", placeholder, "\\}\\}")
    n_hits <- lengths(regmatches(txt, gregexpr(pattern, txt)))
    if (n_hits == 0) {
      problems <<- c(problems, sprintf("%s: placeholder {{%s}} not found in %s", label, placeholder, from_file))
    } else if (n_hits > 1) {
      problems <<- c(problems, sprintf("%s: placeholder {{%s}} appears %d times in %s (expected exactly once)",
                                        label, placeholder, n_hits, from_file))
    }
  }

  direct <- config$parameters[config$parameters$role == "direct", ]
  for (i in seq_len(nrow(direct))) {
    check_placeholder(direct$from_file[i], direct$name[i], sprintf("parameter '%s'", direct$name[i]))
  }

  for (pc in config$plf_curves) {
    check_placeholder(pc$from_file, pc$placeholder, sprintf("plf_curves '%s'", pc$name))
    if (is.unsorted(pc$x_values, strictly = TRUE))
      problems <- c(problems, sprintf("plf_curves '%s': x_values must be strictly increasing", pc$name))
  }

  if (length(problems) > 0)
    stop("Parameter validation failed:\n  - ", paste(problems, collapse = "\n  - "), call. = FALSE)

  invisible(TRUE)
}

#' Names of every free parameter in a config
#'
#' The flattened set of names that calibration/sensitivity-analysis code
#' should treat as inputs: every `direct` parameter plus every
#' `curve_input` parameter (the shape parameters of any `plf_curves`).
#'
#' @param config A `daisyr_param_config` object.
#' @return Character vector of parameter names.
#' @export
param_config_names <- function(config) {
  config$parameters$name
}

#' Default values for every config parameter
#'
#' @param config A `daisyr_param_config` object.
#' @return Named numeric vector, one value per [param_config_names()].
#' @export
param_config_default_values <- function(config) {
  stats::setNames(as.numeric(config$parameters$default), config$parameters$name)
}

#' Expand a name selection so each PLF curve is kept as a unit
#'
#' If any shape parameter of a `plf_curves` entry is included, the rest of
#' that curve's `params` are added. Direct parameters are unchanged.
#'
#' @param config A `daisyr_param_config` object.
#' @param names Character vector of parameter names (may be `NULL` or empty
#'   to mean every config name).
#' @return Character vector of names, in config order.
#' @export
expand_calibrate_names <- function(config, names = NULL) {
  all_n <- param_config_names(config)
  if (is.null(names) || !length(names)) return(all_n)
  names <- unique(as.character(names))
  names <- names[nzchar(names)]
  for (pc in config$plf_curves) {
    if (length(intersect(names, pc$params)))
      names <- union(names, pc$params)
  }
  missing <- setdiff(names, all_n)
  if (length(missing))
    stop("Unknown calibration name(s): ", paste(missing, collapse = ", "))
  all_n[all_n %in% names]
}

#' Build a full parameter vector, pinning unspecified names at defaults
#'
#' Used when calibration or a best-parameter run only varies a subset of
#' the config. Templates still need every placeholder filled.
#'
#' @param config A `daisyr_param_config` object.
#' @param p Numeric vector of values for `names` (same order).
#' @param names Character vector of names to overwrite. `NULL` means
#'   every config name (so `p` must be complete).
#' @return Named numeric vector covering [param_config_names()].
#' @export
fill_param_config_values <- function(config, p, names = NULL) {
  names <- expand_calibrate_names(config, names)
  p <- as.numeric(p)
  if (length(p) != length(names))
    stop("`p` has length ", length(p), " but `names` has ", length(names), " name(s).")
  values <- param_config_default_values(config)
  values[names] <- p
  values
}
