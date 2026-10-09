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
#'   \item{`parameters`}{A list of scalar parameters. Each entry has `name`
#'     and either (a) `default`/`min`/`max` for a free parameter, plus
#'     `from_file`/`to_file` when it is substituted into a `{{name}}`
#'     placeholder (optionally with `plf` metadata), or no `from_file` when
#'     it is only referenced by a `plf_curves` entry's `params`; or (b)
#'     `expression` instead of `default`, an arithmetic formula in other
#'     parameter names (e.g. `1 - Ap_silt - Ap_sand`). An `expression`
#'     parameter is still written to its `{{name}}` placeholder, but it is
#'     not a free calibration/SA input: its value is computed after the free
#'     parameters are set. Optional `min`/`max` on an `expression` entry are
#'     validity bounds (candidates that break them are rejected).}
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
#'   `role` column: `"direct"`, `"curve_input"`, or `"derived"`), `plf_curves`
#'   (a list of curve specifications), and `derived_expressions` (compiled
#'   `expression` formulas in evaluation order, empty if none).
#' @export
read_param_config <- function(path) {
  if (!file.exists(path)) stop(sprintf("Parameters file not found: %s", path))
  raw <- yaml::read_yaml(path)

  if (is.null(raw$parameters) || length(raw$parameters) == 0)
    stop("Parameters YAML must define at least one entry under `parameters`")

  params <- data.table::rbindlist(lapply(raw$parameters, function(p) {
    if (is.null(p$name) || !nzchar(as.character(p$name)))
      stop("Parameter entry is missing required field(s): name")
    expression <- if (is.null(p$expression) || identical(p$expression, ""))
      NA_character_ else as.character(p$expression)
    has_expression <- !is.na(expression)
    if (has_expression && !is.null(p$default))
      stop(sprintf("Parameter '%s' has an expression; omit default (it is computed from the expression)",
                    p$name))
    if (!has_expression) {
      required <- c("default", "min", "max")
      missing <- setdiff(required, names(p))
      if (length(missing) > 0)
        stop(sprintf("Parameter '%s' is missing required field(s): %s",
                      p$name, paste(missing, collapse = ", ")))
    }
    data.table::data.table(
      name       = p$name,
      default    = if (is.null(p$default)) NA_real_ else as.numeric(p$default),
      min        = if (is.null(p$min)) NA_real_ else as.numeric(p$min),
      max        = if (is.null(p$max)) NA_real_ else as.numeric(p$max),
      expression = expression,
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

  # An `expression` entry is derived (computed, not free). Every remaining
  # name referenced by a plf_curves entry is a "curve_input"; everything
  # else must be "direct" (i.e. have its own from_file/to_file).
  curve_input_names <- unique(unlist(lapply(plf_curves, function(pc) pc$params)))
  params[, role := ifelse(!is.na(expression) & nzchar(expression), "derived",
                   ifelse(name %in% curve_input_names, "curve_input", "direct"))]

  derived_on_curve <- intersect(params$name[params$role == "derived"], curve_input_names)
  if (length(derived_on_curve) > 0)
    stop("Parameter(s) with an expression cannot also be plf_curves inputs: ",
         paste(derived_on_curve, collapse = ", "))

  derived_expressions <- .compile_derived_expressions(params)

  config <- list(parameters = params[], plf_curves = plf_curves,
                 derived_expressions = derived_expressions)
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

#' Operators allowed in a parameter `expression`
#' @keywords internal
.param_expression_ops <- c("+", "-", "*", "/")

#' Parse an `expression` into a call and the parameter names it references
#' @keywords internal
.parse_param_expression <- function(expression, name, known_names) {
  expr <- tryCatch(parse(text = expression, n = 1L)[[1L]], error = function(e) {
    stop(sprintf("Parameter '%s' has an invalid expression '%s': %s",
                  name, expression, e$message),
         call. = FALSE)
  })
  deps <- character(0)
  walk <- function(e) {
    if (is.numeric(e) || is.integer(e) || is.logical(e) || is.null(e)) return()
    if (is.symbol(e)) {
      nm <- as.character(e)
      if (!nzchar(nm)) return()
      deps <<- c(deps, nm)
      return()
    }
    if (is.call(e)) {
      op <- as.character(e[[1L]])
      if (length(op) != 1L || !op %in% .param_expression_ops)
        stop(sprintf("Parameter '%s' expression uses unsupported operation '%s' (allowed: %s)",
                      name, paste(op, collapse = ""),
                      paste(.param_expression_ops, collapse = ", ")),
             call. = FALSE)
      for (i in seq_along(e)[-1L]) walk(e[[i]])
      return()
    }
    stop(sprintf("Parameter '%s' expression is not numeric", name), call. = FALSE)
  }
  walk(expr)
  deps <- unique(deps)
  if (name %in% deps)
    stop(sprintf("Parameter '%s' expression refers to itself", name), call. = FALSE)
  unknown <- setdiff(deps, known_names)
  if (length(unknown) > 0)
    stop(sprintf("Parameter '%s' expression references unknown name(s): %s",
                  name, paste(unknown, collapse = ", ")), call. = FALSE)
  list(expr = expr, deps = deps)
}

#' Compile derived-parameter expressions into evaluation order
#' @keywords internal
.compile_derived_expressions <- function(params) {
  derived <- params[role == "derived"]
  if (nrow(derived) == 0L) return(list())
  known <- params$name
  parsed <- lapply(seq_len(nrow(derived)), function(i) {
    .parse_param_expression(derived$expression[i], derived$name[i], known)
  })
  names(parsed) <- derived$name

  remaining <- derived$name
  order <- character(0)
  while (length(remaining)) {
    ready <- remaining[vapply(remaining, function(nm) {
      all(parsed[[nm]]$deps %in% c(setdiff(known, remaining), order))
    }, logical(1))]
    if (!length(ready))
      stop("Circular parameter expression(s) among: ", paste(remaining, collapse = ", "))
    order <- c(order, ready)
    remaining <- setdiff(remaining, ready)
  }
  parsed[order]
}

#' Evaluate `expression` parameters from the current free-parameter values
#'
#' Overwrites any existing values for derived names. Used by
#' [fill_param_config_values()] and [render_templates()].
#'
#' @param config A `daisyr_param_config` object.
#' @param values Named numeric vector of current parameter values.
#' @return `values` with derived names filled in.
#' @keywords internal
.apply_param_expressions <- function(config, values) {
  values <- stats::setNames(as.numeric(unlist(values)), names(unlist(values)))
  compiled <- config$derived_expressions
  if (is.null(compiled) || !length(compiled)) return(values)
  for (nm in names(compiled)) {
    deps <- compiled[[nm]]$deps
    missing <- setdiff(deps, names(values))
    if (length(missing) > 0)
      stop("Missing value(s) for parameter(s): ", paste(missing, collapse = ", "))
    env <- list2env(as.list(values[deps]), parent = emptyenv())
    env$`+` <- base::`+`
    env$`-` <- base::`-`
    env$`*` <- base::`*`
    env$`/` <- base::`/`
    values[[nm]] <- eval(compiled[[nm]]$expr, envir = env, enclos = emptyenv())
  }
  values
}

#' Whether every derived parameter with min/max is inside those bounds
#' @keywords internal
.derived_in_bounds <- function(config, values) {
  derived <- config$parameters[role == "derived"]
  if (nrow(derived) == 0L) return(TRUE)
  for (i in seq_len(nrow(derived))) {
    v <- values[[derived$name[i]]]
    lo <- derived$min[i]
    hi <- derived$max[i]
    if (!is.na(lo) && isTRUE(v < lo)) return(FALSE)
    if (!is.na(hi) && isTRUE(v > hi)) return(FALSE)
  }
  TRUE
}

#' Validate the internal consistency of a config (structure only)
#'
#' Checks performed independently of any `.dai` template files: every
#' `direct` or `derived` parameter has both `from_file` and `to_file`; every
#' `curve_input` parameter has neither (it's only meaningful inside its
#' `plf_curves` entry); every name referenced by a `plf_curves` entry's
#' `params` exists in `parameters`; and every `curve` name is either a
#' built-in ([known_plf_curves()]) or has been registered via
#' [register_plf_curve()].
#'
#' @param config A `daisyr_param_config` object, as returned by
#'   [read_param_config()].
#' @keywords internal
validate_param_config_structure <- function(config) {
  params <- config$parameters

  written <- params[role %in% c("direct", "derived")]
  bad_written <- written[is.na(from_file) | is.na(to_file)]
  if (nrow(bad_written) > 0)
    stop("Parameter(s) with role 'direct' or 'derived' must have both from_file and to_file: ",
         paste(bad_written$name, collapse = ", "))

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

  written <- config$parameters[config$parameters$role %in% c("direct", "derived"), ]
  for (i in seq_len(nrow(written))) {
    check_placeholder(written$from_file[i], written$name[i], sprintf("parameter '%s'", written$name[i]))
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
#' Derived parameters (`expression` entries) are omitted: they are computed from
#' the free names, not sampled.
#'
#' @param config A `daisyr_param_config` object.
#' @return Character vector of parameter names.
#' @export
param_config_names <- function(config) {
  config$parameters$name[config$parameters$role != "derived"]
}

#' Default values for every config parameter
#'
#' Free parameters use their YAML `default`. Derived parameters are computed
#' from those defaults via their `expression`.
#'
#' @param config A `daisyr_param_config` object.
#' @return Named numeric vector covering every parameter in `config`
#'   (free names plus derived names).
#' @export
param_config_default_values <- function(config) {
  free <- config$parameters[role != "derived"]
  values <- stats::setNames(as.numeric(free$default), free$name)
  .apply_param_expressions(config, values)
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
  derived <- config$parameters$name[config$parameters$role == "derived"]
  hit_derived <- intersect(names, derived)
  if (length(hit_derived))
    stop("Derived parameter(s) cannot be calibrated (they have an expression): ",
         paste(hit_derived, collapse = ", "))
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
#'   every free config name (so `p` must be complete). Derived names are
#'   not accepted.
#' @return Named numeric vector covering every parameter in `config`
#'   (free names plus derived names computed from their `expression`).
#' @export
fill_param_config_values <- function(config, p, names = NULL) {
  names <- expand_calibrate_names(config, names)
  p <- as.numeric(p)
  if (length(p) != length(names))
    stop("`p` has length ", length(p), " but `names` has ", length(names), " name(s).")
  values <- param_config_default_values(config)
  values[names] <- p
  .apply_param_expressions(config, values)
}
