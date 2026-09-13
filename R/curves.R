#' @keywords internal
.plf_curve_registry <- new.env(parent = emptyenv())

#' Register a custom PLF curve family
#'
#' Makes a curve function available to `plf_curves` entries in a config
#' under the given `name`, alongside the built-ins ([known_plf_curves()]).
#'
#' @param name Character scalar. Curve identifier, used as the `curve` field
#'   in a config's `plf_curves` entries.
#' @param fun A function with signature `function(x, params)` where `x` is a
#'   numeric vector of knot locations and `params` is a named numeric vector
#'   (names matching the curve's expected parameter names, values taken from
#'   the config in the order given by that `plf_curves` entry's `params`).
#'   Must return a numeric vector the same length as `x`.
#'
#' @return Invisibly, `name`.
#' @export
register_plf_curve <- function(name, fun) {
  stopifnot(is.character(name), length(name) == 1, is.function(fun))
  assign(name, fun, envir = .plf_curve_registry)
  invisible(name)
}

#' Look up a registered custom PLF curve function
#' @param name Character scalar.
#' @param quiet If `TRUE`, return `NULL` instead of raising an error when not found.
#' @return The registered function, or `NULL`.
#' @keywords internal
get_plf_curve <- function(name, quiet = FALSE) {
  if (exists(name, envir = .plf_curve_registry, inherits = FALSE))
    return(get(name, envir = .plf_curve_registry, inherits = FALSE))
  if (quiet) return(NULL)
  stop(sprintf("No custom PLF curve registered under '%s'", name))
}

#' Evaluate a built-in or custom PLF curve
#'
#' @param curve Character scalar. One of [known_plf_curves()], or the name of
#'   a curve registered via [register_plf_curve()].
#' @param x Numeric vector of fixed x-knots.
#' @param params Numeric vector of shape parameters, given *positionally* in
#'   the order the curve expects (see Details) - a config's `plf_curves`
#'   entry supplies these via its own arbitrarily-named `parameters` entries,
#'   listed in `params` in the matching order, so the curve function itself
#'   never needs to know those config-level names.
#'
#' @details
#' Built-in curve families and their expected parameter order, all monotonic
#' in `x` for sensible parameter ranges:
#' \describe{
#'   \item{`logistic`}{`y = L / (1 + exp(-k * (x - x0)))`; order `(L, k, x0)`.}
#'   \item{`gompertz`}{`y = L * exp(-b * exp(-k * x))`; order `(L, b, k)`.}
#'   \item{`richards`}{`y = L / (1 + exp(-k * (x - x0)))^(1 / v)`; order `(L, k, x0, v)`.}
#'   \item{`exponential`}{`y = a * exp(b * x)`; order `(a, b)`.}
#' }
#' Custom curves registered via [register_plf_curve()] receive `params` as
#' the same plain (unnamed) numeric vector and are free to interpret it
#' however they like.
#'
#' @return Numeric vector, same length as `x`.
#' @export
render_plf_curve <- function(curve, x, params) {
  params <- as.numeric(params)
  switch(curve,
    logistic = {
      p <- .require_params(params, curve)
      p[1] / (1 + exp(-p[2] * (x - p[3])))
    },
    gompertz = {
      p <- .require_params(params, curve)
      p[1] * exp(-p[2] * exp(-p[3] * x))
    },
    richards = {
      p <- .require_params(params, curve)
      p[1] / (1 + exp(-p[2] * (x - p[3])))^(1 / p[4])
    },
    exponential = {
      p <- .require_params(params, curve)
      p[1] * exp(p[2] * x)
    },
    {
      fun <- get_plf_curve(curve)
      fun(x, params)
    }
  )
}

#' Parameter names (in order) expected by a built-in curve family
#'
#' Single source of truth for each built-in curve's argument order, used by
#' [render_plf_curve()] (for validation/error messages) and by
#' [create_param_config()] (to name the scaffolded shape parameters).
#'
#' @param curve Character scalar. One of [known_plf_curves()].
#' @return Character vector of argument names, in the order [render_plf_curve()]
#'   expects them.
#' @keywords internal
.plf_curve_arg_names <- function(curve) {
  switch(curve,
    logistic    = c("L", "k", "x0"),
    gompertz    = c("L", "b", "k"),
    richards    = c("L", "k", "x0", "v"),
    exponential = c("a", "b"),
    stop(sprintf(
      "'%s' is not a built-in curve with known argument names; only %s have canonical names",
      curve, paste(known_plf_curves(), collapse = ", ")))
  )
}

#' @keywords internal
.require_params <- function(params, curve) {
  arg_names <- .plf_curve_arg_names(curve)
  if (length(params) != length(arg_names))
    stop(sprintf("Curve '%s' expects %d parameter(s) in order (%s), got %d",
                  curve, length(arg_names), paste(arg_names, collapse = ", "), length(params)))
  params
}
