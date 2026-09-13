#' One calibration iteration: render templates, run Daisy, score the result
#'
#' The core building block of [calibrate_daisy()], exposed separately so it
#' can also be called directly (e.g. to evaluate a single candidate, or the
#' best-fit parameters found by an optimizer) without going through an
#' optimizer's calling convention.
#'
#' @param p Numeric vector of parameter values, in the order of `names`
#'   (default: every name in [param_config_names()]).
#' @param config A `daisyr_param_config` object.
#' @param run_file,daisy_exe,working_dir,show_log,cmd Passed to [run_daisy()].
#'   `daisy_exe` and `cmd` may be `NULL` to use [get_daisy_path()] / the
#'   default local command.
#' @param objective A `daisyr_objective`, as returned by [objective_spec()].
#' @param sim_file Character scalar. Path to the Daisy output file the
#'   objective should be evaluated against (relative to `working_dir` if
#'   Daisy was run there).
#' @param template_dir,output_dir Passed to [render_templates()].
#' @param maximize If `TRUE`, the returned score is `-summary` so that
#'   minimizers (like `DEoptim`) can be used to maximize a metric such as KGE.
#' @param names Character vector of config names to vary. Others stay at
#'   config defaults. `NULL` means every name. PLF shape parameters are
#'   expanded to whole curves via [expand_calibrate_names()].
#'
#' @return Numeric scalar: the objective's `summary` score (or its negation
#'   if `maximize = TRUE`).
#' @export
evaluate_daisy_candidate <- function(p, config, run_file, daisy_exe = NULL, objective, sim_file,
                                      working_dir = NULL, template_dir = ".", output_dir = ".",
                                      show_log = FALSE, maximize = FALSE, cmd = NULL,
                                      names = NULL) {
  values <- fill_param_config_values(config, p, names = names)
  render_templates(config, values, template_dir = template_dir, output_dir = output_dir)
  run_daisy(run_file, daisy_exe = daisy_exe, working_dir = working_dir,
            show_log = show_log, cmd = cmd)

  score <- evaluate_objective(objective, sim_file)$summary
  if (maximize) score <- -score
  score
}

#' Calibrate Daisy parameters against observed data
#'
#' Fits `names` (or every name in [param_config_names()]) against
#' [evaluate_daisy_candidate()]. Bounds come from the config `min` / `max`.
#'
#' \describe{
#'   \item{`DEoptim`}{Differential evolution. Control: `NP`, `itermax`.}
#'   \item{`BOBYQA`}{`minqa::bobyqa`. Control: `maxfun` / `maxeval`, `start`.}
#'   \item{`multi-BOBYQA`}{Several BOBYQA runs. Control: `nstarts` (default 8),
#'     `seed`, `include_default`, plus BOBYQA keys.}
#'   \item{`DDS`}{Dynamically Dimensioned Search (in-package). Control:
#'     `maxeval`, `r`, `seed`, `start`.}
#'   \item{`CMA-ES`}{`cmaes::cma_es`. Control: `maxit` / `maxeval`, `sigma`,
#'     `start`.}
#' }
#'
#' @inheritParams evaluate_daisy_candidate
#' @param method `"DEoptim"`, `"BOBYQA"`, `"multi-BOBYQA"`, `"DDS"`, or `"CMA-ES"`.
#' @param control Method-specific list.
#' @param reporter Optional function called after every candidate evaluation.
#'   It receives `evals`, `iter`, `np`, `n_total`, `generation_end`,
#'   `last_score`, `best_score`, and `best_params`.
#'
#' @return A list with `best_params`, `fitted_names`, `best_score`, `method`,
#'   and `fit`.
#' @export
calibrate_daisy <- function(config, run_file, daisy_exe = NULL, objective, sim_file,
                             working_dir = NULL, template_dir = ".", output_dir = ".",
                             show_log = FALSE, maximize = FALSE,
                             method = "DEoptim", control = list(),
                             reporter = NULL, cmd = NULL,
                             names = NULL) {
  method <- .normalize_calibrate_method(method)
  if (is.null(control)) control <- list()

  params <- config$parameters
  param_names <- expand_calibrate_names(config, names)
  if (!length(param_names))
    stop("No calibration names selected.")
  hit <- match(param_names, params$name)
  lower <- as.numeric(params$min[hit])
  upper <- as.numeric(params$max[hit])
  defaults <- param_config_default_values(config)
  start <- .calibrate_start(defaults, param_names, lower, upper, control)

  if (!is.null(reporter) && identical(method, "DEoptim")) control$trace <- FALSE
  extra <- list(
    config = config, run_file = run_file, daisy_exe = daisy_exe,
    objective = objective, sim_file = sim_file, working_dir = working_dir,
    template_dir = template_dir, output_dir = output_dir,
    show_log = show_log, maximize = maximize, cmd = cmd,
    names = param_names
  )
  eval_fn <- function(p, ...) do.call(evaluate_daisy_candidate, c(list(p = p), extra))
  n_total <- .calibrate_n_total(method, control, length(param_names))
  np <- .calibrate_np(method, control, length(param_names))
  fill_best <- function(shown) fill_param_config_values(config, shown, names = param_names)
  fn <- .wrap_calibrate_reporter(
    eval_fn, reporter, maximize, fill_best, n_total, np
  )

  got <- .run_calibrate_optimizer(method, fn, start, lower, upper, control)
  best_params <- fill_param_config_values(config, got$par, names = param_names)
  best_score <- if (maximize) -got$value else got$value

  list(
    best_params = best_params,
    fitted_names = param_names,
    best_score = best_score,
    method = method,
    fit = got$fit
  )
}
