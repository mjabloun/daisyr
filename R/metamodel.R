#' Drop a stacked `run_id` column from a design output table
#' @keywords internal
.drop_run_id <- function(tbl) {
  tbl <- data.table::as.data.table(tbl)
  if ("run_id" %in% names(tbl)) tbl[, run_id := NULL]
  tbl[]
}

.slice_design_run_outputs <- function(outputs, rid) {
  rid <- as.character(rid)
  take <- function(tbl) {
    if (is.null(tbl) || !NROW(tbl)) return(tbl)
    tbl <- data.table::as.data.table(tbl)
    if ("run_id" %in% names(tbl))
      tbl <- tbl[as.character(tbl$run_id) == rid]
    .drop_run_id(tbl)
  }
  if (is.data.frame(outputs)) return(take(outputs))
  stats::setNames(lapply(outputs, take), names(outputs))
}

.objective_output_stems <- function(objective) {
  comps <- if (inherits(objective, "daisyr_composite_objective"))
    objective$components else list(objective)
  stems <- vapply(comps, function(comp) {
    ofile <- as.character(comp$output_file)
    if (!length(ofile) || is.na(ofile[[1]]) || !nzchar(ofile[[1]]))
      return(NA_character_)
    tools::file_path_sans_ext(basename(ofile[[1]]))
  }, character(1))
  stems[nzchar(stems) & !is.na(stems)]
}

.match_design_output_key <- function(stem, keys) {
  if (!length(keys)) return(NA_character_)
  san <- gsub("[^A-Za-z0-9_]+", "_", stem)
  if (stem %in% keys) return(stem)
  if (san %in% keys) return(san)
  hit <- keys[tolower(keys) == tolower(stem) | tolower(keys) == tolower(san)]
  if (length(hit)) return(hit[[1]])
  NA_character_
}

# Map stacked design tables onto the objective's .dlf names so
# evaluate_objective() can apply each component's join / value_map / sim_reader.
.select_sim_outputs_for_objective <- function(sliced, objective, output_name = NULL) {
  if (is.data.frame(sliced)) return(sliced)
  keys <- names(sliced)
  stems <- .objective_output_stems(objective)
  mapped <- list()
  for (stem in stems) {
    key <- .match_design_output_key(stem, keys)
    if (is.na(key))
      stop("Design outputs do not include '", stem,
           "', the .dlf mapped on the selected objective. Available: ",
           paste(keys, collapse = ", "))
    mapped[[stem]] <- sliced[[key]]
    if (!identical(key, stem)) mapped[[key]] <- sliced[[key]]
  }
  if (length(mapped)) return(mapped)
  if (!is.null(output_name) && nzchar(output_name) && output_name %in% keys)
    return(sliced[[output_name]])
  sliced
}

#' Score every run of a parameter design against an objective
#'
#' The glue between [run_param_design()] (or [read_param_design()]) and
#' [fit_metamodel()]: [compute_generic_objective()] already reduces one
#' simulation to a single scalar score (`$summary`), this just loops that
#' over every row of a design and attaches the result as a `score` column,
#' producing the `(X, y)` shape a metamodel needs.
#'
#' Works against either form a design run can take. If `run$outputs` is
#' present (in-memory tables from `read = TRUE` or [read_param_design()]),
#' each `run_id` is scored by passing that slice as
#' `compute_generic_objective()`'s `sim_data`. Otherwise an archived run
#' (`keep_dir` from [run_param_design()] with `keep_files = TRUE`) is scored
#' file-by-file via [evaluate_objective()]. Either way you get back `design`
#' (the same table [run_param_design()] returned) plus one new `score`
#' column. When both `outputs` and `keep_dir` are present, the in-memory
#' tables are used so any `sim_mutator` already applied at read time is kept.
#'
#' @param run A list with a `design` `data.frame` (`run_id` + parameter
#'   columns) and either `keep_dir` (archived files) or `outputs`
#'   (in-memory tables) -- i.e. the return value of [run_param_design()] or
#'   [read_param_design()].
#' @param objective A `daisyr_objective`, as returned by [objective_spec()].
#' @param output_name Character scalar. Which of `output_files`/`run$outputs`
#'   the objective should be evaluated against -- the same names used in
#'   [run_param_design()]'s `output_files` argument (e.g. `"harvest"`).
#'   Optional when the objective (or its components) already set
#'   `output_file`, or when `run$outputs` is a single stacked `data.frame`.
#'   Still required for archived runs whose objective has no `output_file`.
#' @param output_files Named character vector of the *original* Daisy
#'   output paths, only needed when `run` is an archived run and doesn't
#'   already carry a `manifest.rds` (i.e. a hand-built `list(design=...,
#'   keep_dir=...)` rather than [run_param_design()]'s own return value).
#'   Ignored for in-memory runs.
#' @param maximize If `TRUE`, the stored `score` is negated -- so it stays
#'   on the same "smaller is better" scale [fit_metamodel()] and
#'   [calibrate_daisy_ego()] expect, and matches
#'   [evaluate_daisy_candidate()]'s own `maximize` convention. Set this the
#'   same way you would set `maximize` on [calibrate_daisy()] /
#'   [calibrate_daisy_ego()] for the same objective.
#' @param return_per_pair If `TRUE`, also keep each run's per-variable
#'   scores (see [compute_generic_objective()]'s `return_per_pair`) as a
#'   `per_variable` attribute on the result, named by `run_id`.
#'
#' @return `run$design` with one appended `score` column (class
#'   `daisyr_scored_design`). An `objective` attribute is attached so
#'   [fit_metamodel()] doesn't need it passed again.
#'
#' @examples
#' \dontrun{
#' output_files <- c(harvest = "Output/harvest.dlf")
#' run <- run_param_design(
#'   design, config, run_file, daisy_exe,
#'   output_files = output_files, working_dir = "path/to/scenario",
#'   keep_files = TRUE, keep_dir = "metamodel_runs"
#' )
#' objective <- objective_spec(
#'   obs_data = obs.yield,
#'   value_map = list(list(obs_col = "yield", sim_col = "WSOrg")),
#'   metric = "RMSE"
#' )
#' scored <- score_param_design(run, objective, output_name = "harvest")
#' scored$score
#' }
#' @seealso [fit_metamodel()], [run_param_design()], [objective_spec()]
#' @export
score_param_design <- function(run, objective, output_name = NULL, output_files = NULL,
                               maximize = FALSE, return_per_pair = FALSE) {
  stopifnot(inherits(objective, "daisyr_objective"))
  design <- as.data.frame(run$design, stringsAsFactors = FALSE)
  if (nrow(design) < 1L || !"run_id" %in% names(design))
    stop("run must have a `design` data.frame with a run_id column -- ",
         "the shape run_param_design()/read_param_design() return")
  run_ids <- as.character(design$run_id)

  has_outputs <- !is.null(run$outputs)
  has_archive <- !is.null(run$keep_dir)
  if (!has_outputs && !has_archive)
    stop("run has neither `keep_dir` (archived files) nor `outputs` (in-memory tables); ",
         "re-run run_param_design()/read_param_design() with keep_files = TRUE or read = TRUE")
  use_archive <- !has_outputs

  if (use_archive) {
    if (is.null(output_files)) {
      man_path <- file.path(run$keep_dir, "manifest.rds")
      if (file.exists(man_path))
        output_files <- unlist(readRDS(man_path)$output_files, use.names = TRUE)
    }
    stems <- .objective_output_stems(objective)
    if (is.null(output_name) || !nzchar(output_name)) {
      if (length(stems)) {
        key <- .match_design_output_key(stems[[1]], names(output_files))
        if (!is.na(key)) output_name <- key
      }
    }
    if (is.null(output_name) || !nzchar(as.character(output_name)[[1]]))
      stop("`output_name` is required when scoring an archived run that has no objective output_file")
    if (is.null(output_files) || !output_name %in% names(output_files))
      stop("output_files must name '", output_name, "' (the Daisy output the ",
           "objective is scored against); pass it explicitly if `run` has no manifest.rds")
    rel <- output_files[[output_name]]
  }

  score_one <- function(rid) {
    if (use_archive) {
      sim_file <- file.path(run$keep_dir, .archived_filename(rid, rel))
      return(evaluate_objective(objective, sim_file, return_per_pair = return_per_pair))
    }
    sliced <- .slice_design_run_outputs(run$outputs, rid)
    sim_outputs <- .select_sim_outputs_for_objective(sliced, objective, output_name)
    evaluate_objective(objective, sim_file = "", sim_outputs = sim_outputs,
                       return_per_pair = return_per_pair)
  }

  results <- stats::setNames(lapply(run_ids, score_one), run_ids)
  scores <- vapply(results, `[[`, numeric(1), "summary")
  if (maximize) scores <- -scores

  scored <- design
  scored$score <- as.numeric(scores)
  class(scored) <- c("daisyr_scored_design", "data.frame")
  attr(scored, "objective") <- objective
  attr(scored, "maximize") <- maximize
  if (return_per_pair)
    attr(scored, "per_variable") <- lapply(results, `[[`, "per_variable")
  scored
}

#' Fit a Kriging metamodel to a scored parameter design
#'
#' Wraps `DiceKriging::km()`: fits a Gaussian-process surrogate mapping
#' parameter combinations to the scalar response [score_param_design()]
#' produced, so cheap predictions (with uncertainty) stand in for further
#' Daisy runs -- for sensitivity analysis, uncertainty bounds, or as the
#' starting model for [calibrate_daisy_ego()].
#'
#' @param scored_design A `data.frame` with one column per parameter and a
#'   scalar response column -- typically a `daisyr_scored_design` from
#'   [score_param_design()], but any table with the right columns works.
#' @param params Parameter bounds: a `data.frame`/list of `{name, min,
#'   max}`, or a `daisyr_param_config` -- the same argument shape as
#'   [generate_param_design()]. Only `name` is used here, to pick and order
#'   `scored_design`'s parameter columns; pass the same `params`/`config`
#'   used to build the design for consistency.
#' @param response_col Character scalar naming `scored_design`'s scalar
#'   response column (default `"score"`, matching [score_param_design()]'s
#'   output).
#' @param covtype Covariance kernel passed to `DiceKriging::km()` (default
#'   `"matern5_2"`, a common default for computer-experiment metamodels).
#' @param ... Additional arguments forwarded to `DiceKriging::km()` (e.g.
#'   `formula`, `nugget`, `estim.method`, `control`).
#'
#' @return A `daisyr_metamodel`: a list with `km` (the fitted `DiceKriging`
#'   `km` object), `param_names`, `params` (normalised bounds), and
#'   `response_col`.
#'
#' @examples
#' \dontrun{
#' metamodel <- fit_metamodel(scored, params, covtype = "matern5_2")
#' predict(metamodel, newdata = data.frame(Ap_clay = 0.08, LAIvsDS_L = 5))
#' }
#' @seealso [score_param_design()], [validate_metamodel()], [calibrate_daisy_ego()]
#' @export
fit_metamodel <- function(scored_design, params, response_col = "score",
                          covtype = "matern5_2", ...) {
  if (!requireNamespace("DiceKriging", quietly = TRUE))
    stop("Package 'DiceKriging' is required for fit_metamodel(); please install it")

  bounds <- .normalize_param_bounds(params)
  param_names <- bounds$name
  missing <- setdiff(param_names, names(scored_design))
  if (length(missing) > 0)
    stop("scored_design is missing parameter(s): ", paste(missing, collapse = ", "))
  if (!response_col %in% names(scored_design))
    stop("scored_design has no '", response_col, "' column; run score_param_design() first, ",
         "or pass response_col = the name of your own scalar response column")

  X <- as.data.frame(scored_design[, param_names, drop = FALSE], stringsAsFactors = FALSE)
  y <- scored_design[[response_col]]

  km_fit <- DiceKriging::km(design = X, response = y, covtype = covtype, ...)
  structure(
    list(km = km_fit, param_names = param_names, params = bounds, response_col = response_col),
    class = "daisyr_metamodel"
  )
}

#' Predict from a fitted Daisy metamodel
#'
#' @param object A `daisyr_metamodel` from [fit_metamodel()] (or the
#'   `metamodel` element of a [calibrate_daisy_ego()] result).
#' @param newdata A `data.frame` (or design-shaped object) containing at
#'   least `object$param_names`; extra columns are ignored.
#' @param type Kriging prediction type passed to `DiceKriging`'s S4
#'   `predict` method for `km` objects (default `"UK"`, universal kriging
#'   -- the usual choice; see `DiceKriging::km-class`'s documentation for
#'   `"SK"`).
#' @param ... Forwarded to that `predict` method (e.g. `se.compute`,
#'   `cov.compute`).
#'
#' @return The list `DiceKriging`'s `predict` method for `km` objects
#'   returns: predicted `mean`, `sd`, and (unless suppressed) confidence
#'   bounds, one row per `newdata` row.
#' @export
#' @method predict daisyr_metamodel
predict.daisyr_metamodel <- function(object, newdata, type = "UK", ...) {
  if (!requireNamespace("DiceKriging", quietly = TRUE))
    stop("Package 'DiceKriging' is required to predict from a daisyr_metamodel; please install it")
  newX <- as.data.frame(newdata)[, object$param_names, drop = FALSE]
  # km's predict method is an S4 method on the standard `predict` generic
  # (DiceKriging has no separate predict.km() S3 function) -- dispatch
  # happens on object$km's class, not our own daisyr_metamodel S3 class.
  stats::predict(object$km, newdata = newX, type = type, ...)
}

#' @export
print.daisyr_metamodel <- function(x, ...) {
  cat("<daisyr_metamodel>\n")
  cat("  names: ", paste(x$param_names, collapse = ", "), "\n", sep = "")
  cat("  training points: ", nrow(x$km@X), "\n", sep = "")
  cat("  covariance: ", x$km@covariance@name, "\n", sep = "")
  invisible(x)
}

#' Validate a fitted metamodel before trusting it
#'
#' A Kriging surrogate is only as useful as its predictive accuracy on
#' points it wasn't fit to -- this checks that before the metamodel is used
#' for sensitivity analysis, uncertainty bounds, or [calibrate_daisy_ego()].
#'
#' \describe{
#'   \item{`method = "loo"`}{Leave-one-out cross-validation via
#'     `DiceKriging::leaveOneOut.km()` -- refits nothing extra (Kriging's
#'     LOO has a closed form), returns predicted values/sd for every
#'     training point with itself removed, and a summary Q2 (predictivity
#'     coefficient; 1 is a perfect fit, 0 means no better than predicting
#'     the mean) and RMSE.}
#'   \item{`method = "kfold"`}{K-fold cross-validation via
#'     `DiceEval::modelFit()` + `DiceEval::crossValidation()`. `DiceEval`
#'     refits its own model internally (once up front via `modelFit()`,
#'     then once per fold inside `crossValidation()`) rather than reusing
#'     `metamodel$km` directly -- this rebuilds it as a `type = "Kriging"`
#'     `DiceEval` model with the same `covtype` as `metamodel`, so the two
#'     stay comparable. Useful mainly because `DiceEval::modelFit()` can
#'     also build `"Linear"`/`"MARS"`/etc. metamodels on the same design if
#'     you want to check whether Kriging is actually buying you anything
#'     over something cheaper. Requires the `DiceEval` package.}
#' }
#'
#' @param metamodel A `daisyr_metamodel` from [fit_metamodel()].
#' @param scored_design The same `scored_design` `fit_metamodel()` was
#'   fit on (needed to recover the true response values for `method =
#'   "loo"`'s Q2/RMSE, and to rebuild `X`/`Y` for `method = "kfold"`).
#' @param method `"loo"` (default) or `"kfold"`.
#' @param K Number of folds for `method = "kfold"` (default 10); ignored
#'   for `"loo"`.
#' @param ... For `"loo"`, forwarded to `DiceKriging::leaveOneOut.km()`
#'   (e.g. `trend.reestim`). For `"kfold"`, forwarded to
#'   `DiceEval::modelFit()` (e.g. `formula`).
#'
#' @return A `daisyr_metamodel_validation`: a list with `method` and
#'   `result`. For `"loo"`, `result` has `predicted`, `sd`, `residuals`,
#'   `Q2`, and `RMSE`. For `"kfold"`, `result` is whatever
#'   `DiceEval::crossValidation()` returns (`Q2`, `RMSE_CV`, `MAE_CV`,
#'   `Ypred`, `folds`, ...).
#'
#' @examples
#' \dontrun{
#' check <- validate_metamodel(metamodel, scored)
#' check$result$Q2
#' check$result$RMSE
#' }
#' @seealso [fit_metamodel()]
#' @export
validate_metamodel <- function(metamodel, scored_design, method = c("loo", "kfold"),
                               K = 10, ...) {
  method <- match.arg(method)
  stopifnot(inherits(metamodel, "daisyr_metamodel"))
  y <- scored_design[[metamodel$response_col]]

  if (method == "loo") {
    if (!requireNamespace("DiceKriging", quietly = TRUE))
      stop("Package 'DiceKriging' is required for method = \"loo\"; please install it")
    loo <- DiceKriging::leaveOneOut.km(metamodel$km, type = "UK", ...)
    # [[..., exact = TRUE]] rather than $ -- same convention as
    # .sa_indices()'s sa_obj[["ee", exact = TRUE]] in sensitivity.R and
    # the dice_lhs branch of .unit_param_sample(): don't let $'s partial
    # matching silently resolve against an unrelated field on an object
    # returned by an external package.
    loo_mean <- loo[["mean", exact = TRUE]]
    loo_sd <- loo[["sd", exact = TRUE]]
    resid <- y - loo_mean
    result <- list(
      predicted = loo_mean, sd = loo_sd, residuals = resid,
      Q2 = 1 - sum(resid^2) / sum((y - mean(y))^2),
      RMSE = sqrt(mean(resid^2))
    )
  } else {
    if (!requireNamespace("DiceEval", quietly = TRUE))
      stop("Package 'DiceEval' is required for method = \"kfold\"; please install it")
    X <- scored_design[, metamodel$param_names, drop = FALSE]
    modfit <- DiceEval::modelFit(X, y, type = "Kriging",
                                 covtype = metamodel$km@covariance@name, ...)
    result <- DiceEval::crossValidation(modfit, K = K)
  }
  structure(list(method = method, result = result), class = "daisyr_metamodel_validation")
}

#' @export
print.daisyr_metamodel_validation <- function(x, ...) {
  cat("<daisyr_metamodel_validation: ", x$method, ">\n", sep = "")
  if (x$method == "loo") {
    cat(sprintf("  Q2   = %.4f\n", x$result$Q2))
    cat(sprintf("  RMSE = %.4g\n", x$result$RMSE))
  } else {
    # x$result is DiceEval::crossValidation()'s raw return value here --
    # exact-match extraction, same reasoning as the "loo" branch above.
    cat(sprintf("  Q2      = %.4f\n", x$result[["Q2", exact = TRUE]]))
    cat(sprintf("  RMSE_CV = %.4g\n", x$result[["RMSE_CV", exact = TRUE]]))
  }
  invisible(x)
}

#' Surrogate-assisted calibration via Efficient Global Optimization (EGO)
#'
#' An alternative to [calibrate_daisy()]'s DEoptim: instead of calling
#' Daisy on every candidate a population-based search proposes,
#' `DiceOptim::EGO.nsteps()` uses the Kriging metamodel to pick, one at a
#' time, the single most promising next candidate (maximum Expected
#' Improvement), calls the *real* Daisy objective only for that candidate
#' via [evaluate_daisy_candidate()], and refits the metamodel before
#' repeating. `nsteps` real Daisy runs, instead of DEoptim's whole
#' population every generation -- worth it once the initial
#' [score_param_design()] batch is paid for and Daisy calls are the
#' bottleneck.
#'
#' `metamodel` must already be fit (via [fit_metamodel()]) on a
#' [score_param_design()] response using the *same* `maximize` convention
#' passed here: [score_param_design()]'s scores, the metamodel trained on
#' them, and `evaluate_daisy_candidate()`'s new evaluations all need to be
#' on the same minimize-this scale, exactly as [calibrate_daisy()] already
#' requires for DEoptim.
#'
#' @param metamodel A `daisyr_metamodel` from [fit_metamodel()], whose
#'   `param_names` must match `param_config_names(config)`.
#' @param config A `daisyr_param_config` object.
#' @param run_file,daisy_exe,working_dir,show_log,cmd Passed to [run_daisy()]
#'   (via [evaluate_daisy_candidate()]). `daisy_exe` may be `NULL` to use
#'   [get_daisy_path()].
#' @param objective A `daisyr_objective`, as returned by [objective_spec()]
#'   -- must be the same objective `metamodel`'s response was scored with.
#' @param sim_file Character scalar. Path to the Daisy output file the
#'   objective is evaluated against for each new candidate (relative to
#'   `working_dir` if Daisy is run there).
#' @param nsteps Number of new candidates to actually evaluate on Daisy
#'   (i.e. number of EGO iterations).
#' @param template_dir,output_dir Passed to [render_templates()].
#' @param maximize If `TRUE`, matches [evaluate_daisy_candidate()]'s
#'   `maximize`: the real objective is negated before being handed to EGO
#'   (which always minimizes), and the reported `best_score` is negated
#'   back. Must match how `metamodel`'s training response was scored.
#' @param control,kmcontrol Forwarded to `DiceOptim::EGO.nsteps()` (control
#'   of its internal `genoud` search and Kriging re-estimation,
#'   respectively).
#'
#' @return A list with:
#'   \itemize{
#'     \item `best_params` -- named numeric vector, the best parameter
#'       combination found across *both* the design `metamodel` was
#'       trained on and the `nsteps` new candidates EGO evaluated.
#'     \item `best_score` -- its objective score (sign-corrected if
#'       `maximize = TRUE`).
#'     \item `fit` -- the raw `DiceOptim::EGO.nsteps()` result (`par`,
#'       `value`, `lastmodel`, ...), for diagnostics.
#'     \item `metamodel` -- a new `daisyr_metamodel` wrapping `fit$lastmodel`
#'       (refit on the original design plus the `nsteps` new points) --
#'       pass this back into `calibrate_daisy_ego()` to run more steps.
#'   }
#'
#' @examples
#' \dontrun{
#' result <- calibrate_daisy_ego(
#'   metamodel, config, run_file, daisy_exe, objective, sim_file,
#'   nsteps = 10, working_dir = "path/to/scenario"
#' )
#' result$best_params
#' result$best_score
#'
#' # Not converged yet -- keep going from where this left off:
#' more <- calibrate_daisy_ego(
#'   result$metamodel, config, run_file, daisy_exe, objective, sim_file,
#'   nsteps = 10, working_dir = "path/to/scenario"
#' )
#' }
#' @seealso [fit_metamodel()], [calibrate_daisy()], [evaluate_daisy_candidate()]
#' @export
calibrate_daisy_ego <- function(metamodel, config, run_file, daisy_exe = NULL, objective, sim_file,
                                nsteps = 10,
                                working_dir = NULL, template_dir = ".", output_dir = ".",
                                show_log = FALSE, maximize = FALSE,
                                control = NULL, kmcontrol = NULL, cmd = NULL) {
  if (!requireNamespace("DiceOptim", quietly = TRUE))
    stop("Package 'DiceOptim' is required for calibrate_daisy_ego(); please install it")
  stopifnot(inherits(metamodel, "daisyr_metamodel"))

  param_names <- param_config_names(config)
  if (!identical(metamodel$param_names, param_names))
    stop("metamodel's names do not match param_config_names(config): ",
         "metamodel has [", paste(metamodel$param_names, collapse = ", "), "], ",
         "parameters has [", paste(param_names, collapse = ", "), "]")

  params <- config$parameters
  real_fun <- function(p) {
    evaluate_daisy_candidate(p, config, run_file, daisy_exe, objective, sim_file,
                             working_dir = working_dir, template_dir = template_dir,
                             output_dir = output_dir, show_log = show_log,
                             maximize = maximize, cmd = cmd)
  }

  fit <- DiceOptim::EGO.nsteps(
    model = metamodel$km, fun = real_fun, nsteps = nsteps,
    lower = params$min, upper = params$max,
    control = control, kmcontrol = kmcontrol
  )

  ## The best point overall may be in the original training design, not
  ## among the nsteps new candidates -- compare across both rather than
  ## just the newly evaluated points. [[..., exact = TRUE]] on fit's
  ## fields for the same reason as validate_metamodel()'s "loo" branch --
  ## fit is DiceOptim::EGO.nsteps()'s raw return value, an external object.
  new_X <- as.matrix(as.data.frame(fit[["par", exact = TRUE]])[, param_names, drop = FALSE])
  new_y <- as.numeric(unlist(fit[["value", exact = TRUE]]))
  all_X <- rbind(as.matrix(metamodel$km@X), new_X)
  all_y <- c(as.numeric(metamodel$km@y), new_y)

  best_i <- which.min(all_y)
  best_params <- stats::setNames(as.numeric(all_X[best_i, ]), param_names)
  best_score <- if (maximize) -all_y[best_i] else all_y[best_i]

  refit <- structure(
    list(km = fit[["lastmodel", exact = TRUE]], param_names = param_names,
         params = metamodel$params, response_col = metamodel$response_col),
    class = "daisyr_metamodel"
  )

  list(best_params = best_params, best_score = best_score, fit = fit, metamodel = refit)
}
