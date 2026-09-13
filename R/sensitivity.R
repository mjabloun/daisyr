#' Build a Morris or Sobol-Jansen design from free parameters
#'
#' Wraps `sensitivity::morris()`/`sensitivity::soboljansen()` in "decoupled"
#' mode (`model = NULL`), using every free parameter in `config`
#' ([param_config_names()]) - i.e. direct parameters and `plf_curves`
#' shape parameters alike - as the design columns, with bounds taken from
#' the config's `min`/`max` columns. Decoupled mode is used because Daisy
#' runs are expensive external processes, not plain R functions: the design
#' matrix is generated up front, evaluated externally with [run_sa_design()],
#' and the responses are attached afterwards with [complete_sa_analysis()].
#'
#' @param config A `daisyr_param_config` object.
#' @param method `"morris"` or `"sobol"` (Sobol-Jansen).
#' @param r Number of trajectories (Morris only).
#' @param design List passed to `sensitivity::morris()`'s `design` argument
#'   (Morris only).
#' @param N Base sample size per parameter (Sobol only).
#' @param scale Whether `min`/`max` should be used for scaling.
#' @param seed Optional seed for reproducibility.
#'
#' @return A `morris`/`sobol` object (from the `sensitivity` package) whose
#'   `$X` matrix contains the parameter sets to evaluate, with `min`/`max`
#'   ranges attached as an `"config"` attribute for convenience.
#' @export
sa_design <- function(config, method = c("morris", "sobol"), r = 10,
                       design = list(type = "oat", levels = 5, grid.jump = 3),
                       N = 1000, scale = TRUE, seed = NULL) {
  method <- match.arg(method)
  params <- config$parameters
  if (!is.null(seed)) set.seed(seed)

  sa_obj <- if (method == "morris") {
    sensitivity::morris(model = NULL, factors = params$name, r = r, design = design,
                         binf = params$min, bsup = params$max, scale = scale)
  } else {
    sensitivity::soboljansen(model = NULL, factors = params$name, N = N,
                              binf = params$min, bsup = params$max, scale = scale)
  }
  attr(sa_obj, "config") <- config
  sa_obj
}

#' Console or callback progress ticks for a Daisy run loop
#' @keywords internal
.run_progress_ticker <- function(progress, n_runs) {
  pb <- NULL
  tick <- function(i) invisible(NULL)
  if (is.function(progress)) {
    tick <- function(i) {
      progress(i, n_runs)
      invisible(NULL)
    }
  } else if (isTRUE(progress) && n_runs > 0L) {
    pb <- utils::txtProgressBar(min = 0, max = n_runs, style = 3)
    tick <- function(i) utils::setTxtProgressBar(pb, i)
  }
  list(tick = tick, pb = pb)
}

#' Run Daisy for every row of a sensitivity design matrix
#'
#' For each parameter set in `sa_obj$X`, renders the config's templates,
#' runs Daisy, reads the requested output file(s), and calls `response_fun`
#' on them to produce the scalar (or vector) response that
#' [complete_sa_analysis()] will later attach to the design via `tell()`.
#'
#' @param sa_obj Object returned by [sa_design()].
#' @param config A `daisyr_param_config` object (normally
#'   `attr(sa_obj, "config")`, i.e. the same one used to build `sa_obj`).
#' @param run_file,daisy_exe,working_dir,show_log,cmd Passed to [run_daisy()].
#'   `daisy_exe` may be `NULL` to use [get_daisy_path()].
#' @param template_dir,output_dir Passed to [render_templates()].
#' @param output_files Named character vector/list of Daisy output file
#'   paths (e.g. `c(swc = "Output/interval_water_content.dlf")`) to read
#'   after each run and pass to `response_fun`.
#' @param output_reader Function used to read each entry of `output_files`.
#'   Defaults to [read_dlf()].
#' @param progress If `TRUE` (the default), print a console progress bar
#'   ([utils::txtProgressBar()]) over the Daisy runs. Set `FALSE` to keep
#'   stdout clean. A function `function(i, n)` is called after every run
#'   (`i` completed out of `n`), and once with `i = 0` before the first run
#'   — use this from Shiny via [shiny::setProgress()].
#' @param response_fun Function taking a named list of tables (one per
#'   `output_files` entry, read via `output_reader`) and returning a numeric
#'   scalar (or a fixed-length numeric vector, for multivariate Sobol
#'   analyses) summarising that run for the sensitivity analysis.
#'
#' @return A list with `run_log` (a `data.table` recording the parameter
#'   values used for every run) and `responses` (a matrix, one row per run).
#' @export
run_sa_design <- function(sa_obj, config, run_file, daisy_exe = NULL,
                           output_files, response_fun,
                           working_dir = NULL, template_dir = ".", output_dir = ".",
                           show_log = FALSE, output_reader = read_dlf,
                           progress = TRUE, cmd = NULL) {
  if (is.null(sa_obj$X)) stop("sa_obj has no design matrix (X)")

  design_matrix <- as.matrix(sa_obj$X)
  param_names <- param_config_names(config)
  if (!all(colnames(design_matrix) %in% param_names))
    stop("sa_obj's design columns do not match param_config_names(config)")

  n_runs <- nrow(design_matrix)
  param_ids <- if (!is.null(rownames(design_matrix))) rownames(design_matrix) else sprintf("run_%04d", seq_len(n_runs))

  run_log <- data.table::data.table(param_id = param_ids, parameters = vector("list", n_runs))
  responses <- vector("list", n_runs)

  prog <- .run_progress_ticker(progress, n_runs)
  if (!is.null(prog$pb)) on.exit(close(prog$pb), add = TRUE)
  prog$tick(0L)

  for (i in seq_len(n_runs)) {
    prog$tick(i - 1L)
    values <- stats::setNames(as.numeric(design_matrix[i, , drop = TRUE]), colnames(design_matrix))
    run_log$parameters[[i]] <- as.list(values)

    render_templates(config, values, template_dir = template_dir, output_dir = output_dir)
    run_daisy(run_file, daisy_exe = daisy_exe, working_dir = working_dir,
              show_log = show_log, cmd = cmd)

    outputs <- lapply(output_files, output_reader)
    names(outputs) <- names(output_files)
    responses[[i]] <- response_fun(outputs)
    prog$tick(i)
  }

  list(run_log = run_log, responses = do.call(rbind, lapply(responses, matrix, nrow = 1)))
}

#' Suggest calibration names from a sensitivity index table
#'
#' Keeps names whose Morris \eqn{\mu^*} (or Sobol `ST`) is at least `frac`
#' of the maximum, then expands PLF curves to whole units via
#' [expand_calibrate_names()]. If nothing passes the cut, the top-ranked
#' name (plus its PLF unit) is kept.
#'
#' @param indices A data.frame from [complete_sa_analysis()] / `plot_sa()`,
#'   with a `name` column and either `mu_star` or `ST`.
#' @param config A `daisyr_param_config` used to expand PLF groups.
#' @param frac Relative threshold (default `0.1`).
#' @return Character vector of parameter names, in config order.
#' @export
suggest_calibrate_names <- function(indices, config, frac = 0.1) {
  idx <- as.data.frame(indices, stringsAsFactors = FALSE)
  if (!nrow(idx) || !"name" %in% names(idx))
    return(expand_calibrate_names(config, character()))
  score <- if ("mu_star" %in% names(idx)) idx$mu_star else if ("ST" %in% names(idx)) idx$ST else NULL
  if (is.null(score))
    return(expand_calibrate_names(config, idx$name))
  mx <- max(score, na.rm = TRUE)
  keep <- character()
  if (is.finite(mx) && mx > 0) {
    keep <- as.character(idx$name[is.finite(score) & score >= frac * mx])
  }
  if (!length(keep)) {
    ord <- order(score, decreasing = TRUE, na.last = TRUE)
    keep <- as.character(idx$name[[ord[[1]]]])
  }
  expand_calibrate_names(config, keep)
}

#' Attach sensitivity-analysis responses and compute indices
#'
#' Calls `sensitivity::tell()` on `sa_obj` with the responses produced by
#' [run_sa_design()], returning the updated object with Morris elementary
#' effects / Sobol first-order & total indices populated.
#'
#' @param sa_obj Object returned by [sa_design()].
#' @param responses Numeric vector (or single-column matrix), one value per
#'   row of `sa_obj$X` - typically `run_sa_design(...)$responses`.
#'
#' @return The updated `sa_obj`, with sensitivity indices computed.
#' @seealso [plot_sa()] to visualise the resulting indices.
#' @export
complete_sa_analysis <- function(sa_obj, responses) {
  if (is.null(sa_obj$X)) stop("sa_obj is missing the design matrix (X)")
  responses <- as.numeric(responses)
  if (length(responses) != nrow(sa_obj$X))
    stop("`responses` must contain ", nrow(sa_obj$X), " values (one per sample row)")
  sensitivity::tell(sa_obj, responses)
}

#' Extract Morris / Sobol indices from a completed SA object
#' @noRd
.sa_indices <- function(sa_obj) {
  if (inherits(sa_obj, "morris")) {
    ee <- sa_obj[["ee", exact = TRUE]]
    if (is.null(ee))
      stop("Morris analysis has no elementary effects; call complete_sa_analysis() first")
    if (length(dim(ee)) == 3)
      stop("plot_sa() does not support multivariate Morris results (3-d elementary effects)")
    param_names <- colnames(ee)
    if (is.null(param_names)) param_names <- paste0("X", seq_len(ncol(ee)))
    indices <- data.frame(
      name  = param_names,
      mu      = colMeans(ee),
      mu_star = colMeans(abs(ee)),
      sigma   = apply(ee, 2, stats::sd),
      row.names = NULL,
      stringsAsFactors = FALSE
    )
    return(list(method = "morris", indices = indices))
  }

  if (inherits(sa_obj, "sobol") || inherits(sa_obj, "soboljansen")) {
    S <- sa_obj[["S", exact = TRUE]]
    T_idx <- sa_obj[["T", exact = TRUE]]
    if (is.null(S) || is.null(T_idx))
      stop("Sobol analysis has no indices; call complete_sa_analysis() first")
    param_names <- rownames(S)
    if (is.null(param_names)) param_names <- paste0("X", seq_len(nrow(S)))
    indices <- data.frame(
      name = param_names,
      S      = S[["original"]],
      ST     = T_idx[["original"]],
      row.names = NULL,
      stringsAsFactors = FALSE
    )
    if ("min. c.i." %in% names(S) && "max. c.i." %in% names(S)) {
      indices$S_min <- S[["min. c.i."]]
      indices$S_max <- S[["max. c.i."]]
    }
    if ("min. c.i." %in% names(T_idx) && "max. c.i." %in% names(T_idx)) {
      indices$ST_min <- T_idx[["min. c.i."]]
      indices$ST_max <- T_idx[["max. c.i."]]
    }
    return(list(method = "sobol", indices = indices))
  }

  stop("plot_sa() expects a morris or sobol object from complete_sa_analysis(), ",
       "not an object of class ", paste(class(sa_obj), collapse = "/"))
}

#' Plot sensitivity-analysis results
#'
#' Visualises the indices computed by [complete_sa_analysis()] with ggplot2
#' (same stack as the Shiny Plots page). Morris \eqn{\mu^*} vs \eqn{\sigma}
#' labels use [ggrepel::geom_text_repel()].
#'
#' \describe{
#'   \item{Morris}{Default `type = "effects"` is the Campolongo plot of
#'     \eqn{\mu^*} (mean absolute elementary effect; importance) against
#'     \eqn{\sigma} (standard deviation of elementary effects;
#'     non-linearity / interactions), with one labelled point per parameter.
#'     `type = "bar"` is a horizontal bar chart of \eqn{\mu^*} ranked
#'     smallest to largest.}
#'   \item{Sobol}{Default `type = "bar"` is a grouped bar chart of
#'     first-order (`S`) and total (`ST`) indices per parameter.
#'     Confidence-interval whiskers are added when the underlying
#'     `sensitivity` object provides `min. c.i.` / `max. c.i.` columns.
#'     `type = "ranked"` is a horizontal bar chart of total indices.}
#' }
#'
#' @param sa_obj A `morris` or Sobol object (`sobol` / `soboljansen`)
#'   returned by [complete_sa_analysis()] (i.e. after [sensitivity::tell()]
#'   has attached the responses).
#' @param type Plot variant. For Morris, `"effects"` (default) or `"bar"`.
#'   For Sobol, `"bar"` (default) or `"ranked"`.
#' @param main Plot title. Sensible defaults depend on `type` / method.
#' @param ... Unused; kept for compatibility with earlier base-graphics calls.
#'
#' @return Invisibly, a `data.frame` of the plotted indices: `name`,
#'   `mu`, `mu_star`, `sigma` for Morris; `name`, `S`, `ST` (and CI
#'   columns when present) for Sobol.
#'
#' @examples
#' yaml <- tempfile(fileext = ".yaml")
#' writeLines("
#' parameters:
#'   - {name: p1, default: 0.5, min: 0, max: 1, from_file: a.dai, to_file: b.dai}
#'   - {name: p2, default: 0.5, min: 0, max: 1, from_file: a.dai, to_file: b.dai}
#' ", yaml)
#' config <- read_param_config(yaml)
#' design <- sa_design(config, method = "morris", r = 5, seed = 1)
#' # Stand-in for Daisy responses: a simple function of the design matrix.
#' y <- as.matrix(design$X) %*% c(2, 0.1)
#' sa <- complete_sa_analysis(design, y)
#' plot_sa(sa)
#'
#' @seealso [complete_sa_analysis()], [sa_design()]
#' @export
plot_sa <- function(sa_obj, type = NULL, main = NULL, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 is required for plot_sa(); please install it")
  if (!requireNamespace("ggrepel", quietly = TRUE))
    stop("ggrepel is required for plot_sa(); please install it")
  parsed <- .sa_indices(sa_obj)
  idx <- parsed$indices

  if (parsed$method == "morris") {
    type <- if (is.null(type)) "effects" else type
    type <- match.arg(type, c("effects", "bar"))
    p <- .plot_sa_morris(idx, type = type, main = main)
  } else {
    type <- if (is.null(type)) "bar" else type
    type <- match.arg(type, c("bar", "ranked"))
    p <- .plot_sa_sobol(idx, type = type, main = main)
  }
  print(p)
  invisible(idx)
}

.sa_ggplot_theme <- function() {
  ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "bottom")
}

#' @noRd
.plot_sa_morris <- function(idx, type, main) {
  if (type == "effects") {
    if (is.null(main)) main <- "Morris elementary effects"
    ggplot2::ggplot(idx, ggplot2::aes(x = mu_star, y = sigma, label = name)) +
      ggplot2::geom_point(size = 2.8, colour = "#1b5e40") +
      ggrepel::geom_text_repel(
        size = 3.4,
        colour = "#1b5e40",
        min.segment.length = 0,
        box.padding = 0.35,
        point.padding = 0.25,
        max.overlaps = Inf,
        seed = 1
      ) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.18))) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.08, 0.08))) +
      ggplot2::labs(title = main, x = expression(mu^"*"), y = expression(sigma)) +
      .sa_ggplot_theme() +
      ggplot2::theme(legend.position = "none")
  } else {
    if (is.null(main)) main <- expression("Morris " * mu^"*")
    ord <- order(idx$mu_star, decreasing = FALSE)
    df <- idx
    df$name <- factor(df$name, levels = df$name[ord])
    ggplot2::ggplot(df, ggplot2::aes(x = mu_star, y = name)) +
      ggplot2::geom_col(fill = "#1b5e40", width = 0.7) +
      ggplot2::labs(title = main, x = expression(mu^"*"), y = NULL) +
      .sa_ggplot_theme() +
      ggplot2::theme(legend.position = "none")
  }
}

#' @noRd
.plot_sa_sobol <- function(idx, type, main) {
  if (identical(type, "ranked")) {
    if (is.null(main)) main <- "Sobol total indices"
    df <- idx
    df$name <- factor(df$name, levels = df$name[order(df$ST, decreasing = FALSE)])
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = ST, y = name)) +
        ggplot2::geom_col(fill = "#1b5e40", width = 0.7) +
        ggplot2::labs(title = main, x = "Total-order index (ST)", y = NULL) +
        .sa_ggplot_theme() +
        ggplot2::theme(legend.position = "none")
    )
  }
  if (is.null(main)) main <- "Sobol indices"
  long <- rbind(
    data.frame(
      name = idx$name, index = "First-order (S)", value = idx$S,
      ymin = if ("S_min" %in% names(idx)) idx$S_min else NA_real_,
      ymax = if ("S_max" %in% names(idx)) idx$S_max else NA_real_,
      stringsAsFactors = FALSE
    ),
    data.frame(
      name = idx$name, index = "Total (ST)", value = idx$ST,
      ymin = if ("ST_min" %in% names(idx)) idx$ST_min else NA_real_,
      ymax = if ("ST_max" %in% names(idx)) idx$ST_max else NA_real_,
      stringsAsFactors = FALSE
    )
  )
  p <- ggplot2::ggplot(long, ggplot2::aes(x = name, y = value, fill = index)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
    ggplot2::scale_fill_manual(values = c("First-order (S)" = "#4a9b6e", "Total (ST)" = "#1b5e40")) +
    ggplot2::labs(title = main, x = NULL, y = "Sensitivity index", fill = NULL) +
    ggplot2::coord_cartesian(ylim = c(0, NA)) +
    .sa_ggplot_theme()
  has_ci <- any(is.finite(long$ymin) & is.finite(long$ymax))
  if (has_ci) {
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ymin, ymax = ymax),
      position = ggplot2::position_dodge(width = 0.8),
      width = 0.2,
      na.rm = TRUE
    )
  }
  p
}
