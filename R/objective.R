#' Normalise a value_map into a consistent data.table
#'
#' Accepts either a list of lists or a data.frame/data.table with columns
#' `obs_col`, `sim_col`, and optionally `weight`, `label`. Provides sensible
#' defaults otherwise (`label` defaults to `obs_col`, `weight` defaults to 1).
#'
#' @param value_map List or data.frame describing observed vs simulated
#'   column pairs.
#'
#' @return A `data.table` with columns `obs_col`, `sim_col`, `label`, `weight`.
#' @keywords internal
normalise_value_map <- function(value_map) {
  if (is.data.frame(value_map)) {
    vm <- data.table::as.data.table(value_map)
  } else if (is.list(value_map)) {
    vm <- data.table::rbindlist(lapply(value_map, data.table::as.data.table), fill = TRUE)
  } else {
    stop("value_map must be a list/data.frame describing observed vs simulated columns")
  }
  if (!("obs_col" %in% names(vm) && "sim_col" %in% names(vm)))
    stop("value_map must define 'obs_col' and 'sim_col' fields")
  if (!"label" %in% names(vm)) vm[, label := obs_col]
  if (!"weight" %in% names(vm)) vm[, weight := 1]
  vm[]
}

#' Compare simulated Daisy output to observed data with a generic objective
#'
#' Generalises crop/site-specific cost functions (e.g. an ad hoc
#' `f.ThetaCost()`) into a single, reusable API: join observed and simulated
#' tables on shared key column(s), score one or more column pairs with a
#' goodness-of-fit metric, and combine them into a single weighted score.
#'
#' @param obs_data data.frame/data.table of observed values. Must contain the
#'   `join_cols` plus every `obs_col` referenced in `value_map`.
#' @param sim_file Character scalar, optional. Path to the Daisy simulation
#'   output to compare against. May be omitted when `sim_data` is supplied.
#' @param sim_data data.frame/data.table, optional. Already-loaded simulation
#'   output (e.g. from [read_dlf()]). Useful when one simulation feeds
#'   multiple objective calculations.
#' @param value_map List/data.frame describing observed vs simulated column
#'   pairs (multi-objective definitions); see [normalise_value_map()].
#' @param join_cols Character vector. Key column(s) used to align obs/sim
#'   data (default `"Date"`); both tables must contain them.
#' @param sim_reader Function that reads `sim_file` and returns a table.
#'   Defaults to [read_dlf()], but any reader (e.g. [data.table::fread()])
#'   works.
#' @param sim_mutator Optional function applied to the simulated table after
#'   reading (e.g. to rename columns or derive metrics).
#' @param metric Character scalar, one of `"RMSE"`, `"KGE"`, `"MAE"`. Ignored
#'   when `metric_fun` is supplied.
#' @param metric_fun Optional function `f(sim, obs)` returning a scalar
#'   goodness-of-fit score; overrides `metric`.
#' @param weights Numeric vector, one per row of `value_map`. Defaults to the
#'   `weight` column of `value_map` (or 1 for every pair).
#' @param return_per_pair If `TRUE`, also return a table with each
#'   objective's score and weight.
#' @param plot If `TRUE`, build a scatter/fit plot for every objective pair
#'   (requires ggplot2).
#' @param plot_labels List with `x`/`y` entries used to label the plots.
#'
#' @return A list with `summary` (the weighted-mean score across all pairs),
#'   and optionally `per_variable` and `plot`.
#'
#' @examples
#' \dontrun{
#' value_map <- list(
#'   list(obs_col = "Theta1", sim_col = "simTheta1", label = "Top layer")
#' )
#' compute_generic_objective(
#'   obs_data = obs.swc, sim_file = "Output/interval_water_content.dlf",
#'   value_map = value_map, join_cols = "Date", metric = "KGE"
#' )
#' }
#' @export
compute_generic_objective <- function(obs_data,
                                       sim_file,
                                       value_map,
                                       join_cols = "Date",
                                       sim_reader = read_dlf,
                                       sim_data = NULL,
                                       sim_mutator = NULL,
                                       metric = c("RMSE", "KGE", "MAE"),
                                       metric_fun = NULL,
                                       weights = NULL,
                                       return_per_pair = FALSE,
                                       plot = FALSE,
                                       plot_labels = list(x = "Observed", y = "Simulated")) {
  if (missing(value_map) || length(value_map) == 0)
    stop("value_map must contain at least one mapping between observed and simulated columns")
  metric <- match.arg(metric)
  if (is.null(metric_fun)) {
    metric_fun <- switch(metric,
      RMSE = hydroGOF::rmse,
      KGE  = hydroGOF::KGE,
      MAE  = hydroGOF::mae)
  }
  obs_dt <- data.table::as.data.table(obs_data)
  if (!is.null(sim_data)) {
    sim_dt <- data.table::as.data.table(sim_data)
  } else {
    if (missing(sim_file) || is.null(sim_file))
      stop("You must provide either `sim_data` or `sim_file`")
    sim_dt <- sim_reader(sim_file)
  }
  if (!is.null(sim_mutator)) sim_dt <- sim_mutator(sim_dt)

  obs_dt <- data.table::copy(obs_dt)
  sim_dt <- data.table::copy(data.table::as.data.table(sim_dt))
  if ("Date" %in% join_cols && !"Date" %in% names(sim_dt)) {
    label <- if (!missing(sim_file) && !is.null(sim_file) &&
                 is.character(sim_file) && length(sim_file) && nzchar(sim_file[[1]]))
      basename(sim_file) else "sim"
    sim_dt <- add_date(sim_dt, label)
  }
  missing_join <- setdiff(join_cols, names(obs_dt))
  if (length(missing_join))
    stop("Observed data is missing join column(s): ", paste(missing_join, collapse = ", "))
  missing_join <- setdiff(join_cols, names(sim_dt))
  if (length(missing_join))
    stop("Simulated data is missing join column(s): ", paste(missing_join, collapse = ", "))

  value_map_dt <- normalise_value_map(value_map)
  overlap <- setdiff(intersect(names(obs_dt), names(sim_dt)), join_cols)
  if (length(overlap)) {
    new_nms <- paste0("sim.", overlap)
    data.table::setnames(sim_dt, overlap, new_nms)
    hit <- value_map_dt$sim_col %in% overlap
    value_map_dt$sim_col[hit] <- paste0("sim.", value_map_dt$sim_col[hit])
  }

  data.table::setkeyv(obs_dt, join_cols)
  data.table::setkeyv(sim_dt, join_cols)
  merged <- obs_dt[sim_dt, nomatch = 0]
  if (nrow(merged) == 0)
    stop("No overlapping rows between observed and simulated data after merging on ", paste(join_cols, collapse = ", "))

  if (is.null(weights)) weights <- value_map_dt$weight
  if (length(weights) != nrow(value_map_dt))
    stop("`weights` length must match the number of rows in value_map")

  scores <- numeric(nrow(value_map_dt))
  for (i in seq_len(nrow(value_map_dt))) {
    sim_col <- value_map_dt$sim_col[i]
    obs_col <- value_map_dt$obs_col[i]
    if (!all(c(sim_col, obs_col) %in% names(merged)))
      stop("Columns ", sim_col, " and/or ", obs_col, " are missing from the merged data")
    scores[i] <- metric_fun(merged[[sim_col]], merged[[obs_col]])
  }
  summary_score <- stats::weighted.mean(scores, w = weights)
  result <- list(summary = summary_score)
  if (return_per_pair) {
    result$per_variable <- data.table::data.table(
      variable = value_map_dt$label, score = scores, weight = weights,
      metric = metric
    )
  }
  if (plot) {
    if (!requireNamespace("ggplot2", quietly = TRUE))
      stop("ggplot2 is required for plotting; please install it or set plot = FALSE")
    result$plot <- build_generic_obj_plot(merged, value_map_dt, plot_labels)
  }
  result
}

#' Build diagnostic scatter plots for a computed objective
#' @keywords internal
build_generic_obj_plot <- function(merged_data, value_map_dt, plot_labels) {
  plot_list <- vector("list", nrow(value_map_dt))
  for (i in seq_len(nrow(value_map_dt))) {
    sim_col <- value_map_dt$sim_col[i]
    obs_col <- value_map_dt$obs_col[i]
    layer_df <- data.frame(
      obs = merged_data[[obs_col]],
      sim = merged_data[[sim_col]]
    )
    plot_list[[i]] <- ggplot2::ggplot(layer_df, ggplot2::aes(x = obs, y = sim)) +
      ggplot2::geom_point(alpha = 0.6) +
      ggplot2::geom_smooth(method = "lm", se = FALSE, formula = y ~ x, colour = "#1b9e77") +
      ggplot2::labs(title = value_map_dt$label[i], x = plot_labels$x, y = plot_labels$y)
  }
  plot_list
}

#' Define a reusable objective specification
#'
#' Bundles everything [compute_generic_objective()] needs except the
#' just-produced simulation output, so it can be passed around as a single
#' object to [calibrate_daisy()] or a sensitivity-analysis `response_fun`.
#'
#' @inheritParams compute_generic_objective
#' @return A list of class `daisyr_objective` holding the supplied arguments.
#' @export
objective_spec <- function(obs_data, value_map, join_cols = "Date",
                            sim_reader = read_dlf, sim_mutator = NULL,
                            metric = c("RMSE", "KGE", "MAE"), metric_fun = NULL,
                            weights = NULL, output_file = NULL) {
  metric <- match.arg(metric)
  structure(
    list(obs_data = obs_data, value_map = value_map, join_cols = join_cols,
         sim_reader = sim_reader, sim_mutator = sim_mutator,
         metric = metric, metric_fun = metric_fun, weights = weights,
         output_file = output_file),
    class = "daisyr_objective"
  )
}

#' Combine named component objectives into one weighted score
#'
#' Each component is a [objective_spec()] (one observation table, join,
#' metric, and optional `output_file`). After a Daisy run, [evaluate_objective()]
#' scores every component against its own `.dlf` (resolved next to `sim_file`)
#' and returns the weighted mean. Use this when yield, biomass, and SWC live
#' in different files or need different joins/metrics.
#'
#' @param components Named list of `daisyr_objective` objects. Names are tags
#'   (e.g. `"yield"`, `"swc"`).
#' @param weights Numeric vector, one per component (default 1).
#' @param name Optional character scalar stored on the object.
#' @return An object of class `daisyr_composite_objective` (also
#'   `daisyr_objective`).
#' @export
composite_objective <- function(components, weights = NULL, name = NULL) {
  if (!is.list(components) || !length(components))
    stop("`components` must be a non-empty named list of objective_spec() objects.")
  if (is.null(names(components)) || any(!nzchar(names(components))))
    stop("Every component needs a non-empty name/tag.")
  if (any(duplicated(names(components))))
    stop("Component tags must be unique.")
  bad <- !vapply(components, inherits, logical(1), "daisyr_objective")
  if (any(bad))
    stop("All components must inherit from daisyr_objective.")
  if (is.null(weights)) weights <- rep(1, length(components))
  if (length(weights) != length(components))
    stop("`weights` length must match the number of components.")
  structure(
    list(
      name = name,
      components = components,
      weights = as.numeric(weights),
      obs_data = NULL,
      value_map = NULL,
      join_cols = character(),
      sim_reader = read_dlf,
      sim_mutator = NULL,
      metric = NA_character_,
      metric_fun = NULL
    ),
    class = c("daisyr_composite_objective", "daisyr_objective")
  )
}

resolve_component_sim_file <- function(output_file, sim_file) {
  if (is.null(output_file) || !nzchar(as.character(output_file)[[1]]))
    return(sim_file)
  bf <- basename(output_file)
  if (!is.null(sim_file) && nzchar(sim_file)) {
    sib <- file.path(dirname(sim_file), bf)
    if (file.exists(sib)) return(sib)
    if (identical(basename(sim_file), bf) && file.exists(sim_file))
      return(sim_file)
  }
  if (file.exists(output_file)) return(output_file)
  sim_file
}

match_sim_output_table <- function(output_file, sim_outputs) {
  if (is.null(sim_outputs) || !length(sim_outputs)) return(NULL)
  if (is.data.frame(sim_outputs)) return(sim_outputs)
  if (is.null(output_file) || !nzchar(output_file)) {
    if (length(sim_outputs) == 1L) return(sim_outputs[[1]])
    return(NULL)
  }
  stem <- tools::file_path_sans_ext(basename(output_file))
  nms <- names(sim_outputs)
  if (!is.null(nms) && stem %in% nms) return(sim_outputs[[stem]])
  sanitized <- gsub("[^A-Za-z0-9_]+", "_", stem)
  if (!is.null(nms) && sanitized %in% nms) return(sim_outputs[[sanitized]])
  NULL
}

# In-memory SA/design tables skip `sim_reader` unless we apply it here.
# Readers may accept `data=` (app make_sim_reader) or a data.frame as the
# first argument; otherwise the raw table is returned and Date is filled
# later by compute_generic_objective().
apply_sim_reader <- function(sim_reader, sim_file, sim_data = NULL) {
  if (is.null(sim_data)) {
    if (is.null(sim_reader)) return(NULL)
    return(sim_reader(sim_file))
  }
  if (is.null(sim_reader)) return(data.table::as.data.table(sim_data))
  fmls <- tryCatch(names(formals(sim_reader)), error = function(e) character())
  if ("data" %in% fmls)
    return(sim_reader(sim_file, data = sim_data))
  loaded <- tryCatch(sim_reader(sim_data), error = function(e) NULL)
  if (is.data.frame(loaded)) return(loaded)
  data.table::as.data.table(sim_data)
}

evaluate_one_objective <- function(objective, sim_file, sim_outputs = NULL, ...) {
  tbl <- match_sim_output_table(objective$output_file, sim_outputs)
  if (!is.null(tbl)) {
    sim_prepared <- apply_sim_reader(objective$sim_reader, sim_file, sim_data = tbl)
    return(compute_generic_objective(
      obs_data = objective$obs_data, sim_data = sim_prepared, value_map = objective$value_map,
      join_cols = objective$join_cols, sim_mutator = objective$sim_mutator,
      metric = objective$metric, metric_fun = objective$metric_fun,
      weights = objective$weights, ...
    ))
  }
  path <- resolve_component_sim_file(objective$output_file, sim_file)
  compute_generic_objective(
    obs_data = objective$obs_data, sim_file = path, value_map = objective$value_map,
    join_cols = objective$join_cols, sim_reader = objective$sim_reader,
    sim_mutator = objective$sim_mutator, metric = objective$metric,
    metric_fun = objective$metric_fun, weights = objective$weights, ...
  )
}

evaluate_composite_objective <- function(objective, sim_file, sim_outputs = NULL, ...) {
  nms <- names(objective$components)
  scores <- numeric(length(objective$components))
  names(scores) <- nms
  extra <- list(...)
  want_pairs <- isTRUE(extra$return_per_pair)
  want_plot <- isTRUE(extra$plot)
  per <- list()
  plots <- list()
  for (i in seq_along(objective$components)) {
    res <- evaluate_one_objective(objective$components[[i]], sim_file, sim_outputs, ...)
    scores[[i]] <- res$summary
    if (want_pairs && !is.null(res$per_variable)) {
      pv <- data.table::as.data.table(res$per_variable)
      pv$component <- nms[[i]]
      per[[i]] <- pv
    }
    if (want_plot && !is.null(res$plot))
      plots <- c(plots, res$plot)
  }
  result <- list(
    summary = stats::weighted.mean(scores, w = objective$weights),
    per_component = data.table::data.table(
      component = nms, score = as.numeric(scores), weight = objective$weights,
      metric = vapply(objective$components, function(comp) {
        m <- comp$metric
        if (is.null(m) || !length(m) || is.na(m[[1]]) || !nzchar(m[[1]]))
          NA_character_
        else as.character(m[[1]])
      }, character(1))
    )
  )
  if (want_pairs && length(per))
    result$per_variable <- data.table::rbindlist(per, fill = TRUE)
  if (want_plot && length(plots)) result$plot <- plots
  result
}

#' Evaluate a `daisyr_objective` against a simulation output file
#'
#' @param objective A `daisyr_objective`, as returned by [objective_spec()]
#'   or [composite_objective()].
#' @param sim_file Character scalar. Path to a Daisy output from this run.
#'   For a composite objective, each component's `output_file` is resolved
#'   in the same directory (so yield and SWC logs from one run are found
#'   together).
#' @param sim_outputs Optional named list of already-loaded tables (as from
#'   a sensitivity `response_fun`). Used when the file path is not available.
#' @param ... Passed on to [compute_generic_objective()] (e.g.
#'   `return_per_pair = TRUE`).
#' @return See [compute_generic_objective()]. Composites also include
#'   `per_component`.
#' @export
evaluate_objective <- function(objective, sim_file, sim_outputs = NULL, ...) {
  stopifnot(inherits(objective, "daisyr_objective"))
  if (inherits(objective, "daisyr_composite_objective"))
    return(evaluate_composite_objective(objective, sim_file, sim_outputs, ...))
  evaluate_one_objective(objective, sim_file, sim_outputs, ...)
}
