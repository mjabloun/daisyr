#' Canonical calibrate() / calibrate_daisy() method name
#' @noRd
.normalize_calibrate_method <- function(method) {
  if (length(method) != 1L || is.na(method) || !nzchar(method)) {
    stop("`method` must be a single character value", call. = FALSE)
  }
  key <- gsub("[^a-z0-9]", "", tolower(method))
  map <- c(
    deoptim = "DEoptim",
    bobyqa = "BOBYQA",
    multibobyqa = "multi-BOBYQA",
    bobyqams = "multi-BOBYQA",
    msbobyqa = "multi-BOBYQA",
    multistartbobyqa = "multi-BOBYQA",
    dds = "DDS",
    cmaes = "CMA-ES",
    cma = "CMA-ES"
  )
  if (!key %in% names(map)) {
    stop("Unknown calibrate method '", method,
         "'. Use DEoptim, BOBYQA, multi-BOBYQA, DDS, or CMA-ES.",
         call. = FALSE)
  }
  unname(map[[key]])
}

#' Nudge a start vector strictly inside [lower, upper]
#' @noRd
.interior_start <- function(par, lower, upper, frac = 0.02) {
  par <- as.numeric(par)
  lower <- as.numeric(lower)
  upper <- as.numeric(upper)
  span <- upper - lower
  span[!is.finite(span) | span <= 0] <- 1
  par <- pmin(pmax(par, lower), upper)
  lo <- par <= lower
  hi <- par >= upper
  par[lo] <- lower[lo] + frac * span[lo]
  par[hi] <- upper[hi] - frac * span[hi]
  par
}

#' Starting vector from defaults or `control$start`
#' @noRd
.calibrate_start <- function(defaults, param_names, lower, upper, control) {
  start <- control[["start", exact = TRUE]]
  if (is.null(start)) start <- control[["par", exact = TRUE]]
  if (is.null(start)) start <- unname(as.numeric(defaults[param_names]))
  start <- as.numeric(start)
  if (length(start) != length(param_names)) {
    stop("`control$start` must have length ", length(param_names),
         call. = FALSE)
  }
  .interior_start(start, lower, upper)
}

#' Wrap an objective so `reporter` is called after every evaluation
#' @noRd
.wrap_calibrate_reporter <- function(eval_fn, reporter, maximize, fill_best,
                                     n_total, np = 1L) {
  if (is.null(reporter)) return(eval_fn)
  if (!is.function(reporter)) stop("`reporter` must be a function.", call. = FALSE)
  n_eval <- 0L
  best_optim <- Inf
  best_p <- NULL
  np <- max(as.integer(np), 1L)
  n_total <- as.integer(n_total)
  function(p, ...) {
    score <- eval_fn(p, ...)
    n_eval <<- n_eval + 1L
    if (is.finite(score) && score < best_optim) {
      best_optim <<- score
      best_p <<- p
    }
    last_score <- if (maximize) -score else score
    best_score <- if (maximize) -best_optim else best_optim
    shown <- if (is.null(best_p)) p else best_p
    reporter(list(
      evals = n_eval,
      iter = as.integer(ceiling(n_eval / np)),
      np = np,
      n_total = n_total,
      generation_end = (n_eval %% np) == 0L || n_eval >= n_total,
      last_score = last_score,
      best_score = best_score,
      best_params = fill_best(shown)
    ))
    score
  }
}

#' Reflect a vector onto [lower, upper]
#' @noRd
.reflect_bounds <- function(x, lower, upper) {
  for (j in seq_along(x)) {
    if (!is.finite(x[[j]])) next
    while (x[[j]] < lower[[j]] || x[[j]] > upper[[j]]) {
      if (x[[j]] < lower[[j]]) x[[j]] <- 2 * lower[[j]] - x[[j]]
      if (x[[j]] > upper[[j]]) x[[j]] <- 2 * upper[[j]] - x[[j]]
    }
  }
  x
}

#' Dynamically Dimensioned Search (Tolson & Shoemaker 2007)
#' @keywords internal
.dds_optimize <- function(fn, par, lower, upper, control = list(), ...) {
  maxeval <- as.integer(control$maxeval %||% 200L)
  if (length(maxeval) != 1L || is.na(maxeval) || maxeval < 1L) {
    stop("`control$maxeval` must be a positive integer", call. = FALSE)
  }
  r <- control$r %||% 0.2
  if (!is.numeric(r) || length(r) != 1L || !is.finite(r) || r <= 0) {
    stop("`control$r` must be a positive number (typical default 0.2)",
         call. = FALSE)
  }
  if (!is.null(control$seed)) set.seed(as.integer(control$seed))

  par <- as.numeric(par)
  lower <- as.numeric(lower)
  upper <- as.numeric(upper)
  n <- length(par)
  span <- upper - lower
  span[span <= 0 | !is.finite(span)] <- 1e-12
  sigma <- r * span

  f0 <- fn(par, ...)
  best_p <- current <- par
  best_f <- current_f <- f0
  n_eval <- 1L
  if (maxeval == 1L) {
    return(list(par = best_p, value = best_f, evaluations = n_eval, method = "DDS"))
  }

  for (i in seq.int(2L, maxeval)) {
    Pi <- 1 - log(i) / log(maxeval)
    include <- stats::runif(n) < Pi
    if (!any(include)) include[sample.int(n, 1L)] <- TRUE
    trial <- current
    n_pert <- sum(include)
    trial[include] <- trial[include] + stats::rnorm(n_pert, mean = 0, sd = sigma[include])
    trial <- .reflect_bounds(trial, lower, upper)
    ft <- fn(trial, ...)
    n_eval <- n_eval + 1L
    if (is.finite(ft) && ft <= current_f) {
      current <- trial
      current_f <- ft
      if (ft < best_f) {
        best_p <- trial
        best_f <- ft
      }
    }
  }
  list(par = best_p, value = best_f, evaluations = n_eval, method = "DDS")
}

#' @noRd
.bobyqa_nstarts <- function(control) {
  n <- as.integer(control$nstarts %||% 8L)
  if (length(n) != 1L || is.na(n) || n < 1L) {
    stop("`control$nstarts` must be a positive integer", call. = FALSE)
  }
  n
}

#' @noRd
.bobyqa_control <- function(control, start, lower, upper) {
  keep <- intersect(names(control), c("npt", "rhobeg", "rhoend", "iprint", "maxfun"))
  bctrl <- control[keep]
  if (is.null(bctrl$maxfun)) {
    bctrl$maxfun <- as.integer(control$maxeval %||% control$itermax %||%
                                 max(40L * length(start), 50L))
  }
  if (is.null(bctrl$rhobeg)) {
    span <- upper - lower
    span <- span[is.finite(span) & span > 0]
    if (length(span)) bctrl$rhobeg <- min(span) / 4
  }
  if (is.null(bctrl$iprint)) bctrl$iprint <- 0
  bctrl
}

#' @noRd
.bobyqa_once <- function(fn, start, lower, upper, bctrl, extra = list()) {
  if (!requireNamespace("minqa", quietly = TRUE)) {
    stop("Package 'minqa' is required for method = \"BOBYQA\" / \"multi-BOBYQA\"",
         call. = FALSE)
  }
  do.call(minqa::bobyqa, c(
    list(par = start, fn = fn, lower = lower, upper = upper, control = bctrl),
    extra
  ))
}

#' @noRd
.multistart_starts <- function(nstarts, start, lower, upper, seed = NULL,
                               include_default = TRUE) {
  nstarts <- as.integer(nstarts)
  k <- length(start)
  if (!is.null(seed)) set.seed(as.integer(seed))
  starts <- matrix(NA_real_, nstarts, k)
  n_draw <- nstarts
  row0 <- 1L
  if (isTRUE(include_default)) {
    starts[1, ] <- as.numeric(start)
    n_draw <- nstarts - 1L
    row0 <- 2L
  }
  if (n_draw > 0L) {
    if (requireNamespace("lhs", quietly = TRUE)) {
      U <- lhs::maximinLHS(n_draw, k)
    } else {
      U <- matrix(stats::runif(n_draw * k), n_draw, k)
    }
    if (is.null(dim(U))) U <- matrix(U, ncol = 1L)
    extra <- sweep(U, 2, upper - lower, `*`)
    extra <- sweep(extra, 2, lower, `+`)
    for (i in seq_len(n_draw)) {
      starts[row0 + i - 1L, ] <- .interior_start(extra[i, ], lower, upper)
    }
  }
  starts
}

#' Dispatch DEoptim / BOBYQA / DDS / CMA-ES
#' @noRd
.run_calibrate_optimizer <- function(method, fn, start, lower, upper, control,
                                     extra = list()) {
  method <- .normalize_calibrate_method(method)
  start <- as.numeric(start)
  lower <- as.numeric(lower)
  upper <- as.numeric(upper)

  if (identical(method, "DEoptim")) {
    if (!requireNamespace("DEoptim", quietly = TRUE)) {
      stop("Package 'DEoptim' is required for method = \"DEoptim\"", call. = FALSE)
    }
    ctrl <- control
    ctrl$start <- NULL
    ctrl$par <- NULL
    ctrl$maxeval <- NULL
    ctrl$r <- NULL
    ctrl$seed <- NULL
    ctrl$maxfun <- NULL
    ctrl$maxit <- NULL
    ctrl$npt <- NULL
    ctrl$rhobeg <- NULL
    ctrl$rhoend <- NULL
    ctrl$iprint <- NULL
    ctrl$sigma <- NULL
    if (is.null(ctrl$NP) || (length(ctrl$NP) && is.na(ctrl$NP))) {
      ctrl$NP <- 10L * length(start)
    }
    if (is.null(ctrl$itermax) || (length(ctrl$itermax) && is.na(ctrl$itermax))) {
      ctrl$itermax <- 200L
    }
    fit <- do.call(DEoptim::DEoptim, c(
      list(fn = fn, lower = lower, upper = upper,
           control = do.call(DEoptim::DEoptim.control, ctrl)),
      extra
    ))
    return(list(par = as.numeric(fit$optim$bestmem), value = fit$optim$bestval,
                fit = fit))
  }

  if (identical(method, "BOBYQA") || identical(method, "multi-BOBYQA")) {
    bctrl <- .bobyqa_control(control, start, lower, upper)
    if (identical(method, "BOBYQA")) {
      fit <- .bobyqa_once(fn, start, lower, upper, bctrl, extra)
      return(list(par = as.numeric(fit$par), value = as.numeric(fit$fval), fit = fit))
    }
    nstarts <- .bobyqa_nstarts(control)
    include_default <- if (is.null(control$include_default)) TRUE else isTRUE(control$include_default)
    starts <- .multistart_starts(
      nstarts, start, lower, upper,
      seed = control$seed, include_default = include_default
    )
    fits <- vector("list", nstarts)
    best_i <- NA_integer_
    best_val <- Inf
    best_par <- start
    for (i in seq_len(nstarts)) {
      fits[[i]] <- .bobyqa_once(fn, starts[i, ], lower, upper, bctrl, extra)
      val <- as.numeric(fits[[i]]$fval)
      if (is.finite(val) && val < best_val) {
        best_val <- val
        best_par <- as.numeric(fits[[i]]$par)
        best_i <- i
      }
    }
    fit <- list(
      starts = starts, fits = fits, best_start = best_i, nstarts = nstarts
    )
    return(list(par = best_par, value = best_val, fit = fit))
  }

  if (identical(method, "DDS")) {
    fit <- do.call(.dds_optimize, c(
      list(fn = fn, par = start, lower = lower, upper = upper, control = control),
      extra
    ))
    return(list(par = as.numeric(fit$par), value = as.numeric(fit$value), fit = fit))
  }

  if (!requireNamespace("cmaes", quietly = TRUE)) {
    stop("Package 'cmaes' is required for method = \"CMA-ES\"", call. = FALSE)
  }
  cctrl <- control
  cctrl$start <- NULL
  cctrl$par <- NULL
  cctrl$r <- NULL
  cctrl$maxeval <- NULL
  cctrl$maxfun <- NULL
  cctrl$seed <- NULL
  cctrl$NP <- NULL
  cctrl$itermax <- NULL
  cctrl$npt <- NULL
  cctrl$rhobeg <- NULL
  cctrl$rhoend <- NULL
  cctrl$iprint <- NULL
  if (is.null(cctrl$maxit)) {
    budget <- as.integer(control$maxeval %||% 200L)
    lambda <- max(4L, as.integer(4 + floor(3 * log(length(start)))))
    cctrl$maxit <- max(20L, as.integer(ceiling(budget / lambda)))
  }
  fit <- do.call(cmaes::cma_es, c(
    list(par = start, fn = fn, lower = lower, upper = upper, control = cctrl),
    extra
  ))
  list(par = as.numeric(fit$par), value = as.numeric(fit$value), fit = fit)
}

#' @noRd
.calibrate_n_total <- function(method, control, n_par) {
  method <- .normalize_calibrate_method(method)
  if (identical(method, "DEoptim")) {
    np <- control$NP %||% (10L * n_par)
    itermax <- control$itermax %||% 200L
    return(as.integer(np) * (as.integer(itermax) + 1L))
  }
  if (identical(method, "BOBYQA")) {
    return(as.integer(control$maxfun %||% control$maxeval %||% control$itermax %||%
                        max(40L * n_par, 50L)))
  }
  if (identical(method, "multi-BOBYQA")) {
    nstarts <- .bobyqa_nstarts(control)
    per <- as.integer(control$maxfun %||% control$maxeval %||% control$itermax %||%
                        max(40L * n_par, 50L))
    return(nstarts * per)
  }
  if (identical(method, "DDS")) {
    return(as.integer(control$maxeval %||% 200L))
  }
  budget <- as.integer(control$maxeval %||% 200L)
  lambda <- max(4L, as.integer(4 + floor(3 * log(n_par))))
  as.integer((control$maxit %||% max(20L, ceiling(budget / lambda))) * lambda)
}

#' @noRd
.calibrate_np <- function(method, control, n_par) {
  method <- .normalize_calibrate_method(method)
  if (identical(method, "DEoptim")) return(as.integer(control$NP %||% (10L * n_par)))
  1L
}
