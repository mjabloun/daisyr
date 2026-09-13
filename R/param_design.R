#' Normalise parameter bounds to a data.frame of name/min/max
#'
#' Accepts a data.frame (columns `name`, `min`, `max`) or a list of
#' `{name, min, max}` entries. List entries may omit `name` when the list
#' itself is named. Length is not hardcoded: one parameter or fifty both
#' work.
#'
#' @param params Parameter bounds, as described above.
#' @return A `data.frame` with character `name` and numeric `min`/`max`.
#' @keywords internal
.normalize_param_bounds <- function(params) {
  if (inherits(params, "daisyr_param_config"))
    params <- params$parameters

  as_row <- function(p, fallback_name = NA_character_) {
    if (is.null(p))
      stop("params contains a NULL entry")
    if (is.numeric(p) && length(p) == 2L && is.null(names(p))) {
      nm <- fallback_name
      if (is.na(nm) || !nzchar(nm))
        stop("A numeric min/max pair needs a name (use a named list or include `name`)")
      return(data.frame(name = nm, min = as.numeric(p[1]), max = as.numeric(p[2]),
                        stringsAsFactors = FALSE))
    }
    if (is.list(p) || is.data.frame(p)) {
      nm <- p[["name", exact = TRUE]]
      if (is.null(nm) || (length(nm) == 1L && is.na(nm))) nm <- fallback_name
      if (is.null(nm) || (length(nm) == 1L && (is.na(nm) || !nzchar(as.character(nm)))))
        stop("Each params entry must have a `name` (or be a named list element)")
      if (is.null(p[["min", exact = TRUE]]) || is.null(p[["max", exact = TRUE]]))
        stop("Each params entry must have `min` and `max`")
      return(data.frame(name = as.character(nm),
                        min = as.numeric(p[["min", exact = TRUE]]),
                        max = as.numeric(p[["max", exact = TRUE]]),
                        stringsAsFactors = FALSE))
    }
    stop("params must be a data.frame or a list of {name, min, max} entries")
  }

  if (is.data.frame(params)) {
    if (!all(c("name", "min", "max") %in% names(params)))
      stop("params data.frame must have columns `name`, `min`, and `max`")
    df <- data.frame(
      name = as.character(params$name),
      min = as.numeric(params$min),
      max = as.numeric(params$max),
      stringsAsFactors = FALSE
    )
  } else if (is.list(params)) {
    nms <- names(params)
    rows <- vector("list", length(params))
    for (i in seq_along(params)) {
      fallback <- if (!is.null(nms) && nzchar(nms[i])) nms[i] else NA_character_
      rows[[i]] <- as_row(params[[i]], fallback)
    }
    df <- do.call(rbind, rows)
  } else {
    stop("params must be a data.frame or a list of {name, min, max} entries")
  }

  if (nrow(df) < 1L)
    stop("params must contain at least one parameter")
  if (anyNA(df$name) || any(!nzchar(df$name)))
    stop("Every parameter must have a non-empty name")
  if (anyDuplicated(df$name) > 0)
    stop("Duplicate parameter name(s): ",
         paste(unique(df$name[duplicated(df$name)]), collapse = ", "))
  if (anyNA(df$min) || anyNA(df$max))
    stop("params min/max must be numeric and non-missing")
  if (any(df$min > df$max))
    stop("Each parameter's min must be <= max")
  rownames(df) <- NULL
  df
}

#' Rescale a unit-hypercube matrix to parameter bounds
#' @keywords internal
.scale_unit_design <- function(unit, bounds) {
  k <- nrow(bounds)
  if (ncol(unit) != k)
    stop("Internal error: unit design has ", ncol(unit), " columns but ", k, " parameters")
  out <- vapply(seq_len(k), function(j) {
    bounds$min[j] + (bounds$max[j] - bounds$min[j]) * unit[, j]
  }, numeric(nrow(unit)))
  if (is.null(dim(out))) out <- matrix(out, ncol = k)
  colnames(out) <- bounds$name
  as.data.frame(out, stringsAsFactors = FALSE)
}

#' Draw a unit-hypercube sample of size `n` in `k` dimensions
#'
#' `...` is only meaningful for `method = "dice_lhs"`, where it is forwarded
#' to `DiceDesign::maximinESE_LHS()` (e.g. `it`, `inner_it`, `J`, `p`, `T0`)
#' so the optimization effort can be tuned without a new argument here for
#' every knob that package exposes.
#' @keywords internal
.unit_param_sample <- function(n, k, method, seed, ...) {
  method <- match.arg(method, c("lhs", "sobol", "dice_lhs"))
  if (!is.null(seed)) set.seed(seed)

  if (method == "lhs") {
    if (!requireNamespace("lhs", quietly = TRUE))
      stop("Package 'lhs' is required for method = \"lhs\"; please install it")
    U <- lhs::maximinLHS(n, k)
  } else if (method == "dice_lhs") {
    if (!requireNamespace("DiceDesign", quietly = TRUE))
      stop("Package 'DiceDesign' is required for method = \"dice_lhs\"; please install it")
    # lhsDesign() draws a starting Latin hypercube in the unit cube;
    # maximinESE_LHS() then runs its Enhanced Stochastic Evolutionary
    # algorithm on that starting design to improve its maximin spacing.
    # DiceDesign's own seed argument only covers lhsDesign()'s starting
    # draw -- set.seed(seed) above (already called) is what makes the ESE
    # optimization itself reproducible too.
    # Use [[..., exact = TRUE]]: the lhsDesign() list has both `design` and
    # `dimension`, so `$design` is the same partial-match hazard as
    # `$seed` vs `seed_bed` elsewhere in this package.
    start <- DiceDesign::lhsDesign(n, dimension = k, seed = seed)
    optimized <- DiceDesign::maximinESE_LHS(start[["design", exact = TRUE]], ...)
    U <- optimized[["design", exact = TRUE]]
  } else {
    if (!requireNamespace("randtoolbox", quietly = TRUE))
      stop("Package 'randtoolbox' is required for method = \"sobol\"; please install it")
    sobol_seed <- if (is.null(seed)) 4711L else as.integer(seed)
    # scrambling = 3 is Owen + Faure-Tezuka (the requested default). Current
    # randtoolbox releases temporarily disable scrambling and warn; muffle
    # that known warning so every design draw isn't noisy. The unscrambled
    # sequence is still a valid, extensible low-discrepancy design.
    U <- withCallingHandlers(
      randtoolbox::sobol(n, dim = k, scrambling = 3, seed = sobol_seed),
      warning = function(w) {
        if (grepl("scrambling is currently disabled", conditionMessage(w),
                  fixed = TRUE))
          invokeRestart("muffleWarning")
      }
    )
  }

  if (is.null(dim(U))) U <- matrix(U, ncol = 1L)
  if (k == 1L && ncol(U) != 1L) U <- matrix(U, ncol = 1L)
  U
}

#' Attach metadata used by [extend_param_design()] / [run_param_design()]
#' @keywords internal
.as_param_design <- function(df, bounds, method, seed, n_drawn) {
  rownames(df) <- NULL
  class(df) <- c("daisyr_param_design", "data.frame")
  attr(df, "params") <- bounds
  attr(df, "method") <- method
  attr(df, "seed") <- seed
  attr(df, "n_drawn") <- as.integer(n_drawn)
  df
}

#' Space-filling parameter design for a Daisy metamodel
#'
#' Draws `n` points in the parameter hyper-rectangle defined by `params`,
#' intended as the training design for a surrogate / emulator of Daisy.
#' Dimension `k` is whatever `params` contains (one parameter or many); it
#' is not hardcoded.
#'
#' \describe{
#'   \item{`method = "lhs"`}{An optimized Latin hypercube via
#'     `lhs::maximinLHS()`. That is the general-purpose choice: its cost
#'     stays reasonable as `n` or `k` grows. When `n * k` is small enough
#'     that `lhs::optimumLHS()`'s simulated-annealing search is practical,
#'     that function typically yields a slightly better maximin design;
#'     it is not used here because its runtime grows quickly.}
#'   \item{`method = "dice_lhs"`}{A Latin hypercube built with
#'     `DiceDesign::lhsDesign()` and then optimized with
#'     `DiceDesign::maximinESE_LHS()`'s Enhanced Stochastic Evolutionary
#'     algorithm. For a fixed, one-shot batch this typically reaches a
#'     better maximin spacing than `"lhs"` for the same `n`/`k`, at the
#'     cost of a heavier optimization pass -- worth it when the design is
#'     small and every Daisy run is expensive, which is the usual
#'     metamodel-training situation. `...` is forwarded to
#'     `maximinESE_LHS()` (e.g. `it`, `inner_it`, `J`, `p`, `T0`) to tune
#'     that search; the defaults from `DiceDesign` are used otherwise. Like
#'     `"lhs"`, the result is not extensible -- see [extend_param_design()].}
#'   \item{`method = "sobol"`}{A Sobol' low-discrepancy sequence via
#'     `randtoolbox::sobol(n, dim = k, scrambling = 3)` (Owen + Faure-Tezuka
#'     scrambling when the installed randtoolbox supports it). Recent
#'     randtoolbox versions temporarily disable scrambling; the call is
#'     unchanged so it will pick scrambling back up when the package
#'     re-enables it. Sobol' designs can be extended later with
#'     [extend_param_design()] without rebuilding the existing points.}
#' }
#'
#' All three methods draw in the unit hypercube and are then rescaled to
#' each parameter's `[min, max]`. `lhs`, `DiceDesign`, and `randtoolbox` are
#' optional dependencies (in Suggests); only the package backing the
#' requested `method` is required.
#'
#' @param params A `data.frame` with columns `name`, `min`, `max` (one row
#'   per parameter), or a list of `{name, min, max}` entries. Named list
#'   elements may omit `name`. A `daisyr_param_config` is also accepted (bounds
#'   taken from its `parameters` table).
#' @param n Number of design points (rows) to generate.
#' @param method `"lhs"`, `"dice_lhs"`, or `"sobol"`.
#' @param seed Optional integer. If given, `set.seed(seed)` is called before
#'   sampling (covering the `"dice_lhs"` optimization pass too, since
#'   `DiceDesign::maximinESE_LHS()` has no seed argument of its own), and
#'   for Sobol' the same value is passed as `randtoolbox::sobol()`'s
#'   scrambling seed so the sequence is reproducible and
#'   [extend_param_design()] can continue it.
#' @param ... Forwarded to `DiceDesign::maximinESE_LHS()` when
#'   `method = "dice_lhs"`; ignored otherwise.
#'
#' @return A `data.frame` (class `daisyr_param_design`) with one column per
#'   parameter and `n` rows. Attributes `params`, `method`, `seed`, and
#'   `n_drawn` are attached for [extend_param_design()].
#'
#' @examples
#' \dontrun{
#' params <- data.frame(
#'   name = c("Ap_clay", "LAIvsDS_L"),
#'   min = c(0.05, 3),
#'   max = c(0.10, 7)
#' )
#' generate_param_design(params, n = 20, method = "lhs", seed = 1)
#' generate_param_design(params, n = 20, method = "dice_lhs", seed = 1)
#' generate_param_design(params, n = 20, method = "sobol", seed = 1)
#' }
#' @seealso [extend_param_design()], [run_param_design()]
#' @export
generate_param_design <- function(params, n, method = c("lhs", "dice_lhs", "sobol"),
                                   seed = NULL, ...) {
  method <- match.arg(method)
  bounds <- .normalize_param_bounds(params)
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n) || n < 1L)
    stop("`n` must be a positive integer")

  U <- .unit_param_sample(n, k = nrow(bounds), method = method, seed = seed, ...)
  df <- .scale_unit_design(U, bounds)
  .as_param_design(df, bounds = bounds, method = method, seed = seed, n_drawn = n)
}

#' Add points to an existing Sobol' parameter design
#'
#' Continues the same Sobol' sequence from where `existing_design` left off
#' (indices `n_drawn + 1` through `n_drawn + n_additional`), so the old and
#' new points together remain a single low-discrepancy design. Returns only
#' the new rows, with the same columns as [generate_param_design()].
#'
#' Optimized LHS designs are not extensible: calling this with
#' `method = "lhs"` or `method = "dice_lhs"` (or an existing_design built
#' with either) stops and tells you to call [generate_param_design()] with
#' a larger `n` instead.
#'
#' @param existing_design A `daisyr_param_design` from
#'   [generate_param_design()] or a previous [extend_param_design()] call.
#'   Always pass the most recently returned object if you are adding points
#'   in several batches (its `n_drawn` attribute is the sequence offset).
#' @param params Parameter bounds. Defaults to `attr(existing_design, "params")`.
#' @param n_additional Number of new rows to draw.
#' @param method Must be `"sobol"`.
#'
#' @return A `daisyr_param_design` of `n_additional` rows (new points only).
#' @seealso [generate_param_design()]
#' @export
extend_param_design <- function(existing_design, params = NULL, n_additional,
                                method = "sobol") {
  method <- match.arg(method, c("lhs", "dice_lhs", "sobol"))
  if (!identical(method, "sobol"))
    stop("extend_param_design() is only meaningful for method = \"sobol\". ",
         "Optimized LHS designs (\"lhs\" or \"dice_lhs\") are not extensible; ",
         "call generate_param_design() with a larger n to build a fresh ",
         "Latin hypercube instead.")

  existing_method <- attr(existing_design, "method")
  if (!is.null(existing_method) && !identical(existing_method, "sobol"))
    stop("existing_design was built with method = \"", existing_method, "\". ",
         "Optimized LHS designs (\"lhs\" or \"dice_lhs\") are not extensible; ",
         "call generate_param_design() with a larger n to build a fresh ",
         "Latin hypercube instead.")

  if (is.null(params)) params <- attr(existing_design, "params")
  if (is.null(params))
    stop("`params` is missing and existing_design has no `params` attribute; ",
         "pass the same bounds used to build the original design")
  bounds <- .normalize_param_bounds(params)

  n_additional <- as.integer(n_additional)
  if (length(n_additional) != 1L || is.na(n_additional) || n_additional < 1L)
    stop("`n_additional` must be a positive integer")

  design_names <- names(existing_design)
  if (!identical(design_names, bounds$name))
    stop("params names do not match existing_design columns: ",
         "design has [", paste(design_names, collapse = ", "), "], ",
         "params has [", paste(bounds$name, collapse = ", "), "]")

  n_drawn <- attr(existing_design, "n_drawn")
  if (is.null(n_drawn)) n_drawn <- nrow(existing_design)
  n_drawn <- as.integer(n_drawn)
  seed <- attr(existing_design, "seed")

  n_total <- n_drawn + n_additional
  U <- .unit_param_sample(n_total, k = nrow(bounds), method = "sobol", seed = seed)
  U_new <- U[seq.int(n_drawn + 1L, n_total), , drop = FALSE]
  df <- .scale_unit_design(U_new, bounds)
  .as_param_design(df, bounds = bounds, method = "sobol", seed = seed,
                   n_drawn = n_total)
}

#' Drop NULL entries from a list
#' @keywords internal
.drop_nulls <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

#' Resolve a Daisy output path against a working directory
#' @keywords internal
.design_src_path <- function(rel, src_root) {
  if (grepl("^(?:[A-Za-z]:[\\\\/]|[\\\\/])", rel)) return(rel)
  if (!is.null(src_root) && nzchar(src_root)) file.path(src_root, rel) else rel
}

#' Archived filename: `run_<id>_<original basename>`
#' @keywords internal
.archived_filename <- function(run_id, path) {
  paste0(run_id, "_", basename(path))
}

#' Ensure selected outputs don't share a basename (they would collide when flattened)
#' @keywords internal
.check_unique_basenames <- function(output_files) {
  bases <- vapply(output_files, basename, character(1))
  if (anyDuplicated(bases) > 0)
    stop("output_files must have unique basenames when kept as ",
         "run_<id>_<filename> in a single directory; duplicates: ",
         paste(unique(bases[duplicated(bases)]), collapse = ", "))
  invisible(bases)
}

#' Copy selected Daisy output files into keep_dir as run_<id>_<filename>
#' @keywords internal
.copy_design_outputs <- function(output_files, src_root, keep_dir, run_id) {
  copied <- stats::setNames(rep(NA_character_, length(output_files)), names(output_files))
  for (nm in names(output_files)) {
    rel <- output_files[[nm]]
    src <- .design_src_path(rel, src_root)
    dest_name <- .archived_filename(run_id, rel)
    dest <- file.path(keep_dir, dest_name)
    if (!file.exists(src)) {
      warning(sprintf("run_param_design: output file not found, skipped: %s", src))
      next
    }
    ok <- file.copy(src, dest, overwrite = TRUE)
    if (!isTRUE(ok)) {
      warning(sprintf("run_param_design: failed to copy %s -> %s", src, dest))
      next
    }
    copied[[nm]] <- dest_name
  }
  copied
}

#' Read one named set of Daisy output files
#' @keywords internal
.read_named_outputs <- function(output_files, src_root, output_reader, reader_args) {
  out <- vector("list", length(output_files))
  names(out) <- names(output_files)
  for (nm in names(output_files)) {
    rel <- output_files[[nm]]
    path <- .design_src_path(rel, src_root)
    if (!file.exists(path)) {
      warning(sprintf("Output file not found, skipped: %s", path))
      next
    }
    out[[nm]] <- do.call(output_reader, c(list(path), reader_args))
  }
  out
}

#' Apply [read_param_design()] / [compute_generic_objective()]-style mutators
#'
#' `sim_mutator` is either a single `function(dt)` applied to every table, or
#' a named list of such functions keyed by `output_files` names.
#' @keywords internal
.apply_sim_mutator <- function(tables, sim_mutator) {
  if (is.null(sim_mutator)) return(tables)
  mutate_one <- function(dt, fun, label) {
    if (is.null(dt) || is.null(fun)) return(dt)
    if (!is.function(fun))
      stop("sim_mutator", if (!is.null(label)) paste0("[['", label, "']]"),
           " must be a function")
    fun(dt)
  }
  if (is.function(sim_mutator)) {
    return(lapply(tables, mutate_one, fun = sim_mutator, label = NULL))
  }
  if (is.list(sim_mutator)) {
    for (nm in names(tables))
      tables[[nm]] <- mutate_one(tables[[nm]], sim_mutator[[nm]], nm)
    return(tables)
  }
  stop("sim_mutator must be NULL, a function, or a named list of functions")
}

#' Stack per-run output tables into one data.table per output file
#'
#' Binds every run of the same named output into a single table with a
#' `run_id` column (parameter values are *not* copied in; join `design` later).
#' A single output file is returned as that data.table; several files are a
#' named list of data.tables.
#' @keywords internal
.stack_design_outputs <- function(outputs_by_run) {
  file_nms <- unique(unlist(lapply(outputs_by_run, names), use.names = FALSE))
  stacked <- lapply(file_nms, function(nm) {
    pieces <- vector("list", length(outputs_by_run))
    for (i in seq_along(outputs_by_run)) {
      dt <- outputs_by_run[[i]][[nm]]
      if (is.null(dt)) next
      if (!is.data.frame(dt))
        dt <- data.table::data.table(value = dt)
      else
        dt <- data.table::copy(data.table::as.data.table(dt))
      rid <- names(outputs_by_run)[[i]]
      dt[, run_id := rid]
      data.table::setcolorder(dt, c("run_id", setdiff(names(dt), "run_id")))
      pieces[[i]] <- dt
    }
    data.table::rbindlist(pieces, fill = TRUE, use.names = TRUE)
  })
  names(stacked) <- file_nms
  if (length(stacked) == 1L) stacked[[1L]] else stacked
}

#' Normalise `output_files` to a named character vector
#' @keywords internal
.normalize_output_files <- function(output_files) {
  if (is.null(output_files) || length(output_files) == 0L)
    stop("`output_files` must name at least one Daisy output file to keep and/or read")
  files <- unlist(output_files, use.names = TRUE)
  if (is.null(names(files)) || any(!nzchar(names(files)))) {
    nms <- names(files)
    if (is.null(nms)) nms <- rep("", length(files))
    missing <- !nzchar(nms)
    nms[missing] <- vapply(files[missing], function(p) {
      tools::file_path_sans_ext(basename(p))
    }, character(1))
    names(files) <- nms
  }
  if (anyDuplicated(names(files)) > 0)
    stop("output_files names must be unique")
  files
}

#' Run Daisy for every row of a metamodel parameter design
#'
#' For each row of `design`, renders the config's templates, runs Daisy,
#' then optionally archives selected output files and/or reads them.
#'
#' Daisy overwrites its output directory in place, so without archiving or
#' reading, a later row would destroy earlier results. Kept files are copied
#' into a **single** `keep_dir` (no per-row folders - that does not scale to
#' thousands of combinations) and renamed `run_<id>_<filename>`, e.g.
#' `run_0001_harvest.dlf`. `design.csv` in the same directory maps each
#' `run_id` to its parameter combination.
#'
#' Daisy can write many log files; only paths listed in `output_files` are
#' kept (and/or read). Reading can happen here or later via
#' [read_param_design()]. If you choose not to keep files (`keep_files = FALSE`),
#' reading is mandatory - otherwise the outputs would be lost when the next
#' row runs. A custom `output_reader` is accepted (default [read_dlf()]).
#' Restrict or reshape columns with `sim_mutator` (same idea as
#' [compute_generic_objective()]), not a separate `cols` argument.
#'
#' @param design A `data.frame` from [generate_param_design()] (or any
#'   table whose columns are parameter names).
#' @param config A `daisyr_param_config` used to render `.dai` templates.
#' @param run_file,daisy_exe,working_dir,show_log,cmd Passed to [run_daisy()].
#'   `daisy_exe` may be `NULL` to use [get_daisy_path()].
#' @param template_dir,output_dir Passed to [render_templates()].
#' @param output_files Named character vector of Daisy output paths relative
#'   to `working_dir` (e.g. `c(harvest = "Output/harvest.dlf", swc =
#'   "Output/interval_water_content.dlf")`). Only these files are archived
#'   or read; other Daisy outputs are ignored.
#' @param keep_files If `TRUE` (default), copy each selected file into
#'   `keep_dir` as `run_<id>_<filename>` after each run.
#' @param keep_dir Directory for the renamed copies and `design.csv`.
#'   Ignored when `keep_files = FALSE`.
#' @param read If `TRUE`, read `output_files` after each run (via
#'   `output_reader`) and return them. Defaults to `FALSE` when
#'   `keep_files = TRUE` (read later with [read_param_design()]), and to
#'   `TRUE` when `keep_files = FALSE`. Setting both to `FALSE` is an error.
#' @param output_reader Function used to parse each file. First argument is
#'   the path. Defaults to [read_dlf()].
#' @param digits Forwarded to `output_reader` when non-`NULL` (matches
#'   [read_dlf()]'s rounding argument).
#' @param reader_args Named list of additional arguments forwarded to
#'   `output_reader` (merged with `digits`; `reader_args` wins on conflict).
#' @param sim_mutator Optional function applied to each simulated table after
#'   reading - the same role as in [compute_generic_objective()]. Use it to
#'   subset columns, [add_date()], rename fields, or derive metrics. A named
#'   list of functions, keyed by `output_files` names, applies a different
#'   mutator per file; missing names are left unchanged.
#' @param progress If `TRUE` (the default), print a console progress bar.
#'   `FALSE` disables it. A function `function(i, n)` is called after every
#'   Daisy run (`i` completed out of `n`), and once with `i = 0` before the
#'   first run.
#'
#' @return A list with:
#'   \itemize{
#'     \item `design` - the input design with a `run_id` column prepended.
#'     \item `keep_dir` - the archive directory, or `NULL` if `keep_files = FALSE`.
#'     \item `outputs` - if `read = TRUE`, the stacked tables (same shape as
#'       [read_param_design()]); otherwise `NULL`.
#'   }
#'
#' @examples
#' \dontrun{
#' output_files <- c(
#'   harvest = "Output/harvest.dlf",
#'   swc     = "Output/interval_water_content.dlf"
#' )
#'
#' # Run Daisy for every design row; keep both logs, do not read yet
#' # (typical for thousands of combinations). Files land in keep_dir as
#' # run_0001_harvest.dlf, run_0001_interval_water_content.dlf, ...
#' run <- run_param_design(
#'   design, config, run_file, daisy_exe,
#'   output_files = output_files,
#'   working_dir  = "path/to/daisy/scenario",
#'   keep_files   = TRUE,
#'   keep_dir     = "metamodel_runs"
#' )
#' run$outputs   # NULL: nothing was read in-loop
#' run$design    # run_id + parameter values (also written to design.csv)
#'
#' # Same two files, read in-loop (small n). outputs is a named list of
#' # two stacked data.tables, one per output_files name.
#' run_and_read <- run_param_design(
#'   design, config, run_file, daisy_exe,
#'   output_files = output_files,
#'   working_dir  = "path/to/daisy/scenario",
#'   keep_files   = TRUE,
#'   keep_dir     = "metamodel_runs",
#'   read         = TRUE,
#'   sim_mutator  = list(
#'     harvest = function(dt) {
#'       dt <- add_date(dt, "harvest")
#'       dt[, c("Date", "WSOrg"), with = FALSE]
#'     },
#'     swc = function(dt) {
#'       dt <- add_date(dt, "swc")
#'       dt[, c("Date", "Theta @ -1.25"), with = FALSE]
#'     }
#'   )
#' )
#' run_and_read$outputs$harvest
#' run_and_read$outputs$swc
#' }
#'
#' @seealso [generate_param_design()], [read_param_design()], [run_sa_design()]
#' @export
run_param_design <- function(design, config, run_file, daisy_exe = NULL,
                              output_files,
                              working_dir = NULL, template_dir = ".", output_dir = ".",
                              show_log = FALSE,
                              keep_files = TRUE, keep_dir = "param_design_runs",
                              read = NULL,
                              output_reader = read_dlf,
                              digits = NULL,
                              reader_args = list(),
                              sim_mutator = NULL,
                              progress = TRUE,
                              cmd = NULL) {
  if (is.null(design) || nrow(design) < 1L)
    stop("`design` must have at least one row")

  keep_files <- isTRUE(keep_files)
  if (is.null(read)) read <- !keep_files
  read <- isTRUE(read)
  if (!keep_files && !read)
    stop("When keep_files = FALSE, outputs must be read in-memory (read = TRUE); ",
         "otherwise each Daisy run would overwrite the previous row's files and they would be lost")

  output_files <- .normalize_output_files(output_files)
  if (keep_files) .check_unique_basenames(output_files)
  reader_args <- .drop_nulls(utils::modifyList(list(digits = digits),
                                               reader_args))

  design_df <- as.data.frame(design, stringsAsFactors = FALSE)
  extra <- intersect(names(design_df), "run_id")
  if (length(extra)) design_df <- design_df[setdiff(names(design_df), extra)]

  param_names <- param_config_names(config)
  missing <- setdiff(param_names, names(design_df))
  if (length(missing) > 0)
    stop("design is missing parameter(s): ", paste(missing, collapse = ", "))

  n_runs <- nrow(design_df)
  run_ids <- if (!is.null(rownames(design)) && !identical(rownames(design), as.character(seq_len(n_runs)))) {
    rownames(design)
  } else {
    sprintf("run_%0*d", max(4L, nchar(as.character(n_runs))), seq_len(n_runs))
  }

  src_root <- if (!is.null(working_dir)) working_dir else "."
  if (keep_files) {
    dir.create(keep_dir, recursive = TRUE, showWarnings = FALSE)
    design_out <- data.frame(run_id = run_ids, design_df[, param_names, drop = FALSE],
                             stringsAsFactors = FALSE)
    utils::write.csv(design_out, file.path(keep_dir, "design.csv"), row.names = FALSE)
    manifest <- list(output_files = as.list(output_files))
    saveRDS(manifest, file.path(keep_dir, "manifest.rds"))
  }

  outputs <- if (read) vector("list", n_runs) else NULL
  if (read) names(outputs) <- run_ids

  prog <- .run_progress_ticker(progress, n_runs)
  if (!is.null(prog$pb)) on.exit(close(prog$pb), add = TRUE)
  prog$tick(0L)

  for (i in seq_len(n_runs)) {
    prog$tick(i - 1L)
    values <- stats::setNames(as.numeric(design_df[i, param_names, drop = TRUE]),
                              param_names)
    render_templates(config, values, template_dir = template_dir, output_dir = output_dir)
    run_daisy(run_file, daisy_exe = daisy_exe, working_dir = working_dir,
              show_log = show_log, cmd = cmd)

    if (keep_files)
      .copy_design_outputs(output_files, src_root, keep_dir, run_ids[i])

    if (read) {
      tables <- .read_named_outputs(output_files, src_root, output_reader, reader_args)
      tables <- .apply_sim_mutator(tables, sim_mutator)
      outputs[[i]] <- tables
    }

    prog$tick(i)
  }

  if (read)
    outputs <- .stack_design_outputs(outputs)

  list(
    design = data.frame(run_id = run_ids, design_df[, param_names, drop = FALSE],
                        stringsAsFactors = FALSE),
    keep_dir = if (keep_files) keep_dir else NULL,
    outputs = outputs
  )
}

#' Read archived outputs from a [run_param_design()] directory
#'
#' Independent of [run_param_design()]: given a `keep_dir` produced with
#' `keep_files = TRUE`, reads every `run_<id>_<filename>` copy and row-binds
#' them into one table per output file. Parameter values are not copied onto
#' those rows - join `design` on `run_id` later if you need them.
#'
#' @param keep_dir Directory created by [run_param_design()] (`keep_files = TRUE`).
#' @param output_files Named character vector of the *original* Daisy output
#'   paths (same as passed to [run_param_design()]). Used only to recover
#'   the `<filename>` suffix. If `NULL`, the `manifest.rds` written by
#'   [run_param_design()] is used.
#' @param output_reader,digits,reader_args,sim_mutator Same meaning as in
#'   [run_param_design()]. `sim_mutator` is applied after reading, before
#'   runs are stacked; use it to subset columns (there is no `cols` argument).
#'
#' @return A list with `design` (from `design.csv`) and `outputs`. `outputs`
#'   is a `data.table` of all runs when a single file was kept, with a
#'   `run_id` column; if several `output_files` were kept, a named list of
#'   such data.tables (one per file).
#'
#' @examples
#' \dontrun{
#' # After run_param_design(..., output_files = c(harvest = ..., swc = ...),
#' # keep_dir = "metamodel_runs"): two stacked tables, not one mixed table.
#' got <- read_param_design(
#'   "metamodel_runs",
#'   sim_mutator = list(
#'     harvest = function(dt) {
#'       dt <- add_date(dt, "harvest")
#'       dt[, c("Date", "WSOrg"), with = FALSE]
#'     },
#'     swc = function(dt) {
#'       dt <- add_date(dt, "swc")
#'       dt[, c("Date", "Theta @ -1.25"), with = FALSE]
#'     }
#'   )
#' )
#'
#' got$design              # all parameter rows
#' got$outputs$harvest     # all runs, harvest columns + run_id
#' got$outputs$swc         # all runs, swc columns + run_id
#'
#' # Parameter values are not copied onto the output rows; join when needed:
#' harvest <- merge(got$design, got$outputs$harvest, by = "run_id")
#' swc     <- merge(got$design, got$outputs$swc,     by = "run_id")
#' }
#'
#' @seealso [run_param_design()]
#' @export
read_param_design <- function(keep_dir, output_files = NULL,
                               output_reader = read_dlf,
                               digits = NULL,
                               reader_args = list(),
                               sim_mutator = NULL) {
  if (!dir.exists(keep_dir))
    stop("keep_dir does not exist: ", keep_dir)

  reader_args <- .drop_nulls(utils::modifyList(list(digits = digits),
                                               reader_args))

  design_path <- file.path(keep_dir, "design.csv")
  design <- if (file.exists(design_path)) {
    utils::read.csv(design_path, stringsAsFactors = FALSE)
  } else {
    NULL
  }

  if (is.null(output_files)) {
    man_path <- file.path(keep_dir, "manifest.rds")
    if (!file.exists(man_path))
      stop("output_files is NULL and keep_dir has no manifest.rds; ",
           "pass output_files explicitly (original Daisy paths, used for the ",
           "run_<id>_<filename> suffix)")
    output_files <- unlist(readRDS(man_path)$output_files, use.names = TRUE)
  }
  output_files <- .normalize_output_files(output_files)

  if (is.null(design) || !"run_id" %in% names(design))
    stop("keep_dir must contain design.csv with a run_id column")
  run_ids <- as.character(design$run_id)

  outputs <- vector("list", length(run_ids))
  names(outputs) <- run_ids

  for (i in seq_along(run_ids)) {
    paths <- vapply(output_files, function(rel) {
      file.path(keep_dir, .archived_filename(run_ids[i], rel))
    }, character(1))
    tables <- .read_named_outputs(paths, src_root = NULL, output_reader, reader_args)
    tables <- .apply_sim_mutator(tables, sim_mutator)
    outputs[[i]] <- tables
  }

  list(design = design, outputs = .stack_design_outputs(outputs))
}
