.DAISYR_DAISY_EXE_OPTION <- "daisyr.daisy_exe"

#' Default locations searched by [get_daisy_path()] after the session option
#' and `DAISY_EXE` environment variable.
#'
#' @noRd
.default_daisy_exe_candidates <- function() {
  c(
    "C:/Program Files/Daisy 5.93/bin/daisy.exe",
    "C:/Program Files/Daisy 6.0/bin/daisy.exe",
    "C:/Program Files/Daisy/bin/daisy.exe",
    unname(Sys.which("daisy.exe")),
    unname(Sys.which("daisy"))
  )
}

#' Set the Daisy executable used by [run_daisy()] and related helpers.
#'
#' Stores a session-wide path in `options(daisyr.daisy_exe = ...)`. Pass
#' `NULL` to clear it (resolution then falls back to `DAISY_EXE` and the
#' usual install locations; see [get_daisy_path()]).
#'
#' @param path Character scalar path to `daisy.exe` (or `daisy` on Unix),
#'   or `NULL` to unset the session option.
#'
#' @return The normalised path, invisibly; `NULL` when clearing.
#' @export
#' @seealso [get_daisy_path()], [run_daisy()]
#' @examples
#' \dontrun{
#' set_daisy_path("C:/Program Files/Daisy 5.93/bin/daisy.exe")
#' run_daisy("test-optim.dai", working_dir = ".")
#' }
set_daisy_path <- function(path) {
  if (is.null(path)) {
    options(daisyr.daisy_exe = NULL)
    return(invisible(NULL))
  }
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
    stop("`path` must be a non-empty character scalar, or NULL to unset",
         call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("Daisy executable not found: ", path, call. = FALSE)
  }
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  options(daisyr.daisy_exe = path)
  invisible(path)
}

#' Resolve the Daisy executable path.
#'
#' Looks up, in order:
#' 1. `getOption("daisyr.daisy_exe")` from [set_daisy_path()]
#' 2. the `DAISY_EXE` environment variable
#' 3. common Windows install paths and `Sys.which("daisy.exe")` / `daisy`
#'
#' @return A single existing path (character scalar).
#' @export
#' @seealso [set_daisy_path()], [run_daisy()]
get_daisy_path <- function() {
  .find_daisy_exe(error = TRUE)
}

#' @noRd
.resolve_daisy_exe <- function(daisy_exe = NULL) {
  if (!is.null(daisy_exe) && nzchar(daisy_exe)) {
    return(daisy_exe)
  }
  get_daisy_path()
}

#' @noRd
.find_daisy_exe <- function(error = TRUE,
                            candidates = .default_daisy_exe_candidates()) {
  opt <- getOption(.DAISYR_DAISY_EXE_OPTION, default = NULL)
  if (is.character(opt) && length(opt) == 1L && nzchar(opt)) {
    if (file.exists(opt)) {
      return(normalizePath(opt, winslash = "/", mustWork = TRUE))
    }
    if (error) {
      stop("getOption(\"daisyr.daisy_exe\") is set but the file is missing: ",
           opt, "\nCall set_daisy_path() with a valid path.", call. = FALSE)
    }
    return("")
  }

  env <- Sys.getenv("DAISY_EXE", unset = "")
  hits <- c(env, candidates)
  hits <- hits[nzchar(hits) & file.exists(hits)]
  if (length(hits)) {
    return(normalizePath(hits[[1]], winslash = "/", mustWork = TRUE))
  }
  if (error) {
    stop(
      "Could not find the Daisy executable. Call set_daisy_path(), set the ",
      "DAISY_EXE environment variable, or pass daisy_exe= to run_daisy().",
      call. = FALSE
    )
  }
  ""
}

#' Run the Daisy executable on a `.dai` setup file
#'
#' Thin wrapper around [system()] that runs a Daisy `.dai` file. By default
#' this invokes a local `daisy.exe` (see [get_daisy_path()]). Pass `cmd` to
#' use another launcher (Singularity, a scheduler wrapper, ...).
#'
#' @param run_file Character scalar. Path to the `.dai` file to run.
#'   Relative paths are resolved against `working_dir` if given.
#' @param daisy_exe Character scalar path to `daisy.exe`, or `NULL`
#'   (default) to use [get_daisy_path()]. Ignored when `cmd` is set and
#'   does not contain `{daisy_exe}`.
#' @param working_dir Character scalar or `NULL`. If given, the working
#'   directory is temporarily changed to this directory for the duration of
#'   the call (and restored afterwards, even on error), matching Daisy's
#'   convention of resolving `(directory ...)`/input paths relative to the
#'   current working directory. Also available as `{working_dir}` in `cmd`.
#' @param show_log If `TRUE`, stream Daisy's console output; otherwise it is
#'   captured/discarded.
#' @param cmd Optional command template, interpolated with
#'   [glue::glue()]. Substitutions: `{run_file}`, `{daisy_exe}`,
#'   `{working_dir}`. If `NULL`, runs `"{daisy_exe}" "{run_file}"`.
#'   Shell snippets such as `$(pwd)` are left for the shell. Example:
#'   `"singularity run --bind $(pwd):$(pwd) ~/daisy/daisy.sif -q $(pwd)/{run_file}"`.
#'
#' @return Integer exit/status code from [system()] (invisibly). Non-zero
#'   indicates Daisy reported an error - the caller decides whether that
#'   should halt a calibration/SA run or just be logged.
#' @export
#' @seealso [set_daisy_path()], [get_daisy_path()]
#' @examples
#' \dontrun{
#' run_daisy("test-optim.dai", working_dir = ".")
#'
#' run_daisy(
#'   "test-optim.dai",
#'   working_dir = ".",
#'   cmd = "singularity run --bind $(pwd):$(pwd) ~/daisy/daisy.sif -q $(pwd)/{run_file}"
#' )
#' }
run_daisy <- function(run_file, daisy_exe = NULL, working_dir = NULL,
                      show_log = FALSE, cmd = NULL) {
  if (!is.null(working_dir)) {
    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    setwd(working_dir)
  }

  cmd <- .build_daisy_cmd(
    run_file = run_file,
    daisy_exe = daisy_exe,
    cmd = cmd,
    working_dir = getwd()
  )

  status <- system(cmd, show.output.on.console = show_log)
  if (status != 0)
    warning(sprintf("Daisy exited with status %d while running %s", status, run_file))

  invisible(status)
}

#' Build the shell command for [run_daisy()].
#'
#' @noRd
.build_daisy_cmd <- function(run_file, daisy_exe = NULL, cmd = NULL,
                             working_dir = "") {
  if (is.null(cmd) || !nzchar(cmd)) {
    cmd <- '"{daisy_exe}" "{run_file}"'
  }
  if (grepl("{daisy_exe}", cmd, fixed = TRUE)) {
    daisy_exe <- .resolve_daisy_exe(daisy_exe)
  }
  glue::glue(
    cmd,
    run_file = run_file,
    daisy_exe = if (is.null(daisy_exe)) "" else daisy_exe,
    working_dir = if (is.null(working_dir)) "" else working_dir
  )
}
