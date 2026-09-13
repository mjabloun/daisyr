#' Escape a string's regex metacharacters, for use inside a larger pattern
#' @keywords internal
.escape_regex <- function(s) {
  special <- c("\\", ".", "+", "*", "?", "^", "$", "(", ")", "[", "]", "{", "}", "|")
  for (ch in special) s <- gsub(ch, paste0("\\", ch), s, fixed = TRUE)
  s
}

#' Locate the balanced-parenthesis block for a named PLF parameter
#'
#' @param txt Character scalar. The full (comment-stripped) `.dai` text.
#' @param parameter Character scalar. Parameter name.
#' @param occurrence Integer. Which occurrence to locate.
#' @param file Character scalar, used only for error messages.
#' @return Character scalar: the `(parameter (x y) (x y) ...)` block, including
#'   the parameter name and its enclosing parentheses.
#' @keywords internal
.locate_plf_block <- function(txt, parameter, occurrence, file) {
  # Match "(parameter" as a whole token (not a prefix of a longer name),
  # i.e. followed by whitespace or a closing paren.
  pattern <- paste0("\\(", .escape_regex(parameter), "(?=[\\s)])")
  starts <- gregexpr(pattern, txt, perl = TRUE)[[1]]
  if (starts[1] == -1)
    stop(sprintf("Parameter '%s' not found in %s", parameter, file))
  if (occurrence > length(starts))
    stop(sprintf("Parameter '%s' occurs %d time(s) in %s; occurrence = %d requested",
                  parameter, length(starts), file, occurrence))

  start <- starts[occurrence]
  # Walk forward from the opening "(" tracking paren depth, to find the
  # matching close - this is what lets the parameter's value span multiple
  # lines (and contain nested sub-lists) safely.
  chars <- strsplit(substring(txt, start), "", fixed = TRUE)[[1]]
  depth <- 0L
  end <- NA_integer_
  for (i in seq_along(chars)) {
    if (chars[i] == "(") {
      depth <- depth + 1L
    } else if (chars[i] == ")") {
      depth <- depth - 1L
      if (depth == 0L) { end <- i; break }
    }
  }
  if (is.na(end))
    stop(sprintf("Unbalanced parentheses while parsing parameter '%s' in %s", parameter, file))

  substring(txt, start, start + end - 1L)
}

#' Read a piecewise linear function (PLF) parameter from a .dai file
#'
#' Extracts the `(x y) (x y) ...` sequence of a named PLF parameter directly
#' from an existing `.dai` file - e.g. to inspect a value already in a
#' template, or to seed `x_values`/starting shape parameters for a
#' `plf_curves` config entry. The parameter's value may span multiple
#' lines and may contain end-of-line comments (`;` to end of line), both of
#' which are handled before parsing.
#'
#' @param file Character scalar. Path to the `.dai` file.
#' @param parameter Character scalar. Name of the PLF parameter to extract
#'   (e.g. `"LAIvsDS"`).
#' @param occurrence Integer. Which occurrence of `parameter` to extract, if
#'   it appears more than once in the file (e.g. once per crop/horizon
#'   definition). Defaults to the first.
#'
#' @return A `data.frame` with columns `x` and `y`, one row per point, in
#'   the order they appear in the file.
#'
#' @examples
#' \dontrun{
#' read_plf("dk-sbarley.dai", "TempEff1")
#' }
#' @export
read_plf <- function(file, parameter, occurrence = 1) {
  if (!file.exists(file)) stop(sprintf("File not found: %s", file))
  raw <- readLines(file, warn = FALSE)
  # Strip ";" end-of-line comments (naive: doesn't special-case ";" inside
  # quoted strings, which Daisy .dai files rarely put in PLF-bearing lines).
  raw <- sub(";.*$", "", raw)
  txt <- paste(raw, collapse = " ")

  block <- .locate_plf_block(txt, parameter, occurrence, file)

  # Extract every "(num num)" pair inside the block; anything else nested
  # in there (units, flags, etc.) simply won't match and is ignored.
  number_re <- "-?[0-9]*\\.?[0-9]+(?:[eE][+-]?[0-9]+)?"
  pair_pattern <- paste0("\\(\\s*(", number_re, ")\\s+(", number_re, ")\\s*\\)")
  matched <- regmatches(block, gregexpr(pair_pattern, block, perl = TRUE))[[1]]
  if (length(matched) == 0)
    stop(sprintf("No (x y) pairs found for parameter '%s' in %s", parameter, file))

  pairs <- lapply(matched, function(s) {
    as.numeric(regmatches(s, gregexpr(number_re, s, perl = TRUE))[[1]])
  })
  data.frame(
    x = vapply(pairs, `[`, numeric(1), 1),
    y = vapply(pairs, `[`, numeric(1), 2)
  )
}

#' Plot a piecewise linear function
#'
#' Plots a PLF's `(x y)` points connected by straight line segments
#' (matching how Daisy itself interpolates a PLF between knots). Accepts
#' either a `.dai` file and parameter name (in which case it calls
#' [read_plf()] itself) or an already-extracted `data.frame` - e.g. so you
#' can inspect/subset the result of [read_plf()] before plotting it.
#'
#' @param x Either a character scalar giving the path to a `.dai` file (in
#'   which case `parameter` is required), or a `data.frame`/list with `x`
#'   and `y` elements, such as the return value of [read_plf()].
#' @param parameter Character scalar. Name of the PLF parameter to extract;
#'   required when `x` is a file path, ignored otherwise.
#' @param occurrence Integer. Passed to [read_plf()] when `x` is a file path.
#' @param xlab,ylab,main Plot labels. `main` defaults to `parameter` when
#'   `x` is a file path, or `NULL` otherwise.
#' @param ... Passed on to [graphics::plot()].
#'
#' @return Invisibly, a `data.frame` with columns `x` and `y`.
#'
#' @examples
#' \dontrun{
#' plot_plf("dk-sbarley.dai", "TempEff1")
#'
#' df <- read_plf("dk-sbarley.dai", "TempEff1")
#' plot_plf(df, main = "TempEff1")
#' }
#' @export
plot_plf <- function(x, parameter = NULL, occurrence = 1,
                      xlab = "x", ylab = "y", main = parameter, ...) {
  if (is.data.frame(x) || is.list(x)) {
    if (!all(c("x", "y") %in% names(x)))
      stop("When `x` is not a file path, it must have 'x' and 'y' elements ",
           "(e.g. the output of read_plf())")
    df <- data.frame(x = x[["x"]], y = x[["y"]])
  } else {
    if (is.null(parameter))
      stop("`parameter` is required when `x` is a .dai file path")
    df <- read_plf(x, parameter, occurrence = occurrence)
  }
  graphics::plot(df$x, df$y, type = "o", pch = 16,
                 xlab = xlab, ylab = ylab, main = main, ...)
  invisible(df)
}
