#' Locate the first column matching a pattern
#'
#' Internal helper shared by [read_dlf()] and [add_date()], so the
#' column-matching convention (case-insensitive regex, first match wins) is
#' defined in exactly one place instead of being copy-pasted in both
#' functions.
#'
#' @param names_vec Character vector of column names to search (typically
#'   `names(dt)`).
#' @param pattern Character scalar. A regular expression.
#'
#' @return The first matching name, or `NA_character_` if none match.
#' @keywords internal
find_col <- function(names_vec, pattern) {
  hit <- grep(pattern, names_vec, ignore.case = TRUE, value = TRUE)
  if (length(hit) == 0) NA_character_ else hit[1]
}

#' Identify the Year/Month/Day/Hour columns of a Daisy data.table
#'
#' Internal helper shared by [read_dlf()] and [add_date()]. Both functions
#' need to recognise the same Year/Month/Day/Hour naming convention (e.g.
#' `"Year"`/`"year"`, `"MDay"`/`"day"`) - keeping the regexes here means
#' they can't drift out of sync between the two call sites.
#'
#' @param dt A `data.table` (or any object `names()` works on).
#'
#' @return A named character vector with elements `year`, `month`, `day`,
#'   `hour`. Each is either the matching column name from `dt`, or
#'   `NA_character_` if that column wasn't found.
#' @keywords internal
find_date_cols <- function(dt) {
  c(
    year  = find_col(names(dt), "^year$"),
    month = find_col(names(dt), "^month$"),
    day   = find_col(names(dt), "^(mday|day)$"),
    hour  = find_col(names(dt), "^hour$")
  )
}

#' Read a Daisy log file (.dlf)
#'
#' Parses a Daisy `.dlf` output file into a `data.table`. Handles both the
#' standard format (a `----------` separator line before the header) and
#' files written with `print_header false` (no separator, so the header is
#' found as the first tab-separated line). Also copes with `print_dimension
#' false`, where the line right after the header is data rather than a
#' units row.
#'
#' @details
#' Column units from Daisy's dimension line (`print_dimension true`, the
#' default) are stored as a **named character vector** in `attr(x, "units")`.
#' Names match `names(x)`; columns with no unit (dates, identifiers, empty
#' cells) are `NA`. Prefer [get_unit()] / [set_unit()] over touching the
#' attribute directly. When the file has no dimension line, every entry is
#' `NA`.
#'
#' A table-level attribute is used instead of renaming columns or attaching
#' a unit to each vector: it matches the existing `source_file` attribute,
#' does not break code that selects columns by name, and is subset with
#' `cols`. Daisy strings such as `"Mg DM/ha"` are not always valid SI, so
#' they are kept as plain text rather than converted to unit objects.
#'
#' @param path Character scalar. Path to the `.dlf` file to read. The
#'   function stops with an error if the file doesn't exist, is empty, has
#'   no detectable header, or has a header but no data rows.
#'
#' @param digits Integer scalar, or `NULL` (default). If `NULL`, values are
#'   returned as read, with no rounding applied. If set, every numeric
#'   column in the *returned* table is rounded to this many decimal places
#'   via `round()`, with one exception: the date/time columns (`Year`,
#'   `Month`, `MDay`/`Day`, `Hour` - matched case-insensitively, whichever
#'   of these are present) are never rounded, even though they're numeric,
#'   since rounding them would corrupt the date.
#'
#' @param cols Character vector of column names, or `NULL` (default). If
#'   `NULL`, all columns from the file are returned. If set, the returned
#'   table is restricted to just these columns plus whichever date/time
#'   columns exist in the file (`Year`/`Month`/`MDay`/`Day`/`Hour`) - the
#'   date/time columns are always kept even if you don't list them, so
#'   downstream helpers like [add_date()] still have what they need. Any
#'   name in `cols` that isn't actually a column in the file is dropped
#'   with a warning rather than causing an error. Column order in the
#'   output follows the file's original order, not the order given in
#'   `cols`.
#'
#' @return A `data.table` with one row per data line in the file, attributes
#'   `"source_file"` (the `path`) and `"units"` (named character vector,
#'   same names as the columns), and column names taken from the file's
#'   header line.
#'
#' @seealso [get_unit()], [set_unit()], [add_date()]
#'
#' @examples
#' \dontrun{
#' # Full table, no rounding
#' read_dlf("harvest.dlf")
#'
#' # Full table, numeric columns rounded to 2 dp (date/time columns untouched)
#' read_dlf("harvest.dlf", digits = 2)
#'
#' # Only WSOrg (plus date/time columns), unrounded
#' read_dlf("harvest.dlf", cols = c("WSOrg"))
#'
#' # Only WSOrg (plus date/time columns), rounded to 2 dp
#' read_dlf("harvest.dlf", digits = 2, cols = c("WSOrg"))
#' }
#' @export
read_dlf <- function(path, digits = NULL, cols = NULL) {
  if (!file.exists(path)) stop(sprintf("File not found: %s", path))

  raw <- readLines(path, warn = FALSE)
  raw <- raw[nzchar(raw)]   # drop blank lines (safe: .dlf files never rely on them)
  if (length(raw) == 0) stop(sprintf("%s is empty", path))

  is_data_row <- function(line) {
    first_tok <- strsplit(line, "\t")[[1]]
    first_tok <- first_tok[nzchar(first_tok)]
    if (length(first_tok) == 0) return(FALSE)
    !is.na(suppressWarnings(as.numeric(first_tok[1])))
  }

  dash_lines <- grep("^-{10,}\\s*$", raw)
  if (length(dash_lines) > 0) {
    header_idx <- dash_lines[1] + 1
  } else {
    # (print_header false): find the column-name line by looking for the
    # first line that contains a tab - metadata/description prose doesn't.
    tab_lines <- which(grepl("\t", raw))
    if (length(tab_lines) == 0)
      stop(sprintf(
        "Could not locate a tab-separated column header in %s (no '----' separator either) - is this a valid .dlf file?",
        path))
    header_idx <- tab_lines[1]
  }

  if (header_idx > length(raw))
    stop(sprintf("%s has no content after its header line", path))

  header_names <- trimws(strsplit(raw[header_idx], "\t")[[1]])

  # (print_dimension false): the line right after the column names may be
  # data already, rather than a units line - check before skipping it.
  candidate <- header_idx + 1
  has_dimension_line <- candidate <= length(raw) && !is_data_row(raw[candidate])
  units <- if (has_dimension_line) {
    .parse_dlf_units_line(raw[candidate], length(header_names))
  } else {
    rep(NA_character_, length(header_names))
  }
  names(units) <- header_names
  data_start <- if (has_dimension_line) candidate + 1 else candidate

  if (data_start > length(raw))
    stop(sprintf("%s has a header but no data rows", path))

  dt <- data.table::fread(text = paste(raw[data_start:length(raw)], collapse = "\n"),
                           sep = "\t", header = FALSE, fill = TRUE, blank.lines.skip = TRUE)

  # Drop any fully-empty trailing column fread sometimes adds because of a
  # trailing tab/newline quirk, then align names to whatever data columns
  # actually came back.
  n <- min(ncol(dt), length(header_names))
  dt <- dt[, seq_len(n), with = FALSE]
  data.table::setnames(dt, header_names[seq_len(n)])
  units <- units[seq_len(n)]

  # drop rows that are entirely NA (can happen on a trailing blank line)
  dt <- dt[rowSums(!is.na(dt)) > 0]

  # Identify the date/time columns (shared convention with add_date()) so
  # they're always preserved and never rounded.
  date_cols <- stats::na.omit(find_date_cols(dt))

  # Optionally restrict the returned columns to `cols` + the date/time columns.
  if (!is.null(cols)) {
    missing_cols <- setdiff(cols, names(dt))
    if (length(missing_cols) > 0)
      warning(sprintf("read_dlf: column(s) not found, skipped: %s",
                       paste(missing_cols, collapse = ", ")))
    keep_cols <- union(intersect(cols, names(dt)), date_cols)
    keep_cols <- names(dt)[names(dt) %in% keep_cols]   # preserve original order
    dt <- dt[, keep_cols, with = FALSE]
    units <- units[keep_cols]
  }

  # Optional rounding of numeric columns, skipping the date/time columns.
  if (!is.null(digits)) {
    round_cols <- setdiff(names(dt), date_cols)
    if (length(round_cols) > 0)
      round_cols <- round_cols[vapply(dt[, round_cols, with = FALSE], is.numeric, logical(1))]
    if (length(round_cols) > 0)
      dt[, (round_cols) := lapply(.SD, round, digits = digits), .SDcols = round_cols]
  }

  data.table::setattr(dt, "source_file", path)
  data.table::setattr(dt, "units", units)
  dt[]
}

#' Parse a Daisy DLF dimension line into one unit string per column.
#'
#' Empty cells become `NA_character_`. The vector is padded or truncated to
#' `n` columns so it always aligns with the header.
#'
#' @noRd
.parse_dlf_units_line <- function(line, n) {
  units <- trimws(strsplit(line, "\t", fixed = TRUE)[[1]])
  if (length(units) < n) {
    units <- c(units, rep("", n - length(units)))
  }
  units <- units[seq_len(n)]
  units[!nzchar(units)] <- NA_character_
  units
}

#' Add a `Date` column to a Daisy data.table
#'
#' Given a `data.table` with Year/Month/Day(/Hour) columns - under either
#' naming convention seen in Daisy output (e.g. `"Year"`/`"year"`,
#' `"MDay"`/`"day"`) - builds a proper `Date` column from them. If an hour
#' column is present, its values are also copied into a `hour` column
#' (added for convenient, consistently-named access downstream, alongside
#' whatever the original hour column was called).
#'
#' `dt` is modified in place (via `data.table`'s `:=`) and returned. New
#' columns (`Date`, and `hour` when copied) get `NA` in `attr(dt, "units")`
#' when that attribute is already present.
#'
#' If the Year/Month/Day columns can't all be identified, a warning is
#' issued (including `label` and the columns actually found, to make the
#' source easy to track down), a `Date` column of `NA`s is added so
#' downstream code can still assume the column exists, and the function
#' returns early without attempting to build a real date.
#'
#' @param dt A `data.table`, typically the output of [read_dlf()].
#' @param label Character scalar used to identify the source of `dt` in the
#'   warning message when Year/Month/Day columns can't be found (e.g. the
#'   name of the file `dt` was read from).
#'
#' @return `dt`, invisibly modified in place and returned, with a `Date`
#'   column added (and an `hour` column, if an hour column was found).
#'
#' @examples
#' \dontrun{
#' harvest_dt <- add_date(read_dlf("harvest.dlf", digits = 2), "harvest")
#' }
#' @export
add_date <- function(dt, label) {
  dc <- find_date_cols(dt)
  ycol <- dc[["year"]]
  mcol <- dc[["month"]]
  dcol <- dc[["day"]]
  hcol <- dc[["hour"]]

  if (any(is.na(c(ycol, mcol, dcol)))) {
    warning(sprintf("[%s] Could not identify Year/Month/Day columns (found: %s) - date-based checks will be skipped",
                     label, paste(names(dt), collapse = ", ")))
    dt[, Date := as.Date(NA)]
    .dlf_align_units(dt)
    return(dt[])
  }

  dt[, Date := as.Date(sprintf("%04d-%02d-%02d", dt[[ycol]], dt[[mcol]], dt[[dcol]]))]
  if (!is.na(hcol)) dt[, hour := dt[[hcol]]]
  .dlf_align_units(dt)
  dt[]
}

#' Realign `attr(dt, "units")` to the current column set.
#'
#' Existing unit strings are kept; new columns get `NA_character_`.
#'
#' @noRd
.dlf_align_units <- function(dt) {
  units <- attr(dt, "units")
  if (is.null(units)) {
    return(invisible(dt))
  }
  aligned <- setNames(rep(NA_character_, ncol(dt)), names(dt))
  common <- intersect(names(units), names(dt))
  aligned[common] <- unname(units[common])
  data.table::setattr(dt, "units", aligned)
  invisible(dt)
}

#' Look up column units on a Daisy log table.
#'
#' Reads `attr(dt, "units")` for the requested columns. Unknown column
#' names are an error. A column that exists but has no unit (or a table
#' with no `units` attribute) returns `NA`.
#'
#' @param dt A `data.table`, typically the output of [read_dlf()].
#' @param cols Character vector of column names.
#'
#' @return A named character vector the same length as `cols`.
#' @export
#' @seealso [set_unit()], [read_dlf()]
#' @examples
#' \dontrun{
#' dt <- read_dlf("harvest.dlf")
#' get_unit(dt, "stem_DM")
#' get_unit(dt, c("stem_DM", "stem_N"))
#' }
get_unit <- function(dt, cols) {
  cols <- .dlf_check_cols(dt, cols)
  units <- attr(dt, "units")
  out <- setNames(rep(NA_character_, length(cols)), cols)
  if (is.null(units)) {
    return(out)
  }
  present <- cols[cols %in% names(units)]
  out[present] <- unname(units[present])
  out
}

#' Set column units on a Daisy log table.
#'
#' Updates `attr(dt, "units")` in place. Use this after adding a derived
#' column so later [get_unit()] calls can label it. The column must
#' already exist. `unit` of length 1 is recycled. `NA` or `""` clears a
#' unit.
#'
#' @param dt A `data.table`, typically the output of [read_dlf()].
#' @param cols Character vector of column names.
#' @param unit Character vector of unit strings (recycled to `length(cols)`).
#'
#' @return `dt`, modified in place and returned.
#' @export
#' @seealso [get_unit()], [read_dlf()]
#' @examples
#' \dontrun{
#' dt <- read_dlf("harvest.dlf")
#' dt[, stem_DM_t := stem_DM / 1000]
#' set_unit(dt, "stem_DM_t", "t DM/ha")
#' }
set_unit <- function(dt, cols, unit) {
  cols <- .dlf_check_cols(dt, cols)
  if (length(unit) == 1L) {
    unit <- rep(unit, length(cols))
  }
  if (length(unit) != length(cols)) {
    stop(sprintf(
      "set_unit: `unit` must be length 1 or length(cols) = %d, not %d",
      length(cols), length(unit)
    ), call. = FALSE)
  }
  unit_chr <- rep(NA_character_, length(unit))
  keep <- !is.na(unit)
  unit_chr[keep] <- as.character(unit[keep])
  unit_chr[keep & !nzchar(unit_chr)] <- NA_character_

  if (is.null(attr(dt, "units"))) {
    data.table::setattr(
      dt, "units",
      setNames(rep(NA_character_, ncol(dt)), names(dt))
    )
  } else {
    .dlf_align_units(dt)
  }
  units <- attr(dt, "units")
  units[cols] <- unit_chr
  data.table::setattr(dt, "units", units)
  dt[]
}

#' @noRd
.dlf_check_cols <- function(dt, cols) {
  if (!is.character(cols) || length(cols) < 1L) {
    stop("`cols` must be a non-empty character vector", call. = FALSE)
  }
  missing <- setdiff(cols, names(dt))
  if (length(missing) > 0) {
    stop(sprintf(
      "column(s) not found: %s",
      paste(missing, collapse = ", ")
    ), call. = FALSE)
  }
  cols
}
