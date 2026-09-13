#' Default Daisy dimension strings for common weather-file columns.
#'
#' @noRd
.dwf_builtin_units <- function(timestep = "24 hours") {
  hours <- suppressWarnings(as.numeric(sub("^([0-9.]+).*", "\\1", timestep)))
  hourly <- grepl("hour", timestep, ignore.case = TRUE) &&
    !is.na(hours) && hours < 24
  precip <- if (hourly) "mm/h" else "mm/d"
  c(
    Year = "year", Month = "month", Day = "mday", Hour = "hour",
    GlobRad = "W/m^2", DiffRad = "W/m^2",
    AirTemp = "dgC", MinTemp = "dgC", MaxTemp = "dgC",
    Precip = precip, RefEvap = precip,
    Wind = "m/s", RelHum = "%", VapPres = "Pa"
  )
}

#' Locate date/time columns in a weather table.
#'
#' Accepts Daisy names (`Year`/`Month`/`Day`/`Hour`) and the uppercase
#' `YEAR`/`MM`/`DD` names used by the original `writeDaisyWeatherFile()`.
#'
#' @noRd
.dwf_time_cols <- function(data) {
  nm <- names(data)
  c(
    year  = find_col(nm, "^year$"),
    month = find_col(nm, "^(month|mm)$"),
    day   = find_col(nm, "^(mday|day|dd)$"),
    hour  = find_col(nm, "^hour$"),
    date  = find_col(nm, "^date$")
  )
}

#' Build Year/Month/Day(/Hour) plus a Date vector from `data`.
#'
#' @noRd
.dwf_calendar <- function(data) {
  tc <- .dwf_time_cols(data)
  ymd <- tc[c("year", "month", "day")]
  if (!anyNA(ymd)) {
    year  <- as.integer(data[[tc[["year"]]]])
    month <- as.integer(data[[tc[["month"]]]])
    day   <- as.integer(data[[tc[["day"]]]])
    dates <- as.Date(sprintf("%04d-%02d-%02d", year, month, day))
    drop  <- unname(ymd)
    map   <- c(Year = tc[["year"]], Month = tc[["month"]], Day = tc[["day"]])
  } else if (!is.na(tc[["date"]])) {
    dates <- as.Date(data[[tc[["date"]]]])
    if (anyNA(dates)) {
      stop("write_dwf: `Date` column contains values that are not dates",
           call. = FALSE)
    }
    lt    <- as.POSIXlt(dates)
    year  <- lt$year + 1900L
    month <- lt$mon + 1L
    day   <- lt$mday
    drop  <- tc[["date"]]
    map   <- c(Year = tc[["date"]], Month = tc[["date"]], Day = tc[["date"]])
  } else {
    stop(
      "write_dwf: need Year/Month/Day columns (or YEAR/MM/DD) or a Date column",
      call. = FALSE
    )
  }
  if (anyNA(dates)) {
    stop("write_dwf: could not build dates from the time columns", call. = FALSE)
  }
  hour <- NULL
  if (!is.na(tc[["hour"]])) {
    hour <- as.integer(data[[tc[["hour"]]]])
    drop <- c(drop, tc[["hour"]])
    map  <- c(map, Hour = tc[["hour"]])
  }
  list(year = year, month = month, day = day, hour = hour, dates = dates,
       drop = drop, map = map)
}

#' Station climate stats from a daily air-temperature series.
#'
#' `t_average` is the mean. `t_amplitude` is half the range of monthly
#' means (a simple annual sinusoid). `max_t_day` is the day-of-year whose
#' mean temperature is highest.
#'
#' @noRd
.dwf_climate_from_temp <- function(dates, temp) {
  ok <- !is.na(temp) & !is.na(dates)
  dates <- dates[ok]
  temp <- as.numeric(temp[ok])
  if (!length(temp)) {
    stop("write_dwf: AirTemp is all missing; pass t_average/t_amplitude/max_t_day",
         call. = FALSE)
  }
  monthly <- tapply(temp, as.integer(format(dates, "%m")), mean)
  by_yday <- tapply(temp, as.integer(format(dates, "%j")), mean)
  list(
    t_average   = mean(temp),
    t_amplitude = (max(monthly) - min(monthly)) / 2,
    max_t_day   = as.integer(names(by_yday)[which.max(by_yday)])
  )
}

#' Write a Daisy weather file (`.dwf`).
#'
#' Adapted from the project helper `writeDaisyWeatherFile()`: the table of
#' daily (or hourly) values is wrapped in Daisy's `dwf-0.0` header. Station
#' name and coordinates are arguments instead of being hardcoded to
#' Taastrup. Begin/End dates come from the table. Climate statistics
#' (`TAverage`, `TAmplitude`, `MaxTDay`) default to values computed from
#' `AirTemp` when that column is present.
#'
#' Date columns may be `Year`/`Month`/`Day` (any case), the older
#' `YEAR`/`MM`/`DD` names, or a single `Date` column. Remaining columns are
#' written in file order. Units come from `units`, then `attr(data, "units")`
#' (as set by [read_dlf()] / [set_unit()]), then built-in Daisy dimensions
#' for common names (`GlobRad`, `AirTemp`, `Precip`, ...).
#'
#' @param data A `data.frame` or [data.table::data.table] of weather
#'   observations.
#' @param path Character scalar. File to write (usually `*.dwf`).
#' @param station Character scalar. Station name written as `Station:`.
#' @param elevation Numeric. Station elevation in metres.
#' @param latitude,longitude Numeric. Latitude (`dgNorth`) and longitude
#'   (`dgEast`). Western longitudes are negative.
#' @param timezone Numeric or `NULL`. Time zone in `dgEast`. If `NULL`,
#'   rounded to the nearest 15 degrees of `longitude`.
#' @param surface Character scalar. Measurement surface (Daisy default
#'   `"reference"`).
#' @param screen_height Numeric. Screen height in metres.
#' @param timestep Character scalar passed to Daisy (e.g. `"24 hours"`).
#' @param begin,end `Date` or `NULL`. Period covered; default is the min/max
#'   date in `data`.
#' @param notes Character vector of `Note:` lines, or `NULL`.
#' @param nh4_wet_dep,nh4_dry_dep,no3_wet_dep,no3_dry_dep Nitrogen
#'   deposition (same defaults as the original helper / Taastrup sample).
#' @param t_average,t_amplitude,max_t_day Climate header values. `NULL`
#'   (default) computes them from an `AirTemp` column.
#' @param units Optional named character vector of dimension strings,
#'   keyed by the **output** column names (`Year`, `GlobRad`, ...).
#' @param extra Optional named character vector of additional `Key: value`
#'   header lines (e.g. `c(PrecipCorrect = "1.1 1.1 ...")`).
#' @param comment Character scalar placed on the first line after
#'   `dwf-0.0 -- `.
#'
#' @return `path`, invisibly.
#' @export
#' @seealso [run_daisy()], [set_unit()]
#' @examples
#' \dontrun{
#' wx <- data.frame(
#'   Date = as.Date("2000-01-01") + 0:2,
#'   GlobRad = c(20, 40, 35),
#'   AirTemp = c(2.1, 3.0, 1.5),
#'   Precip = c(0, 1.2, 0)
#' )
#' write_dwf(wx, "site.dwf", station = "Example",
#'           elevation = 30, latitude = 56, longitude = 12)
#' }
write_dwf <- function(data, path,
                      station,
                      elevation,
                      latitude,
                      longitude,
                      timezone = NULL,
                      surface = "reference",
                      screen_height = 2,
                      timestep = "24 hours",
                      begin = NULL,
                      end = NULL,
                      notes = NULL,
                      nh4_wet_dep = 0.9,
                      nh4_dry_dep = 2.2,
                      no3_wet_dep = 0.6,
                      no3_dry_dep = 1.1,
                      t_average = NULL,
                      t_amplitude = NULL,
                      max_t_day = NULL,
                      units = NULL,
                      extra = NULL,
                      comment = "Daisy weather file.") {
  if (missing(station) || !nzchar(station)) {
    stop("write_dwf: `station` is required", call. = FALSE)
  }
  if (nrow(data) < 1L) {
    stop("write_dwf: `data` has no rows", call. = FALSE)
  }

  cal <- .dwf_calendar(data)
  begin <- if (is.null(begin)) min(cal$dates) else as.Date(begin)
  end   <- if (is.null(end))   max(cal$dates) else as.Date(end)
  if (is.null(timezone)) {
    timezone <- round(longitude / 15) * 15
  }

  weather_cols <- setdiff(names(data), cal$drop)
  out <- data.table::data.table(
    Year  = cal$year,
    Month = cal$month,
    Day   = cal$day
  )
  if (!is.null(cal$hour)) {
    data.table::set(out, j = "Hour", value = cal$hour)
  }
  if (length(weather_cols)) {
    extra_dt <- data.table::as.data.table(data)[, weather_cols, with = FALSE]
    out <- cbind(out, extra_dt)
  }

  air_name <- find_col(names(out), "^airtemp$")
  need_climate <- is.null(t_average) || is.null(t_amplitude) || is.null(max_t_day)
  if (need_climate) {
    if (is.na(air_name)) {
      stop("write_dwf: no AirTemp column; pass t_average, t_amplitude, and max_t_day",
           call. = FALSE)
    }
    clim <- .dwf_climate_from_temp(cal$dates, out[[air_name]])
    if (is.null(t_average))   t_average   <- clim$t_average
    if (is.null(t_amplitude)) t_amplitude <- clim$t_amplitude
    if (is.null(max_t_day))   max_t_day   <- clim$max_t_day
  }

  dim_line <- .dwf_units_line(out, data, cal$map, units, timestep)

  header <- c(
    paste0("dwf-0.0 -- ", comment),
    "",
    "# General information about the station itself.",
    "",
    sprintf("Station: %s", station),
    sprintf("Elevation: %s m", elevation),
    sprintf("Longitude: %s dgEast", longitude),
    sprintf("Latitude: %s dgNorth", latitude),
    sprintf("TimeZone: %s dgEast", timezone),
    "",
    "# Information about the measurement conditions.",
    "",
    sprintf("Surface: %s", surface),
    sprintf("ScreenHeight: %s m", screen_height),
    "",
    "# General information about the available data.",
    "",
    sprintf("Begin: %s", format(begin, "%Y-%m-%d")),
    sprintf("End: %s", format(end, "%Y-%m-%d")),
    sprintf("Timestep: %s", timestep)
  )
  if (length(notes)) {
    header <- c(header, "", paste0("Note: ", notes))
  }
  header <- c(
    header,
    "",
    "# Fixed deposit values.",
    "",
    sprintf("NH4WetDep: %s ppm", nh4_wet_dep),
    sprintf("NH4DryDep: %s kgN/ha/year", nh4_dry_dep),
    sprintf("NO3WetDep: %s ppm", no3_wet_dep),
    sprintf("NO3DryDep: %s kgN/ha/year", no3_dry_dep),
    "",
    "# Temperature averages",
    "",
    sprintf("TAverage: %s dgC", t_average),
    sprintf("TAmplitude: %s dgC", t_amplitude),
    sprintf("MaxTDay: %s yday", as.integer(max_t_day))
  )
  if (length(extra)) {
    if (is.null(names(extra)) || any(!nzchar(names(extra)))) {
      stop("write_dwf: `extra` must be a named character vector", call. = FALSE)
    }
    header <- c(header, "", sprintf("%s: %s", names(extra), extra))
  }
  header <- c(
    header,
    "",
    paste(rep("-", 78L), collapse = ""),
    paste(names(out), collapse = "\t"),
    paste(dim_line, collapse = "\t")
  )

  writeLines(header, path, useBytes = FALSE)
  old_scipen <- getOption("scipen")
  on.exit(options(scipen = old_scipen), add = TRUE)
  options(scipen = 50)
  data.table::fwrite(out, file = path, sep = "\t", append = TRUE,
                     col.names = FALSE, na = "NA")
  invisible(path)
}

#' Dimension line aligned with the columns of `out`.
#'
#' @noRd
.dwf_units_line <- function(out, data, map, units, timestep) {
  builtin <- .dwf_builtin_units(timestep)
  src <- attr(data, "units")
  nms <- names(out)
  line <- setNames(rep(NA_character_, length(nms)), nms)

  from_builtin <- intersect(nms, names(builtin))
  line[from_builtin] <- unname(builtin[from_builtin])

  if (!is.null(src)) {
    common <- intersect(nms, names(src))
    line[common] <- unname(src[common])
    mapped <- intersect(names(map), nms)
    for (nm in mapped) {
      src_name <- unname(map[[nm]])
      if (src_name %in% names(src) && !is.na(src[[src_name]]) && nzchar(src[[src_name]])) {
        line[[nm]] <- unname(src[[src_name]])
      }
    }
  }

  if (!is.null(units)) {
    if (is.null(names(units))) {
      stop("write_dwf: `units` must be a named character vector", call. = FALSE)
    }
    overlap <- intersect(names(units), nms)
    line[overlap] <- unname(units[overlap])
  }

  line[is.na(line) | !nzchar(line)] <- ""
  unname(line)
}
