tiny_weather <- function() {
  data.frame(
    YEAR = c(2000L, 2000L, 2000L),
    MM = c(1L, 1L, 1L),
    DD = c(1L, 2L, 3L),
    GlobRad = c(20, 40, 35),
    AirTemp = c(2.1, 3.0, 1.5),
    Precip = c(0, 1.2, 0),
    RefEvap = c(0.4, 0.5, 0.3)
  )
}

write_tmp <- function(data, ...) {
  path <- tempfile(fileext = ".dwf")
  write_dwf(
    data, path,
    station = "Test",
    elevation = 30,
    latitude = 56,
    longitude = 12,
    ...
  )
  path
}

test_that("write_dwf writes a dwf-0.0 header and YEAR/MM/DD table", {
  path <- write_tmp(tiny_weather())
  on.exit(unlink(path), add = TRUE)
  txt <- readLines(path)

  expect_match(txt[1], "^dwf-0.0")
  expect_true(any(grepl("^Station: Test$", txt)))
  expect_true(any(grepl("^Elevation: 30 m$", txt)))
  expect_true(any(grepl("^Begin: 2000-01-01$", txt)))
  expect_true(any(grepl("^End: 2000-01-03$", txt)))
  expect_true(any(grepl("^TimeZone: 15 dgEast$", txt)))

  dash <- grep("^-{10,}$", txt)[1]
  expect_equal(txt[dash + 1], "Year\tMonth\tDay\tGlobRad\tAirTemp\tPrecip\tRefEvap")
  expect_equal(txt[dash + 2], "year\tmonth\tmday\tW/m^2\tdgC\tmm/d\tmm/d")
  expect_match(txt[dash + 3], "^2000\t1\t1\t20")
})

test_that("write_dwf accepts a Date column", {
  wx <- data.frame(
    Date = as.Date("2001-06-01") + 0:1,
    AirTemp = c(10, 12),
    GlobRad = c(100, 110),
    Precip = c(0, 0)
  )
  path <- write_tmp(wx)
  on.exit(unlink(path), add = TRUE)
  txt <- readLines(path)
  expect_true(any(grepl("^Begin: 2001-06-01$", txt)))
  expect_true(any(grepl("^End: 2001-06-02$", txt)))
  dash <- grep("^-{10,}$", txt)[1]
  expect_match(txt[dash + 3], "^2001\t6\t1\t")
})

test_that("write_dwf computes climate stats from AirTemp", {
  wx <- data.frame(
    Date = as.Date(c("2000-01-15", "2000-07-15")),
    AirTemp = c(0, 20),
    GlobRad = c(10, 200),
    Precip = c(0, 0)
  )
  path <- write_tmp(wx)
  on.exit(unlink(path), add = TRUE)
  txt <- readLines(path)
  expect_true(any(grepl("^TAverage: 10 dgC$", txt)))
  expect_true(any(grepl("^TAmplitude: 10 dgC$", txt)))
  expect_true(any(grepl("^MaxTDay: 197 yday$", txt)))
})

test_that("write_dwf uses explicit climate stats and units", {
  wx <- tiny_weather()
  path <- write_tmp(
    wx,
    t_average = 7.8,
    t_amplitude = 8.5,
    max_t_day = 209,
    units = c(Precip = "mm/h")
  )
  on.exit(unlink(path), add = TRUE)
  txt <- readLines(path)
  expect_true(any(grepl("^TAverage: 7.8 dgC$", txt)))
  expect_true(any(grepl("^MaxTDay: 209 yday$", txt)))
  dash <- grep("^-{10,}$", txt)[1]
  expect_match(txt[dash + 2], "mm/h")
})

test_that("write_dwf errors without dates or AirTemp/climate", {
  expect_error(
    write_tmp(data.frame(GlobRad = 1, AirTemp = 1, Precip = 0)),
    "Year/Month/Day"
  )
  expect_error(
    write_tmp(data.frame(Date = as.Date("2000-01-01"), GlobRad = 1, Precip = 0)),
    "AirTemp"
  )
})

test_that("write_dwf uses attr(data, 'units') when present", {
  wx <- tiny_weather()
  attr(wx, "units") <- c(GlobRad = "MJ/m^2", AirTemp = "dgC")
  path <- write_tmp(wx, t_average = 1, t_amplitude = 1, max_t_day = 1)
  on.exit(unlink(path), add = TRUE)
  txt <- readLines(path)
  dash <- grep("^-{10,}$", txt)[1]
  expect_match(txt[dash + 2], "MJ/m\\^2")
})
