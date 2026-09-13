test_that("read_dlf parses a print_header/print_dimension false .dlf file", {
  path <- test_path("fixtures", "soil_water_content.dlf")
  dt <- read_dlf(path)

  expect_s3_class(dt, "data.table")
  expect_true(all(c("year", "month", "mday", "hour", "Theta @ -1.25") %in% names(dt)))
  expect_gt(nrow(dt), 0)
  expect_equal(attr(dt, "source_file"), path)
  units <- attr(dt, "units")
  expect_type(units, "character")
  expect_named(units, names(dt))
  expect_true(all(is.na(units)))
})

test_that("read_dlf stores Daisy dimension-line units in attr(., 'units')", {
  path <- test_path("fixtures", "harvest.dlf")
  dt <- read_dlf(path)

  units <- attr(dt, "units")
  expect_named(units, names(dt))
  expect_true(is.na(units[["year"]]))
  expect_true(is.na(units[["crop"]]))
  expect_equal(unname(units[["stem_DM"]]), "Mg DM/ha")
  expect_equal(unname(units[["stem_N"]]), "kg N/ha")
})

test_that("read_dlf subsets units when `cols` is set", {
  path <- test_path("fixtures", "harvest.dlf")
  dt <- read_dlf(path, cols = "stem_DM")

  units <- attr(dt, "units")
  expect_named(units, names(dt))
  expect_equal(unname(units[["stem_DM"]]), "Mg DM/ha")
  expect_false("stem_N" %in% names(units))
})

test_that("read_dlf respects `cols` and always keeps date columns", {
  path <- test_path("fixtures", "soil_water_content.dlf")
  dt <- read_dlf(path, cols = "Theta @ -1.25")

  expect_true(all(c("year", "month", "mday", "hour", "Theta @ -1.25") %in% names(dt)))
  expect_false("Theta @ -3.75" %in% names(dt))
})

test_that("read_dlf warns but doesn't fail on unknown `cols`", {
  path <- test_path("fixtures", "soil_water_content.dlf")
  expect_warning(dt <- read_dlf(path, cols = "not_a_real_column"), "not found")
  expect_false("not_a_real_column" %in% names(dt))
})

test_that("read_dlf `digits` rounds numeric columns but not date/time columns", {
  path <- test_path("fixtures", "soil_water_content.dlf")
  dt <- read_dlf(path, digits = 2)

  expect_equal(dt$year[1], 1986)     # untouched despite being numeric
  expect_equal(dt[["Theta @ -1.25"]][1], round(0.185163, 2))
})

test_that("add_date builds a Date column from year/month/mday", {
  path <- test_path("fixtures", "soil_water_content.dlf")
  dt <- add_date(read_dlf(path), "swc")

  expect_s3_class(dt$Date, "Date")
  expect_equal(dt$Date[1], as.Date("1986-12-01"))
  expect_equal(dt$hour[1], 1)
})

test_that("add_date extends units for new Date/hour columns", {
  path <- test_path("fixtures", "harvest.dlf")
  dt <- add_date(read_dlf(path), "harvest")

  units <- attr(dt, "units")
  expect_named(units, names(dt))
  expect_equal(unname(units[["stem_DM"]]), "Mg DM/ha")
  expect_true(is.na(units[["Date"]]))
})

test_that("add_date warns and fills NA when date columns can't be found", {
  dt <- data.table::data.table(foo = 1:3, bar = 4:6)
  expect_warning(dt2 <- add_date(dt, "mystery"), "Could not identify")
  expect_true(all(is.na(dt2$Date)))
})

test_that("get_unit returns named units and NA when missing", {
  path <- test_path("fixtures", "harvest.dlf")
  dt <- read_dlf(path)

  expect_equal(unname(get_unit(dt, "stem_DM")), "Mg DM/ha")
  expect_equal(
    get_unit(dt, c("stem_DM", "year")),
    c(stem_DM = "Mg DM/ha", year = NA_character_)
  )
  expect_true(is.na(get_unit(dt, "crop")))
  expect_error(get_unit(dt, "not_a_column"), "not found")
})

test_that("get_unit returns NA when the table has no units attribute", {
  dt <- data.table::data.table(foo = 1:3)
  expect_equal(get_unit(dt, "foo"), c(foo = NA_character_))
})

test_that("set_unit records units on derived columns", {
  path <- test_path("fixtures", "harvest.dlf")
  dt <- read_dlf(path)
  dt[, stem_DM_t := stem_DM / 1000]
  set_unit(dt, "stem_DM_t", "t DM/ha")

  expect_equal(unname(get_unit(dt, "stem_DM_t")), "t DM/ha")
  expect_equal(unname(get_unit(dt, "stem_DM")), "Mg DM/ha")
})

test_that("set_unit recycles, clears, and rejects unknown columns", {
  path <- test_path("fixtures", "harvest.dlf")
  dt <- read_dlf(path)

  set_unit(dt, c("crop", "column"), "id")
  expect_equal(unname(get_unit(dt, "crop")), "id")
  expect_equal(unname(get_unit(dt, "column")), "id")

  set_unit(dt, "crop", NA_character_)
  expect_true(is.na(get_unit(dt, "crop")))

  expect_error(set_unit(dt, "missing", "x"), "not found")
  expect_error(set_unit(dt, c("stem_DM", "stem_N"), c("a", "b", "c")), "length")
})

test_that("read_dlf errors clearly on missing/empty files", {
  expect_error(read_dlf("does-not-exist.dlf"), "not found")

  tmp <- tempfile(fileext = ".dlf")
  writeLines(character(0), tmp)
  on.exit(unlink(tmp))
  expect_error(read_dlf(tmp), "empty")
})
