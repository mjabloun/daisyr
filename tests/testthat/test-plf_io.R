test_that("read_plf extracts a PLF that spans multiple lines with a comment", {
  path <- test_path("fixtures", "plf_sample.dai")
  df <- read_plf(path, "TempEff1")

  expect_s3_class(df, "data.frame")
  expect_equal(df$x, c(0.0, 4.0, 19.0, 24.0, 30.0))
  expect_equal(df$y, c(0.0, 0.01, 1.0, 1.0, 0.01))
})

test_that("read_plf extracts a single-line PLF", {
  path <- test_path("fixtures", "plf_sample.dai")
  df <- read_plf(path, "SingleLinePLF")

  expect_equal(df$x, c(-0.3, 0.0, 1.0, 2.0))
  expect_equal(df$y, c(0.0, 0.5, 5.0, 0.0))
})

test_that("read_plf's `occurrence` selects among repeated parameter names", {
  path <- test_path("fixtures", "plf_sample.dai")

  first <- read_plf(path, "TempEff1", occurrence = 1)
  second <- read_plf(path, "TempEff1", occurrence = 2)

  expect_equal(nrow(first), 5)
  expect_equal(nrow(second), 2)
  expect_equal(second$x, c(0.0, 10.0))
  expect_equal(second$y, c(0.0, 1.0))
})

test_that("read_plf errors clearly when the parameter isn't found", {
  path <- test_path("fixtures", "plf_sample.dai")
  expect_error(read_plf(path, "NotAParameter"), "not found")
})

test_that("read_plf errors clearly when `occurrence` is out of range", {
  path <- test_path("fixtures", "plf_sample.dai")
  expect_error(read_plf(path, "TempEff1", occurrence = 3), "occurs 2 time")
})

test_that("read_plf errors on a missing file", {
  expect_error(read_plf("does-not-exist.dai", "TempEff1"), "not found")
})

test_that("plot_plf reads and plots a PLF from a file, returning the points invisibly", {
  path <- test_path("fixtures", "plf_sample.dai")
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp)
  on.exit({ grDevices::dev.off(); unlink(tmp) })

  df <- plot_plf(path, "SingleLinePLF")

  expect_equal(df$x, c(-0.3, 0.0, 1.0, 2.0))
  expect_equal(df$y, c(0.0, 0.5, 5.0, 0.0))
})

test_that("plot_plf accepts the data.frame output of read_plf() directly", {
  path <- test_path("fixtures", "plf_sample.dai")
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp)
  on.exit({ grDevices::dev.off(); unlink(tmp) })

  df_in <- read_plf(path, "TempEff1")
  df_out <- plot_plf(df_in, main = "TempEff1")

  expect_equal(df_out, df_in)
})

test_that("plot_plf accepts a plain list with x/y elements", {
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp)
  on.exit({ grDevices::dev.off(); unlink(tmp) })

  df <- plot_plf(list(x = c(0, 1, 2), y = c(0, 1, 0)))
  expect_equal(df$x, c(0, 1, 2))
  expect_equal(df$y, c(0, 1, 0))
})

test_that("plot_plf errors when `parameter` is missing for a file path", {
  path <- test_path("fixtures", "plf_sample.dai")
  expect_error(plot_plf(path), "`parameter` is required")
})

test_that("plot_plf errors when a data.frame input lacks x/y", {
  expect_error(plot_plf(data.frame(a = 1, b = 2)), "must have 'x' and 'y'")
})
