test_that("render_plf_curve computes each built-in family correctly", {
  x <- c(-0.3, 0.0, 1.0, 2.0)

  y_logistic <- render_plf_curve("logistic", x, c(5, 2, 0.7))
  expect_equal(y_logistic, 5 / (1 + exp(-2 * (x - 0.7))))

  y_gompertz <- render_plf_curve("gompertz", x, c(5, 3, 2))
  expect_equal(y_gompertz, 5 * exp(-3 * exp(-2 * x)))

  y_richards <- render_plf_curve("richards", x, c(5, 2, 0.7, 1.5))
  expect_equal(y_richards, 5 / (1 + exp(-2 * (x - 0.7)))^(1 / 1.5))

  y_exp <- render_plf_curve("exponential", x, c(1, 0.5))
  expect_equal(y_exp, 1 * exp(0.5 * x))
})

test_that("built-in curves are monotonic in x for sensible parameters", {
  x <- seq(-0.3, 2, by = 0.1)
  expect_true(all(diff(render_plf_curve("logistic", x, c(5, 2, 0.7))) >= 0))
  expect_true(all(diff(render_plf_curve("gompertz", x, c(5, 3, 2))) >= 0))
})

test_that("render_plf_curve errors on wrong number of parameters", {
  expect_error(render_plf_curve("logistic", c(0, 1), c(1, 2)), "expects 3 parameter")
})

test_that("register_plf_curve makes a custom curve available", {
  register_plf_curve("double_it", function(x, params) x * params[1])
  expect_equal(render_plf_curve("double_it", c(1, 2, 3), 4), c(4, 8, 12))
})

test_that("unregistered custom curve names error clearly", {
  expect_error(render_plf_curve("not_a_curve", 0:1, 1), "No custom PLF curve")
})

test_that("plot_plf_curves returns the plotted points invisibly", {
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp)
  on.exit({ grDevices::dev.off(); unlink(tmp) })

  df <- plot_plf_curves()

  expect_s3_class(df, "data.frame")
  expect_setequal(unique(df$curve), known_plf_curves())
  expect_true(all(c("curve", "x", "y") %in% names(df)))
})

test_that("plot_plf_curves respects a custom curve subset and x-range", {
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp)
  on.exit({ grDevices::dev.off(); unlink(tmp) })

  x <- seq(0, 1, length.out = 10)
  df <- plot_plf_curves(x = x, curves = "logistic")

  expect_equal(unique(df$curve), "logistic")
  expect_equal(nrow(df), length(x))
})

test_that("known_plf_curves(plot = TRUE) draws a plot and still returns curve names", {
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp)
  on.exit({ grDevices::dev.off(); unlink(tmp) })

  curves <- known_plf_curves(plot = TRUE)
  expect_equal(curves, known_plf_curves())
})
