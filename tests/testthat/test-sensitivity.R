# Sensitivity-analysis plotting. Daisy is not invoked: responses are a
# stand-in linear function of the design matrix, the same trick the
# plot_sa() example uses.

test_that("plot_sa errors on an incomplete Morris design", {
  config <- read_param_config(test_path("fixtures", "param_config_ok.yaml"))
  design <- sa_design(config, method = "morris", r = 5, seed = 1)
  expect_error(plot_sa(design), "no elementary effects")
})

test_that("plot_sa errors on an unrelated object", {
  expect_error(plot_sa(list(X = 1)), "morris or sobol")
})

test_that("plot_sa draws a Morris mu* vs sigma plot and returns indices", {
  config <- read_param_config(test_path("fixtures", "param_config_ok.yaml"))
  design <- sa_design(config, method = "morris", r = 5, seed = 1)
  y <- as.matrix(design$X) %*% c(10, 0.1, 0.1, 0.1)
  sa <- complete_sa_analysis(design, y)

  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp)
  on.exit({ grDevices::dev.off(); unlink(tmp) })

  df <- plot_sa(sa)

  expect_s3_class(df, "data.frame")
  expect_equal(df$name, param_config_names(config))
  expect_true(all(c("mu", "mu_star", "sigma") %in% names(df)))
  expect_true(all(is.finite(df$mu_star)))
})

test_that("plot_sa(type = 'bar') returns the same Morris indices", {
  config <- read_param_config(test_path("fixtures", "param_config_ok.yaml"))
  design <- sa_design(config, method = "morris", r = 5, seed = 1)
  y <- as.matrix(design$X) %*% c(10, 0.1, 0.1, 0.1)
  sa <- complete_sa_analysis(design, y)

  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp)
  on.exit({ grDevices::dev.off(); unlink(tmp) })

  expect_equal(plot_sa(sa, type = "bar"), plot_sa(sa, type = "effects"))
})

test_that("plot_sa rejects an unknown Morris type", {
  config <- read_param_config(test_path("fixtures", "param_config_ok.yaml"))
  design <- sa_design(config, method = "morris", r = 5, seed = 1)
  sa <- complete_sa_analysis(design, as.matrix(design$X) %*% c(10, 0.1, 0.1, 0.1))
  expect_error(plot_sa(sa, type = "spaghetti"), "should be one of")
})

test_that("plot_sa errors on an incomplete Sobol design", {
  set.seed(1)
  n <- 20
  X1 <- data.frame(p1 = stats::runif(n), p2 = stats::runif(n))
  X2 <- data.frame(p1 = stats::runif(n), p2 = stats::runif(n))
  design <- sensitivity::soboljansen(model = NULL, X1 = X1, X2 = X2)
  expect_error(plot_sa(design), "no indices")
})

test_that("plot_sa draws Sobol S vs ST bars and returns indices", {
  set.seed(1)
  n <- 30
  X1 <- data.frame(p1 = stats::runif(n), p2 = stats::runif(n))
  X2 <- data.frame(p1 = stats::runif(n), p2 = stats::runif(n))
  design <- sensitivity::soboljansen(model = NULL, X1 = X1, X2 = X2)
  sa <- complete_sa_analysis(design, as.matrix(design$X) %*% c(2, 0.1))

  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp)
  on.exit({ grDevices::dev.off(); unlink(tmp) })

  df <- plot_sa(sa)

  expect_s3_class(df, "data.frame")
  expect_equal(df$name, c("p1", "p2"))
  expect_true(all(c("S", "ST") %in% names(df)))
  expect_true(all(is.finite(df$S)))
  expect_true(all(is.finite(df$ST)))
  expect_gt(df$S[df$name == "p1"], df$S[df$name == "p2"])
})

test_that("suggest_calibrate_names keeps influential names and expands PLF units", {
  config <- read_param_config(test_path("fixtures", "param_config_ok.yaml"))
  idx <- data.frame(
    name = c("Ap_clay", "LAIvsDS_L", "LAIvsDS_k", "LAIvsDS_x0"),
    mu_star = c(1, 0.02, 0.8, 0.01),
    stringsAsFactors = FALSE
  )
  got <- suggest_calibrate_names(idx, config, frac = 0.1)
  expect_true("Ap_clay" %in% got)
  expect_setequal(got, c("Ap_clay", "LAIvsDS_L", "LAIvsDS_k", "LAIvsDS_x0"))
})
