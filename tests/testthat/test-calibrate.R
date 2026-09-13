test_that("calibrate_daisy DDS minimises a mocked quadratic", {
  config <- read_param_config(test_path("fixtures", "param_config_ok.yaml"))
  local_mocked_bindings(
    evaluate_daisy_candidate = function(p, ...) {
      (as.numeric(p)[[1]] - 0.07)^2
    }
  )
  res <- calibrate_daisy(
    config, run_file = "dummy.dai", daisy_exe = "daisy.exe",
    objective = structure(list(), class = "daisyr_objective"),
    sim_file = "out.dlf",
    method = "DDS",
    names = "Ap_clay",
    control = list(maxeval = 60, seed = 1, start = 0.09)
  )
  expect_equal(res$method, "DDS")
  expect_equal(res$fitted_names, "Ap_clay")
  expect_lt(res$best_score, 1e-4)
  expect_equal(unname(res$best_params[["Ap_clay"]]), 0.07, tolerance = 0.005)
})

test_that("calibrate_daisy multi-BOBYQA tries several starts", {
  skip_if_not_installed("minqa")
  config <- read_param_config(test_path("fixtures", "param_config_ok.yaml"))
  local_mocked_bindings(
    evaluate_daisy_candidate = function(p, ...) {
      (as.numeric(p)[[1]] - 0.07)^2
    }
  )
  res <- calibrate_daisy(
    config, run_file = "dummy.dai", daisy_exe = "daisy.exe",
    objective = structure(list(), class = "daisyr_objective"),
    sim_file = "out.dlf",
    method = "multi-BOBYQA",
    names = "Ap_clay",
    control = list(nstarts = 3, maxfun = 25, seed = 1, start = 0.09)
  )
  expect_equal(res$method, "multi-BOBYQA")
  expect_equal(res$fit$nstarts, 3L)
  expect_lt(res$best_score, 1e-6)
})

test_that("calibrate_daisy BOBYQA minimises a mocked quadratic", {
  skip_if_not_installed("minqa")
  config <- read_param_config(test_path("fixtures", "param_config_ok.yaml"))
  local_mocked_bindings(
    evaluate_daisy_candidate = function(p, ...) {
      (as.numeric(p)[[1]] - 0.07)^2
    }
  )
  res <- calibrate_daisy(
    config, run_file = "dummy.dai", daisy_exe = "daisy.exe",
    objective = structure(list(), class = "daisyr_objective"),
    sim_file = "out.dlf",
    method = "bobyqa",
    names = "Ap_clay",
    control = list(maxfun = 40, start = 0.09)
  )
  expect_equal(res$method, "BOBYQA")
  expect_lt(res$best_score, 1e-6)
})

test_that("calibrate_daisy CMA-ES minimises a mocked quadratic", {
  skip_if_not_installed("cmaes")
  config <- read_param_config(test_path("fixtures", "param_config_ok.yaml"))
  local_mocked_bindings(
    evaluate_daisy_candidate = function(p, ...) {
      (as.numeric(p)[[1]] - 0.07)^2
    }
  )
  res <- suppressMessages(calibrate_daisy(
    config, run_file = "dummy.dai", daisy_exe = "daisy.exe",
    objective = structure(list(), class = "daisyr_objective"),
    sim_file = "out.dlf",
    method = "CMA-ES",
    names = "Ap_clay",
    control = list(maxit = 30, start = 0.09)
  ))
  expect_equal(res$method, "CMA-ES")
  expect_lt(res$best_score, 0.002)
})
