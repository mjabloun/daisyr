test_that("compute_generic_objective scores a single obs/sim pair", {
  obs <- data.table::data.table(Date = as.Date("2020-01-01") + 0:4, Theta = c(0.2, 0.22, 0.25, 0.23, 0.21))
  sim <- data.table::data.table(Date = as.Date("2020-01-01") + 0:4, simTheta = c(0.21, 0.21, 0.24, 0.24, 0.20))

  res <- compute_generic_objective(
    obs_data = obs, sim_data = sim,
    value_map = list(list(obs_col = "Theta", sim_col = "simTheta")),
    metric = "RMSE"
  )

  expect_type(res$summary, "double")
  expect_equal(res$summary, hydroGOF::rmse(sim$simTheta, obs$Theta))
})

test_that("compute_generic_objective supports multiple weighted pairs", {
  obs <- data.table::data.table(Date = as.Date("2020-01-01") + 0:2,
                                 Theta1 = c(0.2, 0.22, 0.25), Theta2 = c(0.3, 0.31, 0.29))
  sim <- data.table::data.table(Date = as.Date("2020-01-01") + 0:2,
                                 simTheta1 = c(0.21, 0.21, 0.24), simTheta2 = c(0.29, 0.30, 0.30))

  value_map <- list(
    list(obs_col = "Theta1", sim_col = "simTheta1", weight = 2, label = "Top"),
    list(obs_col = "Theta2", sim_col = "simTheta2", weight = 1, label = "Mid")
  )
  res <- compute_generic_objective(obs_data = obs, sim_data = sim, value_map = value_map,
                                    metric = "RMSE", return_per_pair = TRUE)

  expect_equal(nrow(res$per_variable), 2)
  s1 <- hydroGOF::rmse(sim$simTheta1, obs$Theta1)
  s2 <- hydroGOF::rmse(sim$simTheta2, obs$Theta2)
  expect_equal(res$summary, stats::weighted.mean(c(s1, s2), w = c(2, 1)))
})

test_that("compute_generic_objective errors when obs/sim don't overlap", {
  obs <- data.table::data.table(Date = as.Date("2020-01-01"), Theta = 0.2)
  sim <- data.table::data.table(Date = as.Date("2021-01-01"), simTheta = 0.2)
  expect_error(
    compute_generic_objective(obs_data = obs, sim_data = sim,
                               value_map = list(list(obs_col = "Theta", sim_col = "simTheta"))),
    "No overlapping rows"
  )
})

test_that("objective_spec + evaluate_objective round-trip via a sim_file reader", {
  obs <- data.table::data.table(Date = as.Date("2020-01-01") + 0:1, Theta = c(0.2, 0.22))

  sim_file <- tempfile(fileext = ".csv")
  on.exit(unlink(sim_file))
  data.table::fwrite(
    data.table::data.table(Date = as.Date("2020-01-01") + 0:1, simTheta = c(0.19, 0.23)),
    sim_file
  )

  obj <- objective_spec(
    obs_data = obs, value_map = list(list(obs_col = "Theta", sim_col = "simTheta")),
    sim_reader = data.table::fread, metric = "RMSE"
  )
  res <- evaluate_objective(obj, sim_file)
  expect_type(res$summary, "double")
})

test_that("composite_objective scores named components with weights", {
  dates <- as.Date("2020-01-01") + 0:1
  obs_a <- data.table::data.table(Date = dates, y = c(10, 12))
  obs_b <- data.table::data.table(Date = dates, theta = c(0.2, 0.3))
  sim_a <- tempfile(fileext = ".dlf")
  sim_b <- tempfile(fileext = ".dlf")
  on.exit(unlink(c(sim_a, sim_b)), add = TRUE)
  data.table::fwrite(data.table::data.table(Date = dates, WSOrg = c(11, 11)), sim_a)
  data.table::fwrite(data.table::data.table(Date = dates, swc = c(0.21, 0.28)), sim_b)

  a <- objective_spec(
    obs_data = obs_a,
    value_map = list(list(obs_col = "y", sim_col = "WSOrg")),
    sim_reader = data.table::fread, metric = "RMSE",
    output_file = basename(sim_a)
  )
  b <- objective_spec(
    obs_data = obs_b,
    value_map = list(list(obs_col = "theta", sim_col = "swc")),
    sim_reader = data.table::fread, metric = "MAE",
    output_file = basename(sim_b)
  )
  comp <- composite_objective(list(yield = a, swc = b), weights = c(2, 1), name = "all")
  res <- evaluate_objective(comp, sim_a, return_per_pair = TRUE)
  expect_equal(nrow(res$per_component), 2)
  expect_equal(res$per_component$component, c("yield", "swc"))
  s1 <- hydroGOF::rmse(c(11, 11), c(10, 12))
  s2 <- hydroGOF::mae(c(0.21, 0.28), c(0.2, 0.3))
  expect_equal(res$summary, stats::weighted.mean(c(s1, s2), w = c(2, 1)))
})

test_that("compute_generic_objective allows the same column name in obs and sim", {
  dates <- as.Date("2020-01-01") + 0:2
  obs <- data.table::data.table(Date = dates, Theta = c(0.2, 0.22, 0.25))
  sim <- data.table::data.table(Date = dates, Theta = c(0.21, 0.21, 0.24))
  res <- compute_generic_objective(
    obs_data = obs, sim_data = sim,
    value_map = list(list(obs_col = "Theta", sim_col = "Theta")),
    metric = "RMSE", plot = TRUE
  )
  expect_equal(res$summary, hydroGOF::rmse(sim$Theta, obs$Theta))
  expect_length(res$plot, 1)
})

test_that("in-memory sim_outputs get a Date column like sim_reader", {
  dates <- as.Date("2020-01-01") + 0:1
  obs <- data.table::data.table(Date = dates, Theta = c(0.2, 0.22))
  raw <- data.table::data.table(
    year = 2020L, month = 1L, mday = 1:2,
    simTheta = c(0.21, 0.23)
  )
  spec <- objective_spec(
    obs_data = obs,
    value_map = list(list(obs_col = "Theta", sim_col = "simTheta")),
    join_cols = "Date",
    metric = "RMSE",
    output_file = "interval_water_content.dlf"
  )
  res <- evaluate_objective(
    spec, sim_file = "unused.dlf",
    sim_outputs = list(interval_water_content = raw)
  )
  expect_equal(res$summary, hydroGOF::rmse(raw$simTheta, obs$Theta))
})
