# daisyr worked example: calibrating two scalar parameters (soil clay
# content and bulk density) against observed soil-water-content data, using
# the real Daisy executable.
#
# Two free parameters, Ap_clay and
# Ap_bulk_density (properties of the Ap horizon), are calibrated against two
# monthly soil water content readings in "observed_data.txt". Using two
# parameters (rather than just one) also makes the Morris sensitivity
# analysis in step 5 non-degenerate - elementary-effects designs are meant
# to explore multi-parameter interactions, and sigma (the interaction/
# non-linearity indicator) can't be computed meaningfully for a single
# parameter.
#
# The parameters YAML, .dai template, and observed data live in
# inst/extdata/calibration_example/ so they ship with the package and can be
# reused from a vignette (see vignettes/daisyr.Rmd) without duplication.
#
# Run this script with the working directory set to this folder
# (examples/calibration/), e.g. via RStudio's "Source" button after opening
# it, or:
#   setwd("examples/calibration")
#   source("run_calibration.R")

devtools::load_all("../..")   # or: library(daisyr)

set_daisy_path("C:/Program Files/Daisy 5.93/bin/daisy.exe")
example_dir <- system.file("extdata", "calibration_example", package = "daisyr")

# 1. Load and validate the parameters -----------------------------------
config <- read_param_config(file.path(example_dir, "parameters.yaml"))
validate_param_config(config, template_dir = example_dir)

# 2. Render the template with its default parameter values, and do a single
#    test run to confirm everything is wired up correctly ----------------
default_values <- stats::setNames(config$parameters$default, config$parameters$name)
render_templates(config, values = default_values, template_dir = example_dir, output_dir = ".")
run_daisy("test-optim.dai", working_dir = ".", show_log = TRUE)

sim <- read_dlf("Output/interval_water_content.dlf")
sim <- add_date(sim, "interval_water_content")
print(sim[, .(Date, hour, tocalibrate)])

# 3. Define the objective: daily-averaged simulated Theta ("tocalibrate")
#    vs. observed Theta (both in [%]) ------------------------------------
obs <- data.table::fread(file.path(example_dir, "observed_data.txt"))
obs[, Date := as.Date(Date, format = "%Y/%m/%d")]

# The Daisy log is hourly; average to daily so it lines up with the
# monthly-snapshot observed dates.
daily_sim_reader <- function(path) {
  sim <- add_date(read_dlf(path), "sim")
  sim[, .(tocalibrate = mean(tocalibrate)), by = Date]
}

objective <- objective_spec(
  obs_data   = obs,
  value_map  = list(list(obs_col = "Theta", sim_col = "tocalibrate", label = "Topsoil (0-15cm)")),
  sim_reader = daily_sim_reader,
  metric     = "RMSE"
)

res <- evaluate_objective(objective, "Output/interval_water_content.dlf", return_per_pair = TRUE)
print(res)

# 4. Calibrate Ap_clay + Ap_bulk_density with DEoptim ---------------------
fit <- calibrate_daisy(
  config    = config,
  run_file    = "test-optim.dai",
  objective   = objective,
  sim_file    = "Output/interval_water_content.dlf",
  working_dir = ".",
  template_dir = example_dir, output_dir = ".",
  control     = list(itermax = 15, NP = 10, trace = TRUE)
)

cat(sprintf("Best Ap_clay = %.4f, Ap_bulk_density = %.4f (RMSE = %.4f)\n",
            fit$best_params[["Ap_clay"]], fit$best_params[["Ap_bulk_density"]], fit$best_score))

# 5. Morris sensitivity analysis on both parameters ------------------------

# helper: daily-average an already-read (raw, hourly) sim table
daily_sim_reader_from_dt <- function(sim_dt) {
  sim_dt <- add_date(sim_dt, "sim")
  sim_dt[, .(tocalibrate = mean(tocalibrate)), by = Date]
}

design <- sa_design(config, method = "morris", r = 4,
                     design = list(type = "oat", levels = 4, grid.jump = 2), seed = 1)

sa_results <- run_sa_design(
  design, config, run_file = "test-optim.dai",
  working_dir = ".", template_dir = example_dir, output_dir = ".",
  output_files = c(swc = "Output/interval_water_content.dlf"),
  response_fun = function(outputs) {
    daily <- daily_sim_reader_from_dt(outputs$swc)
    compute_generic_objective(
      obs_data = obs, sim_data = daily,
      value_map = list(list(obs_col = "Theta", sim_col = "tocalibrate")),
      metric = "RMSE"
    )$summary
  }
)

sa <- complete_sa_analysis(design, sa_results$responses)
print(sa)
plot_sa(sa)
