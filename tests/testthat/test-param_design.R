# Space-filling designs. lhs/DiceDesign/randtoolbox are optional; skip when missing.
# run_param_design is tested with mocked Daisy (no daisy.exe).

bounds_df <- function() {
  data.frame(
    name = c("p1", "p2"),
    min = c(0, 10),
    max = c(1, 20),
    stringsAsFactors = FALSE
  )
}

test_that("generate_param_design accepts a list of {name, min, max} and a data.frame", {
  skip_if_not_installed("lhs")
  from_df <- generate_param_design(bounds_df(), n = 8, method = "lhs", seed = 1)
  from_list <- generate_param_design(
    list(
      list(name = "p1", min = 0, max = 1),
      list(name = "p2", min = 10, max = 20)
    ),
    n = 8, method = "lhs", seed = 1
  )
  expect_equal(from_df, from_list, ignore_attr = FALSE)
  expect_equal(names(from_df), c("p1", "p2"))
  expect_equal(nrow(from_df), 8)
})

test_that("generate_param_design works for 1 parameter and many", {
  skip_if_not_installed("lhs")
  one <- generate_param_design(
    list(list(name = "only", min = -2, max = 5)),
    n = 5, method = "lhs", seed = 2
  )
  expect_equal(ncol(one), 1)
  expect_equal(names(one), "only")
  expect_true(all(one$only >= -2 & one$only <= 5))

  many_params <- data.frame(
    name = paste0("x", seq_len(12)),
    min = 0,
    max = 1
  )
  many <- generate_param_design(many_params, n = 15, method = "lhs", seed = 3)
  expect_equal(ncol(many), 12)
  expect_equal(nrow(many), 15)
})

test_that("generate_param_design rescales to [min, max] and respects seed", {
  skip_if_not_installed("lhs")
  a <- generate_param_design(bounds_df(), n = 10, method = "lhs", seed = 42)
  b <- generate_param_design(bounds_df(), n = 10, method = "lhs", seed = 42)
  expect_equal(a, b, ignore_attr = TRUE)
  expect_true(all(a$p1 >= 0 & a$p1 <= 1))
  expect_true(all(a$p2 >= 10 & a$p2 <= 20))
})

test_that("generate_param_design Sobol' is reproducible and in bounds", {
  skip_if_not_installed("randtoolbox")
  a <- generate_param_design(bounds_df(), n = 16, method = "sobol", seed = 7)
  b <- generate_param_design(bounds_df(), n = 16, method = "sobol", seed = 7)
  expect_equal(unname(as.matrix(a)), unname(as.matrix(b)))
  expect_true(all(a$p1 >= 0 & a$p1 <= 1))
  expect_true(all(a$p2 >= 10 & a$p2 <= 20))
})

test_that("extend_param_design continues the same Sobol' sequence", {
  skip_if_not_installed("randtoolbox")
  full <- generate_param_design(bounds_df(), n = 12, method = "sobol", seed = 11)
  first <- generate_param_design(bounds_df(), n = 7, method = "sobol", seed = 11)
  extra <- extend_param_design(first, n_additional = 5)
  expect_equal(nrow(extra), 5)
  expect_equal(names(extra), names(full))
  expect_equal(unname(as.matrix(first)), unname(as.matrix(full[1:7, ])))
  expect_equal(unname(as.matrix(extra)), unname(as.matrix(full[8:12, ])))

  more <- extend_param_design(extra, n_additional = 3)
  full15 <- generate_param_design(bounds_df(), n = 15, method = "sobol", seed = 11)
  expect_equal(unname(as.matrix(more)), unname(as.matrix(full15[13:15, ])))
})

test_that("extend_param_design refuses LHS", {
  skip_if_not_installed("lhs")
  d <- generate_param_design(bounds_df(), n = 4, method = "lhs", seed = 1)
  expect_error(extend_param_design(d, n_additional = 2, method = "lhs"), "not extensible")
  expect_error(extend_param_design(d, n_additional = 2), "not extensible")
})

test_that("generate_param_design dice_lhs is in bounds, reproducible, and not extensible", {
  skip_if_not_installed("DiceDesign")
  a <- generate_param_design(bounds_df(), n = 6, method = "dice_lhs", seed = 9,
                             it = 1, inner_it = 2)
  b <- generate_param_design(bounds_df(), n = 6, method = "dice_lhs", seed = 9,
                             it = 1, inner_it = 2)
  expect_equal(unname(as.matrix(a)), unname(as.matrix(b)))
  expect_equal(nrow(a), 6)
  expect_equal(attr(a, "method"), "dice_lhs")
  expect_true(all(a$p1 >= 0 & a$p1 <= 1))
  expect_true(all(a$p2 >= 10 & a$p2 <= 20))

  one <- generate_param_design(
    list(list(name = "only", min = -2, max = 5)),
    n = 5, method = "dice_lhs", seed = 2, it = 1, inner_it = 2
  )
  expect_equal(ncol(one), 1)
  expect_true(all(one$only >= -2 & one$only <= 5))

  expect_error(extend_param_design(a, n_additional = 2), "not extensible")
  expect_error(extend_param_design(a, n_additional = 2, method = "dice_lhs"),
               "not extensible")
})

test_that("generate_param_design validates inputs", {
  expect_error(generate_param_design(data.frame(name = "a"), n = 2, method = "lhs"),
               "name.*min.*max")
  expect_error(generate_param_design(bounds_df(), n = 0, method = "lhs"), "positive")
})

test_that("run_param_design archives renamed files and read_param_design reloads them", {
  config <- read_param_config(test_path("fixtures", "param_config_ok.yaml"))
  fixture <- test_path("fixtures", "soil_water_content.dlf")

  work <- withr_local_tempdir()
  keep <- file.path(work, "archive")
  out_rel <- file.path("Output", "swc.dlf")
  dir.create(file.path(work, "Output"), recursive = TRUE)
  file.copy(fixture, file.path(work, out_rel))

  design <- data.frame(
    Ap_clay = c(0.06, 0.09),
    LAIvsDS_L = c(4, 6),
    LAIvsDS_k = c(1, 2),
    LAIvsDS_x0 = c(0.5, 0.8)
  )

  local_mocked_bindings(
    render_templates = function(...) invisible(character()),
    run_daisy = function(...) invisible(0L)
  )

  res <- run_param_design(
    design, config,
    run_file = "dummy.dai", daisy_exe = "daisy.exe",
    output_files = c(swc = out_rel),
    working_dir = work, template_dir = test_path("fixtures"), output_dir = work,
    keep_files = TRUE, keep_dir = keep,
    read = FALSE,
    progress = FALSE
  )

  expect_equal(res$design$run_id, c("run_0001", "run_0002"))
  expect_true(file.exists(file.path(keep, "design.csv")))
  expect_true(file.exists(file.path(keep, "run_0001_swc.dlf")))
  expect_true(file.exists(file.path(keep, "run_0002_swc.dlf")))
  expect_false(dir.exists(file.path(keep, "run_0001")))
  expect_false(file.exists(file.path(keep, "run_0001", "parameters.csv")))
  expect_null(res$outputs)

  loaded <- read_param_design(keep)
  expect_equal(loaded$design$run_id, res$design$run_id)
  expect_s3_class(loaded$outputs, "data.table")
  expect_true("Theta @ -1.25" %in% names(loaded$outputs))
  expect_false("Ap_clay" %in% names(loaded$outputs))
  expect_equal(unique(loaded$outputs$run_id), c("run_0001", "run_0002"))

  with_mutator <- read_param_design(
    keep,
    sim_mutator = function(dt) {
      dt <- add_date(dt, "swc")
      dt[, c("Date", "Theta @ -1.25"), with = FALSE]
    }
  )
  expect_s3_class(with_mutator$outputs$Date, "Date")
  expect_true("Theta @ -1.25" %in% names(with_mutator$outputs))
  expect_false("Theta @ -3.75" %in% names(with_mutator$outputs))
  expect_equal(unique(with_mutator$outputs$run_id), c("run_0001", "run_0002"))
})

test_that("run_param_design requires reading when files are not kept", {
  config <- read_param_config(test_path("fixtures", "param_config_ok.yaml"))
  expect_error(
    run_param_design(
      data.frame(Ap_clay = 0.06, LAIvsDS_L = 4, LAIvsDS_k = 1, LAIvsDS_x0 = 0.5),
      config, run_file = "x.dai", daisy_exe = "daisy.exe",
      output_files = c(swc = "Output/swc.dlf"),
      keep_files = FALSE, read = FALSE, progress = FALSE
    ),
    "must be read"
  )
})

test_that("run_param_design can read in-loop with a custom reader", {
  config <- read_param_config(test_path("fixtures", "param_config_ok.yaml"))
  fixture <- test_path("fixtures", "soil_water_content.dlf")
  work <- withr_local_tempdir()
  dir.create(file.path(work, "Output"))
  file.copy(fixture, file.path(work, "Output", "swc.dlf"))

  design <- data.frame(
    Ap_clay = 0.07, LAIvsDS_L = 5, LAIvsDS_k = 2, LAIvsDS_x0 = 0.7
  )

  local_mocked_bindings(
    render_templates = function(...) invisible(character()),
    run_daisy = function(...) invisible(0L)
  )

  custom <- function(path, what = "nrow") {
    dt <- read_dlf(path)
    if (identical(what, "nrow")) nrow(dt) else dt
  }

  res <- run_param_design(
    design, config,
    run_file = "dummy.dai", daisy_exe = "daisy.exe",
    output_files = c(swc = file.path("Output", "swc.dlf")),
    working_dir = work, output_dir = work,
    keep_files = FALSE, read = TRUE,
    output_reader = custom,
    reader_args = list(what = "nrow"),
    progress = FALSE
  )

  expect_null(res$keep_dir)
  expect_s3_class(res$outputs, "data.table")
  expect_gt(res$outputs$value[1], 0)
})
