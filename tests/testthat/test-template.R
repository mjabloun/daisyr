test_that("render_templates substitutes a direct scalar parameter", {
  reg <- read_param_config(test_path("fixtures", "param_config_ok.yaml"))
  out_dir <- withr_local_tempdir()

  values <- c(Ap_clay = 0.0857, LAIvsDS_L = 5, LAIvsDS_k = 2, LAIvsDS_x0 = 0.7)
  written <- render_templates(reg, values,
                               template_dir = test_path("fixtures"),
                               output_dir = out_dir)

  horizon_out <- readLines(file.path(out_dir, "horizon.dai"))
  expect_true(any(grepl("(clay 0.0857)", horizon_out, fixed = TRUE)))
})

test_that("render_templates computes and substitutes a plf_curves block", {
  reg <- read_param_config(test_path("fixtures", "param_config_ok.yaml"))
  out_dir <- withr_local_tempdir()

  values <- c(Ap_clay = 0.08, LAIvsDS_L = 5, LAIvsDS_k = 2, LAIvsDS_x0 = 0.7)
  render_templates(reg, values,
                    template_dir = test_path("fixtures"),
                    output_dir = out_dir)

  crop_out <- paste(readLines(file.path(out_dir, "crop.dai")), collapse = " ")
  expected_y <- render_plf_curve("logistic", c(-0.3, 0.0, 1.0, 2.0), c(5, 2, 0.7))
  expected_block <- format_plf_block(c(-0.3, 0.0, 1.0, 2.0), expected_y, "%.3f")
  expect_true(grepl(expected_block, crop_out, fixed = TRUE))
})

test_that("render_templates errors when a required value is missing", {
  reg <- read_param_config(test_path("fixtures", "param_config_ok.yaml"))
  out_dir <- withr_local_tempdir()

  values <- c(Ap_clay = 0.08, LAIvsDS_L = 5, LAIvsDS_k = 2)  # missing LAIvsDS_x0
  expect_error(
    render_templates(reg, values, template_dir = test_path("fixtures"), output_dir = out_dir),
    "Missing value"
  )
})

test_that("format_plf_block formats an (x y) sequence in Daisy syntax", {
  expect_equal(
    format_plf_block(c(-0.3, 0, 1), c(0, 0.5, 4.87), "%.2f"),
    "(-0.3 0.00) (0 0.50) (1 4.87)"
  )
})
