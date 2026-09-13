test_that("read_param_config parses direct and curve_input parameters", {
  reg <- read_param_config(test_path("fixtures", "param_config_ok.yaml"))

  expect_s3_class(reg, "daisyr_param_config")
  expect_setequal(reg$parameters$name,
                   c("Ap_clay", "LAIvsDS_L", "LAIvsDS_k", "LAIvsDS_x0"))
  expect_equal(reg$parameters[name == "Ap_clay"]$role, "direct")
  expect_equal(reg$parameters[name == "LAIvsDS_L"]$role, "curve_input")

  expect_length(reg$plf_curves, 1)
  expect_equal(reg$plf_curves$LAIvsDS$curve, "logistic")
  expect_equal(reg$plf_curves$LAIvsDS$x_values, c(-0.3, 0.0, 1.0, 2.0))
})

test_that("read_param_config rejects duplicate parameter names", {
  yaml_txt <- "
parameters:
  - {name: p1, default: 1, min: 0, max: 2, from_file: a.dai, to_file: b.dai}
  - {name: p1, default: 1, min: 0, max: 2, from_file: a.dai, to_file: b.dai}
"
  tmp <- tempfile(fileext = ".yaml")
  writeLines(yaml_txt, tmp)
  on.exit(unlink(tmp))
  expect_error(read_param_config(tmp), "Duplicate parameter")
})

test_that("read_param_config rejects plf_curves referencing unknown parameters", {
  yaml_txt <- "
parameters:
  - {name: p1, default: 1, min: 0, max: 2}
plf_curves:
  - name: curve1
    from_file: a.dai
    to_file: b.dai
    placeholder: BLOCK
    x_values: [0, 1]
    curve: logistic
    params: [p1, does_not_exist]
"
  tmp <- tempfile(fileext = ".yaml")
  writeLines(yaml_txt, tmp)
  on.exit(unlink(tmp))
  expect_error(read_param_config(tmp), "unknown parameter")
})

test_that("direct parameters must have both from_file and to_file", {
  yaml_txt <- "
parameters:
  - {name: p1, default: 1, min: 0, max: 2, from_file: a.dai}
"
  tmp <- tempfile(fileext = ".yaml")
  writeLines(yaml_txt, tmp)
  on.exit(unlink(tmp))
  expect_error(read_param_config(tmp), "role 'direct'")
})

test_that("param_config_names lists every free parameter", {
  reg <- read_param_config(test_path("fixtures", "param_config_ok.yaml"))
  expect_setequal(param_config_names(reg),
                   c("Ap_clay", "LAIvsDS_L", "LAIvsDS_k", "LAIvsDS_x0"))
})

test_that("validate_param_config checks placeholders exist exactly once in templates", {
  reg <- read_param_config(test_path("fixtures", "param_config_ok.yaml"))
  expect_true(validate_param_config(reg, template_dir = test_path("fixtures")))
})

test_that("validate_param_config fails when a placeholder is missing from its template", {
  yaml_txt <- sprintf("
parameters:
  - name: not_in_template
    from_file: %s
    to_file: out.dai
    default: 1
    min: 0
    max: 2
", file.path(test_path("fixtures"), "horizon_template.dai"))
  tmp <- tempfile(fileext = ".yaml")
  writeLines(yaml_txt, tmp)
  on.exit(unlink(tmp))
  reg <- read_param_config(tmp)
  expect_error(validate_param_config(reg, template_dir = "."), "not found")
})

test_that("validate_param_config rejects non-increasing plf_curves x_values", {
  yaml_txt <- "
parameters:
  - {name: p1, default: 1, min: 0, max: 2}
plf_curves:
  - name: curve1
    from_file: crop_template.dai
    to_file: crop.dai
    placeholder: LAIvsDS_BLOCK
    x_values: [1, 0]
    curve: logistic
    params: [p1]
"
  tmp <- tempfile(fileext = ".yaml")
  writeLines(yaml_txt, tmp)
  on.exit(unlink(tmp))
  reg <- read_param_config(tmp)
  expect_error(validate_param_config(reg, template_dir = test_path("fixtures")), "strictly increasing")
})

test_that("expand_calibrate_names keeps a PLF curve as one unit", {
  reg <- read_param_config(test_path("fixtures", "param_config_ok.yaml"))
  expect_equal(
    expand_calibrate_names(reg, "LAIvsDS_k"),
    c("LAIvsDS_L", "LAIvsDS_k", "LAIvsDS_x0")
  )
  expect_equal(expand_calibrate_names(reg, "Ap_clay"), "Ap_clay")
  expect_equal(expand_calibrate_names(reg, NULL), param_config_names(reg))
})

test_that("fill_param_config_values pins unspecified names at default", {
  reg <- read_param_config(test_path("fixtures", "param_config_ok.yaml"))
  v <- fill_param_config_values(reg, 0.09, names = "Ap_clay")
  expect_equal(unname(v[["Ap_clay"]]), 0.09)
  expect_equal(unname(v[["LAIvsDS_L"]]), 5)
  expect_equal(unname(v[["LAIvsDS_k"]]), 2)
})
