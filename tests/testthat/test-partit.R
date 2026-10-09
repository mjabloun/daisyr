test_that("render_partit builds a falling leaf table and a stem hump", {
  x <- c(0, 0.6, 1.2, 2)
  fr <- .partit_fractions(x, c(20, 1.2, 20, 0.6))

  expect_gt(fr$leaf[1], 0.99)
  expect_lt(fr$stem[1], 0.01)
  expect_lt(fr$leaf[4], 0.01)
  expect_lt(fr$stem[4], 0.01)
  expect_true(all(fr$leaf >= 0))
  expect_true(all(fr$stem >= 0))
  expect_lte(max(fr$leaf + fr$stem), 1 + 1e-8)
  # Stem is the non-leaf share of the non-storage shoot, so it peaks in between.
  expect_gt(fr$stem[2], fr$stem[1])
  expect_gt(fr$stem[2], fr$stem[4])

  txt <- render_partit(x, c(20, 1.2, 20, 0.6), format = "%.4f")
  expect_match(txt, "^\\(Leaf ")
  expect_match(txt, "\n    \\(Stem ")
})

test_that("read_param_config accepts partit beside plf_curves", {
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp))
  writeLines("
parameters:
  - {name: Ap_clay, default: 0.08, min: 0.05, max: 0.10, from_file: soil.dai, to_file: soil.dai}
  - {name: LAIvsDS_L, default: 5, min: 3, max: 7}
  - {name: LAIvsDS_k, default: 2, min: 0.5, max: 5}
  - {name: LAIvsDS_x0, default: 0.7, min: 0.3, max: 1.2}
  - {name: Shoot_sorg_steepness, default: 10, min: 3, max: 25}
  - {name: Shoot_sorg_centre, default: 1.2, min: 0.8, max: 1.8}
  - {name: Shoot_leaf_steepness, default: 10, min: 3, max: 25}
  - {name: Shoot_leaf_centre, default: 0.6, min: 0.3, max: 1.0}
plf_curves:
  - name: LAIvsDS
    from_file: crop.dai
    to_file: crop.dai
    placeholder: LAIvsDS_PLF
    x_values: [0, 1, 2]
    curve: logistic
    params: [LAIvsDS_L, LAIvsDS_k, LAIvsDS_x0]
partit:
  - name: Shoot
    from_file: crop.dai
    to_file: crop.dai
    placeholder: Shoot_PARTIT
    x_values: [0, 1, 2]
    params: [Shoot_sorg_steepness, Shoot_sorg_centre, Shoot_leaf_steepness, Shoot_leaf_centre]
", tmp)

  reg <- read_param_config(tmp)
  expect_equal(reg$parameters[name == "Shoot_sorg_steepness"]$role, "curve_input")
  expect_equal(reg$parameters[name == "Ap_clay"]$role, "direct")
  expect_equal(reg$partit$Shoot$placeholder, "Shoot_PARTIT")
  expect_equal(
    expand_calibrate_names(reg, "Shoot_sorg_centre"),
    c("Shoot_sorg_steepness", "Shoot_sorg_centre", "Shoot_leaf_steepness", "Shoot_leaf_centre")
  )
  expect_equal(expand_calibrate_names(reg, "LAIvsDS_k"), c("LAIvsDS_L", "LAIvsDS_k", "LAIvsDS_x0"))
})

test_that("a shape parameter cannot feed both a PLF curve and a partit block", {
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp))
  writeLines("
parameters:
  - {name: shared, default: 1, min: 0, max: 2}
  - {name: b, default: 1, min: 0, max: 2}
  - {name: c, default: 1, min: 0, max: 2}
  - {name: d, default: 1, min: 0, max: 2}
  - {name: e, default: 1, min: 0, max: 2}
plf_curves:
  - {name: LAI, from_file: a.dai, to_file: b.dai, placeholder: LAI_PLF, x_values: [0, 1], curve: logistic, params: [shared, b, c]}
partit:
  - {name: Shoot, from_file: a.dai, to_file: b.dai, placeholder: Shoot_PARTIT, x_values: [0, 1], params: [shared, d, e, c]}
", tmp)
  expect_error(read_param_config(tmp), "both plf_curves and partit")
})

test_that("partit rejects the wrong number of parameters and unknown names", {
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp))
  writeLines("
parameters:
  - {name: a, default: 1, min: 0, max: 2}
partit:
  - {name: Shoot, from_file: a.dai, to_file: b.dai, placeholder: Shoot_PARTIT, x_values: [0, 1], params: [a]}
", tmp)
  expect_error(read_param_config(tmp), "expects 4 parameter")

  writeLines("
parameters:
  - {name: a, default: 1, min: 0, max: 2}
  - {name: b, default: 1, min: 0, max: 2}
  - {name: c, default: 1, min: 0, max: 2}
partit:
  - {name: Shoot, from_file: a.dai, to_file: b.dai, placeholder: Shoot_PARTIT, x_values: [0, 1], params: [a, b, c, missing]}
", tmp)
  expect_error(read_param_config(tmp), "unknown parameter")
})

test_that("an expression cannot also be a partit input", {
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp))
  writeLines("
parameters:
  - {name: a, default: 1, min: 0, max: 2, from_file: a.dai, to_file: b.dai}
  - {name: b, from_file: a.dai, to_file: b.dai, expression: 1 - a}
  - {name: c, default: 1, min: 0, max: 2}
  - {name: d, default: 1, min: 0, max: 2}
partit:
  - {name: Shoot, from_file: a.dai, to_file: b.dai, placeholder: Shoot_PARTIT, x_values: [0, 1], params: [b, a, c, d]}
", tmp)
  expect_error(read_param_config(tmp), "partit inputs")
})

test_that("render_templates writes Leaf and Stem into the PARTIT placeholder", {
  dai <- tempfile(fileext = ".dai")
  yaml <- tempfile(fileext = ".yaml")
  out_dir <- withr_local_tempdir()
  on.exit(unlink(c(dai, yaml)), add = TRUE)
  writeLines("(Partit\n    (Root (0.00 0.65) (2.00 0.00))\n    {{Shoot_PARTIT}}\n    (RSR (0.00 0.50) (2.00 0.25))\n)\n", dai)
  dai_name <- basename(dai)
  writeLines(sprintf("
parameters:
  - {name: Shoot_sorg_steepness, default: 20, min: 3, max: 25}
  - {name: Shoot_sorg_centre, default: 1.2, min: 0.8, max: 1.8}
  - {name: Shoot_leaf_steepness, default: 20, min: 3, max: 25}
  - {name: Shoot_leaf_centre, default: 0.6, min: 0.3, max: 1.0}
partit:
  - name: Shoot
    from_file: %s
    to_file: crop.dai
    placeholder: Shoot_PARTIT
    x_values: [0, 2]
    params: [Shoot_sorg_steepness, Shoot_sorg_centre, Shoot_leaf_steepness, Shoot_leaf_centre]
", dai_name), yaml)

  reg <- read_param_config(yaml)
  expect_true(validate_param_config(reg, template_dir = dirname(dai)))
  render_templates(reg, param_config_default_values(reg),
                   template_dir = dirname(dai), output_dir = out_dir)
  txt <- paste(readLines(file.path(out_dir, "crop.dai")), collapse = "\n")
  expect_true(grepl("(Leaf ", txt, fixed = TRUE))
  expect_true(grepl("(Stem ", txt, fixed = TRUE))
  expect_false(grepl("Shoot_PARTIT", txt, fixed = TRUE))
  expect_true(grepl("(Root (0.00 0.65) (2.00 0.00))", txt, fixed = TRUE))
})

test_that("validate_param_config rejects a missing PARTIT placeholder and unsorted knots", {
  dai <- tempfile(fileext = ".dai")
  yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(c(dai, yaml)))
  writeLines("(crop)", dai)
  writeLines(sprintf("
parameters:
  - {name: a, default: 10, min: 3, max: 25}
  - {name: b, default: 1.2, min: 0.8, max: 1.8}
  - {name: c, default: 10, min: 3, max: 25}
  - {name: d, default: 0.6, min: 0.3, max: 1.0}
partit:
  - name: Shoot
    from_file: %s
    to_file: crop.dai
    placeholder: Shoot_PARTIT
    x_values: [0, 1]
    params: [a, b, c, d]
", basename(dai)), yaml)
  reg <- read_param_config(yaml)
  expect_error(validate_param_config(reg, template_dir = dirname(dai)), "Shoot_PARTIT")

  writeLines(sprintf("
parameters:
  - {name: a, default: 10, min: 3, max: 25}
  - {name: b, default: 1.2, min: 0.8, max: 1.8}
  - {name: c, default: 10, min: 3, max: 25}
  - {name: d, default: 0.6, min: 0.3, max: 1.0}
partit:
  - name: Shoot
    from_file: %s
    to_file: crop.dai
    placeholder: Shoot_PARTIT
    x_values: [1, 0]
    params: [a, b, c, d]
", basename(dai)), yaml)
  reg <- read_param_config(yaml)
  expect_error(validate_param_config(reg, template_dir = dirname(dai)), "strictly increasing")
})

test_that("create_param_config scaffolds a partit entry", {
  txt <- create_param_config("Shoot", type = "partit")
  expect_true(grepl("name: Shoot_sorg_steepness", txt))
  expect_true(grepl("name: Shoot_leaf_centre", txt))
  expect_true(grepl("partit:", txt))
  expect_true(grepl("placeholder: Shoot_PARTIT", txt))
  expect_false(grepl("plf_curves:", txt))
  expect_true(grepl("\\{\\{Shoot_PARTIT\\}\\}", txt))
})
