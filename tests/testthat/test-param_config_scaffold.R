test_that("create_param_config scaffolds a scalar parameter", {
  txt <- create_param_config("Ap_clay", type = "scalar")

  expect_true(grepl("name: Ap_clay", txt))
  expect_true(grepl("\\{\\{Ap_clay\\}\\}", txt))
  expect_true(grepl("REPLACE_ME.dai", txt))
  expect_false(grepl("plf_curves:", txt))
})

test_that("create_param_config scaffolds a plf parameter's shape params and curve entry", {
  txt <- create_param_config("LAIvsDS", type = "plf", curve = "logistic")

  expect_true(grepl("name: LAIvsDS_L", txt))
  expect_true(grepl("name: LAIvsDS_k", txt))
  expect_true(grepl("name: LAIvsDS_x0", txt))
  expect_true(grepl("plf_curves:", txt))
  expect_true(grepl("name: LAIvsDS\\b", txt))
  expect_true(grepl("placeholder: LAIvsDS_BLOCK", txt))
  expect_true(grepl("curve: logistic", txt))
  expect_true(grepl("params: \\[LAIvsDS_L, LAIvsDS_k, LAIvsDS_x0\\]", txt))
})

test_that("create_param_config scaffolds a richards curve with 4 shape params", {
  txt <- create_param_config("Growth", type = "plf", curve = "richards")
  for (suffix in c("L", "k", "x0", "v")) {
    expect_true(grepl(paste0("name: Growth_", suffix), txt))
  }
})

test_that("create_param_config mixes scalar and plf types, recycled", {
  txt <- create_param_config(c("Ap_clay", "LAIvsDS"), type = c("scalar", "plf"))
  expect_true(grepl("name: Ap_clay", txt))
  expect_true(grepl("name: LAIvsDS_L", txt))
})

test_that("create_param_config writes to a file when `path` is given", {
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp))
  result <- create_param_config("Ap_clay", path = tmp)

  expect_equal(as.character(result), tmp)
  expect_true(file.exists(tmp))
  expect_true(any(grepl("Ap_clay", readLines(tmp))))
})

test_that("the scaffolded YAML is parseable by read_param_config after filling in placeholders", {
  tmp_yaml <- tempfile(fileext = ".yaml")
  tmp_dai <- tempfile(fileext = ".dai")
  on.exit(unlink(c(tmp_yaml, tmp_dai)))

  writeLines("(clay {{Ap_clay}})", tmp_dai)

  txt <- create_param_config("Ap_clay", type = "scalar")
  txt <- gsub("REPLACE_ME.dai", basename(tmp_dai), txt)
  writeLines(txt, tmp_yaml)

  config <- read_param_config(tmp_yaml)
  expect_equal(config$parameters$name, "Ap_clay")
  expect_true(validate_param_config(config, template_dir = dirname(tmp_dai)))
})

test_that("create_param_config rejects duplicate names and bad types", {
  expect_error(create_param_config(c("a", "a")), "duplicate")
  expect_error(create_param_config("a", type = "not_a_type"), "scalar' or 'plf'")
})
