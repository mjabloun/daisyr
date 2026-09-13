test_that("list_dai_sections returns the documented section/description table", {
  df <- list_dai_sections()
  expect_s3_class(df, "data.frame")
  expect_setequal(df$section, c("directory", "path", "libraries", "comment", "horizons",
                                 "columns", "crops", "activities", "programs", "run"))
})

test_that("scaffold_dai_yaml rejects unknown sections and an empty selection", {
  expect_error(scaffold_dai_yaml(sections = "not_a_section"), "Unknown section")
  expect_error(scaffold_dai_yaml(sections = character(0)), "No sections selected")
})

test_that("scaffold_dai_yaml(sections = 'activities') produces valid, runnable YAML", {
  txt <- scaffold_dai_yaml(sections = c("activities", "programs", "run"))
  cfg <- yaml::yaml.load(txt)
  expect_true(!is.null(cfg$activities))
  expect_true(!is.null(cfg$programs))
  expect_true(!is.null(cfg$run))

  ## The scaffold's placeholders are cross-referenced, so it should run
  ## through generate_dai() without error even though it's not "real" data.
  dai_txt <- generate_dai(cfg)
  expect_match(dai_txt, "defaction")
  expect_match(dai_txt, "defprogram")
  expect_match(dai_txt, "\\(run ")
})

test_that("scaffold_dai_yaml writes to a file when output_path is given", {
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp))
  result <- scaffold_dai_yaml(sections = "horizons", output_path = tmp)
  expect_type(result, "character")
  expect_true(file.exists(tmp))
  expect_true(any(grepl("horizons:", readLines(tmp))))
})

test_that("the full default scaffold (every section) also runs through generate_dai() cleanly", {
  txt <- scaffold_dai_yaml()
  cfg <- yaml::yaml.load(txt)
  dai_txt <- generate_dai(cfg)
  expect_match(dai_txt, "defhorizon")
  expect_match(dai_txt, "defcolumn")
  expect_match(dai_txt, "defcrop")
  expect_match(dai_txt, "defaction")
  expect_match(dai_txt, "defprogram")
})
