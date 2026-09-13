test_that("records_from_df folds <field>_value/<field>_unit column pairs", {
  df <- data.frame(
    name = c("Hor_25", "Hor_50"),
    texture_class = c("FAO3", "FAO3"),
    dry_bulk_density_value = c(1.46, 1.62),
    dry_bulk_density_unit = c("g/cm^3", "g/cm^3"),
    clay_value = c(0.22, 0.30),
    clay_unit = c("", ""),
    hydraulic = c("Cosby_et_al", "Cosby_et_al"),
    stringsAsFactors = FALSE
  )
  records <- records_from_df(df)

  expect_length(records, 2)
  expect_equal(records[[1]]$name, "Hor_25")
  expect_equal(records[[1]]$dry_bulk_density, list(value = 1.46, unit = "g/cm^3"))
  expect_equal(records[[1]]$clay, list(value = 0.22, unit = ""))
  expect_equal(records[[1]]$hydraulic, "Cosby_et_al")

  ## the folded records should render through render_horizon() unmodified
  txt <- render_horizon(records[[1]])
  expect_match(txt, "\\(dry_bulk_density 1\\.46 \\[g/cm\\^3\\]\\)")
})

test_that("records_from_df leaves unpaired *_value columns untouched", {
  df <- data.frame(name = "X", lonely_value = 5, stringsAsFactors = FALSE)
  records <- records_from_df(df)
  expect_equal(records[[1]][["lonely_value"]], 5)
  expect_null(records[[1]][["lonely", exact = TRUE]])
})

test_that("records_from_df drops NA fields entirely (both scalar and the value half of a pair)", {
  df <- data.frame(
    name = c("A", "B"), extra = c(1, NA),
    v_value = c(1.0, NA), v_unit = c("cm", "cm"),
    stringsAsFactors = FALSE
  )
  records <- records_from_df(df)
  expect_equal(records[[1]]$extra, 1)
  expect_null(records[[2]]$extra)
  expect_equal(records[[1]]$v, list(value = 1.0, unit = "cm"))
  expect_null(records[[2]]$v)
})

test_that("records_from_df passes list-columns through unchanged (e.g. a Devel method-pair per row)", {
  df <- data.frame(name = "X", stringsAsFactors = FALSE)
  df$Devel <- list(list("original", list(DSRate2 = 0.025)))
  records <- records_from_df(df)
  expect_equal(records[[1]]$Devel, list("original", list(DSRate2 = 0.025)))
})

test_that("records_from_df handles a zero-row data.frame", {
  expect_equal(records_from_df(data.frame(name = character(0))), list())
})

test_that("records_from_df requires an actual data.frame", {
  expect_error(records_from_df(list(a = 1)), "requires a data.frame")
})
