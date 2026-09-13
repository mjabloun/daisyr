test_that(".q always quotes, .bare never quotes strings/keywords", {
  expect_equal(.q("Spring Barley"), '"Spring Barley"')
  expect_equal(.q('has "quotes"'), '"has \\"quotes\\""')
  expect_equal(.bare("Cosby_et_al"), "Cosby_et_al")
  expect_equal(.bare(TRUE), "true")
  expect_equal(.bare(FALSE), "false")
  expect_equal(.bare(100), "100")
})

test_that(".fmt_num prints integers bare and keeps decimals otherwise", {
  expect_equal(.fmt_num(100), "100")
  expect_equal(.fmt_num(-1), "-1")
  expect_equal(.fmt_num(12.73), "12.73")
})

test_that(".fmt_frac always shows at least one decimal place", {
  expect_equal(.fmt_frac(1), "1.0")
  expect_equal(.fmt_frac(0.85), "0.85")
  expect_equal(.fmt_frac(0), "0.0")
})

test_that("parse_mm_dd accepts MM-DD, YYYY-MM-DD, and a 2-element vector", {
  expect_equal(parse_mm_dd("04-01"), list(month = 4L, day = 1L))
  expect_equal(parse_mm_dd("2020-04-01"), list(month = 4L, day = 1L))
  expect_equal(parse_mm_dd(c(4, 1)), list(month = 4L, day = 1L))
})

test_that("render_wait_from_date renders wait_mm_dd/wait_days/wait forms", {
  expect_equal(render_wait_from_date("04-01"), "(wait_mm_dd 4 1)")
  expect_equal(render_wait_from_date(list(days = 5)), "(wait_days 5)")
  expect_equal(render_wait_from_date(list(mm_dd = "04-01")), "(wait_mm_dd 4 1)")
  expect_null(render_wait_from_date(NULL))
})

test_that("render_condition handles mm_dd, ds (with default_crop), and combined ds+mm_dd", {
  expect_equal(render_condition(list(mm_dd = "08-20")), "(mm_dd 8 20)")
  expect_equal(render_condition(list(ds = 2.0), default_crop = "Spring Barley"),
               '(crop_ds_after "Spring Barley" 2.0 [])')
  cond <- render_condition(list(ds = 2.0, mm_dd = "08-20"), default_crop = "Spring Barley")
  expect_match(cond, "^\\(or ")
  expect_match(cond, 'crop_ds_after "Spring Barley" 2\\.0')
  expect_match(cond, "mm_dd 8 20")
})

test_that("render_condition requires a crop for a bare 'ds' condition", {
  expect_error(render_condition(list(ds = 1.8)), "needs a 'crop'")
})

test_that("render_condition supports not/any_of/all_of/raw", {
  expect_equal(render_condition(list(raw = "(custom)")), "(custom)")
  expect_equal(render_condition(list(not = list(mm_dd = "01-01"))), "(not (mm_dd 1 1))")
  any_txt <- render_condition(list(any_of = list(list(mm_dd = "01-01"), list(mm_dd = "02-02"))))
  expect_match(any_txt, "^\\(or ")
  all_txt <- render_condition(list(all_of = list(list(mm_dd = "01-01"), list(mm_dd = "02-02"))))
  expect_match(all_txt, "^\\(and ")
})

test_that("render_generic_value handles bare/logical/value-unit/plf/method-pair/nested shapes", {
  expect_equal(render_generic_value("water_stress_effect", "none"), "(water_stress_effect none)")
  expect_equal(render_generic_value("enable_N_stress", FALSE), "(enable_N_stress false)")
  expect_equal(render_generic_value("clay", list(value = 0.22, unit = "")), "(clay 0.22 [])")
  expect_equal(render_generic_value("dry_bulk_density", list(value = 1.46, unit = "g/cm^3")),
               "(dry_bulk_density 1.46 [g/cm^3])")
  plf_txt <- render_generic_value("Leaf", list(list(0, 1), list(1, 0)))
  expect_equal(plf_txt, "(Leaf (0.0 1.0)(1.0 0.0))")
  method_txt <- render_generic_value("Devel", list("original", list(DSRate2 = 0.025)))
  expect_equal(method_txt, "(Devel original (DSRate2 0.025))")
  expect_equal(render_generic_value("description", "hello"), '(description "hello")')
})

test_that("render_generic_value skips NULL fields", {
  expect_null(render_generic_value("x", NULL))
})

test_that(".render_parent_ref handles 'default' and named parents", {
  expect_equal(.render_parent_ref(NULL), "default")
  expect_equal(.render_parent_ref("default"), "default")
  expect_equal(.render_parent_ref("Spring Barley"), '"Spring Barley"')
})
