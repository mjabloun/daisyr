test_that("plow/harrow/seed_bed render bare tillage forms", {
  expect_equal(render_field_operation(list(plow = list())), "  (plowing)")
  expect_equal(render_field_operation(list(harrow = list())), "  (harrowing)")
  expect_equal(render_field_operation(list(seed_bed = list())), "  (seed_bed_preparation)")
})

test_that("plow with a date prepends a wait", {
  txt <- render_field_operation(list(plow = list(date = "04-01")))
  expect_equal(txt, "  (wait_mm_dd 4 1)\n  (plowing)")
})

test_that("generic tillage escape hatch renders name + params", {
  txt <- render_field_operation(list(tillage = list(name = "rolling", date = "04-01",
                                                      params = list(depth = 5))))
  expect_match(txt, "\\(wait_mm_dd 4 1\\)")
  expect_match(txt, "\\(rolling \\(depth 5\\)\\)")
})

test_that("fertilise renders a named mineral fertilizer (quoted, per .q()'s always-quote design), no 'to'/'from'", {
  txt <- render_field_operation(list(fertilise = list(date = "03-05", product = "N25S", weight = 115)))
  expect_equal(txt, '  (wait_mm_dd 3 5)\n  (fertilize ("N25S" (weight 115 [kg N/ha])))')
})

test_that("fertilise renders an organic amendment with volatilization and 'to' as a sibling", {
  txt <- render_field_operation(list(fertilise = list(
    date = "03-05", product = "pig_slurry", weight = 28, unit = "T w.w./ha",
    volatilization = 5, to = -1
  )))
  expect_equal(
    txt,
    '  (wait_mm_dd 3 5)\n  (fertilize ("pig_slurry" (volatilization 5 [%]) (weight 28 [T w.w./ha])) (to -1 [cm]))'
  )
})

test_that("fertilise defaults to 'T w.w./ha' when volatilization is set but unit isn't", {
  txt <- render_field_operation(list(fertilise = list(product = "cattle_slurry", weight = 20, volatilization = 5)))
  expect_match(txt, "\\[T w\\.w\\./ha\\]")
})

test_that("fertilise requires product and weight", {
  expect_error(render_field_operation(list(fertilise = list(product = "N25S"))), "product.*weight")
})

test_that("both 'fertilise' and 'fertilize' spellings are accepted", {
  a <- render_field_operation(list(fertilise = list(product = "N25S", weight = 100)))
  b <- render_field_operation(list(fertilize = list(product = "N25S", weight = 100)))
  expect_equal(a, b)
})

test_that("sow renders a bare crop reference with no extra params", {
  expect_equal(render_field_operation(list(sow = list(crop = "Spring Barley"))),
               '  (sow "Spring Barley")')
})

test_that("sow's seed_bed:true does not leak into a bogus 'seed' parameter (regression)", {
  txt <- render_field_operation(list(sow = list(date = "04-05", seed_bed = TRUE, crop = "Spring Barley")))
  expect_match(txt, "\\(seed_bed_preparation\\)")
  expect_match(txt, '\\(sow "Spring Barley"\\)')
  expect_false(grepl("\\(seed ", txt))
})

test_that("sow's real numeric 'seed' parameter still renders correctly", {
  txt <- render_field_operation(list(sow = list(crop = "X", seed = 2765)))
  expect_match(txt, "\\(seed 2765 \\[kg w\\.w\\./ha\\]\\)")
})

test_that("sow harrow/seed_bed convenience flags insert bare ops before sow", {
  txt <- render_field_operation(list(sow = list(crop = "X", harrow = TRUE, seed_bed = TRUE)))
  lines <- strsplit(txt, "\n")[[1]]
  expect_equal(lines, c("  (harrowing)", "  (seed_bed_preparation)", '  (sow "X")'))
})

test_that("irrigate_until renders (while (wait ..) (repeat ..)), matching real dk-veg-man.dai syntax", {
  txt <- render_field_operation(list(irrigate_until = list(date = "06-15", "repeat" = "irrigation")))
  expect_match(txt, "^  \\(while \\(wait_mm_dd 6 15\\)")
  expect_match(txt, '\\(repeat "irrigation"\\)\\)$')
})

test_that("irrigate_until requires date and repeat", {
  expect_error(render_field_operation(list(irrigate_until = list("repeat" = "irrigation"))), "requires 'date'")
  expect_error(render_field_operation(list(irrigate_until = list(date = "06-15"))), "requires 'repeat'")
})

test_that("irrigate (overhead) renders rate/hours", {
  txt <- render_field_operation(list(irrigate = list(rate = 10, hours = 3)))
  expect_equal(txt, "  (irrigate_overhead 10 [mm/h] (hours 3))")
})

test_that("irrigate (subsoil) requires volume and renders it", {
  expect_error(render_field_operation(list(irrigate = list(type = "subsoil", rate = 5, hours = 2))), "volume")
  txt <- render_field_operation(list(irrigate = list(type = "subsoil", rate = 5, hours = 2, volume = "myvol")))
  expect_match(txt, "\\(irrigate_subsoil 5 \\[mm/h\\] \\(hours 2\\) \\(volume myvol\\)\\)")
})

test_that("harvest renders stub/sorg/stem/leaf and requires at least one", {
  txt <- render_field_operation(list(harvest = list(
    crop = "Spring Barley", stub = list(value = 8, unit = "cm"), stem = 0.70
  )))
  expect_match(txt, '\\(harvest "Spring Barley"')
  expect_match(txt, "\\(stub 8\\.0 \\[cm\\]\\)")
  expect_match(txt, "\\(stem 0\\.7\\)")

  expect_error(render_field_operation(list(harvest = list(crop = "X"))), "none of stub/sorg/stem/leaf")
})

test_that("harvest's bare numeric stub defaults to cm", {
  txt <- render_field_operation(list(harvest = list(crop = "X", stub = 8)))
  expect_match(txt, "\\(stub 8\\.0 \\[cm\\]\\)")
})

test_that("cut renders the same shape as harvest but with the 'cut' token", {
  txt <- render_field_operation(list(cut = list(crop = "Grass Scotland", stub = list(value = 8, unit = "cm"),
                                                  stem = 0.90, leaf = 0.90)))
  expect_match(txt, '^\\s*\\(cut "Grass Scotland"')
})

test_that("harvest with a 'condition' wraps in (if COND (harvest ...)), separate from 'date'", {
  txt <- render_field_operation(list(harvest = list(
    date = "10-01", crop = "X", condition = list(ds = 0.1), stem = 0.0
  )))
  expect_match(txt, "^  \\(wait_mm_dd 10 1\\)")
  expect_match(txt, "\\(if \\(crop_ds_after \"X\" 0\\.1 \\[\\]\\)")
})

test_that("use_activity and raw pass through verbatim", {
  expect_equal(render_field_operation(list(use_activity = "Sowing Grass")), '  "Sowing Grass"')
  expect_equal(render_field_operation(list(raw = "(print_time periodic)")), "  (print_time periodic)")
})

test_that("comment and standalone wait operations render correctly", {
  expect_equal(render_field_operation(list(comment = "1994")), "  ;; 1994")
  expect_equal(render_field_operation(list(wait = list(mm_dd = "04-01"))), "  (wait_mm_dd 4 1)")
  expect_error(render_field_operation(list(wait = NULL)), "requires a value")
})

test_that("an unknown field_operation type errors with a helpful message", {
  expect_error(render_field_operation(list(not_a_real_op = list())), "Unknown field_operation type")
})

test_that("render_activity assembles a full (defaction ...) block", {
  txt <- render_activity(list(
    name = "Simple",
    field_operations = list(list(plow = list(date = "04-01")), list(sow = list(crop = "X")))
  ))
  expect_match(txt, '^\\(defaction "Simple" activity\\n')
  expect_match(txt, "\\(plowing\\)")
  expect_match(txt, '\\(sow "X"\\)')
  expect_match(txt, "\\)\\s*$")
})

test_that("render_activity requires name and non-empty field_operations", {
  expect_error(render_activity(list(field_operations = list())), "missing its 'name'")
  expect_error(render_activity(list(name = "X")), "no field_operations")
})

test_that("render_activity prepends a comment line when given", {
  txt <- render_activity(list(name = "X", comment = "note",
                               field_operations = list(list(plow = list()))))
  expect_match(txt, "^;; note\\n")
})
