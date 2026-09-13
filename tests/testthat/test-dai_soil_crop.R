test_that("render_horizon builds a full defhorizon block", {
  txt <- render_horizon(list(
    name = "Hor_25", texture_class = "FAO3",
    dry_bulk_density = list(value = 1.46, unit = "g/cm^3"),
    clay = list(value = 0.22, unit = ""),
    hydraulic = "Cosby_et_al"
  ))
  expect_match(txt, '^\\(defhorizon "Hor_25" FAO3\\n')
  expect_match(txt, "\\(dry_bulk_density 1\\.46 \\[g/cm\\^3\\]\\)")
  expect_match(txt, "\\(clay 0\\.22 \\[\\]\\)")
  expect_match(txt, "\\(hydraulic Cosby_et_al\\)")
})

test_that("render_horizon requires name and texture_class", {
  expect_error(render_horizon(list(texture_class = "FAO3")), "missing its 'name'")
  expect_error(render_horizon(list(name = "X")), "missing 'texture_class'")
})

test_that("render_column builds Soil/Movement/Groundwater blocks", {
  txt <- render_column(list(
    name = "Soil_Column",
    Soil = list(MaxRootingDepth = list(value = 100, unit = "cm"),
                horizons = list(list(depth = -25, unit = "cm", horizon = "Hor_25"))),
    Movement = list(type = "vertical", zplus = c(-25, -50)),
    Groundwater = "deep"
  ))
  expect_match(txt, '^\\(defcolumn "Soil_Column" default')
  expect_match(txt, "\\(Soil \\(MaxRootingDepth 100 \\[cm\\]\\)")
  expect_match(txt, '\\(horizons \\(-25 \\[cm\\] "Hor_25"\\)\\)')
  expect_match(txt, "\\(Movement vertical")
  expect_match(txt, "\\(Geometry \\(zplus -25 -50\\)\\)")
  expect_match(txt, "\\(Groundwater deep\\)")
})

test_that("render_column's Soil requires MaxRootingDepth and at least one horizon", {
  expect_error(render_column(list(name = "X", Soil = list())), "MaxRootingDepth")
  expect_error(render_column(list(name = "X", Soil = list(MaxRootingDepth = list(value = 100, unit = "cm")))),
               "at least one entry")
})

test_that("render_column's Movement supports a 2D rectangle grid (zplus + xplus)", {
  txt <- render_column(list(
    name = "X",
    Soil = list(MaxRootingDepth = list(value = 100, unit = "cm"),
                horizons = list(list(depth = -25, unit = "cm", horizon = "H"))),
    Movement = list(type = "rectangle", zplus = c(-25, -50), xplus = c(0, 50))
  ))
  expect_match(txt, "\\(Movement rectangle")
  expect_match(txt, "\\(zplus -25 -50\\) \\(xplus 0 50\\)")
})

test_that("render_crop builds a derived defcrop block with a quoted parent", {
  txt <- render_crop(list(name = "Grass Scotland", based_on = "Grass to grain",
                           enable_N_stress = FALSE, water_stress_effect = "none"))
  expect_equal(txt, '(defcrop "Grass Scotland" "Grass to grain"\n  (enable_N_stress false)\n  (water_stress_effect none)\n)')
})

test_that("render_crop defaults based_on to 'default'", {
  txt <- render_crop(list(name = "X"))
  expect_match(txt, '^\\(defcrop "X" default\\)$')
})

test_that("render_crop supports nested method-pair overrides (Devel/Partit)", {
  txt <- render_crop(list(name = "Scot Barley", based_on = "Spring Barley",
                           Devel = list("original", list(DSRate2 = 0.025))))
  expect_match(txt, "\\(Devel original \\(DSRate2 0\\.025\\)\\)")
})
