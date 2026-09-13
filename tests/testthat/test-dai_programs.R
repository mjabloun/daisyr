test_that("render_program builds a base Daisy program with weather/column/time/stop/manager/output", {
  txt <- render_program(list(
    name = "MyProgram", type = "Daisy",
    weather = list(file = "site.dwf"),
    column = "MyColumn",
    time = list(year = 2020, month = 1, day = 1),
    stop = list(year = 2020, month = 12, day = 31),
    manager = list("My activity"),
    output = list("harvest")
  ))
  expect_match(txt, '^\\(defprogram "MyProgram" Daisy\\n')
  expect_match(txt, '\\(weather default "site\\.dwf"\\)')
  expect_match(txt, '\\(column "MyColumn"\\)')
  expect_match(txt, "\\(time 2020 1 1\\)")
  expect_match(txt, "\\(stop 2020 12 31\\)")
  expect_match(txt, '\\(manager activity\\n\\s*"My activity"\\n\\s*\\)')
  expect_match(txt, "\\(output\\n\\s*harvest\\n\\s*\\)")
})

test_that("render_program accepts YYYY-MM-DD strings and an optional hour for time/stop", {
  txt <- render_program(list(name = "X", type = "Daisy", weather = "none", column = "C",
                              time = "2020-01-01", stop = list(year = 2020, month = 1, day = 2, hour = 12),
                              manager = list("A"), output = list("harvest")))
  expect_match(txt, "\\(time 2020 1 1\\)")
  expect_match(txt, "\\(stop 2020 1 2 12\\)")
})

test_that("render_program builds an inheriting program with a quoted parent", {
  txt <- render_program(list(name = "west", based_on = "Common",
                              weather = list(file = "West.dwf"), description = "west runs",
                              log_prefix = "west_"))
  expect_match(txt, '^\\(defprogram "west" "Common"\\n')
  expect_match(txt, '\\(weather default "West\\.dwf"\\)')
  expect_match(txt, '\\(description "west runs"\\)')
  expect_match(txt, '\\(log_prefix "west_"\\)')
})

test_that("render_program builds a batch program", {
  txt <- render_program(list(name = "main", type = "batch", directory = "out", run = list("2022", "2023")))
  expect_match(txt, '^\\(defprogram "main" batch\\n')
  expect_match(txt, '\\(directory "out"\\)')
  expect_match(txt, '\\(batch \\(run "2022"\\)\\)')
  expect_match(txt, '\\(batch \\(run "2023"\\)\\)')
})

test_that("render_program rejects an unrecognised type without based_on", {
  expect_error(render_program(list(name = "X", type = "weird")), "expected 'Daisy' or 'batch'")
})

test_that("render_manager_list expands {use, repeat} shorthand into repeated literal entries", {
  txt <- render_manager_list(list("Fixed", list(use = "Annual", "repeat" = 3)), "  ")
  expect_match(txt, '"Fixed"')
  matches <- gregexpr('"Annual"', txt)[[1]]
  expect_equal(length(matches[matches > 0]), 3)
})

test_that("render_manager_list rejects an empty list and unrecognised entries", {
  expect_error(render_manager_list(list(), "  "), "at least one entry")
  expect_error(render_manager_list(list(list(bogus = 1)), "  "), "Unrecognised manager entry")
})

test_that("render_output_block supports bare keywords and structured log requests", {
  txt <- render_output_block(list(
    "harvest",
    list(name = "Crop Production", when = "daily", print_header = FALSE, print_dimension = FALSE),
    list(name = "Field water", to = list(value = -100, unit = "cm"), when = "daily",
         where = "Daily_FWB.dlf")
  ), "  ")
  expect_match(txt, "harvest")
  expect_match(txt, '\\("Crop Production" \\(when daily\\) \\(print_header false\\) \\(print_dimension false\\)\\)')
  expect_match(txt, '\\("Field water" \\(to -100 \\[cm\\]\\) \\(when daily\\) \\(where "Daily_FWB\\.dlf"\\)\\)')
})

test_that("render_output_block returns NULL for an empty/absent output", {
  expect_null(render_output_block(NULL, "  "))
  expect_null(render_output_block(list(), "  "))
})
