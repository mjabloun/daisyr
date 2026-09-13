test_that("parse_dai(generate_dai(config)) round-trips a complete script", {
  west_txt <- yaml_to_dai(test_path("fixtures", "dai_yaml", "west-soil-column.yaml"))
  config <- parse_dai(west_txt)
  expect_equal(generate_dai(config), west_txt)
})

test_that("parse_dai(generate_dai(config)) round-trips sbarley/cauliflower management", {
  sbarley_txt <- yaml_to_dai(test_path("fixtures", "dai_yaml", "sbarley-management.yaml"))
  expect_equal(generate_dai(parse_dai(sbarley_txt)), sbarley_txt)

  cauliflower_txt <- yaml_to_dai(test_path("fixtures", "dai_yaml", "cauliflower-management.yaml"))
  expect_equal(generate_dai(parse_dai(cauliflower_txt)), cauliflower_txt)
})

test_that("parse_dai recovers the documented top-level sections", {
  txt <- yaml_to_dai(test_path("fixtures", "dai_yaml", "west-soil-column.yaml"))
  config <- parse_dai(txt)
  expect_true(all(c("horizons", "columns", "crops", "activities", "programs", "run") %in% names(config)))
  expect_equal(config$horizons[[1]]$name, "Hor_25")
  expect_equal(config$columns[[1]]$name, "Soil_Column")
})

test_that("parse_dai recovers config$source_file from generate_dai()'s header comment", {
  txt <- yaml_to_dai(test_path("fixtures", "dai_yaml", "sbarley-management.yaml"))
  config <- parse_dai(txt)
  expect_equal(config$source_file, "sbarley-management.yaml")
})

test_that("parse_dai recovers a file-level comment as config$comment", {
  txt <- generate_dai(list(comment = "line one\nline two", run = list("X")))
  config <- parse_dai(txt)
  expect_equal(config$comment, "line one\nline two")
})

test_that("parse_dai recovers a block's own comment field", {
  ## Two horizons: the first horizon's comment is unambiguously its own
  ## (only a *leading* comment run at the very start of the file is
  ## treated as the file-level `comment` -- see parse_dai()'s docs).
  txt <- generate_dai(list(horizons = list(
    list(name = "H0", texture_class = "FAO3"),
    list(name = "H1", texture_class = "FAO3", comment = "note")
  )))
  config <- parse_dai(txt)
  expect_equal(config$horizons[[2]]$comment, "note")
})

test_that("parse_dai keeps unrecognized top-level Daisy as daisy_script and generate_dai writes it back", {
  text <- paste(
    '(directory "Output")',
    '(deflog "X" default)',
    "(defchemical foo)",
    '(run "Y")',
    sep = "\n"
  )
  config <- parse_dai(text)
  expect_equal(length(config$daisy_script), 2)
  expect_match(config$daisy_script[[1]]$raw, "deflog")
  expect_equal(config$daisy_script[[1]]$after, "top")
  expect_match(config$daisy_script[[2]]$raw, "defchemical")
  regenerated <- generate_dai(config)
  expect_match(regenerated, '\\(deflog "X" default\\)')
  expect_match(regenerated, "\\(defchemical foo\\)")
  expect_match(regenerated, '\\(run "Y"\\)')
  ## Placement: after directory (top), before run.
  expect_true(regexpr('(deflog "X" default)', regenerated, fixed = TRUE) <
                regexpr('(run "Y")', regenerated, fixed = TRUE))
})

test_that("parse_dai keeps an unrecognized defprogram field as daisy_script on that program", {
  text <- paste(
    '(defprogram MySim Daisy',
    '  (column Andeby)',
    '  (print_time false)',
    '  (manager "SBarley w. MF")',
    ')',
    sep = "\n"
  )
  config <- parse_dai(text)
  expect_equal(config$programs[[1]]$daisy_script[[1]]$raw, "(print_time false)")
  expect_equal(config$programs[[1]]$daisy_script[[1]]$after, "column")
  regenerated <- generate_dai(config)
  expect_match(regenerated, "\\(print_time false\\)")
})

test_that("parse_dai accepts a built-in output log with fields, e.g. (harvest (print_header false) ...)", {
  text <- paste(
    '(defprogram MySim Daisy',
    '  (output (harvest (print_header false) (print_dimension false)))',
    ')',
    sep = "\n"
  )
  config <- parse_dai(text)
  expect_equal(config$programs[[1]]$output[[1]]$name, "harvest")
  expect_false(config$programs[[1]]$output[[1]]$print_header)
  expect_match(generate_dai(config), '\\("harvest" \\(print_header false\\)')
})

test_that("parse_dai still errors on a malformed recognized construct", {
  expect_error(parse_dai("(defhorizon Ap)"), "defhorizon needs a name and a bare texture_class")
})

test_that("parse_dai collects every malformed recognized construct into one error", {
  text <- paste(
    '(deflog "X" default)',
    "(defhorizon Ap)",
    "(defcolumn OnlyName)",
    sep = "\n"
  )
  err <- tryCatch(parse_dai(text), error = function(e) e)
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "found 2 problem")
  expect_match(conditionMessage(err), "construct #2 \\(defhorizon\\)")
  expect_match(conditionMessage(err), "construct #3 \\(defcolumn\\)")
})

test_that("parse_dai reports the source line number of each problem", {
  text <- paste(
    '(directory "Output")',   # line 1
    "",                       # line 2
    '(deflog "X" default)',   # line 3 -- kept as daisy_script
    "",                       # line 4
    "(defhorizon Ap)",        # line 5 -- missing texture_class
    sep = "\n"
  )
  err <- tryCatch(parse_dai(text), error = function(e) e)
  expect_match(conditionMessage(err), "line 5, construct #3 \\(defhorizon\\)")
})

test_that("parse_dai gives a clear, actionable error when passed a file path instead of .dai text", {
  tmp <- tempfile(fileext = ".dai")
  on.exit(unlink(tmp))
  writeLines('(run "X")', tmp)
  expect_error(parse_dai(tmp), "expects the \\*text\\*.*read_dai")
})

test_that("read_dai is the file-reading counterpart to parse_dai, matching yaml_to_dai/generate_dai", {
  tmp <- tempfile(fileext = ".dai")
  on.exit(unlink(tmp))
  txt <- yaml_to_dai(test_path("fixtures", "dai_yaml", "sbarley-management.yaml"), output_path = tmp)
  expect_equal(read_dai(tmp), parse_dai(txt))
})

test_that("read_dai reads a .dai file from disk and round-trips through generate_dai", {
  tmp <- tempfile(fileext = ".dai")
  on.exit(unlink(tmp))
  txt <- yaml_to_dai(test_path("fixtures", "dai_yaml", "west-soil-column.yaml"), output_path = tmp)
  config <- read_dai(tmp)
  expect_equal(generate_dai(config), txt)
})

test_that("read_dai errors clearly on a missing file", {
  expect_error(read_dai("does-not-exist.dai"), "not found")
})

test_that("read_dai can also write the parsed config out as YAML", {
  tmp_dai <- tempfile(fileext = ".dai")
  tmp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(c(tmp_dai, tmp_yaml)))
  yaml_to_dai(test_path("fixtures", "dai_yaml", "sbarley-management.yaml"), output_path = tmp_dai)
  config <- read_dai(tmp_dai, yaml_path = tmp_yaml)
  expect_true(file.exists(tmp_yaml))
  reloaded <- yaml::read_yaml(tmp_yaml)
  expect_equal(reloaded$activities[[1]]$name, config$activities[[1]]$name)
})

test_that("editing a parsed config and regenerating changes only what was edited", {
  west_txt <- yaml_to_dai(test_path("fixtures", "dai_yaml", "west-soil-column.yaml"))
  config <- parse_dai(west_txt)
  prog_idx <- which(vapply(config$programs, function(p) identical(p$name, "west"), logical(1)))
  config$programs[[prog_idx]]$log_prefix <- "revised_"
  regenerated <- generate_dai(config)
  expect_match(regenerated, '\\(log_prefix "revised_"\\)')
  expect_false(identical(regenerated, west_txt))
})

## -- reading real hand-written .dai files (beyond generate_dai()'s own output) --

test_that("parse_dai recovers a top-level (description ...) and generate_dai() round-trips it", {
  txt <- generate_dai(list(description = "Demonstrate automatic parameter calibration.", run = list("X")))
  expect_match(txt, '\\(description "Demonstrate automatic parameter calibration\\."\\)')
  config <- parse_dai(txt)
  expect_equal(config$description, "Demonstrate automatic parameter calibration.")
  expect_equal(generate_dai(config), txt)
})

test_that("parse_dai accepts bare (unquoted) names for defhorizon/defcolumn/defcrop/defprogram/defaction", {
  text <- paste(
    "(defhorizon Ap FAO3 (clay 0.1))",
    "(defcolumn Andeby default",
    '  (Soil (MaxRootingDepth 60 [cm]) (horizons (-20 Ap)))',
    ")",
    '(defcrop Barley default (enable_N_stress false))',
    "(defaction MyAction activity (plowing))",
    "(defprogram MySim Daisy (column Andeby))",
    sep = "\n"
  )
  config <- parse_dai(text)
  expect_equal(config$horizons[[1]]$name, "Ap")
  expect_equal(config$columns[[1]]$name, "Andeby")
  expect_equal(config$crops[[1]]$name, "Barley")
  expect_equal(config$activities[[1]]$name, "MyAction")
  expect_equal(config$programs[[1]]$name, "MySim")
  expect_equal(config$programs[[1]]$column, "Andeby")
})

test_that("parse_dai accepts a bare product name in fertilize and a bare use_activity reference", {
  text <- paste(
    '(defaction A activity',
    "  (fertilize (KAS (weight 100 [kg N/ha])))",
    "  MyOtherActivity",
    ")",
    sep = "\n"
  )
  config <- parse_dai(text)
  ops <- config$activities[[1]]$field_operations
  expect_equal(ops[[1]]$fertilise$product, "KAS")
  expect_equal(ops[[2]]$use_activity, "MyOtherActivity")
})

test_that("parse_dai consumes Daisy's optional positional description string on defhorizon/defcolumn/defcrop/defprogram", {
  text <- paste(
    '(defhorizon "Ap" FAO3 "Andeby top soil." (clay 0.1))',
    '(defcolumn "Andeby" default "Data from the Andeby farm."',
    "  (Soil (MaxRootingDepth 60 [cm]) (horizons (-20 \"Ap\"))))",
    '(defcrop "Barley" default "A barley variety." (enable_N_stress false))',
    '(defprogram "MySim" Daisy "A demo simulation." (column "Andeby"))',
    sep = "\n"
  )
  config <- parse_dai(text)
  expect_equal(config$horizons[[1]]$description, "Andeby top soil.")
  expect_equal(config$columns[[1]]$description, "Data from the Andeby farm.")
  expect_equal(config$crops[[1]]$description, "A barley variety.")
  expect_equal(config$programs[[1]]$description, "A demo simulation.")

  ## And regenerating still produces valid, self-consistent .dai text
  ## (description is rendered as an explicit (description "...") field
  ## rather than reproducing its original positional placement).
  regenerated <- generate_dai(config)
  expect_match(regenerated, '\\(description "Andeby top soil\\."\\)')
  expect_equal(generate_dai(parse_dai(regenerated)), regenerated)
})

test_that("parse_dai accepts Soil's more flexible real-world shape: any field order, optional per-point unit, extra fields", {
  text <- paste(
    '(defcolumn "Andeby" default',
    '  (Soil (horizons (-20 "Ap") (-2.5 [m] "C"))',
    "        (border -1 [m])",
    "        (MaxRootingDepth 60.0 [cm]))",
    ")",
    sep = "\n"
  )
  config <- parse_dai(text)
  soil <- config$columns[[1]]$Soil
  expect_equal(soil$MaxRootingDepth, list(value = 60, unit = "cm"))
  expect_equal(soil$horizons[[1]], list(depth = -20, horizon = "Ap"))
  expect_equal(soil$horizons[[2]], list(depth = -2.5, unit = "m", horizon = "C"))
  expect_equal(soil$border, list(value = -1, unit = "m"))

  ## The extra 'border' field isn't silently dropped on regeneration.
  regenerated <- generate_dai(config)
  expect_match(regenerated, "\\(border -1 \\[m\\]\\)")
  expect_equal(generate_dai(parse_dai(regenerated)), regenerated)
})

test_that("a program's manager can be a single bare/quoted name sourced from a library, not just an activity list", {
  txt <- generate_dai(list(programs = list(list(
    name = "MySim", type = "Daisy", manager = "SBarley w. MF"
  ))))
  expect_match(txt, '\\(manager "SBarley w\\. MF"\\)')
  expect_false(grepl("manager activity", txt))

  config <- parse_dai(txt)
  expect_equal(config$programs[[1]]$manager, "SBarley w. MF")
  expect_equal(generate_dai(config), txt)
})

test_that("parse_dai keeps an interleaved comment inside a defprogram body", {
  text <- paste(
    '(defprogram MySim Daisy',
    '  (column Andeby)',
    '  ;; note about output',
    '  (output ("Sample water content" (print_header false)))',
    ')',
    sep = "\n"
  )
  config <- parse_dai(text)
  expect_equal(config$programs[[1]]$column, "Andeby")
  expect_equal(config$programs[[1]]$output[[1]]$name, "Sample water content")
  expect_equal(config$programs[[1]]$field_comments$output, "note about output")
  expect_match(generate_dai(config), ";; note about output")
})

test_that("parse_dai keeps an inline comment interleaved inside a generic parameter block", {
  text <- paste(
    "(defhorizon Ap FAO3",
    "  (clay 0.1)",
    "  ; a note about normalize",
    "  (normalize true)",
    ")",
    sep = "\n"
  )
  config <- parse_dai(text)
  expect_equal(config$horizons[[1]]$clay, 0.1)
  expect_equal(config$horizons[[1]]$normalize, TRUE)
  expect_equal(config$horizons[[1]]$field_comments$normalize, "a note about normalize")
  expect_match(generate_dai(config), ";; a note about normalize")
})

test_that("parse_dai accepts a bare program name in (run ...)", {
  config <- parse_dai("(run MySim)")
  expect_equal(config$run, list("MySim"))
})

test_that("parse_dai keeps a trailing file-end comment", {
  text <- paste('(run "X")', "; test-minimize.dai ends here.", sep = "\n")
  config <- parse_dai(text)
  expect_equal(config$run, list("X"))
  expect_equal(config$trailing_comment, "test-minimize.dai ends here.")
  expect_match(generate_dai(config), ";; test-minimize.dai ends here\\.")
})

test_that("parse_dai keeps comments on libraries and run, and end-of-line field comments", {
  text <- paste(
    '(directory "Output")',
    ";; Use standard parameterizations.",
    '(input file "crop.dai")',
    '(input file "tillage.dai")',
    ";(input file \"skip.dai\")",
    '(input file "log.dai")',
    "(defhorizon Ap FAO3",
    "  (clay 0.1)",
    "  (normalize true) ;eol note",
    ")",
    ";; Or optimize it.",
    "(run MySim)",
    ";;; file ends here.",
    sep = "\n"
  )
  config <- parse_dai(text)
  expect_equal(config$libraries[[1]]$comment, "Use standard parameterizations.")
  expect_equal(config$libraries[[1]]$file, "crop.dai")
  expect_equal(config$libraries[[2]], "tillage.dai")
  expect_equal(config$libraries[[3]]$file, "log.dai")
  expect_match(config$libraries[[3]]$comment, "input file")
  expect_equal(config$horizons[[1]]$field_comments$normalize, "eol note")
  expect_equal(config$run[[1]]$comment, "Or optimize it.")
  expect_equal(config$run[[1]]$name, "MySim")
  expect_equal(config$trailing_comment, "file ends here.")
  regenerated <- generate_dai(config)
  expect_match(regenerated, ";; Use standard parameterizations\\.")
  expect_match(regenerated, ";; Or optimize it\\.")
  expect_match(regenerated, ";; file ends here\\.")
})

test_that(".dai_tokenize/.dai_parse_tree attach the correct 1-based source line to each node", {
  text <- paste(
    '(directory "Output")',  # line 1
    "",                      # line 2
    "(defhorizon Ap FAO3",   # line 3
    "  (clay 0.1))",         # line 4
    sep = "\n"
  )
  nodes <- .dai_parse_raw(text)
  expect_equal(nodes[[1]]$line, 1L)
  expect_equal(nodes[[2]]$line, 3L)
  ## children: "defhorizon", "Ap", "FAO3" (all line 3), then the inner
  ## (clay 0.1) sub-form, which starts on line 4.
  expect_equal(nodes[[2]]$children[[3]]$line, 3L)
  expect_equal(nodes[[2]]$children[[4]]$line, 4L)
})
