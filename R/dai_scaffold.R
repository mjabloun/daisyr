## dai_scaffold.R -- discover and scaffold the yaml_to_dai() config schema,
## so a new user doesn't have to start from a blank file (or read the
## other dai_*.R files' header comments) to find out what's available.
##
## Two functions:
##   list_dai_sections()               -- what top-level sections exist
##   scaffold_dai_yaml(sections, ...)  -- generate a starter YAML file
##
## `list_dai_sections()`'s section names are the single source of truth --
## `scaffold_dai_yaml()`'s default `sections` argument is built from the
## same vector, so the two can never drift out of sync.
##
## ---------------------------------------------------------------------

## -- the section catalogue -----------------------------------------------

#' @keywords internal
.DAI_SECTIONS <- c(
  directory  = "Top-level scratch/output directory -> (directory \"...\")",
  path       = "Daisy's input-file search path -> (path \"...\" ...)",
  libraries  = "Library (input file ...) includes",
  comment    = "A file-level header comment (Daisy ;; lines)",
  horizons   = "Soil horizons (defhorizon)",
  columns    = "Soil columns (defcolumn)",
  crops      = "Derived crops (defcrop)",
  activities = "Management activities (defaction)",
  programs   = "Simulation programs (defprogram)",
  run        = "Which programs get a standalone (run \"NAME\")"
)

#' List the top-level YAML sections `yaml_to_dai()` understands
#'
#' A small lookup table -- one row per top-level config key -- so the
#' schema is discoverable without reading every dai_*.R file's header
#' comments. The `section` column is exactly what `scaffold_dai_yaml()`'s
#' `sections` argument accepts.
#'
#' @return A data frame with columns `section` and `description`.
#'
#' @examples
#' list_dai_sections()
#' scaffold_dai_yaml(sections = c("activities", "programs", "run"))
#' @export
list_dai_sections <- function() {
  data.frame(
    section = names(.DAI_SECTIONS),
    description = unname(.DAI_SECTIONS),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

## -- per-section scaffold text ---------------------------------------------
##
## Placeholder names are deliberately cross-referenced across sections
## ("MyHorizon" -> "MyColumn" -> "MyCrop" -> "My activity" -> "MyProgram"
## -> the top-level run:), so the scaffold runs through yaml_to_dai()
## unmodified and produces valid (placeholder-valued) .dai output -- a
## working starting point, not just a shape to copy fragments from. When a
## section a placeholder would normally reference isn't itself selected,
## the reference stays but picks up a plain YAML `#` comment explaining it
## needs to point somewhere real, rather than a silent dangling reference.

#' @keywords internal
.scaffold_directory <- function() {
  paste(c(
    '# --- directory -----------------------------------------------------------',
    'directory: "C:/path/to/your/Daisy/project"   # top-level scratch/output directory'
  ), collapse = "\n")
}

#' @keywords internal
.scaffold_path <- function() {
  paste(c(
    '# --- path ------------------------------------------------------------------',
    'path:',
    '  - "C:/Program Files/Daisy X.XX/lib"   # wherever your Daisy install keeps its library .dai files',
    '  - "./"                                # this directory'
  ), collapse = "\n")
}

#' @keywords internal
.scaffold_libraries <- function() {
  paste(c(
    '# --- libraries -------------------------------------------------------------',
    'libraries:',
    '  - "tillage.dai"',
    '  - "crop.dai"',
    '  # add any other library .dai files your activities/crops/horizons reference'
  ), collapse = "\n")
}

#' @keywords internal
.scaffold_comment <- function() {
  paste(c(
    '# --- comment (a Daisy header comment, not this YAML comment) ---------------',
    '# comment: "Optional free text -> a Daisy header comment in the generated file"'
  ), collapse = "\n")
}

#' @keywords internal
.scaffold_horizons <- function() {
  paste(c(
    '# --- horizons ----------------------------------------------------------------',
    'horizons:',
    '  - name: "MyHorizon"',
    '    texture_class: FAO3   # e.g. FAO3, ISSS4, USDA3',
    '    dry_bulk_density: {value: 1.5, unit: "g/cm^3"}',
    '    clay: {value: 0.25, unit: ""}',
    '    silt: {value: 0.25, unit: ""}',
    '    sand: {value: 0.50, unit: ""}',
    '    humus: {value: 0.02, unit: ""}',
    '    hydraulic: Cosby_et_al'
  ), collapse = "\n")
}

#' @keywords internal
.scaffold_columns <- function(has_horizons) {
  lines <- c(
    '# --- columns -----------------------------------------------------------------',
    'columns:',
    '  - name: "MyColumn"',
    '    # based_on: default   # or another column to derive from',
    '    Soil:',
    '      MaxRootingDepth: {value: 100, unit: "cm"}',
    '      horizons:'
  )
  if (!has_horizons) {
    lines <- c(lines,
      '        # NOTE: "MyHorizon" below isn\'t defined in this file -- point it',
      '        # at a horizon from a library, or add back a \'horizons:\' section.')
  }
  lines <- c(lines,
    '        - {depth: -25, unit: "cm", horizon: "MyHorizon"}',
    '    Movement:',
    '      type: vertical   # or "rectangle" for a 2D grid (zplus + xplus)',
    '      zplus: [-25, -50, -100]',
    '    Groundwater: deep'
  )
  paste(lines, collapse = "\n")
}

#' @keywords internal
.scaffold_crops <- function() {
  paste(c(
    '# --- crops -------------------------------------------------------------------',
    'crops:',
    '  - name: "MyCrop"',
    '    based_on: "Parent crop name"   # a crop already defined in a library, or "default"',
    '    # enable_N_stress: false',
    '    # water_stress_effect: none'
  ), collapse = "\n")
}

#' @keywords internal
.scaffold_activities <- function(has_crops) {
  lines <- c(
    '# --- activities --------------------------------------------------------------',
    'activities:',
    '  - name: "My activity"',
    '    field_operations:',
    '      - plow: {date: "04-01"}'
  )
  if (!has_crops) {
    lines <- c(lines,
      '      # NOTE: "MyCrop" below isn\'t defined in this file -- point it at',
      '      # a crop from a library, or add back a \'crops:\' section.')
  }
  lines <- c(lines,
    '      - sow: {date: "04-15", seed_bed: true, crop: "MyCrop"}',
    '      - harvest: {date: "09-01", crop: "MyCrop", stub: {value: 8, unit: "cm"}}'
  )
  paste(lines, collapse = "\n")
}

#' @keywords internal
.scaffold_programs <- function(has_columns, has_activities) {
  lines <- c(
    '# --- programs ------------------------------------------------------------------',
    'programs:',
    '  - name: "MyProgram"',
    '    type: Daisy',
    '    weather: {file: "your-weather-file.dwf"}'
  )
  if (!has_columns) {
    lines <- c(lines,
      '    # NOTE: "MyColumn" below isn\'t defined in this file -- point it at',
      '    # a column from a library, or add back a \'columns:\' section.')
  }
  lines <- c(lines,
    '    column: "MyColumn"',
    '    time: {year: 2020, month: 1, day: 1}',
    '    stop: {year: 2020, month: 12, day: 31}'
  )
  if (!has_activities) {
    lines <- c(lines,
      '    # NOTE: "My activity" below isn\'t defined in this file -- point it',
      '    # at an activity from a library, or add back an \'activities:\' section.')
  }
  lines <- c(lines,
    '    manager:',
    '      - "My activity"',
    '    output:',
    '      - harvest'
  )
  paste(lines, collapse = "\n")
}

#' @keywords internal
.scaffold_run <- function(has_programs) {
  lines <- c(
    '# --- run -----------------------------------------------------------------------',
    'run:'
  )
  if (!has_programs) {
    lines <- c(lines,
      '  # NOTE: "MyProgram" below isn\'t defined in this file -- name a',
      '  # program from a library, or add back a \'programs:\' section.')
  }
  lines <- c(lines, '  - "MyProgram"')
  paste(lines, collapse = "\n")
}

## -- public entry point -----------------------------------------------------

#' Scaffold a starter YAML config for `yaml_to_dai()`
#'
#' Generates a YAML file containing just the requested top-level sections
#' (see `list_dai_sections()`), always in the same order `yaml_to_dai()`
#' itself emits them in, regardless of the order given here. Placeholder
#' values are cross-referenced across sections, so the scaffold runs
#' through `yaml_to_dai()` unmodified and produces valid (placeholder-
#' valued) `.dai` output -- replace the placeholders with your own values,
#' or delete any section you don't need.
#'
#' @param sections Character vector of section names from
#'   `list_dai_sections()$section`. Defaults to every section.
#' @param output_path Character scalar, or `NULL` (default). If given, the
#'   generated YAML is also written to this path (via `writeLines()`).
#'
#' @return The generated YAML text, as a character scalar, invisibly.
#'
#' @examples
#' \dontrun{
#' scaffold_dai_yaml(output_path = "new-config.yaml")
#' scaffold_dai_yaml(sections = c("activities", "programs", "run"))
#' }
#' @export
scaffold_dai_yaml <- function(sections = names(.DAI_SECTIONS), output_path = NULL) {
  unknown <- setdiff(sections, names(.DAI_SECTIONS))
  if (length(unknown) > 0)
    stop("Unknown section(s): ", paste(unknown, collapse = ", "),
         ". See list_dai_sections() for valid names.")
  sel <- names(.DAI_SECTIONS)[names(.DAI_SECTIONS) %in% sections]  # canonical order
  if (length(sel) == 0)
    stop("No sections selected. See list_dai_sections() for valid names.")

  has <- function(x) x %in% sel

  chunks <- character(0)
  if (has("directory"))  chunks <- c(chunks, .scaffold_directory())
  if (has("path"))       chunks <- c(chunks, .scaffold_path())
  if (has("libraries"))  chunks <- c(chunks, .scaffold_libraries())
  if (has("comment"))    chunks <- c(chunks, .scaffold_comment())
  if (has("horizons"))   chunks <- c(chunks, .scaffold_horizons())
  if (has("columns"))    chunks <- c(chunks, .scaffold_columns(has("horizons")))
  if (has("crops"))      chunks <- c(chunks, .scaffold_crops())
  if (has("activities")) chunks <- c(chunks, .scaffold_activities(has("crops")))
  if (has("programs"))   chunks <- c(chunks, .scaffold_programs(has("columns"), has("activities")))
  if (has("run"))        chunks <- c(chunks, .scaffold_run(has("programs")))

  header <- paste(c(
    "# yaml_to_dai() scaffold -- generated by scaffold_dai_yaml().",
    "# Replace the placeholder values below with your own; delete any",
    "# section you don't need (every section is optional). Placeholder",
    "# names are cross-referenced across sections (MyHorizon -> MyColumn",
    "# -> MyCrop -> \"My activity\" -> MyProgram -> the top-level run:), so",
    "# this file runs through yaml_to_dai() unmodified and produces valid",
    "# (placeholder-valued) .dai output -- a starting point you edit, not",
    "# just a shape to copy from."
  ), collapse = "\n")

  text <- paste(c(header, chunks), collapse = "\n\n")

  if (!is.null(output_path)) {
    writeLines(text, output_path)
    message("Wrote ", output_path)
  }
  invisible(text)
}

## dai_scaffold.R ends here.
