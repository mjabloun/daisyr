## dai_generate.R -- Generate a complete, runnable Daisy `.dai` file from a
## YAML config. This is the top-level orchestrator: it sources the
## per-domain generator files below, handles the top-level fields
## (`directory`, `path`, `libraries`, a file-level `comment`, and the
## `run:` list), and assembles everything in the order a hand-written
## Daisy script uses (confirmed against several real .dai files in this
## project): directory, path, library includes, horizons, columns, crops,
## management activities, programs, then standalone `run` statements.
##
## Two public entry points, for two different starting points:
##   yaml_to_dai(yaml_path, ...)   reads a YAML file, then calls...
##   generate_dai(config)          ...this: the actual generator, which
##                                  works on a plain R list and doesn't
##                                  care whether that list came from a
##                                  YAML file or was built directly in an
##                                  R script -- see generate_dai()'s own
##                                  documentation below for why that
##                                  matters (automating many .dai files
##                                  from data you already have in R,
##                                  e.g. one site per row of a table).
##
## ---------------------------------------------------------------------
## Files
## ---------------------------------------------------------------------
##   dai_helpers.R    shared formatting, the date/wait mini-language,
##                     comments, and the generic nested-block mechanism
##   dai_management.R defaction (management activity) blocks
##   dai_horizons.R    defhorizon blocks
##   dai_columns.R     defcolumn blocks
##   dai_crops.R       defcrop blocks
##   dai_programs.R    defprogram blocks
##   dai_scaffold.R    list_dai_sections() / scaffold_dai_yaml()
##   dai_dataframe.R   records_from_df() -- build config sections from a
##                     data.frame instead of writing YAML/R lists by hand
##   dai_parse_helpers.R  the inverse of dai_helpers.R -- tokenizer, raw
##                     tree parser, and the inverse generic mechanism
##   dai_read.R        parse_dai() / read_dai() -- the inverse direction:
##                     read an existing .dai file back into the config
##                     list this file's generate_dai() consumes
##   dai_generate.R    this file -- top-level assembly + public entry points
##
## ---------------------------------------------------------------------
## YAML schema (top level)
## ---------------------------------------------------------------------
## description: "Demonstrate automatic parameter calibration."
##                                             # -> (description "...") -- a
##                                             # top-level, file-wide
##                                             # description, distinct from
##                                             # a `comment:` (which becomes
##                                             # ";;" line(s), not a Daisy
##                                             # parameter Daisy itself sees)
## directory: "C:/JHI/Projects/.../Daisy"     # -> (directory "...")
## path: ["C:/Program Files/Daisy 5.93/lib", "./"]
##                                             # -> (path "..." "...")
## libraries: ["tillage.dai", "crop.dai"]      # -> consecutive (input file "...")
##                                             #    lines (no blank line between)
## comment: "optional free text"               # -> ;; lines in the file header
##
## daisy_script:                               # unrecognized Daisy, kept verbatim
##   - raw: "(deflog ...)"                     #   after: which known section it
##     after: columns                          #   followed (columns, programs, ...)
##
## horizons: [...]     # see dai_horizons.R
## columns: [...]      # see dai_columns.R
## crops: [...]        # see dai_crops.R
## activities: [...]   # see dai_management.R
## programs: [...]     # see dai_programs.R
##
## run: ["west"]       # which of the `programs` above get a standalone
##                      # (run "NAME") at the end of the file -- distinct
##                      # from a *batch*-type program's own `run:` list
##                      # (dai_programs.R), which is a different field at a
##                      # different nesting level (inside one program, not
##                      # at the top of the file) and doesn't collide with
##                      # this one
##
## Every section is optional -- a YAML file can be just `activities` (as
## before), just `horizons`/`columns`/`crops`, or a complete runnable
## script with everything. Sections that aren't present are omitted from
## the output entirely, not emitted empty.
## ---------------------------------------------------------------------

## -- top-level field rendering -------------------------------------------

#' @keywords internal
render_path_field <- function(paths) {
  if (is.null(paths) || length(paths) == 0) return(NULL)
  lines <- vapply(paths, .q, character(1))
  sprintf("(path %s)", paste(lines, collapse = "\n\t\t"))
}

.dai_section_comment <- function(config, key) {
  comments <- config$section_comments
  if (is.null(comments)) return(NULL)
  comments[[key]]
}

render_library_entry <- function(f) {
  if (is.list(f)) {
    file <- f$file %||% f[["file", exact = TRUE]]
    if (is.null(file)) stop("A libraries entry needs 'file' (or be a bare filename string)")
    .dai_render_with_comment(f$comment, "", sprintf("(input file %s)", .q(file)))
  } else {
    sprintf("(input file %s)", .q(f))
  }
}

render_run_entry <- function(nm) {
  if (is.list(nm)) {
    name <- nm$name %||% nm[["name", exact = TRUE]]
    if (is.null(name)) stop("A run entry needs 'name' (or be a bare program name string)")
    .dai_render_with_comment(nm$comment, "", sprintf("(run %s)", .q(name)))
  } else {
    sprintf("(run %s)", .q(nm))
  }
}

render_daisy_script_entry <- function(entry) {
  if (is.character(entry) && length(entry) == 1) return(entry)
  if (!is.list(entry) || is.null(entry$raw))
    stop("A daisy_script entry needs 'raw' (or be a bare Daisy text string)")
  .dai_render_with_comment(entry$comment, "", entry$raw)
}

.dai_script_after <- function(entry, default = "columns") {
  if (is.character(entry)) return(default)
  entry$after %||% default
}

flush_daisy_script <- function(script, section) {
  if (is.null(script) || length(script) == 0) return(character(0))
  keep <- vapply(script, function(e) identical(.dai_script_after(e), section), logical(1))
  if (!any(keep)) return(character(0))
  paste(vapply(script[keep], render_daisy_script_entry, character(1)), collapse = "\n\n")
}

## -- public entry points -----------------------------------------------------

#' Generate Daisy `.dai` text from a config list
#'
#' The engine behind `yaml_to_dai()`, and a supported entry point in its
#' own right -- not just an internal step. `yaml_to_dai()` does exactly
#' one thing before handing off to this function: read a YAML file into
#' the nested-list shape `generate_dai()` expects (a YAML mapping becomes
#' a named list, a sequence becomes an unnamed list -- the schema
#' documented at the top of this file, and per-section in
#' dai_management.R / dai_horizons.R / dai_columns.R / dai_crops.R /
#' dai_programs.R). Nothing about `generate_dai()` itself cares where
#' that list came from.
#'
#' That matters most once you need many `.dai` files, not one -- e.g.
#' running Daisy across many sites in a country, not just one field.
#' Build `config` directly in an R script instead of writing a YAML file
#' by hand: as a plain nested list, assembled programmatically from a
#' table of site/soil/crop data you already have (see `records_from_df()`
#' in dai_dataframe.R, which turns one data.frame row into one record --
#' e.g. one `defhorizon` or one column's `Soil.horizons` reference), or
#' some mix of both. A loop over the rows of a sites table, each building
#' its own `config` and calling `generate_dai(config, output_path = ...)`,
#' is the natural way to automate a batch of `.dai` files this way.
#'
#' @param config A list in the schema documented at the top of this file
#'   (top-level keys: `directory`, `path`, `libraries`, `comment`,
#'   `horizons`, `columns`, `crops`, `activities`, `programs`, `run`,
#'   `daisy_script`) --
#'   the same shape `yaml::read_yaml()` produces from a YAML file, built
#'   however you like. Every key is optional.
#' @param output_path Character scalar, or `NULL` (default). If given, the
#'   generated text is also written to this path (via `writeLines()`).
#'
#' @return The generated `.dai` text, as a character scalar.
#'
#' @examples
#' \dontrun{
#' ## Built entirely as a plain R list, no YAML file involved:
#' config <- list(
#'   crops = list(list(name = "Scot Barley", based_on = "Spring Barley",
#'                      Devel = list("original", list(DSRate2 = 0.025))))
#' )
#' cat(generate_dai(config))
#'
#' ## One .dai file per row of a sites table:
#' for (i in seq_len(nrow(sites))) {
#'   site <- sites[i, ]
#'   site_config <- list(
#'     columns = list(list(name = site$column, Soil = list(...), Movement = list(...))),
#'     programs = list(list(name = site$name, type = "Daisy",
#'                           weather = list(file = site$weather_file),
#'                           column = site$column, ...)),
#'     run = list(site$name)
#'   )
#'   generate_dai(site_config, output_path = paste0(site$name, ".dai"))
#' }
#' }
#' @export
generate_dai <- function(config, output_path = NULL) {
  header <- c(
    if (!is.null(config$source_file))
      sprintf(";;; Generated by generate_dai() from %s -- edits will be lost on regeneration.",
              config$source_file)
    else
      ";;; Generated by generate_dai() -- edits will be lost on regeneration.",
    sprintf(";;; Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M %Z"))
  )
  comment_lines <- render_comment_lines(config$comment)
  if (!is.null(comment_lines)) header <- c(header, comment_lines)
  header <- paste(header, collapse = "\n")

  top_lines <- c(
    .dai_render_with_comment(.dai_section_comment(config, "description"), "",
      if (!is.null(config$description)) sprintf("(description %s)", .q(config$description)) else NULL),
    .dai_render_with_comment(.dai_section_comment(config, "directory"), "",
      if (!is.null(config$directory)) sprintf("(directory %s)", .q(config$directory)) else NULL),
    .dai_render_with_comment(.dai_section_comment(config, "path"), "",
      render_path_field(config$path))
  )
  top_lines <- top_lines[!vapply(top_lines, is.null, logical(1))]

  libraries <- config$libraries
  include_lines <- if (!is.null(libraries) && length(libraries) > 0)
    paste(vapply(libraries, render_library_entry, character(1)), collapse = "\n")
  else character(0)

  horizon_blocks <- if (!is.null(config$horizons) && length(config$horizons) > 0)
    vapply(config$horizons, render_horizon, character(1)) else character(0)

  column_blocks <- if (!is.null(config$columns) && length(config$columns) > 0)
    vapply(config$columns, render_column, character(1)) else character(0)

  crop_blocks <- if (!is.null(config$crops) && length(config$crops) > 0)
    vapply(config$crops, render_crop, character(1)) else character(0)

  ## `activities` also accepts a bare single-activity config (no wrapping
  ## list) for backward compatibility with the original management-only
  ## schema.
  activities <- config$activities
  if (is.null(activities) && !is.null(config$field_operations)) activities <- list(config)
  activity_blocks <- if (!is.null(activities) && length(activities) > 0)
    vapply(activities, render_activity, character(1)) else character(0)

  program_blocks <- if (!is.null(config$programs) && length(config$programs) > 0)
    vapply(config$programs, render_program, character(1)) else character(0)

  run_lines <- if (!is.null(config$run) && length(config$run) > 0)
    vapply(config$run, render_run_entry, character(1)) else character(0)

  script <- config$daisy_script
  trailing <- render_comment_lines(config$trailing_comment)
  sections <- list(
    header, flush_daisy_script(script, "header"),
    top_lines, flush_daisy_script(script, "top"),
    include_lines, flush_daisy_script(script, "libraries"),
    horizon_blocks, flush_daisy_script(script, "horizons"),
    column_blocks, flush_daisy_script(script, "columns"),
    crop_blocks, flush_daisy_script(script, "crops"),
    activity_blocks, flush_daisy_script(script, "activities"),
    program_blocks, flush_daisy_script(script, "programs"),
    run_lines, flush_daisy_script(script, "run"),
    trailing
  )
  sections <- Filter(function(s) length(s) > 0, sections)
  dai_text <- paste(vapply(sections, paste, character(1), collapse = "\n\n"), collapse = "\n\n")

  if (!is.null(output_path)) {
    writeLines(dai_text, output_path)
    message("Wrote ", output_path)
  }
  dai_text
}

#' Generate a Daisy `.dai` file from a YAML config
#'
#' Reads a config in the YAML schema documented at the top of this file
#' (and, per-section, in dai_management.R / dai_horizons.R / dai_columns.R
#' / dai_crops.R / dai_programs.R) and renders it to Daisy's `.dai`
#' s-expression syntax: `directory`/`path`/library includes, `defhorizon`,
#' `defcolumn`, `defcrop`, `defaction`, and `defprogram` blocks, and
#' standalone `run` statements, in that order. Every section is optional.
#'
#' Building `config` yourself instead of writing YAML -- as a plain R
#' list, or from a data.frame via `records_from_df()` (dai_dataframe.R) --
#' and calling `generate_dai()` directly is an equally supported way to
#' use this generator; `yaml_to_dai()` is just the YAML-file-shaped
#' convenience wrapper around it. See `generate_dai()`'s own
#' documentation.
#'
#' @param yaml_path Character scalar. Path to the input YAML file.
#' @param output_path Character scalar, or `NULL` (default). If given, the
#'   generated text is also written to this path (via `writeLines()`).
#'
#' @return The generated `.dai` text, as a character scalar, invisibly.
#'
#' @examples
#' \dontrun{
#' yaml_to_dai("cauliflower-management.yaml", "cauliflower-man.dai")
#' cat(yaml_to_dai("west-soil-column.yaml"))
#' }
#' @export
yaml_to_dai <- function(yaml_path, output_path = NULL) {
  if (!file.exists(yaml_path)) stop("File not found: ", yaml_path)

  config <- yaml::read_yaml(yaml_path)
  config$source_file <- basename(yaml_path)

  invisible(generate_dai(config, output_path = output_path))
}

## dai_generate.R ends here.
