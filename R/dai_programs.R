## dai_programs.R -- Generate Daisy `defprogram` blocks from YAML.
##
## Programs form a small hierarchy, not a flat list of independent
## simulations:
##   - a BASE program (`type: Daisy`) with its own `weather`/`column`/
##     `time`/`stop`/`manager`/`output` -- the full simulation setup
##   - an INHERITING program (`based_on: "OtherProgramName"` instead of
##     `type: Daisy`) that names another program and overrides only
##     specific fields (typically `weather`, `description`, `log_prefix`)
##   - a BATCH program (`type: batch`) that runs a sequence of other
##     programs instead of simulating anything itself
##
## Which of the programs defined in a file actually get run is controlled
## separately, at the top level of the YAML config (a `run:` list of
## program names, each emitted as a standalone `(run "NAME")` at the end
## of the file) -- see dai_generate.R. A batch program's own `run:` list
## (naming the programs *it* runs, each wrapped as `(batch (run "NAME"))`)
## is a different thing at a different nesting level and doesn't collide
## with the top-level one.
##
## Requires dai_helpers.R to already be sourced (`%||%`, `.q`, `.bare`,
## `.fmt_num`, `render_generic_value`, `.render_parent_ref`,
## `.with_comment`).
##
## ---------------------------------------------------------------------
## YAML schema
## ---------------------------------------------------------------------
## programs:                         # one or more simulation programs
##   - name: "Common"
##     type: Daisy                   # a base program (or omit `type` and
##                                    # set `based_on` instead, for an
##                                    # inheriting program -- see below)
##     comment: "optional free text" # -> a ;; comment line before (defprogram ...)
##
##     weather: none                 # -> (weather none), when this program
##                                    # is never run directly, only
##                                    # inherited from -- or:
##     # weather: {file: "West.dwf"}          -> (weather default "West.dwf")
##     # weather: {method: default, file: ..} -> explicit method
##
##     column: "Soil_Column"         # -> (column "Soil_Column")
##
##     time: {year: 1994, month: 1, day: 1}     # -> (time 1994 1 1)
##     stop: {year: 1997, month: 12, day: 31}   # -> (stop 1997 12 31)
##     # time/stop also accept a plain "YYYY-MM-DD" string; `hour` is
##     # optional on either, matching Daisy itself (confirmed: hour need
##     # not be given)
##
##     manager: "SBarley w. MF"       # -> (manager "SBarley w. MF") -- a
##                                    # single manager sourced directly from
##                                    # a library's own definition, no
##                                    # `activity` submodel wrapper -- or:
##     # manager:                      -> (manager activity ...), in order
##     #   - "Grass for cutting"                          # a bare activity name
##     #   - {use: "UK Grass Annual Cycle", repeat: 20}    # repeated N times --
##     #                                # Daisy has no repeat construct of its
##     #                                # own, this expands to the name written
##     #                                # out 20 times, exactly as a hand-written
##     #                                # 20-year rotation would
##
##     output:                       # -> (output ...), requests existing
##                                    # log types only (no custom `deflog`
##                                    # definitions -- out of scope)
##       - harvest                              # a bare built-in log keyword
##       - name: "Crop Production"
##         when: daily
##         print_header: false
##         print_dimension: false
##       - name: "Field water"
##         to: {value: -100, unit: "cm"}
##         when: daily
##         where: "Daily_FWB.dlf"
##         print_header: false
##         print_dimension: false
##
##     description: "optional"       # -> (description "...")
##     log_prefix: "west_"           # -> (log_prefix "west_")
##
##   - name: "west"                  # an INHERITING program
##     based_on: "Common"            # -> (defprogram "west" "Common" ...)
##     weather: {file: "West.dwf"}
##     description: "west runs"
##     log_prefix: "west_"
##
##   - name: "main"                  # a BATCH program
##     type: batch
##     directory: "..."              # this batch's own output directory
##     run: ["2022", "2023"]         # -> (batch (run "2022"))(batch (run "2023"))
##
## Which of these get a standalone `(run "NAME")` in the output file is set
## at the top level of the config, not here -- see dai_generate.R's `run:`.
## ---------------------------------------------------------------------

#' @keywords internal
render_weather <- function(w, indent) {
  if (is.null(w)) return(NULL)
  if (is.character(w)) return(sprintf("%s(weather %s)", indent, .bare(w)))
  method <- w$method %||% "default"
  file_txt <- if (!is.null(w$file)) sprintf(" %s", .q(w$file)) else ""
  sprintf("%s(weather %s%s)", indent, .bare(method), file_txt)
}

#' Render a `time`/`stop` field: `{year, month, day, hour}` (hour
#' optional, matching Daisy itself) or a plain `"YYYY-MM-DD"` string.
#' @keywords internal
render_time_field <- function(key, spec, indent) {
  if (is.null(spec)) return(NULL)
  if (is.character(spec)) {
    parts <- as.integer(strsplit(spec, "-", fixed = TRUE)[[1]])
    if (length(parts) != 3) stop("'", key, "' string must look like 'YYYY-MM-DD', got: '", spec, "'")
    spec <- list(year = parts[1], month = parts[2], day = parts[3])
  }
  if (is.null(spec$year) || is.null(spec$month) || is.null(spec$day))
    stop("'", key, "' requires 'year', 'month', and 'day'")
  args <- c(spec$year, spec$month, spec$day)
  if (!is.null(spec$hour)) args <- c(args, spec$hour)
  sprintf("%s(%s %s)", indent, key, paste(vapply(args, .fmt_num, character(1)), collapse = " "))
}

#' Render a `manager:` list to `(manager activity ...)`, expanding any
#' `{use: "Name", repeat: N}` shorthand into N literal repeated entries --
#' Daisy itself has no repeat construct; this is purely a YAML-side
#' convenience for what would otherwise be N hand-written identical lines.
#' @keywords internal
render_manager_list <- function(manager, indent) {
  if (is.null(manager) || length(manager) == 0)
    stop("A program's 'manager' list must have at least one entry")
  names_out <- unlist(lapply(manager, function(entry) {
    if (is.character(entry)) return(entry)
    if (!is.null(entry$use)) {
      n <- entry[["repeat"]] %||% 1
      return(rep(entry$use, n))
    }
    stop("Unrecognised manager entry, expected a bare activity name or ",
         "{use: \"Name\", repeat: N}, got: ", paste(names(entry), collapse = ", "))
  }))
  lines <- vapply(names_out, .q, character(1))
  cont <- paste0(indent, "  ")
  sprintf("%s(manager activity\n%s%s\n%s)", indent, cont,
          paste(lines, collapse = paste0("\n", cont)), indent)
}

#' Render a program's `manager:` field. A single bare/quoted name (YAML:
#' `manager: "SBarley w. MF"`) renders as a plain `(manager "SBarley w.
#' MF")` -- a manager sourced directly from a library's own `defaction`
#' (or other manager model) definition, with no `activity` submodel
#' wrapper. A list (the usual case) renders via `render_manager_list()`'s
#' `(manager activity ...)` form.
#' @keywords internal
render_manager_field <- function(manager, indent) {
  if (is.null(manager)) return(NULL)
  if (is.character(manager) && length(manager) == 1)
    return(sprintf("%s(manager %s)", indent, .q(manager)))
  render_manager_list(manager, indent)
}

#' Render one `output:` list entry -- a bare built-in log keyword
#' (`harvest`), or a structured request for an existing log type.
#' @keywords internal
render_output_entry <- function(entry, indent) {
  if (is.character(entry)) return(sprintf("%s%s", indent, .bare(entry)))
  if (!is.null(entry$raw))
    return(.dai_render_with_comment(entry$comment, indent, paste0(indent, entry$raw)))
  if (is.null(entry$name))
    stop("An output entry needs 'name' (or be a bare built-in log keyword like \"harvest\")")
  parts <- character(0)
  if (!is.null(entry$to)) parts <- c(parts, trimws(render_generic_value("to", entry$to, "")))
  if (!is.null(entry$from)) parts <- c(parts, trimws(render_generic_value("from", entry$from, "")))
  if (!is.null(entry$when)) parts <- c(parts, sprintf("(when %s)", .bare(entry$when)))
  if (!is.null(entry$where)) parts <- c(parts, sprintf("(where %s)", .q(entry$where)))
  if (!is.null(entry$crop)) parts <- c(parts, sprintf("(crop %s)", .q(entry$crop)))
  if (!is.null(entry$print_header))
    parts <- c(parts, sprintf("(print_header %s)", .bare(entry$print_header)))
  if (!is.null(entry$print_dimension))
    parts <- c(parts, sprintf("(print_dimension %s)", .bare(entry$print_dimension)))
  line <- sprintf("%s(%s %s)", indent, .q(entry$name), paste(parts, collapse = " "))
  .dai_render_with_comment(entry$comment, indent, line)
}

#' @keywords internal
render_output_block <- function(output, indent) {
  if (is.null(output) || length(output) == 0) return(NULL)
  cont <- paste0(indent, "  ")
  lines <- vapply(output, render_output_entry, character(1), indent = cont)
  sprintf("%s(output\n%s\n%s)", indent, paste(lines, collapse = "\n"), indent)
}

#' Shared body (everything but the `(defprogram NAME TYPE-OR-PARENT` header
#' line and its closing paren) for a base or inheriting program.
#' @keywords internal
.render_program_body <- function(cfg, indent) {
  fc <- cfg$field_comments %||% list()
  with_fc <- function(key, rendered) .dai_render_with_comment(fc[[key]], indent, rendered)
  script <- cfg$daisy_script
  flush_after <- function(section) {
    keep <- vapply(script %||% list(), function(e) identical(.dai_script_after(e, default = ""), section), logical(1))
    if (!any(keep)) return(NULL)
    vapply(script[keep], function(e) {
      raw <- if (is.character(e)) e else e$raw
      .dai_render_with_comment(if (is.list(e)) e$comment else NULL, indent, paste0(indent, raw))
    }, character(1))
  }
  parts <- c(
    with_fc("weather", render_weather(cfg$weather, indent)),
    flush_after("weather"),
    with_fc("column", if (!is.null(cfg$column)) sprintf("%s(column %s)", indent, .q(cfg$column)) else NULL),
    flush_after("column"),
    with_fc("time", render_time_field("time", cfg$time, indent)),
    flush_after("time"),
    with_fc("stop", render_time_field("stop", cfg$stop, indent)),
    flush_after("stop"),
    with_fc("manager", if (!is.null(cfg$manager)) render_manager_field(cfg$manager, indent) else NULL),
    flush_after("manager"),
    with_fc("output", render_output_block(cfg$output, indent)),
    flush_after("output"),
    with_fc("description", if (!is.null(cfg$description)) render_generic_value("description", cfg$description, indent) else NULL),
    flush_after("description"),
    with_fc("log_prefix", if (!is.null(cfg$log_prefix)) sprintf("%s(log_prefix %s)", indent, .q(cfg$log_prefix)) else NULL),
    flush_after("log_prefix")
  )
  leftover <- vapply(script %||% list(), function(e) identical(.dai_script_after(e, default = ""), ""), logical(1))
  if (any(leftover))
    parts <- c(parts, vapply(script[leftover], function(e) {
      raw <- if (is.character(e)) e else e$raw
      .dai_render_with_comment(if (is.list(e)) e$comment else NULL, indent, paste0(indent, raw))
    }, character(1)))
  if (!is.null(fc[[".trailing"]]))
    parts <- c(parts, render_comment_lines(fc[[".trailing"]], indent))
  parts[!vapply(parts, is.null, logical(1))]
}

#' @keywords internal
render_batch_program <- function(cfg) {
  if (is.null(cfg$run) || length(cfg$run) == 0)
    stop("Program '", cfg$name, "' (type: batch) requires a 'run' list of program names")
  parts <- character(0)
  if (!is.null(cfg$directory)) parts <- c(parts, sprintf("  (directory %s)", .q(cfg$directory)))
  run_lines <- vapply(cfg$run, function(nm) sprintf("  (batch (run %s))", .q(nm)), character(1))
  parts <- c(parts, run_lines)
  block <- sprintf("(defprogram %s batch\n%s\n)", .q(cfg$name), paste(parts, collapse = "\n"))
  paste(.with_comment(cfg, "", block), collapse = "\n")
}

#' Render one `programs[[i]]` entry to a complete `(defprogram ...)` block
#' -- dispatches to base / inheriting / batch based on `type`/`based_on`.
#' @keywords internal
render_program <- function(cfg) {
  if (is.null(cfg$name)) stop("A program is missing its 'name' field")

  if (identical(cfg$type, "batch")) return(render_batch_program(cfg))

  if (!is.null(cfg$based_on)) {
    parts <- .render_program_body(cfg, "  ")
    block <- sprintf("(defprogram %s %s\n%s\n)", .q(cfg$name), .render_parent_ref(cfg$based_on),
                      paste(parts, collapse = "\n"))
    return(paste(.with_comment(cfg, "", block), collapse = "\n"))
  }

  if (!is.null(cfg$type) && !identical(cfg$type, "Daisy"))
    stop("Program '", cfg$name, "' has type '", cfg$type, "' -- expected 'Daisy' or 'batch', ",
         "or omit 'type' and set 'based_on' instead for an inheriting program")

  parts <- .render_program_body(cfg, "  ")
  block <- sprintf("(defprogram %s Daisy\n%s\n)", .q(cfg$name), paste(parts, collapse = "\n"))
  paste(.with_comment(cfg, "", block), collapse = "\n")
}

## -- parsing (the inverse direction) --------------------------------------

#' Inverse of `render_weather()`. A bare-string weather (`weather: none`)
#' and a dict weather with only `method` and no `file` render identically
#' (`(weather none)` either way), so a single bare-atom form always
#' reconstructs as the simpler bare-string shape -- byte-identical either
#' way, see this file's parsing header note pattern (dai_management.R).
#' @keywords internal
parse_weather_field <- function(children) {
  if (length(children) == 1 && children[[1]]$kind == "atom") return(children[[1]]$text)
  if (length(children) == 2 && children[[1]]$kind == "atom" && children[[2]]$kind == "string")
    return(list(method = children[[1]]$text, file = children[[2]]$text))
  stop("dai parse error: unrecognized weather field shape")
}

#' Inverse of `render_time_field()`.
#' @keywords internal
parse_time_field <- function(children) {
  vals <- as.numeric(vapply(children, function(c2) c2$text, character(1)))
  out <- list(year = vals[1], month = vals[2], day = vals[3])
  if (length(vals) >= 4) out$hour <- vals[4]
  out
}

#' Inverse of `render_manager_list()`. Reconstructs a flat list of names
#' (never the `{use:, repeat:}` shorthand -- Daisy's own text has no way
#' to tell "written out 20 times" apart from "20 different lines that
#' happen to repeat", and a flat list of the same name N times regenerates
#' the exact same `.dai` text either way).
#' @keywords internal
parse_manager_list <- function(children) {
  if (length(children) < 1 || children[[1]]$kind != "atom" || !identical(children[[1]]$text, "activity"))
    stop("dai parse error: a manager block must start with 'activity'")
  lapply(children[-1], function(nd) nd$text)
}

#' Inverse of `render_manager_field()`. A single bare/quoted name (no
#' `activity` submodel wrapper -- a manager sourced directly from a
#' library's own definition) reconstructs as a scalar string; the
#' `(manager activity ...)` form reconstructs as the list
#' `parse_manager_list()` already produces.
#' @keywords internal
parse_manager_field <- function(children) {
  if (length(children) == 1 && children[[1]]$kind %in% c("atom", "string"))
    return(children[[1]]$text)
  parse_manager_list(children)
}

#' Inverse of `render_output_entry()`.
#' @keywords internal
parse_output_entry <- function(nd) {
  if (nd$kind == "atom" || nd$kind == "string") return(nd$text)
  if (nd$kind != "list" || length(nd$children) < 1 ||
      !(nd$children[[1]]$kind %in% c("atom", "string")))
    return(list(raw = .dai_unparse_node(nd)))
  entry <- list(name = nd$children[[1]]$text)
  for (part in nd$children[-1]) {
    if (part$kind == "comment") next
    if (part$kind != "list" || length(part$children) < 1 || part$children[[1]]$kind != "atom")
      return(list(raw = .dai_unparse_node(nd)))
    key <- part$children[[1]]$text
    if (key %in% c("to", "from")) {
      entry[[key]] <- .dai_parse_generic_rest(part$children[-1])
    } else if (identical(key, "when")) {
      entry$when <- part$children[[2]]$text
    } else if (identical(key, "where")) {
      entry$where <- part$children[[2]]$text
    } else if (identical(key, "crop")) {
      entry$crop <- part$children[[2]]$text
    } else if (identical(key, "print_header")) {
      entry$print_header <- identical(part$children[[2]]$text, "true")
    } else if (identical(key, "print_dimension")) {
      entry$print_dimension <- identical(part$children[[2]]$text, "true")
    } else {
      return(list(raw = .dai_unparse_node(nd)))
    }
  }
  entry
}

#' @keywords internal
parse_output_block <- function(children) {
  split <- .dai_split_commented_items(children)
  lapply(split$items, function(it) {
    entry <- parse_output_entry(it$node)
    if (!is.null(it$comment)) {
      if (is.character(entry)) entry <- list(name = entry, comment = it$comment)
      else entry$comment <- it$comment
    }
    entry
  })
}

#' Inverse of `.render_program_body()`.
#' @keywords internal
parse_program_body <- function(children) {
  split <- .dai_split_commented_items(children)
  cfg <- list()
  field_comments <- list()
  last_key <- ""
  for (it in split$items) {
    nd <- it$node
    if (nd$kind != "list" || length(nd$children) < 1 || nd$children[[1]]$kind != "atom")
      stop("dai parse error: unrecognized entry inside defprogram")
    key <- nd$children[[1]]$text
    rest <- nd$children[-1]
    if (identical(key, "weather")) cfg$weather <- parse_weather_field(rest)
    else if (identical(key, "column")) cfg$column <- rest[[1]]$text
    else if (identical(key, "time")) cfg$time <- parse_time_field(rest)
    else if (identical(key, "stop")) cfg$stop <- parse_time_field(rest)
    else if (identical(key, "manager")) cfg$manager <- parse_manager_field(rest)
    else if (identical(key, "output")) cfg$output <- parse_output_block(rest)
    else if (identical(key, "description")) cfg$description <- .dai_parse_generic_rest(rest)
    else if (identical(key, "log_prefix")) cfg$log_prefix <- rest[[1]]$text
    else {
      entry <- list(raw = .dai_unparse_node(nd), after = last_key)
      if (!is.null(it$comment)) entry$comment <- it$comment
      cfg$daisy_script <- c(cfg$daisy_script, list(entry))
      next
    }
    field_comments <- .dai_set_field_comment(field_comments, key, it$comment)
    last_key <- key
  }
  if (!is.null(split$trailing))
    field_comments <- .dai_set_field_comment(field_comments, ".trailing", split$trailing)
  if (length(field_comments) > 0) cfg$field_comments <- field_comments
  cfg
}

#' Inverse of `render_batch_program()`.
#' @keywords internal
parse_batch_program <- function(name, children) {
  cfg <- list(name = name, type = "batch")
  run_names <- character(0)
  split <- .dai_split_commented_items(children)
  field_comments <- list()
  for (it in split$items) {
    nd <- it$node
    if (nd$kind != "list" || length(nd$children) < 1 || nd$children[[1]]$kind != "atom")
      stop("dai parse error: unrecognized entry inside a batch defprogram")
    key <- nd$children[[1]]$text
    field_comments <- .dai_set_field_comment(field_comments, key, it$comment)
    if (identical(key, "directory")) {
      cfg$directory <- nd$children[[2]]$text
    } else if (identical(key, "batch")) {
      inner <- nd$children[[2]]
      run_names <- c(run_names, inner$children[[2]]$text)
    } else {
      stop("dai parse error: unrecognized entry inside a batch defprogram")
    }
  }
  if (!is.null(split$trailing))
    field_comments <- .dai_set_field_comment(field_comments, ".trailing", split$trailing)
  if (length(field_comments) > 0) cfg$field_comments <- field_comments
  cfg$run <- run_names
  cfg
}

#' Parse a `(defprogram name type-or-parent [description] field...)`
#' form's body children (i.e. everything after the leading `defprogram`
#' keyword atom) back into one `programs[[i]]` entry -- the inverse of
#' `render_program()`, also accepting a bare (unquoted) name and/or
#' Daisy's optional positional description string (see
#' dai_parse_helpers.R). Dispatches on the bare `Daisy`/`batch` keyword
#' vs. a parent reference, exactly as `render_program()` dispatches the
#' other way. The block's own `comment` field, if any, is attached by the
#' caller (`parse_dai()`, dai_read.R), not here.
#' @keywords internal
parse_program <- function(body_children) {
  if (length(body_children) < 2 || !(body_children[[1]]$kind %in% c("atom", "string")))
    stop("dai parse error: a defprogram needs a name and a type/parent")
  name <- .dai_parse_name_node(body_children[[1]])
  type_node <- body_children[[2]]
  desc <- .dai_consume_positional_description(body_children[-(1:2)])
  rest <- desc$rest

  if (type_node$kind == "atom" && identical(type_node$text, "batch"))
    return(parse_batch_program(name, rest))
  if (type_node$kind == "atom" && identical(type_node$text, "Daisy")) {
    cfg <- c(list(name = name), parse_program_body(rest))
    if (!is.null(desc$description) && is.null(cfg$description)) cfg$description <- desc$description
    return(cfg)
  }

  ## An inheriting program: the parent is either a bare `default` atom or
  ## a quoted name -- unlike defcolumn/defcrop's based_on, this can't
  ## collapse a bare "default" to "omit based_on": omitting it here would
  ## regenerate as a *base* ("Daisy") program instead of an inheriting one
  ## with parent "default" -- two different `.dai` texts, not one.
  if (!(type_node$kind %in% c("atom", "string")))
    stop("dai parse error: unrecognized defprogram type/parent")
  based_on <- type_node$text
  cfg <- c(list(name = name, based_on = based_on), parse_program_body(rest))
  if (!is.null(desc$description) && is.null(cfg$description)) cfg$description <- desc$description
  cfg
}

## dai_programs.R ends here.
