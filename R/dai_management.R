## dai_management.R -- Generate Daisy `defaction` (management activity)
## blocks from YAML.
##
## Daisy's management language ("defaction ... activity ...") is a sequence
## of Lisp-style s-expressions executed in order: wait for a date or
## condition, then do something (till, fertilize, sow, irrigate, harvest,
## cut), repeat. This file turns a YAML description of that same sequence
## into the equivalent .dai text.
##
## The YAML mirrors Daisy's own vocabulary as directly as possible: each
## field operation carries its own `date` (the condition Daisy should wait
## for before doing it) instead of a separate preceding "wait" entry, which
## is how you'd naturally write "plow on 1 April" rather than "wait until
## 1 April; plow". A field operation with no `date` simply runs immediately
## after the previous one, exactly like adjacent forms in the real .dai
## files (e.g. `(seed_bed_preparation)(sow "Crop")` with no wait between).
## An activity's body can also be composed entirely of other named
## activities with no dates of their own at all (e.g. combining a sowing
## activity and a repeated-cuts activity into one composite activity) --
## `use_activity` entries never take a `date`, since whatever they
## reference already carries its own internal timing.
##
## NOTE ON SPELLING: the YAML keys below use British spelling (`fertilise`)
## to match this project's convention, but Daisy's own DSL keyword is
## spelt `fertilize` -- the renderer always emits Daisy's actual token
## regardless of which YAML key produced it. Renaming YAML keys is free;
## renaming Daisy's own vocabulary is not.
##
## Requires dai_helpers.R to already be sourced (`%||%`, `.q`, `.fmt_num`,
## `.fmt_frac`, `render_wait_from_date`, `render_condition`,
## `render_comment_lines`, `.with_comment`).
##
## Because .dai is an s-expression language, indentation/newlines are
## purely cosmetic -- correctness only depends on matched parentheses and
## correct tokens/strings. The renderers below format output to be
## readable, matching the style of dk-management.dai / dk-veg-man.dai in
## this project, but nothing here depends on exact whitespace.
##
## ---------------------------------------------------------------------
## YAML schema
## ---------------------------------------------------------------------
## activities:                       # one or more named management activities
##   - name: "Cauliflower - transplanted management"
##     comment: "optional free text"  # -> a ;; comment line before (defaction ...)
##     field_operations:             # rendered in order, one Daisy form each
##
##       - comment: "1994"           # standalone comment line, no operation
##                                    # of its own -- e.g. a year marker
##                                    # between a run of repeated operations
##
##       - wait: {mm_dd: "04-01"}    # a standalone wait, when nothing else
##                                   # needs doing at that point (rare -- most
##                                   # of the time, put `date` directly on
##                                   # the operation that needs to wait)
##
##       - plow: {date: "04-01"}                    # (plowing)
##       - harrow: {}                                # (harrowing)
##       - seed_bed: {}                               # (seed_bed_preparation)
##       - tillage: {name: "rolling", date: "...", params: {}}
##                                    # escape hatch for other tillage.dai actions
##
##       - fertilise:                                # (fertilize (KAS (weight ...)))
##           date: "05-10"            # optional; omit to run right after
##                                    # the previous operation
##           comment: "optional"      # -> a ;; comment line right before it
##           product: KAS
##           weight: 100
##           unit: "kg N/ha"          # default "kg N/ha", or "T w.w./ha"
##                                    # automatically if volatilization is set
##           volatilization: 5        # [%] -- presence implies an organic
##                                    # amendment; adds (volatilization .. [%])
##                                    # *inside* the fertilizer reference, e.g.
##                                    # (fertilize ("cattle_slurry"
##                                    #    (volatilization 5 [%])
##                                    #    (weight 20 [T w.w./ha]))
##                                    #   (to -1 [cm]))
##           to: -1                   # optional, a sibling arg *inside* the
##                                    # (fertilize ...) form -- (to .. [cm])
##                                    # placement depth
##           from: 0                  # optional (from .. [cm]), same way
##
##       - sow:                                      # (sow "Crop" ...)
##           date: "05-10"            # optional
##           crop: "Cauliflower - transplanted"
##           harrow: true             # optional convenience: insert a bare
##           seed_bed: true           # (harrowing)/(seed_bed_preparation)
##                                    # immediately before this sow, no
##                                    # separate field_operations entries needed
##           seed: 2765                # -> (seed 2765 [kg w.w./ha])
##           row_width: 1              # -> (row_width 1 [m])
##           depth: 4                  # -> (depth 4 [cm])
##           density: 350              # -> (density 350 [plants/m^2])
##
##       - irrigate_until:                # (while (wait <date>) (repeat ..))
##           date: {mm_dd: "06-15"}        # the until-condition -- see below
##           repeat: irrigation            # name of a (defaction "irrigation"
##                                          # ...) trigger defined in a library
##
##       - irrigate:                       # a single (non-looping) irrigation event
##           date: "06-01"                 # optional
##           type: overhead                # "overhead" or "subsoil"
##           rate: 10
##           unit_rate: "mm/h"
##           hours: 3
##
##       - harvest:
##           date: {ds: 1.8, mm_dd: "10-01"}   # wait for whichever comes first
##           crop: "Cauliflower - transplanted"
##           stub: {value: 0.0, unit: "cm"}
##           sorg: 1.0
##           stem: 0.0
##           leaf: 0.0
##           condition: {ds: 0.1}    # optional; wraps in (if COND (harvest
##                                   # ...)) for a conditional (non-waiting)
##                                   # harvest -- different from `date`,
##                                   # which waits. `crop` defaults to this
##                                   # harvest's own `crop`.
##
##       - cut:                      # same shape as harvest, but grass-
##                                    # specific: emits (cut ...) instead of
##                                    # (harvest ...). Same fields: date,
##                                    # crop, condition, stub, sorg, stem, leaf.
##           date: {mm_dd: "06-09"}
##           crop: "Grass Scotland"
##           stub: {value: 8, unit: "cm"}
##           stem: 0.90
##           leaf: 0.90
##
##       - use_activity: "Sowing Grass"  # compose another named activity,
##                                       # e.g. for multi-year rotations or
##                                       # combining a sowing activity with a
##                                       # separate repeated-cuts activity
##                                       # into one composite activity
##       - raw: "(print_time periodic)"  # escape hatch: inserted verbatim
##
## `date` (and `condition`) accept one of:
##   "MM-DD"                                  -> (wait_mm_dd M D)
##   "YYYY-MM-DD"                             -> year is ignored (management
##                                                 activities are not year-specific)
##   {days: N}                                -> (wait_days N)
##   {ds: 1.8}                                -> (crop_ds_after CROP 1.8 []),
##                                                 CROP defaults to this
##                                                 operation's own `crop`
##                                                 field, or set explicitly
##                                                 with {ds: 1.8, crop: "X"}
##   {ds: 1.8, mm_dd: "10-01"}                -> (or (crop_ds_after CROP 1.8 [])
##                                                    (mm_dd 10 1))
##                                                 -- "whichever comes first",
##                                                 the most common Daisy pattern
##   {crop_dm_over: {crop, amount, unit, height, unit_height}}
##                                             -> (crop_dm_over "X" .. (height ..))
##   {soil_water_pressure_above: {height, unit_height, potential, unit_potential}}
##   {not: <date spec>}                       -> (not ..)
##   {any_of: [<date spec>, ...]}             -> (or .. ..)
##   {all_of: [<date spec>, ...]}             -> (and .. ..)
##   {raw: "(...)"}                           -> inserted verbatim
##
## ---------------------------------------------------------------------

## -- per-operation renderers ---------------------------------------------

#' Prefix an operation's own line(s) with its `comment` line (if any) and
#' then its `(wait ...)` line (if any), in that order.
#' @keywords internal
.with_wait <- function(fields, indent, body_lines, default_crop = NULL) {
  wait_txt <- render_wait_from_date(fields$date, default_crop)
  lines <- if (is.null(wait_txt)) body_lines else c(paste0(indent, wait_txt), body_lines)
  .with_comment(fields, indent, lines)
}

#' @keywords internal
render_wait_op <- function(op, indent) {
  wait_txt <- render_wait_from_date(op$wait)
  if (is.null(wait_txt))
    stop("A standalone 'wait' field_operation requires a value, e.g. '- wait: {mm_dd: \"04-01\"}'")
  .with_comment(op$wait, indent, paste0(indent, wait_txt))
}

#' @keywords internal
render_comment_op <- function(value, indent) {
  render_comment_lines(value, indent)
}

#' @keywords internal
render_tillage_op <- function(name, fields, indent) {
  fields <- fields %||% list()
  params <- fields[!names(fields) %in% c("date", "comment")]
  body <- if (length(params) == 0) {
    sprintf("%s(%s)", indent, name)
  } else {
    args <- vapply(names(params), function(k) {
      v <- params[[k]]
      sprintf("(%s %s)", k, if (is.character(v)) .q(v) else .fmt_num(v))
    }, character(1))
    sprintf("%s(%s %s)", indent, name, paste(args, collapse = " "))
  }
  .with_wait(fields, indent, body)
}

#' @keywords internal
render_generic_tillage_op <- function(fields, indent) {
  name <- fields$name
  if (is.null(name) || !nzchar(name))
    stop("A generic 'tillage' field_operation requires a 'name'")
  params <- fields$params %||% list()
  body <- if (length(params) == 0) {
    sprintf("%s(%s)", indent, name)
  } else {
    args <- vapply(names(params), function(k) {
      v <- params[[k]]
      sprintf("(%s %s)", k, if (is.character(v)) .q(v) else .fmt_num(v))
    }, character(1))
    sprintf("%s(%s %s)", indent, name, paste(args, collapse = " "))
  }
  .with_wait(fields, indent, body)
}

#' @keywords internal
render_fertilise_op <- function(fields, indent) {
  s <- fields
  if (is.null(s$product) || is.null(s$weight))
    stop("fertilise requires at least 'product' and 'weight'")

  product <- .q(s$product)
  weight <- .fmt_num(s$weight)
  unit <- if (!is.null(s$unit)) s$unit
          else if (!is.null(s$volatilization)) "T w.w./ha"
          else "kg N/ha"
  weight_txt <- if (is.null(unit) || identical(unit, "none"))
    sprintf("(weight %s)", weight)
  else
    sprintf("(weight %s [%s])", weight, unit)

  volat_txt <- if (!is.null(s$volatilization))
    sprintf("(volatilization %s [%%]) ", .fmt_num(s$volatilization))
  else ""

  amendment_txt <- sprintf("(%s %s%s)", product, volat_txt, weight_txt)

  ## `from`/`to` are siblings *inside* the (fertilize ...) form, alongside
  ## the fertilizer-reference tuple -- confirmed against a real script:
  ## (fertilize ("cattle_slurry" (volatilization 5 [%])(weight 20 [T w.w./ha]))
  ##            (to -1 [cm]))
  ## -- NOT two separate top-level forms.
  extra <- character(0)
  if (!is.null(s$from))
    extra <- c(extra, sprintf("(from %s [%s])", .fmt_num(s$from), s$unit_from %||% "cm"))
  if (!is.null(s$to))
    extra <- c(extra, sprintf("(to %s [%s])", .fmt_num(s$to), s$unit_to %||% "cm"))

  body <- if (length(extra) == 0)
    sprintf("%s(fertilize %s)", indent, amendment_txt)
  else
    sprintf("%s(fertilize %s %s)", indent, amendment_txt, paste(extra, collapse = " "))

  .with_wait(fields, indent, body)
}

#' @keywords internal
render_sow_op <- function(fields, indent) {
  s <- fields
  if (is.null(s$crop)) stop("sow requires 'crop'")
  crop <- .q(s$crop)

  parts <- character(0)
  if (!is.null(s$depth))
    parts <- c(parts, sprintf("(depth %s [%s])", .fmt_num(s$depth), s$unit_depth %||% "cm"))
  if (!is.null(s$density))
    parts <- c(parts, sprintf("(density %s [%s])", .fmt_num(s$density), s$unit_density %||% "plants/m^2"))
  ## exact (not `$`) lookup: `$seed` would silently partial-match the
  ## unrelated `seed_bed` field below via R's `$`-on-lists partial matching
  ## (confirmed against a real config: produced a bogus "(seed TRUE ...)").
  seed_val <- s[["seed", exact = TRUE]]
  if (!is.null(seed_val))
    parts <- c(parts, sprintf("(seed %s [%s])", .fmt_num(seed_val), s$unit_seed %||% "kg w.w./ha"))
  if (!is.null(s$row_width))
    parts <- c(parts, sprintf("(row_width %s [%s])", .fmt_num(s$row_width), s$unit_row_width %||% "m"))

  sow_line <- if (length(parts) == 0) {
    sprintf("%s(sow %s)", indent, crop)
  } else {
    cont <- paste0(indent, "    ")
    sprintf("%s(sow %s\n%s%s)", indent, crop, cont, paste(parts, collapse = paste0("\n", cont)))
  }

  pre_lines <- character(0)
  if (isTRUE(s$harrow)) pre_lines <- c(pre_lines, sprintf("%s(harrowing)", indent))
  if (isTRUE(s$seed_bed)) pre_lines <- c(pre_lines, sprintf("%s(seed_bed_preparation)", indent))

  .with_wait(fields, indent, c(pre_lines, sow_line))
}

#' @keywords internal
render_irrigate_until_op <- function(fields, indent) {
  s <- fields
  if (is.null(s$date)) stop("irrigate_until requires 'date' (the until-condition)")
  repeat_name <- s[["repeat"]]
  if (is.null(repeat_name)) stop("irrigate_until requires 'repeat' (the trigger activity name)")

  wait_txt <- render_wait_from_date(s$date, s$crop)
  body <- sprintf("%s(while %s\n%s       (repeat %s))", indent, wait_txt, indent, .q(repeat_name))
  .with_comment(fields, indent, body)
}

#' @keywords internal
render_irrigate_op <- function(fields, indent) {
  s <- fields
  if (is.null(s$rate) || is.null(s$hours))
    stop("irrigate requires 'rate' and 'hours'")
  type <- s$type %||% "overhead"
  rate <- .fmt_num(s$rate)
  unit_rate <- s$unit_rate %||% "mm/h"
  hours <- .fmt_num(s$hours)

  body <- if (identical(type, "overhead")) {
    sprintf("%s(irrigate_overhead %s [%s] (hours %s))", indent, rate, unit_rate, hours)
  } else if (identical(type, "subsoil")) {
    volume <- s$volume
    if (is.null(volume)) stop("irrigate of type 'subsoil' requires 'volume' (a defvolume name)")
    solute_txt <- ""
    if (!is.null(s$solute)) {
      terms <- vapply(s$solute, function(sol)
        sprintf("(%s %s [%s])", sol$name, .fmt_num(sol$value), sol$unit %||% "mg N/l"),
        character(1))
      solute_txt <- sprintf("\n%s                  (solute %s)", indent, paste(terms, collapse = ""))
    }
    sprintf("%s(irrigate_subsoil %s [%s] (hours %s) (volume %s)%s)",
            indent, rate, unit_rate, hours, volume, solute_txt)
  } else {
    stop("irrigate 'type' must be 'overhead' or 'subsoil', got: ", type)
  }
  .with_wait(fields, indent, body)
}

#' Shared by `harvest` and `cut` -- identical shape, different Daisy token.
#' @keywords internal
.render_harvest_like_op <- function(fields, indent, token) {
  s <- fields
  if (is.null(s$crop)) stop(token, " requires 'crop'")
  crop <- .q(s$crop)

  parts <- character(0)
  if (!is.null(s$stub)) {
    if (is.list(s$stub)) {
      stub_val <- s$stub$value
      stub_unit <- s$stub$unit %||% "cm"
    } else {
      stub_val <- s$stub
      stub_unit <- "cm"
    }
    parts <- c(parts, sprintf("(stub %s [%s])", .fmt_frac(stub_val), stub_unit))
  }
  if (!is.null(s$sorg)) parts <- c(parts, sprintf("(sorg %s)", .fmt_frac(s$sorg)))
  if (!is.null(s$stem)) parts <- c(parts, sprintf("(stem %s)", .fmt_frac(s$stem)))
  if (!is.null(s$leaf)) parts <- c(parts, sprintf("(leaf %s)", .fmt_frac(s$leaf)))
  if (length(parts) == 0)
    stop(token, " for '", s$crop, "' specifies none of stub/sorg/stem/leaf")

  wrapped <- !is.null(s$condition)
  base_indent <- if (wrapped) paste0(indent, "    ") else indent
  cont <- paste0(base_indent, "    ")
  op_core <- sprintf("(%s %s\n%s%s)", token, crop, cont,
                      paste(parts, collapse = paste0("\n", cont)))
  op_line <- paste0(base_indent, op_core)

  body <- if (wrapped) {
    cond_txt <- render_condition(s$condition, s$crop)
    sprintf("%s(if %s\n%s)", indent, cond_txt, op_line)
  } else {
    op_line
  }
  ## the wait (from `date`) must precede the whole (if ..) / (harvest|cut ..)
  ## form, so it's prepended after `body` is fully assembled, not via
  ## .with_wait()'s plain indent-prefixed line -- same effect, done inline:
  wait_txt <- render_wait_from_date(s$date, s$crop)
  lines <- if (is.null(wait_txt)) body else c(paste0(indent, wait_txt), body)
  .with_comment(fields, indent, lines)
}

#' @keywords internal
render_harvest_op <- function(fields, indent) .render_harvest_like_op(fields, indent, "harvest")

#' Grass-specific defoliation, distinct from `harvest` -- same shape
#' (crop/date/condition/stub/sorg/stem/leaf), emits `(cut ...)`.
#' @keywords internal
render_cut_op <- function(fields, indent) .render_harvest_like_op(fields, indent, "cut")

#' @keywords internal
render_use_activity_op <- function(value, indent) {
  sprintf("%s%s", indent, .q(value))
}

#' @keywords internal
render_raw_op <- function(value, indent) {
  lines <- strsplit(value, "\n", fixed = TRUE)[[1]]
  paste0(indent, lines, collapse = "\n")
}

#' Render one field operation (a single-key list, e.g.
#' `list(fertilise = list(...))`) to its Daisy text (possibly several
#' lines: a leading comment and/or wait, plus the operation itself).
#' @keywords internal
render_field_operation <- function(op, indent = "  ") {
  if (length(op) == 0 || is.null(names(op)) || !nzchar(names(op)[1]))
    stop("Each field_operations entry must be a single-key mapping, ",
         "e.g. '- plow: {date: \"04-01\"}'")
  key <- names(op)[1]
  lines <- switch(key,
    comment       = render_comment_op(op$comment, indent),
    wait          = render_wait_op(op, indent),
    plow          = render_tillage_op("plowing", op$plow, indent),
    harrow        = render_tillage_op("harrowing", op$harrow, indent),
    seed_bed      = render_tillage_op("seed_bed_preparation", op$seed_bed, indent),
    tillage       = render_generic_tillage_op(op$tillage, indent),
    fertilise     = render_fertilise_op(op$fertilise, indent),
    fertilize     = render_fertilise_op(op$fertilize, indent),  # accept either spelling
    sow           = render_sow_op(op$sow, indent),
    irrigate_until = render_irrigate_until_op(op$irrigate_until, indent),
    irrigate      = render_irrigate_op(op$irrigate, indent),
    harvest       = render_harvest_op(op$harvest, indent),
    cut           = render_cut_op(op$cut, indent),
    use_activity  = render_use_activity_op(op$use_activity, indent),
    raw           = render_raw_op(op$raw, indent),
    stop("Unknown field_operation type '", key, "'. Recognised: comment, wait, plow, ",
         "harrow, seed_bed, tillage, fertilise, sow, irrigate_until, ",
         "irrigate, harvest, cut, use_activity, raw.")
  )
  paste(lines, collapse = "\n")
}

## -- activity assembly ----------------------------------------------------

#' Render one `activities[[i]]` entry to a complete `(defaction ...)` block.
#' @keywords internal
render_activity <- function(activity_cfg) {
  if (is.null(activity_cfg$name)) stop("An activity is missing its 'name' field")
  if (is.null(activity_cfg$field_operations) || length(activity_cfg$field_operations) == 0)
    stop("Activity '", activity_cfg$name, "' has no field_operations")

  op_lines <- vapply(activity_cfg$field_operations, render_field_operation, character(1), indent = "  ")
  block <- sprintf("(defaction %s activity\n%s\n)", .q(activity_cfg$name), paste(op_lines, collapse = "\n"))
  paste(.with_comment(activity_cfg, "", block), collapse = "\n")
}

## -- parsing (the inverse direction) --------------------------------------
##
## `parse_activity()` and its helpers below are the inverse of everything
## above. One simplification makes this tractable: a `date`/`comment`
## carried *on* a field_operation and a *separate*, standalone `wait`/
## `comment` entry immediately before that operation render to byte-
## identical text (`.with_wait()`/`.with_comment()` always put the comment
## line(s), then the wait line, then the operation's own line(s), with
## nothing to tell them apart once they're just consecutive lines of
## text). So rather than trying to recover which one the file's author
## originally wrote, parsing always reconstructs the second form: a
## standalone `{wait: ...}` entry, and any leading comment as its own
## standalone `{comment: ...}` entry just before whatever it precedes --
## *never* folded into that entry's own fields (tempting for a dict-valued
## entry like `fertilise`, but wrong for `wait`'s `mm_dd`/`days` shorthand
## specifically: `render_wait_from_date()`'s fast path for those requires
## the field's dict to carry *no other keys*, so adding `comment` there
## would silently fall through to the slower generic-condition rendering
## and change the output). Always standalone is simple and uniformly
## correct -- regenerating either way reproduces the exact same `.dai`
## text, so this loses nothing towards this file's actual goal.
##
## An operation form this file doesn't recognize (an unhandled tillage
## action, or a date/condition shape the mini-language has no shorthand
## for) round-trips through `raw:`/the condition's own `raw` key -- the
## schema's own documented escape hatches -- rather than failing, per
## dai_parse_helpers.R's scope note.

#' @keywords internal
.dai_parse_tillage_params <- function(nodes) {
  out <- list()
  for (nd in nodes) {
    if (nd$kind != "list" || length(nd$children) != 2 || nd$children[[2]]$kind %in% c("list", "bracket"))
      stop("dai parse error: unrecognized tillage parameter shape")
    key <- nd$children[[1]]$text
    vnode <- nd$children[[2]]
    out[[key]] <- if (vnode$kind == "string") vnode$text else as.numeric(vnode$text)
  }
  out
}

#' @keywords internal
.dai_looks_like_tillage_params <- function(nodes) {
  all(vapply(nodes, function(nd)
    nd$kind == "list" && length(nd$children) == 2 && nd$children[[1]]$kind == "atom" &&
      nd$children[[2]]$kind %in% c("atom", "string"), logical(1)))
}

#' @keywords internal
.dai_parse_fertilise_op <- function(ch) {
  amendment <- ch[[1]]
  ach <- amendment$children
  if (!(ach[[1]]$kind %in% c("atom", "string")))
    stop("dai parse error: fertilize's amendment needs a product name")
  s <- list(product = ach[[1]]$text)
  idx <- 2L
  if (length(ach) >= idx && ach[[idx]]$kind == "list" && ach[[idx]]$children[[1]]$text == "volatilization") {
    s$volatilization <- as.numeric(ach[[idx]]$children[[2]]$text)
    idx <- idx + 1L
  }
  weight_node <- ach[[idx]]
  s$weight <- as.numeric(weight_node$children[[2]]$text)
  ## always set `unit` explicitly (rather than relying on render_fertilise_op's
  ## default-by-presence-of-volatilization heuristic), so regenerating always
  ## reproduces exactly the unit text this file actually had.
  s$unit <- if (length(weight_node$children) >= 3) weight_node$children[[3]]$text else "none"

  for (extra in ch[-1]) {
    key <- extra$children[[1]]$text
    val <- as.numeric(extra$children[[2]]$text)
    unit <- extra$children[[3]]$text
    if (identical(key, "from")) { s$from <- val; s$unit_from <- unit }
    if (identical(key, "to")) { s$to <- val; s$unit_to <- unit }
  }
  s
}

#' @keywords internal
.dai_parse_sow_op <- function(ch) {
  s <- list(crop = ch[[1]]$text)
  for (part in ch[-1]) {
    key <- part$children[[1]]$text
    val <- as.numeric(part$children[[2]]$text)
    unit <- part$children[[3]]$text
    if (identical(key, "depth")) { s$depth <- val; s$unit_depth <- unit }
    if (identical(key, "density")) { s$density <- val; s$unit_density <- unit }
    if (identical(key, "seed")) { s$seed <- val; s$unit_seed <- unit }
    if (identical(key, "row_width")) { s$row_width <- val; s$unit_row_width <- unit }
  }
  s
}

#' Shared by `harvest` and `cut`'s inverse.
#' @keywords internal
.dai_parse_harvest_like <- function(ch) {
  s <- list(crop = ch[[1]]$text)
  for (part in ch[-1]) {
    key <- part$children[[1]]$text
    if (identical(key, "stub")) {
      s$stub <- list(value = as.numeric(part$children[[2]]$text), unit = part$children[[3]]$text)
    } else if (key %in% c("sorg", "stem", "leaf")) {
      s[[key]] <- as.numeric(part$children[[2]]$text)
    }
  }
  s
}

#' @keywords internal
.dai_parse_conditional_harvest <- function(ch) {
  if (length(ch) != 2) return(list(raw = .dai_unparse_node(
    list(kind = "list", children = c(list(list(kind = "atom", text = "if")), ch)))))
  cond_node <- ch[[1]]
  inner <- ch[[2]]
  if (inner$kind != "list" || inner$children[[1]]$kind != "atom" ||
      !(inner$children[[1]]$text %in% c("harvest", "cut")))
    return(list(raw = .dai_unparse_node(
      list(kind = "list", children = c(list(list(kind = "atom", text = "if")), ch)))))
  token <- inner$children[[1]]$text
  s <- .dai_parse_harvest_like(inner$children[-1])
  s$condition <- .dai_parse_condition(cond_node)
  setNames(list(s), token)
}

#' @keywords internal
.dai_parse_irrigate_until_op <- function(ch) {
  date_spec <- .dai_parse_wait_form(ch[[1]])
  if (is.null(date_spec)) stop("dai parse error: irrigate_until's (while ...) needs a wait_mm_dd/wait_days/wait form")
  repeat_node <- ch[[2]]
  list(date = date_spec, `repeat` = repeat_node$children[[2]]$text)
}

#' @keywords internal
.dai_parse_irrigate_op <- function(head, ch) {
  rate <- as.numeric(ch[[1]]$text)
  unit_rate <- ch[[2]]$text
  hours <- as.numeric(ch[[3]]$children[[2]]$text)
  if (identical(head, "irrigate_overhead"))
    return(list(type = "overhead", rate = rate, unit_rate = unit_rate, hours = hours))

  volume <- ch[[4]]$children[[2]]$text
  s <- list(type = "subsoil", rate = rate, unit_rate = unit_rate, hours = hours, volume = volume)
  if (length(ch) >= 5) {
    solute_node <- ch[[5]]
    s$solute <- lapply(solute_node$children[-1], function(term_node) {
      f <- .dai_parse_field_form(term_node)
      list(name = f$key, value = f$value$value, unit = f$value$unit)
    })
  }
  s
}

#' Parse one field_operation node (a `comment` node is handled by the
#' caller, `parse_activity()`, not here) into its single-key list form,
#' e.g. `list(fertilise = list(...))` -- the inverse of
#' `render_field_operation()`.
#' @keywords internal
.dai_parse_field_operation <- function(node) {
  if (node$kind %in% c("string", "atom")) return(list(use_activity = node$text))
  if (node$kind != "list" || length(node$children) < 1 || node$children[[1]]$kind != "atom")
    return(list(raw = .dai_unparse_node(node)))

  head <- node$children[[1]]$text
  ch <- node$children[-1]

  if (head %in% c("wait_mm_dd", "wait_days", "wait")) {
    wf <- .dai_parse_wait_form(node)
    if (!is.null(wf)) return(list(wait = wf))
  }
  if (identical(head, "fertilize")) return(list(fertilise = .dai_parse_fertilise_op(ch)))
  if (identical(head, "sow")) return(list(sow = .dai_parse_sow_op(ch)))
  if (identical(head, "while")) return(list(irrigate_until = .dai_parse_irrigate_until_op(ch)))
  if (head %in% c("irrigate_overhead", "irrigate_subsoil"))
    return(list(irrigate = .dai_parse_irrigate_op(head, ch)))
  if (identical(head, "if")) return(.dai_parse_conditional_harvest(ch))
  if (identical(head, "harvest")) return(list(harvest = .dai_parse_harvest_like(ch)))
  if (identical(head, "cut")) return(list(cut = .dai_parse_harvest_like(ch)))
  if (identical(head, "plowing")) return(list(plow = .dai_parse_tillage_params(ch)))
  if (identical(head, "harrowing")) return(list(harrow = .dai_parse_tillage_params(ch)))
  if (identical(head, "seed_bed_preparation")) return(list(seed_bed = .dai_parse_tillage_params(ch)))
  if (.dai_looks_like_tillage_params(ch))
    return(list(tillage = list(name = head, params = .dai_parse_tillage_params(ch))))

  list(raw = .dai_unparse_node(node))
}

#' Parse a `(defaction name activity op...)` form's body children (i.e.
#' everything after the leading `defaction` keyword atom) back into one
#' `activities[[i]]` entry -- the inverse of `render_activity()`, also
#' accepting a bare (unquoted) name (see dai_parse_helpers.R). The
#' activity's own `comment` field, if any, is attached by the caller
#' (`parse_dai()`, dai_read.R), not here -- see that file for why.
#' @keywords internal
parse_activity <- function(body_children) {
  if (length(body_children) < 2 || !(body_children[[1]]$kind %in% c("atom", "string")) ||
      !(body_children[[2]]$kind == "atom" && identical(body_children[[2]]$text, "activity")))
    stop("dai parse error: a defaction needs a name and the 'activity' keyword")
  name <- .dai_parse_name_node(body_children[[1]])
  op_nodes <- body_children[-(1:2)]
  if (length(op_nodes) == 0) stop("dai parse error: activity '", name, "' has no field_operations")

  field_operations <- list()
  i <- 1L; n <- length(op_nodes)
  while (i <= n) {
    nd <- op_nodes[[i]]
    if (nd$kind == "comment") {
      j <- i
      while (j <= n && op_nodes[[j]]$kind == "comment") j <- j + 1L
      comment_text <- paste(vapply(op_nodes[i:(j - 1L)], `[[`, character(1), "text"), collapse = "\n")
      if (j > n) {
        field_operations[[length(field_operations) + 1L]] <- list(comment = comment_text)
        break
      }
      construct <- op_nodes[[j]]
      i <- j
    } else {
      comment_text <- NULL
      construct <- nd
    }

    op <- .dai_parse_field_operation(construct)
    if (!is.null(comment_text))
      field_operations[[length(field_operations) + 1L]] <- list(comment = comment_text)
    field_operations[[length(field_operations) + 1L]] <- op
    i <- i + 1L
  }
  list(name = name, field_operations = field_operations)
}

## dai_management.R ends here.
