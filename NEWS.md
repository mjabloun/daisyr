# daisyr (development version)

Initial development. Not yet released. Features are tracked here as they're
added, grouped by area.

## Breaking

* `param_config_factor_names()`, `expand_calibrate_factors()`, and
  `suggest_calibrate_factors()` have been removed. Use `param_config_names()`,
  `expand_calibrate_names()`, and `suggest_calibrate_names()`. The argument
  formerly called `factors` is now `names`. Sensitivity index tables use a
  `name` column (not `factor`). Metamodel objects store `param_names`.

## Parameters

* `read_param_config()` / `validate_param_config()` - YAML parameters file
  describing free parameters (calibration and sensitivity analysis)
  across one or more `.dai` template/target file pairs.
* Two parameter roles: `direct` (own placeholder in a template) and
  `curve_input` (consumed only by a `plf_curves` entry).
* `param_config_names()` - flattened list of every free parameter.
* `validate_param_config()` checks every declared `{{placeholder}}` exists
  exactly once in its template before any Daisy run is attempted.
* `create_param_config()` - scaffolds a starter YAML parameters file (with
  `# TODO` comments marking what to fill in) from just a vector of
  parameter/PLF names and types, so the schema doesn't need to be
  hand-written from scratch. For `type = "plf"` entries it also scaffolds
  the curve's shape parameters (e.g. `L`/`k`/`x0` for `"logistic"`) and the
  matching `plf_curves` entry.
* Preferred filename is `parameters.yaml`; older `registry.yaml` files still
  load. Function names use `param_config` (`read_param_config()`,
  `validate_param_config()`, and so on).

## Templating

* `render_templates()` - renders every `.dai` template implied by the
  parameters file, given a full parameter vector. Placeholders use double braces
  (`{{name}}`) to avoid colliding with Daisy's own `${...}` syntax.
* `format_plf_block()` - formats an `(x y) (x y) ...` sequence in Daisy
  syntax.

## Piecewise-linear-function (PLF) curve generation

* `render_plf_curve()` - evaluates a named curve family at fixed x-knots.
* Built-in curve families (`known_plf_curves()`): `logistic`, `gompertz`,
  `richards`, `exponential`.
* `register_plf_curve()` - register a custom curve family.
* `plot_plf_curves()` / `known_plf_curves(plot = TRUE)` - quick visual
  reference plotting an example of each built-in family.
* Individual PLF points can also be calibrated directly (no curve fit) as a
  plain scalar parameter with `plf` metadata for documentation.

## Running Daisy

* `run_daisy()` - wrapper around the Daisy launcher. `daisy_exe` is
  optional (`set_daisy_path()` / `DAISY_EXE` / common install paths).
  `cmd` is an optional [glue] template for HPC/Singularity (e.g.
  `"singularity run ... $(pwd)/{run_file}"`); `{daisy_exe}` is only
  resolved if the template uses it.
* `set_daisy_path()` / `get_daisy_path()` - session default for the Daisy
  executable (`options(daisyr.daisy_exe)`).

## Weather files

* `write_dwf()` - write a Daisy `.dwf` weather file from a table of daily
  (or hourly) values. Station name and coordinates are arguments (the
  older `writeDaisyWeatherFile()` helper hardcoded Taastrup). Begin/End
  come from the data; `TAverage`/`TAmplitude`/`MaxTDay` are computed from
  `AirTemp` unless supplied. Date columns may be `Year`/`Month`/`Day`,
  `YEAR`/`MM`/`DD`, or a `Date` column.

## Inspecting existing .dai files

* `read_plf()` - extracts a named piecewise-linear-function parameter's
  `(x y) (x y) ...` sequence directly from an existing `.dai` file (e.g. to
  inspect a value already in a template, or seed `x_values`/starting shape
  parameters for a `plf_curves` config entry). Handles values that span
  multiple lines and end-of-line comments; `occurrence` selects among
  repeated parameter names (e.g. one per crop/horizon definition).
* `plot_plf()` - plots a PLF as connected line segments (matching Daisy's
  own linear interpolation between knots). Accepts either a `.dai` file +
  parameter name (calling `read_plf()` itself) or an already-extracted
  `data.frame`/list with `x`/`y` elements, e.g. the output of `read_plf()`.

## Reading Daisy output

* `read_dlf()` - robust `.dlf` log parser, handling both the standard
  `----` separator header and `print_header false`/`print_dimension false`
  variants. Column units from Daisy's dimension line are stored as a
  named character vector in `attr(x, "units")` (missing units are `NA`).
* `add_date()` - builds a `Date` column from Year/Month/Day(/Hour) columns,
  and keeps `attr(x, "units")` aligned with any newly added columns.
* `get_unit()` / `set_unit()` - look up or assign unit strings for one or
  more columns (including derived columns added after `read_dlf()`).

## Objective functions

* `compute_generic_objective()` - multi-objective, weighted goodness-of-fit
  scoring (RMSE/KGE/MAE or a custom metric function) between observed data
  and one or more simulated output columns, with optional diagnostic plots.
* `objective_spec()` / `evaluate_objective()` - reusable objective
  specification that can be passed around and evaluated against a
  just-produced simulation output.

## Calibration

* `evaluate_daisy_candidate()` - one calibration iteration (render -> run ->
  score).
* `calibrate_daisy()` - DEoptim, BOBYQA (`minqa`), multi-BOBYQA, DDS
  (in-package), or CMA-ES (`cmaes`) over free parameters. Optional `reporter`
  callback after each candidate (used by daisyr.studio instead of DEoptim's
  console `trace`).
* `calibrate_daisy_ego()` - surrogate-assisted EGO calibration (see
  Metamodel / surrogate designs).

## Metamodel / surrogate designs

* `generate_param_design()` - space-filling training design over caller-defined
  parameter bounds (`name`/`min`/`max`; 1 parameter or many). `method = "lhs"`
  uses `lhs::maximinLHS()` (optional dependency); `method = "dice_lhs"`
  uses `DiceDesign::lhsDesign()` then `DiceDesign::maximinESE_LHS()`
  (`...` forwarded to the ESE step); `method = "sobol"` uses
  `randtoolbox::sobol(..., scrambling = 3)` (scrambling is requested; some
  randtoolbox versions currently ignore it). All three rescale the unit
  hypercube to each parameter's `[min, max]`. Optional `seed` for
  reproducibility.
* `extend_param_design()` - continues a Sobol' sequence from where an existing
  design left off (returns only the new rows). Errors for `"lhs"` and
  `"dice_lhs"`: those designs are not extensible; build a fresh, larger
  `generate_param_design()` instead.
* `run_param_design()` - runs Daisy for every design row. Selected
  `output_files` are copied into a single `keep_dir` as
  `run_<id>_<filename>` (no per-row folders; scales to thousands of
  combinations) with `design.csv` mapping `run_id` to parameter values,
  and/or read in-loop. Reading is required when `keep_files = FALSE`.
  Custom `output_reader` (default `read_dlf()`) plus `digits` /
  `reader_args`. Column subsetting and derived fields go through
  `sim_mutator` (same idea as `compute_generic_objective()`).
* `read_param_design()` - independently re-reads an archived `keep_dir`
  (different reader, `digits`, `reader_args`, or `sim_mutator`). Returns
  `outputs` as one `data.table` of all runs (or a named list of tables if
  several output files were kept), with a `run_id` column only; join
  `design` later for parameter values. Column subsetting belongs in
  `sim_mutator`.

* `score_param_design()` - scores every design row with a
  `daisyr_objective`, appending a `score` column (`(X, y)` for a
  metamodel). Uses in-memory `outputs` when present, otherwise archived
  `keep_dir` files.
* `fit_metamodel()` / `predict()` / `validate_metamodel()` - Kriging
  surrogate via `DiceKriging::km()` (leave-one-out, or k-fold via
  `DiceEval`). Optional dependencies.
* `calibrate_daisy_ego()` - Efficient Global Optimization
  (`DiceOptim::EGO.nsteps`) using the fitted metamodel; each step is one
  real Daisy evaluation via [evaluate_daisy_candidate()].

## Sensitivity analysis

* `sa_design()` - decoupled Morris or Sobol-Jansen design over every free
  parameter in a config.
* `run_sa_design()` - runs Daisy for every row of a design, via a
  user-supplied `response_fun`. Prints a console progress bar by default
  (`progress = TRUE`); pass `progress = FALSE` to silence it.
* `complete_sa_analysis()` - attaches responses and computes sensitivity
  indices.
* `plot_sa()` - plots those indices with base graphics: Morris mu* vs
  sigma (or a ranked mu* bar chart), and Sobol first-order vs total
  indices (with CI whiskers when the `sensitivity` object provides them).
  Returns the plotted table invisibly.

## Generating .dai files from YAML

A separate concern from the parameter config/calibration/SA workflow
above: writing a complete, runnable `.dai` file from scratch given a
structured config, instead of hand-editing Daisy's S-expression syntax.
Every construct's generated syntax has been confirmed by actually running
it through `daisy.exe` (not just pattern-matched against real `.dai` files).

* `generate_dai(config, output_path = NULL)` - the core generator. Works on
  a plain R list (the same shape `yaml::read_yaml()` produces), so config
  can come from a YAML file, be built directly in R, or be assembled from a
  data.frame (see `records_from_df()` below) - or any mix of the three.
  Every top-level section is optional and omitted entirely (not emitted
  empty) when absent: `directory`, `path`, `libraries`, a file-level
  `comment`, `horizons` (`defhorizon`), `columns` (`defcolumn`), `crops`
  (`defcrop`, derived crops only), `activities` (`defaction`, management
  operations), `programs` (`defprogram` - base, inheriting, or batch), and
  `run` (standalone `(run "NAME")` statements). `libraries` render as
  consecutive `(input file "...")` lines with no blank line between them.
  Multi-line `comment` / `trailing_comment` blocks are consecutive `;;`
  lines (no blank line between lines of the same block).
* `yaml_to_dai(yaml_path, output_path = NULL)` - reads a YAML file and
  calls `generate_dai()`.
* Management activities (`activities`) support a rich per-operation
  vocabulary mirroring Daisy's own: `plow`/`harrow`/`seed_bed` (bare
  tillage ops), a `tillage` escape hatch for other named tillage
  operations, `fertilise`/`fertilize` (mineral/named/organic, with
  `volatilization` and `to`/`from` incorporation depth), `sow` (with
  `seed`/`density`/`row_width`/`depth` and `harrow`/`seed_bed`
  convenience flags), `harvest`/`cut` (stub/sorg/stem/leaf, plus an
  optional `condition` for a non-waiting `(if ...)`-wrapped harvest),
  `irrigate_until` (the `(while (wait ...) (repeat ...))` pattern) and
  `irrigate` (overhead/subsoil), `use_activity` (compose other named
  activities), `raw` (verbatim escape hatch), and standalone
  `comment`/`wait` entries. Each operation's own `date` field describes
  when Daisy should wait for it (a `"MM-DD"` string, `{days: N}`,
  `{ds: ..}` possibly combined with `{mm_dd: ..}` for "whichever comes
  first", or a nested `not`/`any_of`/`all_of`/`raw` condition) - no
  separate "wait" entry needed for the common case.
* `horizons`/`columns`/`crops` use a generic nested-block mechanism that
  passes Daisy's own parameter vocabulary straight through from YAML
  (`{value: X, unit: "u"}` -> `(key X [u])`, a list of 2-item lists -> a
  PLF table, `[method, {overrides}]` -> a method keyword with its own
  parameters, ...), so new Daisy parameters don't need bespoke schema
  support to use.
* `parse_dai(text)` / `read_dai(dai_path, yaml_path = NULL)` - the inverse
  direction: parse existing `.dai` text (or read a `.dai` file) back into
  the same nested-list config shape `generate_dai()` consumes, so a file
  can be read, edited in R, and regenerated
  (`generate_dai(read_dai("west.dai"))` reproduces it). `parse_dai()` is
  the pure text-in/list-out worker (the counterpart to `generate_dai()`'s
  pure list-in/text-out); `read_dai()` is the file-reading convenience
  wrapper around it, matching how `yaml_to_dai()` wraps `generate_dai()`
  for YAML files. Passing a file path to `parse_dai()` by mistake (meaning
  to call `read_dai()` instead) is caught directly with a specific
  message rather than surfacing as a confusing generic parse error.
  Bounded to the same schema `generate_dai()` can produce, plus a
  passthrough: unrecognized top-level Daisy (`deflog`, `defchemical`, ...)
  and unrecognized `defprogram` fields (e.g. `print_time`) are kept as
  `daisy_script` entries (original source text, or an equivalent unparse
  for nested fields) and written back as-is. If a recognized construct is
  malformed, `parse_dai()` still collects every such problem across the
  whole file and raises one error listing them all, each naming the
  source line number it starts on.
  `read_dai(..., yaml_path = ...)` also writes the parsed config out as
  YAML, e.g. to hand-edit from there instead.
* A top-level `description` field (`(description "...")`), matching the
  same field already supported inside `defcrop`/`defprogram`.
* Reading also tolerates several idioms common in real hand-written
  `.dai` files that `generate_dai()`'s own renderers never happen to
  produce: bare (unquoted) names anywhere Daisy allows them (a name with
  no spaces need not be quoted) for `defhorizon`/`defcolumn`/`defcrop`/
  `defprogram`/`defaction`, a `fertilize` product, a `use_activity`
  reference, and `(run ...)`; Daisy's optional positional description
  string right after a `defhorizon`/`defcolumn`/`defcrop`/`defprogram`'s
  name/parent-or-type arguments (folded into that entry's `description`,
  same as the explicit field); a `Soil` block's more flexible real-world
  shape (its `MaxRootingDepth`/`horizons`/other sub-blocks in any order,
  an optional per-point unit on `horizons` entries, and other recognized
  parameters like `border` passed through rather than rejected); a
  program's `manager` given as a single bare/quoted name sourced directly
  from a library's own definition (`(manager "SBarley w. MF")`, no
  `activity` submodel wrapper), alongside the existing activity-list
  form; a `;`/`;;` comment interleaved between parameters inside a
  generic block, a `defprogram` body, a `defcolumn` body, or a `Soil` /
  `output` block (kept on `field_comments` and regenerated immediately
  before the field it belongs to); a comment immediately before an
  `(input file ...)` or `(run ...)` (kept on that entry); and a trailing
  file-end comment banner (kept as `trailing_comment`). `;;;` lines that
  are `generate_dai()`'s own header stay metadata and are not round-tripped;
  other `;;;` banners are kept as comments. Unrecognized constructs are
  kept as `daisy_script` rather than rejected.
* `records_from_df()` - converts a data.frame into the list-of-records
  shape every renderer expects (one named list per row), folding
  `<field>_value`/`<field>_unit` column pairs back into the `{value, unit}`
  shape and dropping `NA` fields entirely - the practical way to automate
  many `.dai` files (e.g. one per site) from a table you already have.
* `list_dai_sections()` / `scaffold_dai_yaml()` - discover the top-level
  YAML schema and scaffold a starter config with cross-referenced
  placeholder values, so it runs through `generate_dai()` unmodified as a
  valid (if not meaningful) starting point.

## Documentation & examples

* `vignette("daisyr")` - end-to-end worked example (parameters ->
  template rendering -> running Daisy -> objective -> calibration ->
  sensitivity analysis), runnable against a real Daisy install.
* `examples/calibration/run_calibration.R` - the same workflow as a plain
  runnable R script.
* Example parameters/template/observed-data files ship in
  `inst/extdata/calibration_example/`.
* `examples/dai_generation/demo.R` plus three bundled YAML configs
  (`sbarley-management.yaml`, `cauliflower-management.yaml`,
  `west-soil-column.yaml`) demonstrating the `.dai` generator, each
  confirmed to actually run through `daisy.exe`.

## Not yet implemented

* A `while "ActivityName" (repeat ...)`-style entry at the top-level
  `programs[[i]]$manager` list (parallel activity + repeated trigger,
  as seen in Daisy's own `sample.dai`) - distinct from the supported
  `irrigate_until` operation *within* an activity's own `field_operations`,
  which already covers the equivalent `(while (wait ...) (repeat ...))`
  pattern for the common case.
