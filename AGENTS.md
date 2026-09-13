# Notes for working on daisyr

## Feature tracking

`NEWS.md` tracks which features are available/added, grouped by area (not
strictly a version-by-version changelog since the package hasn't been
released yet). **Update `NEWS.md` whenever a feature is added or changed**,
so it stays an accurate catalog of current capabilities.

## Verification workflow

`devtools::check()` rebuilds `vignettes/daisyr.Rmd`, and that vignette
actually invokes the real Daisy executable (calibration + Morris SA), which
takes ~5 minutes per run. **Do not run a full `devtools::check()` after every
small change** - it's too slow for iterative development.

Instead:

- While adding/changing a feature, only run the tests relevant to it, e.g.
  `testthat::test_file("tests/testthat/test-curves.R")` or
  `devtools::test(filter = "curves")`.
- `devtools::test()` (the full test suite, no vignette rebuild) is fast
  (~2s) and fine to run whenever.
- Reserve a full `devtools::check()` (or at least a manual vignette rebuild
  via `rmarkdown::render("vignettes/daisyr.Rmd", output_dir = tempdir())`)
  for the end of a work session / before considering a larger chunk of work
  done - not after every incremental edit.

## Validating .dai syntax against real Daisy

For the `.dai`-generation feature (`generate_dai()`/`yaml_to_dai()` and the
`dai_*.R` renderers), unit tests check the *generated text* against known-
correct patterns, but that alone doesn't prove Daisy actually accepts it.
When adding/changing a renderer, additionally splice the generated
fragment into a small known-working `.dai` scenario (e.g. adapt
`R-DAISY_v2/Example/test-optim.dai`'s Andeby column + `dk-taastrup.dwf`
weather) and run it through `daisy.exe` directly (not via `devtools::test()`
- this is a manual one-off check, not part of the automated suite) to
confirm it actually parses/runs. Delete the scratch `.dai`/`Output/` files
afterwards; don't commit them. This caught real bugs that pure text-pattern
tests wouldn't have (e.g. `render_sow_op()`'s `s$seed` silently partial-
matching the unrelated `seed_bed` field via R's `$`-on-lists partial
matching - fixed with `s[["seed", exact = TRUE]]`). Watch for this same
partial-matching hazard elsewhere: prefer `x[["name", exact = TRUE]]` over
`x$name` whenever a field name could be a prefix of another field in the
same list (e.g. `seed`/`seed_bed`).

## Local environment

- R lives at `C:/JHI/R/R-4.5.0/bin/x64/Rscript.exe` (not on PATH).
- Daisy lives at `C:/Program Files/Daisy 5.93/bin/daisy.exe`.
- The example `.dai` templates need Daisy's own `lib/` and `sample/`
  directories on the `(path ...)` search list (see
  `inst/extdata/calibration_example/test-optim_Generic.dai`) - Daisy does not
  search its own install directories by default.
- daisyr's own placeholders use double braces (`{{name}}`), not single
  braces, because Daisy's `.dai` syntax uses single-brace-free `${...}` for
  internal self-reference (e.g. inside a `deflog`).
