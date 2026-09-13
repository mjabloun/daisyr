# daisyr

R driver for the **Daisy** agroecosystem model: write `.dai` setups, run
the official Daisy executable, parse `.dlf` logs, score simulations
against observations, and run calibration, sensitivity analysis, and
metamodel workflows on top of those primitives.

This is not a reimplementation of Daisy.

## What you need

1. A **Daisy** install with `daisy.exe` (Windows) or `daisy` (Unix).
2. At least one working **`.dai` setup** (and the weather, soil, and
   management files it includes). Build or edit setups with
   `generate_dai()` / `yaml_to_dai()`, or start from an existing Daisy
   file and `render_templates()`.

You do not need to put Daisy *inside* this package. Point at it:

```r
library(daisyr)
set_daisy_path("C:/Program Files/Daisy 5.93/bin/daisy.exe")
```

Or set the environment variable `DAISY_EXE`. `run_daisy()` also searches
common install paths if neither is set.

A worked calibration example ships in the package
(`inst/extdata/calibration_example/`) and is walked through in
`examples/calibration/run_calibration.R` and `vignettes/daisyr.Rmd`.

## Typical run

```r
run_daisy("setup.dai", working_dir = ".")
sim <- read_dlf("Output/harvest.dlf")
```

## Status

Driver, `.dai` generate/parse, weather writer (`write_dwf()`), YAML
parameters (`read_param_config()`), template rendering, PLF curves,
objectives, `calibrate_daisy()` (`DEoptim`, `BOBYQA`, `multi-BOBYQA`,
`DDS`, `CMA-ES`) and `calibrate_daisy_ego()`, Morris / Sobol, and
space-filling metamodel designs (`generate_param_design()`,
`run_param_design()`, `fit_metamodel()`).

Daisy itself remains the Aarhus University / University of Copenhagen
model.
