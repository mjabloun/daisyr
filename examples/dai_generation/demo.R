# daisyr worked example: generating complete .dai files from YAML.
#
# Three bundled configs, each demonstrating a different part of the schema
# (see ?yaml_to_dai, ?read_management_config is NOT used here - this is the
# separate "generate a .dai from scratch" feature, distinct from the
# config/calibration/SA workflow in examples/calibration/):
#
#   sbarley-management.yaml    - two management activities only (mineral vs.
#                                 organic fertilizer, a compound {ds, mm_dd}
#                                 harvest condition, sow's seed_bed shorthand).
#                                 Reproduces "SBarley w. MF"/"SBarley w. OF"
#                                 from Daisy's own dk-management.dai.
#   cauliflower-management.yaml - a single activity using irrigate_until
#                                 (the (while (wait ..) (repeat ..)) pattern).
#   west-soil-column.yaml      - a complete, runnable script: directory/path,
#                                 4 horizons, a column, a derived crop, 3
#                                 activities (including a composite activity
#                                 built from two others via use_activity),
#                                 2 programs (base + inheriting), and a run.
#
# Run this script with the working directory set to this folder
# (examples/dai_generation/), e.g. via RStudio's "Source" button after
# opening it, or:
#   setwd("examples/dai_generation")
#   source("demo.R")

devtools::load_all("../..")   # or: library(daisyr)

cat("======================================================================\n")
cat("Spring Barley management -> matches 'SBarley w. MF' / 'SBarley w. OF'\n")
cat("in Daisy's own dk-management.dai (confirmed by actually running the\n")
cat("generated defaction blocks through daisy.exe).\n")
cat("======================================================================\n")
cat(yaml_to_dai("sbarley-management.yaml"), "\n\n")

cat("======================================================================\n")
cat("Cauliflower management -> exercises irrigate_until, i.e. the\n")
cat("(while (wait ..) (repeat ..)) pattern confirmed against dk-veg-man.dai.\n")
cat("======================================================================\n")
cat(yaml_to_dai("cauliflower-management.yaml"), "\n\n")

cat("======================================================================\n")
cat("West/Soil_Column -> a complete, runnable script: directory/path,\n")
cat("defhorizon x4, defcolumn, defcrop, defaction x3 (incl. the grass-\n")
cat("specific 'cut' operation and a composite activity), defprogram x2\n")
cat("(base + inheriting), and a standalone run.\n")
cat("======================================================================\n")
cat(yaml_to_dai("west-soil-column.yaml"), "\n\n")

## Uncomment to also write the generated .dai files to disk:
# yaml_to_dai("sbarley-management.yaml", "sbarley-management.dai")
# yaml_to_dai("cauliflower-management.yaml", "cauliflower-man.dai")
# yaml_to_dai("west-soil-column.yaml", "west-soil-column.dai")

## To scaffold a new config from scratch:
# list_dai_sections()
# scaffold_dai_yaml(output_path = "new-config.yaml")
