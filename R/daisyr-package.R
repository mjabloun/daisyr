#' daisyr: Sensitivity Analysis and Calibration Tools for the Daisy Model
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom data.table :=
#' @importFrom utils globalVariables
## usethis namespace: end
NULL

# Column names referenced via data.table's non-standard evaluation (`[`, `:=`)
# rather than as function arguments; declared here so R CMD check doesn't
# flag them as undefined globals.
utils::globalVariables(c(
  "name", "role", "from_file", "to_file",  # param_config.R
  "Date", "hour", ".SD",                    # read_dlf.R
  "obs_col", "label", "weight",             # objective.R
  "obs", "sim",                             # objective.R (build_generic_obj_plot)
  "run_id"                                  # param_design.R
))
