#' Parse the `partit:` list from a parameters YAML file
#' @param raw The `partit` value from [yaml::read_yaml()], or `NULL`.
#' @return A named list of partit specifications. Names are the entry names.
#' @keywords internal
.parse_partit_entries <- function(raw) {
  if (is.null(raw)) raw <- list()
  partit <- lapply(raw, function(pt) {
    required <- c("name", "from_file", "to_file", "placeholder", "x_values", "params")
    missing <- setdiff(required, names(pt))
    if (length(missing) > 0)
      stop(sprintf("partit entry '%s' is missing required field(s): %s",
                    if (is.null(pt$name)) "<unnamed>" else pt$name,
                    paste(missing, collapse = ", ")))
    pt$x_values <- as.numeric(pt$x_values)
    pt$params <- as.character(pt$params)
    if (is.null(pt$format)) pt$format <- "%.4f"
    n_expected <- length(.partit_shape_suffixes())
    if (length(pt$params) != n_expected)
      stop(sprintf("partit entry '%s' expects %d parameter(s) in order (%s), got %d",
                    pt$name, n_expected, paste(.partit_shape_suffixes(), collapse = ", "),
                    length(pt$params)))
    pt
  })
  nms <- vapply(partit, function(pt) pt$name, character(1))
  if (anyDuplicated(nms) > 0)
    stop("Duplicate partit name(s): ",
         paste(unique(nms[duplicated(nms)]), collapse = ", "))
  names(partit) <- nms
  partit
}

#' Shape-parameter suffixes for a `partit` entry, in `params` order
#' @keywords internal
.partit_shape_suffixes <- function() {
  c("sorg_steepness", "sorg_centre", "leaf_steepness", "leaf_centre")
}

#' Suggested default, min, and max for each Partit shape parameter
#'
#' Rows follow [.partit_shape_suffixes()]. Steepness is the logistic slope
#' (positive: storage-organ share rises, leaf share of the remainder falls).
#' Centre is the development-stage inflection.
#' @keywords internal
.partit_shape_defaults <- function() {
  data.frame(
    suffix  = .partit_shape_suffixes(),
    default = c(10, 1.2, 10, 0.6),
    min     = c(3, 0.8, 3, 0.3),
    max     = c(25, 1.8, 25, 1.0),
    stringsAsFactors = FALSE
  )
}

#' Names of the four shape parameters for a Partit entry
#'
#' @param name Character scalar. Entry name, the placeholder stem of
#'   `{{name_PARTIT}}` (for `Shoot_PARTIT`, `name` is `"Shoot"`).
#' @return Character vector of length 4, in the order [render_partit()]
#'   expects: storage-organ steepness and centre, then leaf steepness and
#'   centre.
#' @export
partit_shape_names <- function(name) {
  stopifnot(is.character(name), length(name) == 1L, nzchar(name))
  paste0(name, "_", .partit_shape_suffixes())
}

#' Leaf and stem shoot fractions from the four Partit sigmoids
#'
#' Storage-organ share of the shoot pool is a rising logistic in development
#' stage. Leaf's share of what remains is a falling logistic. Stem is the
#' rest of that remainder, so the two tables stay non-negative and sum to
#' at most 1. Daisy treats that sum as the shoot split and assigns the
#' remainder to the storage organ. Root is a separate fraction of total
#' assimilate and is not computed here.
#'
#' @param x Numeric vector of development-stage knots.
#' @param params Numeric vector of length 4: `sorg_steepness`, `sorg_centre`,
#'   `leaf_steepness`, `leaf_centre`.
#' @return A list with numeric vectors `leaf` and `stem`, each the same
#'   length as `x`.
#' @keywords internal
.partit_fractions <- function(x, params) {
  params <- as.numeric(params)
  suffixes <- .partit_shape_suffixes()
  if (length(params) != length(suffixes))
    stop(sprintf("Partit expects %d parameter(s) in order (%s), got %d",
                  length(suffixes), paste(suffixes, collapse = ", "), length(params)))
  sorg <- render_plf_curve("logistic", x, c(1, params[[1]], params[[2]]))
  leaf_share <- render_plf_curve("logistic", x, c(1, -params[[3]], params[[4]]))
  sorg <- pmin(pmax(sorg, 0), 1)
  leaf_share <- pmin(pmax(leaf_share, 0), 1)
  leaf <- (1 - sorg) * leaf_share
  stem <- (1 - sorg) - leaf
  list(leaf = leaf, stem = stem)
}

#' Render Daisy Leaf and Stem partitioning tables
#'
#' Builds the shoot half of a Daisy `Partit` block from four logistic
#' parameters. The returned text is what a `{{Name_PARTIT}}` placeholder
#' is replaced with: a `(Leaf ...)` table and a `(Stem ...)` table. Root
#' and `RSR` stay in the template, outside the placeholder.
#'
#' Parameter order is `sorg_steepness`, `sorg_centre`, `leaf_steepness`,
#' `leaf_centre`. Steepness should be positive, so the storage-organ share
#' rises from 0 toward 1 and the leaf share of the remaining shoot falls
#' from 1 toward 0. Centre is the development stage of each inflection.
#'
#' @param x Numeric vector of development-stage knots. Fixed, not calibrated.
#' @param params Numeric vector of length 4, in the order above.
#' @param format `sprintf()` format applied to each fraction, as in
#'   [format_plf_block()].
#'
#' @return Character scalar, two Daisy tables separated by a newline.
#' @export
#' @seealso [format_plf_block()], [read_param_config()]
#'
#' @examples
#' x <- seq(0, 2, by = 0.5)
#' render_partit(x, c(10, 1.2, 10, 0.6))
render_partit <- function(x, params, format = "%.4f") {
  fr <- .partit_fractions(x, params)
  paste0(
    sprintf("(Leaf %s)", format_plf_block(x, fr$leaf, format)),
    "\n    ",
    sprintf("(Stem %s)", format_plf_block(x, fr$stem, format))
  )
}
