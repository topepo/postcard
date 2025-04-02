#' @eval options::as_roxygen_docs()
NULL

#' @eval options::as_params()
#' @name options
#'
NULL

options::define_option(
  "verbose",
  default = 2,
  desc = "`numeric` verbosity level. Higher values means more information is
  printed in console. A value of 0 means nothing is printed to console during
  execution"
)

#' @noRd
dummy <- function() {
  earth::earth
  xgboost::xgboost
}
