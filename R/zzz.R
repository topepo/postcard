#' @eval options::as_roxygen_docs()
NULL

#' @eval options::as_params()
#' @name options
#'
NULL

options::define_option(
  "verbose",
  default = 2,
  desc = "`numeric` verbosity level specifying how much information should be printed to the user"
)
