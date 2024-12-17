#' @eval options::as_roxygen_docs()
NULL

#' @eval options::as_params()
#' @name options
#'
NULL

options::define_option(
  "verbose",
  default = 1,
  desc = "Verbosity level specifying how much information should be printed to the user. Allowed values are 1 and 0"
)
