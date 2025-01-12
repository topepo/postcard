#' @name rctglm_methods
#' @rdname rctglm_methods
#'
#' @title Methods for objects of class `rctglm`
#'
#' @description
#' Methods mostly to extract information from model fit and inference. See
#' details for more information on each method.
#'
#' @param x an object of class `rctglm`
#' @param object an object of class `rctglm`
#' @param digits a `numeric` with the number of digits to display when printing
#' @param ... additional arguments passed to methods
#'
#' @details
#' `estimand` and `se_estimand` are methods for extracting the estimated
#' estimand value as well as the standard error (SE) of the estimand.
#' Extract the plug-in estimation of the estimand, found by using the
#' estimand function on the predicted counterfactual means of each group.
#' These functions are a shortcut to extract the list elements `estimand`
#' and `se_estimand` of an `rctglm` class object.
#'
#' `summary` summarises information related to the estimand as well as
#' the underlying GLM fit.
#'
#' `coef` just use the corresponding `glm` methods on the `glm`
#' fit contained within the `rctglm` object. Thus, this function is a
#' shortcuts to running `coef(x$glm)`.
#'
#' @examples
#' # Generate some data to showcase example
#' n <- 100
#' dat_binom <- glm_data(
#'   1+1.5*X1+2*A,
#'   X1 = rnorm(n),
#'   A = rbinom(n, 1, .5),
#'   family = binomial()
#' )
#'
#' # Fit the model
#' ate <- rctglm(formula = Y ~ .,
#'               group_indicator = A,
#'               data = dat_binom,
#'               family = binomial,
#'               estimand_fun = "ate")
#'
#' print(ate)
#' estimand(ate)
#' se_estimand(ate)
#' summary(ate)
#' coef(ate)
NULL

#' @rdname rctglm_methods
#' @export
print.rctglm <- function(x,
                         digits = max(3L, getOption("digits") - 3L),
                         ...) {
  cat("\nObject of class 'rctglm'\n\n")
  cat("Call:  ",
      paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  cat("Counterfactual control mean (psi_0=E[Y|X, A=0]) estimate: ",
      format(x$counterfactual_mean0, digits = digits),
      "\n",
      sep = "")
  cat("Counterfactual control mean (psi_1=E[Y|X, A=1]) estimate: ",
      format(x$counterfactual_mean1, digits = digits),
      "\n",
      sep = "")
  print_estimand_info(x,
                      digits = max(3L, getOption("digits") - 3L),
                      ...)

  return(invisible())
}

#' @export
#' @rdname rctglm_methods
estimand <- function(object) {
  UseMethod("estimand")
}

#' @export
#' @rdname rctglm_methods
estimand.rctglm <- function(object) {
  object$estimand
}

#' @export
#' @rdname rctglm_methods
se_estimand <- function(object) {
  UseMethod("se_estimand")
}

#' @export
#' @rdname rctglm_methods
se_estimand.rctglm <- function(object) {
  object$se_estimand
}

#' @export
#' @rdname rctglm_methods
summary.rctglm <- function(object, ...) {
  sum <- list(estimand = object$estimand,
              se_estimand = object$se_estimand,
              var_estimand = object$var_estimand,
              estimand_fun = object$estimand_fun,
              group_indicator = object$group_indicator,
              call = object$call,
              glm_summary = summary(object$glm, ...))

  out <- structure(sum, class = "summary.rctglm")

  return(out)
}

#' @export
#' @noRd
print.summary.rctglm <- function(
    x,
    digits = max(3L, getOption("digits") - 3L),
    ...) {

  cat("\nCall:  ",
      paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")

  cli::cli_h2("Summary of estimand related statistics:")
  cat("Counterfactual means, psi0 and psi1, based on groups in column ",
      x$group_indicator,
      "\n",
      sep = "")
  print_estimand_info(x,
                      digits = max(3L, getOption("digits") - 3L),
                      ...)
  cat("\n")

  cli::cli_h2("Summary of glm fit:")
  glm_summary_without_call(x$glm_summary)

  return(invisible())
}

#' @export
#' @rdname rctglm_methods
coef.rctglm <- function(object, ...) {
  coef(object$glm)
}

print_estimand_info <- function(x,
                                digits = max(3L, getOption("digits") - 3L),
                                ...) {
  cat("Estimand function r: ",
      deparse_fun_body(x$estimand_fun),
      "\n",
      sep = "")
  cat("Estimand (r(psi_1, psi_0)) estimate (SE): ",
      format(x$estimand, digits = digits),
      " (",
      format(x$se_estimand, digits = digits),
      ")\n",
      sep = "")
  return(invisible())
}

# Utility functions for methods
glm_summary_without_call <- function(glm_summary) {
  glm_summary$call <- NULL
  glm_summary_as_str <- paste(
    utils::capture.output(
      print(glm_summary)
    ),
    collapse = "\n")
  cat(gsub("\nCall:\nNULL\n\n", "", glm_summary_as_str))
  return(invisible())
}
