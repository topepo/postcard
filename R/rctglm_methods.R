#' Methods for objects of class `rctglm`
#'
#' Methods mostly to extract information from model fit and inference. See
#' details for more information on each method.
#'
#' @param x an object of class `rctglm`
#' @param object an object of class `rctglm`
#' @param digits a `numeric` with the number of digits to display when printing
#' @param ... additional arguments passed to methods
#'
#' @details
#' `coef` and `summary` just use the corresponding `glm` methods on the `glm`
#' fit contained within the `rctglm` object. Thus, these functions are shortcuts
#' of running `coef(x$glm)` and `summary(x$glm)`.
#'
#' `estimand` and `se_estimand` are methods for extracting the estimated
#' estimand value as well as the standard error (SE) of the estimand.
#' Extract the plug-in estimation of the estimand, found by using the
#' estimand function on the predicted counterfactual means of each group.
#' These functions are a shortcut to extract the list elements `estimand`
#' and `se_estimand`of an `rctglm` class object.
#'
#' @rdname rctglm_methods
#' @export
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
#' coef(ate)
#' summary(ate)
print.rctglm <- function(x, digits = 3, ...) {
  cat("Object of class 'rctglm'\n\n")
  cat("Call:  ",
      paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  cat("- Counterfactual control mean (Psi_0=E[Y|X, A=0]) estimate: ",
      format(x$counterfactual_mean0, digits = digits),
      "\n",
      sep = "")
  cat("- Counterfactual control mean (Psi_1=E[Y|X, A=1]) estimate: ",
      format(x$counterfactual_mean1, digits = digits),
      "\n",
      sep = "")
  cat("- Estimand function r: ",
      deparse_fun_body(x$estimand_fun),
      "\n",
      sep = "")
  cat("- Estimand (r(Psi_1, Psi_0)) estimate (SE): ",
      format(x$estimand, digits = digits),
      " (",
      format(x$se_estimand, digits = digits),
      ")\n",
      sep = "")

  return(invisible())
}

#' @export
#' @rdname rctglm_methods
coef.rctglm <- function(object, ...) {
  coef(object$glm)
}

#' @export
#' @rdname rctglm_methods
summary.rctglm <- function(object, ...) {
  summary(object$glm)
}

#' @export
#' @rdname rctglm_methods
estimand <- function(x) {
  UseMethod("estimand")
}

#' @export
#' @rdname rctglm_methods
estimand.rctglm <- function(x) {
  x$estimand
}

#' @export
#' @rdname rctglm_methods
se_estimand <- function(x) {
  UseMethod("se_estimand")
}

#' @export
#' @rdname rctglm_methods
se_estimand.rctglm <- function(x) {
  x$se_estimand
}
