#' @export
#' @noRd
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
#' @noRd
coef.rctglm <- function(object, ...) {
  coef(object$glm)
}

#' @export
#' @noRd
summary.rctglm <- function(object, ...) {
  summary(object$glm)
}

#' Generic for extracting estimand value
#'
#' Extract the plug-in estimation of the estimand, found by using the
#' estimand function on the predicted counterfactual means of each group.
#' This function is a shortcut to extract the list element `estimand` of
#' an `rctglm` class object.
#'
#' @param x an object of some class to dispatch on
#' @param ... additional arguments passed to methods
#'
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
#' estimand(ate)
estimand <- function(x, ...) {
  UseMethod("estimand")
}

#' @export
#' @rdname estimand
estimand.rctglm <- function(x) {
  x$estimand
}

#' Generic for extracting standard error (SE) of estimand
#'
#' Extract the standard error (SE) of the estimand. See more details in
#' [rctglm] and [estimand].
#' This function is a shortcut to extract the list element `se_estimand` of
#' an `rctglm` class object.
#'
#' @inheritParams estimand
#'
#' @export
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
#' se_estimand(ate)
se_estimand <- function(x, ...) {
  UseMethod("se_estimand")
}

#' @export
#' @rdname se_estimand
se_estimand.rctglm <- function(x) {
  x$se_estimand
}
