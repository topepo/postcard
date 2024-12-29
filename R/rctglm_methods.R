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

#' Generic for extracting estimand
#'
#' @param x an object of some class to dispatch on
#' @param ... additional arguments passed to methods
#'
#' @export
estimand <- function(x, ...) {
  UseMethod("estimand")
}

#' @export
#' @noRd
estimand.rctglm <- function(x, ...) {
  x$estimand
}
