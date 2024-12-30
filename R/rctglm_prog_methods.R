#' Generic for extracting the fitted prognostic model
#'
#' Extract the fitted prognostic model.
#'
#' @inheritParams estimand
#'
#' @export
#' @examples
#' # Generate some data
#' n <- 100
#' b0 <- 1
#' b1 <- 1.5
#' b2 <- 2
#' W1 <- runif(n, min = -2, max = 2)
#'
#' dat_treat <- glm_data(
#'   b0+b1*abs(sin(W1))+b2*A,
#'   W1 = W1,
#'   A = rbinom (n, 1, .5)
#' )
#'
#' dat_notreat <- glm_data(
#'   b0+b1*abs(sin(W1)),
#'   W1 = W1
#' )
#'
#' ate <- rctglm_with_prognosticscore(
#'   formula = Y ~ .,
#'   group_indicator = A,
#'   data = dat_treat,
#'   family = gaussian(),
#'   estimand_fun = "ate",
#'   data_hist = dat_notreat)
#'
#' prog_model(ate)
prog_model <- function(x, ...) {
  UseMethod("prog_model")
}

#' @export
#' @rdname prog_model
prog_model.rctglm_prog <- function(x) {
  x$prognostic_info$model_fit
}
