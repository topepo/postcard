#' Predict for class random
#'
#' @param object   An object of class random, for example generated from random.hist.
#' @param ...      additional arguments for the predict function.
#' @param newdata  The new data that the randomly predicted values should be determined for.
#'
#' @return
#' Returns a numeric object with the "predicted" values.
#'
#' @importFrom stats runif
#'
#' @export
#'
#' @examples
#' data <- sim.lm(N.sim = 1, N.hist.control = 100, N.hist.treatment = 100,
#'               N.control = 50, N.treatment = 50)
#'
#' object <- random.hist(data[[1]]$hist)
#'
#' predict(object, newdata = data[[1]]$rct)
#'
predict.random <- function(object, ..., newdata) {
  fit <- stats::runif(nrow(newdata), object[1], object[2])
  return(fit)
}



