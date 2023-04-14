#' Predict for class random
#'
#' @param ran      object of class random.
#' @param newdata  The new data that
#'
#' @return
#' Returns a numeric object with the "predicted" values.
#'
#' @export
#'
predict.random <- function(ran, newdata) {
  fit <- runif(nrow(newdata), ran[1], ran[2])
  return(fit)
}
