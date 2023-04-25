#' Find range of outcome variable
#' This function can be used to generate a random prediction model, in the sense that it
#' determines the range of the outcome variable in the historical data, and returns this
#' range as a object of class "random". Then predict.random randomly predicts the outcome
#' within this range.
#'
#'
#' @param data.hist  Data.frame of the historical data used to find the range of the outcome variable in order to randomly predict a value.
#'
#' @return
#' Returns a vector of the minimum and maximum value in data.hist with the class "random".
#'
#' @export
#'
#' @importFrom dplyr select
#'
#' @examples
#' data <- sim.lm(N.sim = 1, N.hist.control = 100, N.hist.treatment = 100,
#'               N.control = 50, N.treatment = 50)
#'
#' random.hist(data[[1]]$hist)
#'
#'
random.hist <- function(data.hist) {
  ran <- data.hist %>% dplyr::select("y") %>% range()
  class(ran) <- c("random", class(ran))
  return(ran)
}



