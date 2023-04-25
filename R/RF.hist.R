#' Random forest regression as a function of the historical data
#'
#' @param data.hist       Data.frame of the historical data used to build the regression model.
#' @param method.covs     Character vector with names of the covariates to use as baseline covariates in the prediction model. Make sure that categorical variables are considered as factors. If the function is intended for building a prediction model used in lm.hist, where historical treatment patients are present, then use the treatment indicator in the method.covs to build the model.
#'
#' @return
#' A RF forest model of object "_ranger" and "model_fit". See further details of
#' parsnip::rand_forest.
#'
#' @export
#'
#' @importFrom dplyr select
#' @importFrom magrittr "%>%"
#' @importFrom parsnip rand_forest set_engine fit
#' @importFrom ranger ranger
#' @importFrom stats formula
#'
#' @examples
#' data <- sim.lm(N.sim = 1, N.hist.control = 100, N.hist.treatment = 100,
#'               N.control = 50, N.treatment = 50)
#'
#' object <- RF.hist(data[[1]]$hist)
#'
#' predict(object, new_data = data[[1]]$rct)
#'
RF.hist <- function(data.hist, method.covs = c("x1", "x2")){

  k <- length(method.covs)
  formula_RF <- stats::formula(paste0("y", " ~ ", paste0(method.covs, collapse = " + ")))

  rf1 <- parsnip::rand_forest(
    mode = "regression",
    mtry = sqrt(k), #based on literature
    trees = 500,
    min_n = 5
  ) %>%
    parsnip::set_engine(engine = "ranger", importance = 'impurity') %>%
    parsnip::fit(formula_RF , data.hist %>% dplyr::select("y", method.covs))

  return(rf1)
}
