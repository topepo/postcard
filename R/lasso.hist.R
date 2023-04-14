#' LASSO regression as function of historical data
#'
#' See glmn::cv.glmnet for further details
#'
#' @param data.hist    Data.frame of the historical data used to build the regression model.
#' @param outcome.var  Character with the name of the outcome variable in both data.hist.
#' @param method.covs  Character vector with names of the covariates to use as baseline covariates in the prediction model. Make sure that categorical variables are considered as factors.
#'
#' @return
#' An object of class "cv.glmnet" is returned, which is a list with
#' the ingredients of the cross-validation fit. See further details of
#' glmnet::cv.glmnet.
#'
#' @importFrom dplyr select pull
#' @importFrom magrittr "%>%"
#' @importFrom glmnet cv.glmnet
#'
#' @export
#'
lasso.hist <- function(data.hist, outcome.var = "y", method.covs = c("x1", "x2")){

  glmnet::cv.glmnet(data.hist %>%
                      dplyr::select(method.covs) %>%
                      as.matrix(),
                    data.hist %>% dplyr::pull(outcome.var))
}
