#' ANCOVA models using historical data
#'
#' @description
#' The function fits ANCOVA models using the function \code{\link[stats]{lm}} but potentially leveraging a historical data set. Historical data can be
#' leveraged through the PROCOVA method and PSM. Setting method = "Procova" the functions will fit the pred.model to $hist and use this to predict
#' the outcome for $rct.
#'
#' If the prediction model is fitted separately the predicted values can be added to the $rct data set and then using method = "None" and adding
#' the name of the column with the predicted values to adj.covs will result in using the PROCOVA method. This could be the case when a model has
#' already been fitted to the historical data before the trial data is available.
#'
#' @param data.list          A list of elements $hist and $rct, which are both data.frames being historical and current RCT data sets, respectively. The object can also include hist_test for prospective power estimation when setting est.power = TRUE.
#' @param method             Method for using historical data. Options: None, PROCOVA, PSM. None refers to ANOVA (for adj.covs = NULL) and ANCOVA (for adj.covs specified). If method = "Procova" and data.list$hist contains treatment patients, then the ANCOVA model is adjusted by \eqn{(\hat{E[Y(0)|X]}, \hat{E[Y(1)|X]})}, that is the prediction for both control and treatment medicine.
#' @param margin             Superiority margin (for non-inferiority margin, a negative value can be provided).
#' @param alpha              Significance level. Due to regulatory guidelines when using a one-sided test, half the specified significance level is used. Thus, for standard alpha = .05, a significance level of 0.025 is used.
#' @param outcome.var        Character with the name of the outcome variable in both the $rct and $hist data set.
#' @param treatment.var      Character with the name of the outcome treatment indicator in both the $rct and $hist data set. Notice that the treatment variable should be an indicator with treatment == 1 and control == 0.
#' @param adj.covs           Character vector with names of the covariates to adjust for as raw covariates in the ANCOVA model for estimating the ATE. Make sure that categorical variables are considered as factors.
#' @param pred.model         Model object which should be a function of the historical data that fits a prediction model based on the baseline covariates. This is only needed for method == "PROCOVA". The model object obtained from the function should be a valid argument for \link[stats]{predict}, where newdata = data.list$rct and with the treatment variable equal to w and outcome variable equal to y. Note that if there is treatment patients in the historical data the prediction model should include treatment.var as a baseline covariate in order to predict \eqn{(E[Y(0)|X], E[Y(1)|X])}
#' @param interaction        Logical value, that determines whether to model interaction effects between covariates and treatment indicator when estimating the ATE. For method = "PROCOVA", the prognostic score is regarded as an additional covariate and thus the interaction between the prognostic score and the treatment indicator is included.
#' @param ...                For method = "Procova", this is extra arguments for stats::predict(). For example using lasso.hist there are some extra arguments s = "lambda.min" and newx = data.hist %>% dplyr::select(method.covs) %>% as.matrix() needed for using stats::predict. Note that in general newdata = data.list$rct for the predict function, however for models such as the glmnet, the new data is given as the input newx, and hence the newx should be provided as an additional argument for lm.hist.
#'
#' @return
#' lm.hist returns an object of \code{\link[base]{class}} "lm". The functions summary and \code{\link[stats]{anova}} are used to obtain and print
#' a summary and analysis of variance table of the results. The generic accessor functions coefficients, effects, fitted.values and residuals
#' extract various useful features of the value returned by lm. The object contains a list of the same components as an object of class "lm"
#' but with the extra component $test_margin, which is a list of the  t-test statistic value 'test_stat' and the result of the t-test 'test_result'
#' based on the superiority margin. Look at \code{\link[stats]{lm}} for further information on the class.
#'
#' @importFrom dplyr rename mutate across case_when
#' @importFrom magrittr "%>%"
#'
#' @export
#'
lm.procova <- function(data.list,
                       method = c("None", "PROCOVA"),
                       margin = 0,
                       alpha = .05,
                       outcome.var = "y",
                       treatment.var = "w",
                       adj.covs = NULL,
                       pred.model = NULL,
                       interaction = FALSE,
                       ...){

  ####### Check if variables are defined correctly ##########
  stopifnot(is.character(method),
            is.numeric(margin), length(margin) == 1L,
            is.numeric(alpha), length(alpha) == 1L,
            is.character(outcome.var), length(outcome.var) == 1L,
            is.character(treatment.var), length(treatment.var) == 1L,
            is.character(adj.covs) | is.null(adj.covs),
            is.function(pred.model) | is.null(pred.model),
            is.logical(interaction))

  ####### Preliminary setting of variables and adjustment of data sets ##########
  method <- tolower(method)

  if (!is.null(data.list$hist)) {
    # Renaming the treatment.var to w and outcome.var to y in hist
    stopifnot(is.data.frame(data.list$hist))
    data.list$hist <- data.list$hist %>% dplyr::rename(w = treatment.var, y = outcome.var)
  }

  if (is.null(data.list$rct)) {
    stop("data.list does not contain object called rct")
  } else {
    stopifnot(is.data.frame(data.list$rct))
    # Saving new list element as demeaned rct data. Used for estimating ATE with interaction effects
    data.list$rct.dm <- data.list$rct %>%
      dplyr::rename(w = treatment.var, y = outcome.var) %>%
      dplyr::mutate(dplyr::across(!c("w", "y"), function(x) x - mean(x)))

    data.list$rct <- data.list$rct %>%
      dplyr::rename(w = treatment.var, y = outcome.var)
  }

  ######## ANOVA or ANCOVA model without use of historical data #########
  if (method == "none") {
    n <- nrow(data.list$rct)
    n.adj <- length(adj.covs) + ifelse(interaction, length(adj.covs), 0)
    crit.val.t <- qt(1 - alpha/2, n - 2 - n.adj)
    rct.dm <- data.list$rct.dm

    ## Treatment estimate without use of historical data
    formula.ancova  <- formula(paste0("y ~ w + ",dplyr::case_when(
      is.null(adj.covs) ~ "1",
      !is.null(adj.covs) & interaction ~ paste0(paste0(adj.covs, collapse = " + "), " + ", paste0(adj.covs, "*w", collapse = " + ")),
      T ~ paste0(adj.covs, collapse = " + "))))

    mod_ANCOVA <- lm(formula = formula.ancova, data = rct.dm)
    s1 <- summary(mod_ANCOVA)
    estimate <- s1$coefficients["w", "Estimate"]
    std.err <- s1$coefficients["w", "Std. Error"]
    test_stat <- (estimate - margin)/std.err
    test <- test_stat > crit.val.t

    mod_ANCOVA$test_margin <- list('test_stat' = test_stat, 'test_result' = test)
  }

  ######## PROCOVA model #########
  if (method == "procova") {
    stopifnot(is.data.frame(data.list$hist), is.function(pred.model))

    n <- nrow(data.list$rct)
    n.adj <- length(adj.covs) + ifelse(interaction, (length(adj.covs) + 1), 0)
    n1_hist <- data.list$hist[data.list$hist$w == 1, ] %>% nrow()
    hist <- data.list$hist
    rct <- data.list$rct
    rct.dm <- data.list$rct.dm

    ## Check if there is 0 historical treatment patients
    if (n1_hist == 0) {
      crit.val.t <- qt(1 - alpha/2, n - 3 - n.adj)

      # Model build on non-demeaned baseline values and then the predicted values are determined from the rct and afterwards demeaned
      prediction_model <- pred.model(hist)
      rct.dm$pred <- predict(prediction_model, newdata = rct, ...)
      rct.dm$pred <- rct.dm$pred - mean(rct.dm$pred)

      # Formula used for estimating ATE with PROCOVA. pred (estimated prognostic score) is estimated using pred.model
      formula.PROCOVA  <- formula(paste0("y ~ w + pred +", dplyr::case_when(
        is.null(adj.covs) & !interaction ~ "1",
        !is.null(adj.covs) & interaction ~ paste0(paste0(adj.covs, collapse = " + "), "+", "pred*w", "+", paste0(adj.covs, "*w", collapse = " + ")),
        is.null(adj.covs) & interaction ~ "w*pred",
        T ~ paste0(adj.covs, collapse = " + "))))

      mod_ANCOVA <- lm(formula = formula.PROCOVA, data = rct.dm)
      s1 <- summary(mod_ANCOVA)
      estimate <- s1$coefficients["w", "Estimate"]
      std.err <- s1$coefficients["w", "Std. Error"]
      test_stat <- (estimate - margin)/std.err
      test <- test_stat > crit.val.t

      attr(mod_ANCOVA, "prediction_model") <- prediction_model
      mod_ANCOVA$test_margin <- list('test_stat' = test_stat, 'test_result' = test)
    }

    ## Check if there is there is any historical treatment patients
    if (n1_hist > 0) {
      crit.val.t <- qt(1 - alpha/2, n - 4 - n.adj) #-4 because we predict both as if they receive control and treatment

      # Model build on non-demeaned baseline values and then the predicted values are determined from the rct and afterwards demeaned
      prediction_model <- pred.model(hist)
      # Predict as if they receive treatment
      rct <- rct %>% dplyr::rename(w_actual = "w")
      rct <- rct %>% dplyr::mutate("w" = 1)
      rct.dm$pred1 <- predict(prediction_model, newdata = rct, ...)
      rct.dm$pred1 <- rct.dm$pred1 - mean(rct.dm$pred1)
      # Predict as if they receive control
      rct <- rct %>% dplyr::mutate("w" = 0)
      rct.dm$pred0 <- predict(prediction_model, newdata = rct, ...)
      rct.dm$pred0 <- rct.dm$pred0 - mean(rct.dm$pred0)

      if (identical(rct.dm$pred0,rct.dm$pred1)) {
        stop("The predicted values for treatment and control are equal.
             You probably didn't include the treatment indicator as a covariate
             in the pred.model.")
      }

      # Formula used for estimating ATE with PROCOVA. pred (estimated prognostic score) is estimated using pred.model
      formula.PROCOVA  <- formula(paste0("y ~ w + pred1 + pred0 + ", dplyr::case_when(
        is.null(adj.covs) & !interaction ~ "1",
        !is.null(adj.covs) & interaction ~ paste0(paste0(adj.covs, collapse = " + "), "+", "pred1*w + pred0*w", "+", paste0(adj.covs, "*w", collapse = " + ")),
        is.null(adj.covs) & interaction ~ "w*pred1 + w*pred0",
        T ~ paste0(adj.covs, collapse = " + "))))

      mod_ANCOVA <- lm(formula = formula.PROCOVA, data = rct.dm)
      s1 <- summary(mod_ANCOVA)
      estimate <- s1$coefficients["w", "Estimate"]
      std.err <- s1$coefficients["w", "Std. Error"]
      test_stat <- (estimate - margin)/std.err
      test <- test_stat > crit.val.t

      attr(mod_ANCOVA, "prediction_model") <- prediction_model
      mod_ANCOVA$test_margin <- list('test_stat' = test_stat, 'test_result' = test)
    }
  }

  attr(mod_ANCOVA, "crit.val.t") <- crit.val.t

  return(mod_ANCOVA)
}
