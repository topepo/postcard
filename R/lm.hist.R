#' ANCOVA models using historical data
#'
#' @description
#' The function fits ANCOVA models using the function \link[stats]{lm} but potentially leveraging a historical data set. Historical data can be
#' leveraged through the PROCOVA method and PSM. Setting method = "Procova" the functions will fit the pred.model to $hist and use this to predict
#' the outcome for $rct.
#'
#' If the prediction model is fitted separately the predicted values can be added to the $rct data set and then using method = "None" and adding
#' the name of the column with the predicted values to adj.covs will result in using the PROCOVA method. This could be the case when a model has
#' already been fitted to the historical data before the trial data is available.
#'
#' @param data.list          A list of elements $hist and $rct, which are both data.frames being historical and current RCT data sets, respectively.
#' @param method             Method for using historical data. Options: None, PROCOVA, PSM. None refers to ANOVA (for adj.covs = NULL) and ANCOVA (for adj.covs specified).
#' @param sup.margin         Superiority margin (for non-inferiority margin, a negative value can be provided).
#' @param alpha              Significance level. Due to regulatory guidelines when using a one-sided test, half the specified significance level is used. Thus, for standard alpha = .05, a significance level of 2.5\% is used.
#' @param outcome.var        Character with the name of the outcome variable in both the $rct and $hist data set.
#' @param treatment.var      Character with the name of the outcome treatment indicator in both the $rct and $hist data set. Notice that the treatment variable should be an indicator with treatment == 1 and control == 0.
#' @param adj.covs           Character vector with names of the covariates to adjust for as raw covariates in the ANCOVA model for estimating the ATE. Make sure that categorical variables are considered as factors.
#' @param pred.model         Model object which should be a function of the historical data that fits a prediction model based on the baseline covariates. This is only needed for method == "PROCOVA" or method == "PSM". The model object obtained from the function should be a valid argument for \link[stats]{predict}.
#' @param interaction        Logical value, that determines whether to model interaction effects between covariates and treatment indicator when estimating the ATE. For method = "PROCOVA", the prognostic score is regarded as an additional covariate and thus the interaction between the prognostic score and the treatment indicator is included.
#'
#'
#' @return
#' lm.hist returns an object of \link[base]{class} "lm". The functions summary and \code{\link[stats:anova]{stats::anova()}} are used to obtain and print
#' a summary and analysis of variance table of the results. The generic accessor functions coefficients, effects, fitted.values and residuals
#' extract various useful features of the value returned by lm. The object contains a list of the same components as an object of class "lm"
#' but with the extra component $test_sup.margin, which is a list of the  t-test statistic value 'test_stat' and the result of the t-test 'test_result'
#' based on the superiority margin. Look at \href{https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/lm}{lm} for further information on the class.
#'
#' @importFrom dplyr rename select mutate case_when all_of select_if
#' @importFrom magrittr "%>%"
#'
#' @export
#'
lm.hist <- function(data.list,
                    method = c("None", "PROCOVA", "PSM"),
                    sup.margin = 0,
                    alpha = .05,
                    outcome.var = "y",
                    treatment.var = "w",
                    adj.covs = NULL,
                    pred.model = NULL,
                    interaction = FALSE){

  ####### Check if variables are defined correctly ##########
  stopifnot(is.character(method),
            is.numeric(sup.margin), length(sup.margin) == 1L,
            is.numeric(alpha), length(alpha) == 1L,
            is.character(outcome.var), length(outcome.var) == 1L,
            is.character(treatment.var), length(treatment.var) == 1L,
            is.character(adj.covs) | is.null(adj.covs),
            is.function(pred.model) | is.null(pred.model),
            is.logical(interaction))


  ####### Preliminary setting of variables and adjustment of data sets ##########
  method <- tolower(method)
  varnames <- colnames(data.list$hist[[1]]) %>% setdiff(outcome.var)

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
    rct <- data.list$rct.dm

    ## Treatment estimate without use of historical data
    formula.ancova  <- formula(paste0("y ~ w + ",dplyr::case_when(
      is.null(adj.covs) ~ "1",
      !is.null(adj.covs) & interaction ~ paste0(paste0(adj.covs, collapse = " + "), " + ", paste0(adj.covs, "*w", collapse = " + ")),
      T ~ paste0(adj.covs, collapse = " + "))))

    mod_ANCOVA <- lm(formula = formula.ancova, data = rct)
    s1 <- summary(mod_ANCOVA)
    estimate <- s1$coefficients["w", "Estimate"]
    std.err <- s1$coefficients["w", "Std. Error"]
    test_stat <- (estimate - sup.margin)/std.err
    test <- test_stat > crit.val.t

    mod_ANCOVA$test_sup.margin <- list('test_stat' = test_stat, 'test_result' = test)
  }


  ######## PROCOVA model #########
  if (method == "procova") {
    stopifnot(is.data.frame(data.list$hist), is.function(pred.model))

    n <- nrow(data.list$rct)
    n.adj <- length(adj.covs) + ifelse(interaction, (length(adj.covs) + 1), 0)
    crit.val.t <- qt(1 - alpha/2, n - 3 - n.adj)
    hist <- data.list$hist
    rct <- data.list$rct
    rct.dm <- data.list$rct.dm

    # Formula used for estimating ATE with PROCOVA. pred (estimated prognostic score) is estimated using pred.model
    formula.PROCOVA  <- formula(paste0("y ~ w + pred +", dplyr::case_when(
      is.null(adj.covs) & !interaction ~ "1",
      !is.null(adj.covs) & interaction ~ paste0(paste0(adj.covs, collapse = " + "), "+", "pred*w", "+", paste0(adj.covs, "*w", collapse = " + ")),
      is.null(adj.covs) & interaction ~ "w*pred",
      T ~ paste0(adj.covs, collapse = " + "))))

    if (pred.model == "linear"){


                        hist <- data.list$hist[[k]]
                        rct <- data.list$rct[[k]]
                        rct.dm <- data.list$rct.dm[[k]]

                        # Training prognostic model model (linear)
                        lm_fit <- lm(formula(paste0("y ~",
                                                    paste0("x",
                                                           method.covs,
                                                           collapse = "+"))),
                                     data = hist)

                        ## Treatment estimate with digital twins
                        rct.dm$pred <- predict(lm_fit, rct)
                        m <- mean(rct.dm$pred)
                        rct.dm$pred <- rct.dm$pred - m

                        mod_DT <- lm(formula = formula.DT, data = rct.dm)

                        s2 <- summary(mod_DT)

                        estimate <- s2$coefficients["w", "Estimate"]
                        std.err <- s2$coefficients["w", "Std. Error"]

                        test_stat <- (estimate-sup.margin)/std.err
                        power <- test_stat > crit.val.t



      out <- bind_rows(out)
    }

    if (pred.model == "lasso"){

      out <- mclapply(1:N.sim,
                      function(k){

                        hist <- data.list$hist[[k]]
                        rct <- data.list$rct[[k]]
                        rct.dm <- data.list$rct.dm[[k]]

                        # Training prognostic model (LASSO)
                        lasso_fit <- cv.glmnet(hist %>%
                                                 dplyr::select(varnames[method.covs]) %>%
                                                 as.matrix(),
                                               hist$y)

                        # Estimating parameters for preliminary power calculation using only historical data
                        if(est.power){

                          if (!is.null(data.list$hist2)){
                            hist <- data.list$hist2[[k]]
                          }
                          else {
                            stop("Additional list of historical data should be provided in data.list$hist2")
                          }

                          # Calculate entities for power estimation
                          n1 <- rct %>% filter(w==1) %>% nrow()
                          n0 <- rct %>% filter(w==0) %>% nrow()
                          r <- n1/n0

                          sigma2 <- var(data.list$hist2[[k]]$y)
                          data.list$hist2[[k]]$pred <- predict(lasso_fit, newx = data.list$hist2[[k]] %>%
                                                                 dplyr::select(varnames[method.covs]) %>%
                                                                 as.matrix(),
                                                               s = "lambda.min")
                          rho <- cor(data.list$hist2[[k]]$pred, data.list$hist2[[k]]$y)

                          nc <- sqrt(r/(1+r)^2 * (n1+n0)) * (ATE-sup.margin)/(sqrt(sigma2*(1-rho^2)))
                          power_nc <- 1 - pt(q = crit.val.t, df = n1+n0-3, ncp=nc)
                          power_FP <- pnorm( sqrt(r/(1+r)^2 * (ATE-sup.margin)^2/(sigma2*(1-rho^2)) * (n1+n0)) - qnorm(1-alpha/2))
                          power_GS <- pnorm( sqrt(r/(1+r)^2 * (ATE-sup.margin)^2/(sigma2*(1-rho^2)) * (n1+n0-qnorm(1-alpha/2)^2/2)) - qnorm(1-alpha/2))

                          prelim <- c(sigma2, rho, power_nc, power_FP, power_GS) %>% setNames(c("sigma2", "rho", "power_nc", "power_FP", "power_GS"))
                        }

                        ## Treatment estimate with digital twins
                        rct.dm$pred <- predict(lasso_fit, newx = rct %>%
                                                 dplyr::select(varnames[method.covs]) %>%
                                                 as.matrix(),
                                               s = "lambda.min")
                        m <- mean(rct.dm$pred)
                        rct.dm$pred <- rct.dm$pred - m

                        mod_DT <- lm(formula = formula.DT, data = rct.dm)

                        s2 <- summary(mod_DT)

                        estimate <- s2$coefficients["w", "Estimate"]
                        std.err <- s2$coefficients["w", "Std. Error"]
                        coverage <- (ATE <= estimate + crit.val.t * std.err &
                                       ATE >= estimate - crit.val.t * std.err)
                        MSE <- (ATE-estimate)^2
                        test_stat <- (estimate-sup.margin)/std.err
                        power <- test_stat > crit.val.t

                        test_stat <- ((estimate - (ATE-sup.margin)) - sup.margin)/std.err
                        type1.err <- test_stat > crit.val.t

                        if(L2){

                          N.covs <- attr(data.list, "N.covs")
                          coefs <- attr(data.list, "coefs")

                          # Calculating true prognostic score (as for pred.model == "oracle0")
                          rct0 <- rct %>%
                            mutate(w = 0)

                          ## Treatment estimate with digital twins
                          X <- model.matrix(formula(paste0("y ~ 1 +",
                                                           paste0("(",
                                                                  paste0("x", 1:N.covs, collapse = "+"),
                                                                  ")^2"),
                                                           "+",
                                                           paste0("I(x", 1:N.covs, "^2)", collapse = "+"),
                                                           "+",
                                                           paste0("I(x", 1:N.covs, "*w)", collapse = "+"))),
                                            data = rct0)
                          # Redefine the 1-column to be 0-column since this is what we multiply ATE by, and we should not
                          # add ATE in the predictions of Y(0)
                          X[,1] <- 0
                          rct.dm$progscore <- X %*% c(ATE, rep(coefs[2], N.covs), rep(coefs[1], N.covs),
                                                      rep(coefs[3], N.covs), rep(coefs[1], ncol(X)-(3*N.covs+1)))

                          rct.dm$progscore <- rct.dm$progscore - m # Demeaning with mean of predicted progscores (making them comparable for evaluating squared difference)

                          L2 <- mean((rct.dm$pred - rct.dm$progscore)^2)
                        }

                        if (L2 & est.power){
                          c(estimate, std.err, coverage, MSE, power, type1.err, L2, prelim) %>%
                            setNames(c("estimate", "std.err", "coverage", "MSE", "power", "type1.err", "L2", names(prelim)))
                        } else if (L2){
                          c(estimate, std.err, coverage, MSE, power, type1.err, L2) %>%
                            setNames(c("estimate", "std.err", "coverage", "MSE", "power", "type1.err", "L2"))
                        } else if (est.power){
                          c(estimate, std.err, coverage, MSE, power, type1.err, prelim) %>%
                            setNames(c("estimate", "std.err", "coverage", "MSE", "power", "type1.err", names(prelim)))
                        } else {
                          c(estimate, std.err, coverage, MSE, power, type1.err) %>%
                            setNames(c("estimate", "std.err", "coverage", "MSE", "power", "type1.err"))
                        }
                      },  mc.cores = N.cores)

      out <- bind_rows(out)
    }

    if (pred.model == "rf"){
      out <- mclapply(1:N.sim,
                      function(k){

                        hist <- data.list$hist[[k]]
                        rct <- data.list$rct[[k]]
                        rct.dm <- data.list$rct.dm[[k]]

                        # Training prognostic model (RF)
                        RF_fit <- randomForest(formula(paste0("y ~",
                                                              paste0("x",
                                                                     method.covs,
                                                                     collapse = "+"))),
                                               ntree = 500,
                                               data = hist)

                        # Estimating parameters for preliminary power calculation using only historical data
                        if(est.power){

                          if (!is.null(data.list$hist2)){
                            hist <- data.list$hist2[[k]]
                          } else {
                            stop("Additional list of historical data should be provided in data.list$hist2")
                          }

                          # Calculate entities for power estimation
                          n1 <- rct %>% filter(w==1) %>% nrow()
                          n0 <- rct %>% filter(w==0) %>% nrow()
                          r <- n1/n0

                          sigma2 <- var(hist$y)
                          hist$pred <- predict(RF_fit, hist)
                          rho <- cor(hist$pred, hist$y)

                          nc <- sqrt(r/(1+r)^2 * (n1+n0)) * (ATE-sup.margin)/(sqrt(sigma2*(1-rho^2)))
                          power_nc <- 1 - pt(q = crit.val.t, df = n1+n0-3, ncp=nc)
                          power_FP <- pnorm( sqrt(r/(1+r)^2 * (ATE-sup.margin)^2/(sigma2*(1-rho^2)) * (n1+n0)) - qnorm(1-alpha/2))
                          power_GS <- pnorm( sqrt(r/(1+r)^2 * (ATE-sup.margin)^2/(sigma2*(1-rho^2)) * (n1+n0-qnorm(1-alpha/2)^2/2)) - qnorm(1-alpha/2))

                          prelim <- c(sigma2, rho, power_nc, power_FP, power_GS) %>% setNames(c("sigma2", "rho", "power_nc", "power_FP", "power_GS"))
                        }

                        ## Treatment estimate with digital twins
                        rct.dm$pred <- predict(RF_fit, rct)
                        m <- mean(rct.dm$pred)
                        rct.dm$pred <- rct.dm$pred - m

                        mod_DT <- lm(formula = formula.DT, data = rct.dm)

                        s2 <- summary(mod_DT)

                        estimate <- s2$coefficients["w", "Estimate"]
                        std.err <- s2$coefficients["w", "Std. Error"]
                        coverage <- (ATE <= estimate + crit.val.t * std.err &
                                       ATE >= estimate - crit.val.t * std.err)
                        MSE <- (ATE-estimate)^2
                        test_stat <- (estimate-sup.margin)/std.err
                        power <- test_stat > crit.val.t

                        test_stat <- ((estimate - (ATE-sup.margin)) - sup.margin)/std.err
                        type1.err <- test_stat > crit.val.t

                        if(L2){

                          N.covs <- attr(data.list, "N.covs")
                          coefs <- attr(data.list, "coefs")

                          # Calculating true prognostic score (as for pred.model == "oracle0")
                          rct0 <- rct %>% mutate(w = 0)

                          ## Treatment estimate with digital twins
                          X <- model.matrix(formula(paste0("y ~ 1 +",
                                                           paste0("(",
                                                                  paste0("x", 1:N.covs, collapse = "+"),
                                                                  ")^2"),
                                                           "+",
                                                           paste0("I(x", 1:N.covs, "^2)", collapse = "+"),
                                                           "+",
                                                           paste0("I(x", 1:N.covs, "*w)", collapse = "+"))),
                                            data = rct0)
                          # Redefine the 1-column to be 0-column since this is what we multiply ATE by, and we should not
                          # add ATE in the predictions of Y(0)
                          X[,1] <- 0
                          rct.dm$progscore <- X %*% c(ATE, rep(coefs[2], N.covs), rep(coefs[1], N.covs),
                                                      rep(coefs[3], N.covs), rep(coefs[1], ncol(X)-(3*N.covs+1)))

                          rct.dm$progscore <- rct.dm$progscore - m # Demeaning with mean of predicted progscores (making them comparable for evaluating squared difference)

                          L2 <- mean((rct.dm$pred - rct.dm$progscore)^2)
                        }

                        if (L2 & est.power){
                          c(estimate, std.err, coverage, MSE, power, type1.err, L2, prelim) %>%
                            setNames(c("estimate", "std.err", "coverage", "MSE", "power", "type1.err", "L2", names(prelim)))
                        } else if (L2){
                          c(estimate, std.err, coverage, MSE, power, type1.err, L2) %>%
                            setNames(c("estimate", "std.err", "coverage", "MSE", "power", "type1.err", "L2"))
                        } else if (est.power){
                          c(estimate, std.err, coverage, MSE, power, type1.err, prelim) %>%
                            setNames(c("estimate", "std.err", "coverage", "MSE", "power", "type1.err", names(prelim)))
                        } else {
                          c(estimate, std.err, coverage, MSE, power, type1.err) %>%
                            setNames(c("estimate", "std.err", "coverage", "MSE", "power", "type1.err"))
                        }
                      },  mc.cores = N.cores)

      out <- bind_rows(out)
    }

    if (pred.model == "oracle"){

      N.covs <- attr(data.list, "N.covs")
      coefs <- attr(data.list, "coefs")

      formula.DT  <- formula(paste0("y ~ w + pred1 + pred0 +",
                                    case_when(is.null(adj.covs) ~ "1",
                                              !is.null(adj.covs) & interaction == T ~
                                                paste0(paste0(varnames[adj.covs], collapse = " + "),
                                                       "+",
                                                       paste0("pred", 0:1, "*w", collapse = "+"),
                                                       "+",
                                                       paste0(varnames[adj.covs], "*w", collapse = " + ")),
                                              T ~ paste0(varnames[adj.covs], collapse = " + "))))

      crit.val.t <- qt(1 - alpha/2, nrow(data.list$rct[[1]])-3-1-length(adj.covs)-ifelse(interaction,
                                                                                         (length(adj.covs)+2),
                                                                                         0))

      out <- mclapply(1:N.sim,
                      function(k){

                        rct <- data.list$rct[[k]]
                        rct.dm <- data.list$rct.dm[[k]]

                        n1 <- rct %>% filter(w==1) %>% nrow()
                        n0 <- rct %>% filter(w==0) %>% nrow()
                        r <- n1/n0

                        # Estimating parameters for preliminary power calculation using only historical data
                        if(est.power){

                          if (!is.null(data.list$hist2)){
                            hist <- data.list$hist2[[k]]
                            hist0 <- hist %>% mutate(w=0)
                            hist1 <- hist %>% mutate(w=1)
                          } else {
                            stop("Additional list of historical data should be provided in data.list$hist2")
                          }

                          # Calculate entities for power estimation
                          sigma2 <- var(hist$y)

                          X <- model.matrix(formula(paste0("y ~ 1 +",
                                                           paste0("(",
                                                                  paste0("x", 1:N.covs, collapse = "+"),
                                                                  ")^2"),
                                                           "+",
                                                           paste0("I(x", 1:N.covs, "^2)", collapse = "+"),
                                                           "+",
                                                           paste0("I(x", 1:N.covs, "*w)", collapse = "+"))),
                                            data = hist1)

                          hist$pred1 <- X %*% c(ATE, rep(coefs[2], N.covs), rep(coefs[1], N.covs),
                                                rep(coefs[3], N.covs), rep(coefs[1], ncol(X)-(3*N.covs+1)))
                          hist$pred1 <- hist$pred1 - mean(hist$pred1)

                          X <- model.matrix(formula(paste0("y ~ -1 +",
                                                           paste0("(",
                                                                  paste0("x", 1:N.covs, collapse = "+"),
                                                                  ")^2"),
                                                           "+",
                                                           paste0("I(x", 1:N.covs, "^2)", collapse = "+"))),
                                            data = hist0)

                          hist$pred0 <- X %*% c(rep(coefs[2], N.covs), rep(coefs[1], ncol(X)-N.covs))
                          hist$pred0 <- hist$pred0 - mean(hist$pred0)

                          if (coefs[3]==0){ # If this is the case, Sigma_X becomes singular
                            rho <- cor(hist$pred0, hist$y)

                            nc <- sqrt(r/(1+r)^2 * (n1+n0)) * (ATE-sup.margin)/(sqrt(sigma2*(1-rho^2)))
                            power_nc <- 1 - pt(q = crit.val.t, df = n1+n0-3, ncp=nc)
                            power_FP <- pnorm( sqrt(r/(1+r)^2 * (ATE-sup.margin)^2/(sigma2*(1-rho^2)) * (n1+n0)) - qnorm(1-alpha/2))
                            power_GS <- pnorm( sqrt(r/(1+r)^2 * (ATE-sup.margin)^2/(sigma2*(1-rho^2)) * (n1+n0-qnorm(1-alpha/2)^2/2)) - qnorm(1-alpha/2))

                            prelim <- c(sigma2, rho, power_nc, power_FP, power_GS) %>% setNames(c("sigma2", "rho", "power_nc", "power_FP", "power_GS"))

                          } else {
                            Sigma_X.I <- hist %>% dplyr::select(pred0, pred1) %>% cov() %>% chol() %>% chol2inv()
                            R2 <- ( cov(hist$y, hist[, c("pred0", "pred1")]) %*% Sigma_X.I %*% cov(hist[, c("pred0", "pred1")], hist$y) ) / var(hist$y)

                            power_FP <- pnorm( sqrt(r/(1+r)^2 * (ATE-sup.margin)^2/(sigma2*(1-R2)) * (n1+n0)) - qnorm(1-alpha/2))
                            power_GS <- pnorm( sqrt(r/(1+r)^2 * (ATE-sup.margin)^2/(sigma2*(1-R2)) * (n1+n0-qnorm(1-alpha/2)^2/2)) - qnorm(1-alpha/2))

                            prelim <- c(sigma2, R2, power_FP, power_GS) %>% setNames(c("sigma2", "R2", "power_FP", "power_GS"))
                          }
                        }

                        rct1 <- rct %>% mutate(w = 1)
                        rct0 <- rct %>% mutate(w = 0)

                        ## Treatment estimate with digital twins
                        X <- model.matrix(formula(paste0("y ~ 1 +",
                                                         paste0("(",
                                                                paste0("x", 1:N.covs, collapse = "+"),
                                                                ")^2"),
                                                         "+",
                                                         paste0("I(x", 1:N.covs, "^2)", collapse = "+"),
                                                         "+",
                                                         paste0("I(x", 1:N.covs, "*w)", collapse = "+"))),
                                          data = rct1)

                        rct.dm$pred1 <- X %*% c(ATE, rep(coefs[2], N.covs), rep(coefs[1], N.covs),
                                                rep(coefs[3], N.covs), rep(coefs[1], ncol(X)-(3*N.covs+1)))
                        rct.dm$pred1 <- rct.dm$pred1 - mean(rct.dm$pred1)

                        X <- model.matrix(formula(paste0("y ~ -1 +",
                                                         paste0("(",
                                                                paste0("x", 1:N.covs, collapse = "+"),
                                                                ")^2"),
                                                         "+",
                                                         paste0("I(x", 1:N.covs, "^2)", collapse = "+"))),
                                          data = rct0)

                        rct.dm$pred0 <- X %*% c(rep(coefs[2], N.covs), rep(coefs[1], ncol(X)-N.covs))
                        rct.dm$pred0 <- rct.dm$pred0 - mean(rct.dm$pred0)

                        mod_DT <- lm(formula = formula.DT, data = rct.dm)

                        s2 <- summary(mod_DT)

                        estimate <- s2$coefficients["w", "Estimate"]
                        std.err <- s2$coefficients["w", "Std. Error"]
                        coverage <- (ATE <= estimate + crit.val.t * std.err &
                                       ATE >= estimate - crit.val.t * std.err)
                        MSE <- (ATE-estimate)^2
                        test_stat <- (estimate-sup.margin)/std.err
                        power <- test_stat > crit.val.t

                        test_stat <- ((estimate - (ATE-sup.margin)) - sup.margin)/std.err
                        type1.err <- test_stat > crit.val.t

                        # # Getting results
                        # c(estimate, std.err, coverage, MSE, power, type1.err) %>%
                        #   setNames(c("estimate", "std.err", "coverage", "MSE", "power", "type1.err"))

                        if (est.power){
                          c(estimate, std.err, coverage, MSE, power, type1.err, prelim) %>%
                            setNames(c("estimate", "std.err", "coverage", "MSE", "power", "type1.err", names(prelim)))
                        } else {
                          c(estimate, std.err, coverage, MSE, power, type1.err) %>%
                            setNames(c("estimate", "std.err", "coverage", "MSE", "power", "type1.err"))
                        }
                      },  mc.cores = N.cores)

      out <- bind_rows(out)
    }

    if (pred.model == "oracle0"){

      N.covs <- attr(data.list, "N.covs")
      coefs <- attr(data.list, "coefs")

      out <- mclapply(1:N.sim,
                      function(k){

                        rct <- data.list$rct[[k]]
                        rct.dm <- data.list$rct.dm[[k]]
                        rct0 <- rct %>% mutate(w = 0)

                        # Estimating parameters for preliminary power calculation using only historical data
                        if(est.power){

                          if (!is.null(data.list$hist2)){
                            hist <- data.list$hist2[[k]] %>% mutate(w=0)
                          } else {
                            stop("Additional list of historical data should be provided in data.list$hist2")
                          }

                          # Calculate entities for power estimation
                          sigma2 <- var(hist$y)

                          X <- model.matrix(formula(paste0("y ~ 1 +",
                                                           paste0("(",
                                                                  paste0("x", 1:N.covs, collapse = "+"),
                                                                  ")^2"),
                                                           "+",
                                                           paste0("I(x", 1:N.covs, "^2)", collapse = "+"),
                                                           "+",
                                                           paste0("I(x", 1:N.covs, "*w)", collapse = "+"))),
                                            data = hist)
                          # Redefine the 1-column to be 0-column since this is what we multiply ATE by, and we should not
                          # add ATE in the predictions of Y(0)
                          X[,1] <- 0
                          hist$pred <- X %*% c(ATE, rep(coefs[2], N.covs), rep(coefs[1], N.covs),
                                               rep(coefs[3], N.covs), rep(coefs[1], ncol(X)-(3*N.covs+1)))

                          rho <- cor(hist$pred, hist$y)

                          n1 <- rct %>% filter(w==1) %>% nrow()
                          n0 <- rct %>% filter(w==0) %>% nrow()
                          r <- n1/n0

                          nc <- sqrt(r/(1+r)^2 * (n1+n0)) * (ATE-sup.margin)/(sqrt(sigma2*(1-rho^2)))
                          power_nc <- 1 - pt(q = crit.val.t, df = n1+n0-3, ncp=nc)
                          power_FP <- pnorm( sqrt(r/(1+r)^2 * (ATE-sup.margin)^2/(sigma2*(1-rho^2)) * (n1+n0)) - qnorm(1-alpha/2))
                          power_GS <- pnorm( sqrt(r/(1+r)^2 * (ATE-sup.margin)^2/(sigma2*(1-rho^2)) * (n1+n0-qnorm(1-alpha/2)^2/2)) - qnorm(1-alpha/2))

                          prelim <- c(sigma2, rho, power_nc, power_FP, power_GS) %>% setNames(c("sigma2", "rho", "power_nc", "power_FP", "power_GS"))
                        }

                        ## Treatment estimate with digital twins
                        X <- model.matrix(formula(paste0("y ~ 1 +",
                                                         paste0("(",
                                                                paste0("x", 1:N.covs, collapse = "+"),
                                                                ")^2"),
                                                         "+",
                                                         paste0("I(x", 1:N.covs, "^2)", collapse = "+"),
                                                         "+",
                                                         paste0("I(x", 1:N.covs, "*w)", collapse = "+"))),
                                          data = rct0)
                        # Redefine the 1-column to be 0-column since this is what we multiply ATE by, and we should not
                        # add ATE in the predictions of Y(0)
                        X[,1] <- 0
                        rct.dm$pred <- X %*% c(ATE, rep(coefs[2], N.covs), rep(coefs[1], N.covs),
                                               rep(coefs[3], N.covs), rep(coefs[1], ncol(X)-(3*N.covs+1)))

                        rct.dm$pred <- rct.dm$pred - mean(rct.dm$pred)

                        mod_DT <- lm(formula = formula.DT, data = rct.dm)

                        s2 <- summary(mod_DT)

                        estimate <- s2$coefficients["w", "Estimate"]
                        std.err <- s2$coefficients["w", "Std. Error"]
                        coverage <- (ATE <= estimate + crit.val.t * std.err &
                                       ATE >= estimate - crit.val.t * std.err)
                        MSE <- (ATE-estimate)^2
                        test_stat <- (estimate-sup.margin)/std.err
                        power <- test_stat > crit.val.t

                        test_stat <- ((estimate - (ATE-sup.margin)) - sup.margin)/std.err
                        type1.err <- test_stat > crit.val.t

                        if (est.power){
                          c(estimate, std.err, coverage, MSE, power, type1.err, prelim) %>%
                            setNames(c("estimate", "std.err", "coverage", "MSE", "power", "type1.err", names(prelim)))
                        } else {
                          c(estimate, std.err, coverage, MSE, power, type1.err) %>%
                            setNames(c("estimate", "std.err", "coverage", "MSE", "power", "type1.err"))
                        }

                      },  mc.cores = N.cores)

      out <- bind_rows(out)
    }

    if (pred.model == "random"){# In this case we use uniformly simulated predictions

      out <- mclapply(1:N.sim,
                      function(k){

                        rct <- data.list$rct[[k]]
                        rct.dm <- data.list$rct.dm[[k]]

                        # "Training" prognostic model (random prediction in range of outcomes)
                        ran <- rct %>% filter(w == 0) %>% dplyr::select(y) %>% range

                        # Estimating parameters for preliminary power calculation using only historical data
                        if(est.power){

                          if (!is.null(data.list$hist2)){
                            hist <- data.list$hist2[[k]]
                          } else {
                            stop("Additional list of historical data should be provided in data.list$hist2")
                          }

                          # Calculate entities for power estimation
                          n1 <- rct %>% filter(w==1) %>% nrow()
                          n0 <- rct %>% filter(w==0) %>% nrow()
                          r <- n1/n0

                          sigma2 <- var(hist$y)
                          hist$pred <- runif(nrow(hist), ran[1], ran[2]) # NB: range is obtained from RCT data.....
                          rho <- cor(hist$pred, hist$y)

                          nc <- sqrt(r/(1+r)^2 * (n1+n0)) * (ATE-sup.margin)/(sqrt(sigma2*(1-rho^2)))
                          power_nc <- 1 - pt(q = crit.val.t, df = n1+n0-3, ncp=nc)
                          power_FP <- pnorm( sqrt(r/(1+r)^2 * (ATE-sup.margin)^2/(sigma2*(1-rho^2)) * (n1+n0)) - qnorm(1-alpha/2))
                          power_GS <- pnorm( sqrt(r/(1+r)^2 * (ATE-sup.margin)^2/(sigma2*(1-rho^2)) * (n1+n0-qnorm(1-alpha/2)^2/2)) - qnorm(1-alpha/2))

                          prelim <- c(sigma2, rho, power_nc, power_FP, power_GS) %>% setNames(c("sigma2", "rho", "power_nc", "power_FP", "power_GS"))
                        }

                        ## Treatment estimate with digital twins
                        rct.dm$pred <- runif(nrow(rct), ran[1], ran[2])
                        m <- mean(rct.dm$pred)
                        rct.dm$pred <- rct.dm$pred - m
                        mod_DT <- lm(formula = formula.DT, data = rct.dm)

                        s2 <- summary(mod_DT)

                        estimate <- s2$coefficients["w", "Estimate"]
                        std.err <- s2$coefficients["w", "Std. Error"]
                        coverage <- (ATE <= estimate + crit.val.t * std.err &
                                       ATE >= estimate - crit.val.t * std.err)
                        MSE <- (ATE-estimate)^2
                        test_stat <- (estimate-sup.margin)/std.err
                        power <- test_stat > crit.val.t

                        test_stat <- ((estimate - (ATE-sup.margin)) - sup.margin)/std.err
                        type1.err <- test_stat > crit.val.t

                        if(L2){

                          N.covs <- attr(data.list, "N.covs")
                          coefs <- attr(data.list, "coefs")

                          # Calculating true prognostic score (as for pred.model == "oracle0")
                          rct0 <- rct %>% mutate(w = 0)

                          ## Treatment estimate with digital twins
                          X <- model.matrix(formula(paste0("y ~ 1 +",
                                                           paste0("(",
                                                                  paste0("x", 1:N.covs, collapse = "+"),
                                                                  ")^2"),
                                                           "+",
                                                           paste0("I(x", 1:N.covs, "^2)", collapse = "+"),
                                                           "+",
                                                           paste0("I(x", 1:N.covs, "*w)", collapse = "+"))),
                                            data = rct0)
                          # Redefine the 1-column to be 0-column since this is what we multiply ATE by, and we should not
                          # add ATE in the predictions of Y(0)
                          X[,1] <- 0
                          rct.dm$progscore <- X %*% c(ATE, rep(coefs[2], N.covs), rep(coefs[1], N.covs),
                                                      rep(coefs[3], N.covs), rep(coefs[1], ncol(X)-(3*N.covs+1)))

                          rct.dm$progscore <- rct.dm$progscore - m # Demeaning with mean of predicted progscores (making them comparable for evaluating squared difference)

                          L2 <- mean((rct.dm$pred - rct.dm$progscore)^2)
                        }

                        if (L2 & est.power){
                          c(estimate, std.err, coverage, MSE, power, type1.err, L2, prelim) %>%
                            setNames(c("estimate", "std.err", "coverage", "MSE", "power", "type1.err", "L2", names(prelim)))
                        } else if (L2){
                          c(estimate, std.err, coverage, MSE, power, type1.err, L2) %>%
                            setNames(c("estimate", "std.err", "coverage", "MSE", "power", "type1.err", "L2"))
                        } else if (est.power){
                          c(estimate, std.err, coverage, MSE, power, type1.err, prelim) %>%
                            setNames(c("estimate", "std.err", "coverage", "MSE", "power", "type1.err", names(prelim)))
                        } else {
                          c(estimate, std.err, coverage, MSE, power, type1.err) %>%
                            setNames(c("estimate", "std.err", "coverage", "MSE", "power", "type1.err"))
                        }
                      },  mc.cores = N.cores)

      out <- bind_rows(out)
    }
  }


  return(mod_ANCOVA)
}
