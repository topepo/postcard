#' Estimation of prospective power for ANCOVA models
#'
#' @details
#' This function calculates the prospective power based on theoretical non-centrality parameter as well as Guenther-Schouten approximations,
#' using historical data provided in data.hist. The entities sigma, rho, and R2 are calculated using the historical data, and these can not
#' be specified by the user. Look at power.NC or power.GS if you want a power calculation based on user specified entities.
#' In addition, the necessary entities for these calculations are outputted.
#'
#' When wanting to estimate the power for the PROCOVA method add a column to data.hist with the predicted values using the prognostic model and add the
#' name of this column to the adj.covs. Note that data.hist should be independent of the data used to build the the prognostic model to obtain accurate
#' power estimations.
#'
#' @param data.hist       Data.frame of the historical data used to estimate sigma, rho, and R2 in the prospective power estimation.
#' @param outcome.var     Character with the name of the outcome variable data.hist.
#' @param treatment.var   Character with the name of the treatment variable data.hist.
#' @param adj.covs        Character vector with names of the covariates to adjust for as raw covariates in the ANCOVA model for estimating the ATE. Make sure that categorical variables are considered as factors.
#' @param interaction     Logical value, that determines whether to model interaction effects between covariates and treatment indicator when estimating R2.
#' @param n               Number of participants in total. From this number of participants in the treatment group is \eqn{n1=(r/(1+r))n} and the control group is \eqn{n1=(1/(1+r))n}.
#' @param r               Allocation ratio \eqn{r=n1/n0}. For one-to-one randomisation r=1.
#' @param ATE             Minimum effect size that we should be able to detect.
#' @param margin          Superiority margin (for non-inferiority margin, a negative value can be provided).
#'
#' @importFrom dplyr pull select all_of select_if
#' @importFrom magrittr "%>%"
#'
#' @return
#' Vector of the estimated entities sigma, rho/R2, power_NC, and power_GS.
#'
#' @export
#'
#'
power.ancova <- function(data.hist,
                         outcome.var = "y",
                         treatment.var = "w",
                         adj.covs = NULL,
                         interaction = NULL,
                         n,
                         r,
                         ATE,
                         margin){

  stopifnot(is.data.frame(data.hist), is.numeric(n), is.numeric(r), is.character(adj.covs) | is.null(adj.covs),)

  # Calculate entities for power estimation
  sigma <- data.hist %>% dplyr::pull(outcome.var) %>% var() %>% as.numeric()

  #### ANOVA
  if (is.null(adj.covs)) {
    power_NC <- power.NC(n = n, r = r, sigma = sigma, ATE = ATE, margin = margin, method = "ANOVA")
    power_GS <- power.GS(n = n, r = r, sigma = sigma, ATE = ATE, margin = margin, method = "ANOVA")

    prelim <- c(sigma, power_NC, power_GS) %>% setNames(c("sigma", "power_NC", "power_GS"))
  }

  #### ANCOVA with 1 covariate and no interaction between W and the covariate
  if (length(adj.covs) == 1 & interaction == FALSE) {
    rho <- cor(data.hist[, adj.covs], data.hist %>% dplyr::pull(outcome.var)) %>% as.numeric()

    power_NC <- power.NC(n = n, r = r, sigma = sigma, ATE = ATE, rho = rho, margin = margin, method = "ANCOVA")
    power_GS <- power.GS(n = n, r = r, sigma = sigma, ATE = ATE, rho = rho, margin = margin, method = "ANCOVA")

    prelim <- c(sigma, rho, power_NC, power_GS) %>% setNames(c("sigma", "rho", "power_NC", "power_GS"))
  }


  #### ANCOVA with p>1 covariates with or without interaction or p=1 with interaction=TRUE
  if (length(adj.covs) > 1 | interaction == TRUE) {

    if (interaction) {
      w <- data.hist %>% dplyr::pull(treatment.var)
      for (col in adj.covs) {
        data.hist[[paste0(col, "_w")]] <- data.hist[[col]] * w
      }
      new_cols <- paste0(adj.covs, "_w")
    }

    if (!interaction) {
      new_cols <- NULL
    }

    # If W=0 for all participants R2 is calculated as if interaction = FALSE
    Sigma_X.I <- data.hist %>% dplyr::select(dplyr::all_of(c(adj.covs, new_cols))) %>% dplyr::select_if(~ sum(.) != 0) %>% cov() %>% chol() %>% chol2inv()
    R2 <- ( cov(data.hist %>% dplyr::pull(outcome.var), data.hist[, c(adj.covs, new_cols)]) %*% Sigma_X.I %*% cov(data.hist[, c(adj.covs, new_cols)], data.hist %>% dplyr::pull(outcome.var)) ) / sigma
    R2 <- R2 %>% as.numeric()

    power_NC <- power.NC(n = n, r = r, sigma = sigma, ATE = ATE, R2 = R2, margin = margin, method = "ANCOVA")
    power_GS <- power.GS(n = n, r = r, sigma = sigma, ATE = ATE, R2 = R2, margin = margin, method = "ANCOVA")

    prelim <- c(sigma, R2, power_NC, power_GS) %>% setNames(c("sigma", "R2", "power_NC", "power_GS"))
  }

  return(prelim)

}
