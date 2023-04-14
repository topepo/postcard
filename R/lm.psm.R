#' PSM for randomised clinical trials
#'
#' The function estimates the ATE using a procedure for one-to-one propensity score matching in the
#' setting of randomised clinical studies where data from multiple external control groups are available.
#' The procedure involves matching the patients in the treatment group to historical controls on
#' observed confounders, as well as estimating the outcome bias between control groups induced
#' by unobserved confounders. It is then possible to use this estimated bias between control groups
#' with the aim of removing the bias in the estimated treatment effect by implicitly taking these
#' unobserved confounders into account. The procedure was described by Lim et al. (Lim J, Walley R, Yuan J, Liu J, Dabral A, Best N, et al. Minimizing Patient Burden
#' Through the Use of Historical Subject-Level Data in Innovative Confirmatory Clinical Trials:
#' Review of Methods and Opportunities. Therapeutic Innovation & Regulatory Science. 2018;52(5):546–559.),
#' ensuring that all control arm patients from the current RCT are used in the treatment effect estimation in equation
#' \deqn{\widehat{ATT} = \frac{1}{n_1} \sum_{i:w_i=1} \left( y_i(1) - \widehat y_{i}(0) \right),}
#' where \eqn{\widehat y_{i}(0)} is the outcome of the matched control group patient to patient i.
#' The description of the procedure can be found in further details in the article by Lim et al.
#'
#' @param data.list          A list of elements $hist and $rct, which are both data.frames being historical and current RCT data sets, respectively.
#' @param margin             Superiority margin (for non-inferiority margin, a negative value can be provided).
#' @param alpha              Significance level. Due to regulatory guidelines when using a one-sided test, half the specified significance level is used. Thus, for standard alpha = .05, a significance level of 0.025 is used.
#' @param outcome.var        Character with the name of the outcome variable in both the $rct and $hist data set.
#' @param treatment.var      Character with the name of the outcome treatment indicator in both the $rct and $hist data set. Notice that the treatment variable should be an indicator with treatment == 1 and control == 0.
#' @param adj.covs           Character vector with names of the covariates to adjust for as raw covariates in the ANCOVA model for estimating the ATE. Make sure that categorical variables are considered as factors.
#' @param interaction        Logical value, that determines whether to model interaction effects between covariates and treatment indicator when estimating the ATE. For method = "PROCOVA", the prognostic score is regarded as an additional covariate and thus the interaction between the prognostic score and the treatment indicator is included.
#' @param B                  Only relevant for method = PSM. Number of bootstraps for estimating bias between HC and CC groups.
#' @param ...                Additionall arguments for MatchIt::matchit() which estimates the propensity scores (with a default of using a logistic regression). The default formula for the regression model is "w ~ . - y - HC" which is all the covariates in the data set except from the response y and indicator of being in the historical data HC.
#'
#'
#' @return
#' @export
#'
#' @importFrom dplyr rename mutate across case_when
#' @importFrom magrittr "%>%"
#' @importFrom MatchIt matchit
#' @import     randomForest
#' @importFrom Matching Match
#' @importFrom extraDistr rinvchisq
#'
#'
#'
lm.psm <- function(data.list,
                   margin = 0,
                   alpha = .05,
                   outcome.var = "y",
                   treatment.var = "w",
                   adj.covs = NULL,
                   interaction = FALSE,
                   B = 100,
                   ...){

  ####### Check if variables are defined correctly ##########
  stopifnot(is.numeric(margin), length(margin) == 1L,
            is.numeric(alpha), length(alpha) == 1L,
            is.character(outcome.var), length(outcome.var) == 1L,
            is.character(treatment.var), length(treatment.var) == 1L,
            is.character(adj.covs) | is.null(adj.covs),
            is.logical(interaction),
            is.numeric(B), length(B) == 1L)


  ####### Preliminary setting of variables and adjustment of data sets ##########
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

  hist <- data.list$hist
  hist <- hist[hist$w == 0, ] %>% dplyr::mutate(HC = 1) #HC = historical controls, and remove the historical treatment group patients, since the method is not developed for this.
  rct <- data.list$rct %>% dplyr::mutate(HC = 0)
  rct.dm <- data.list$rct.dm
  data.match <- rbind(rct, hist)

  n <- nrow(rct)
  n1 <- rct[rct$w == 1, ] %>% nrow()
  n0 <- rct[rct$w == 0, ] %>% nrow()
  n_hist <- hist %>%  nrow()
  n.adj <- length(adj.covs) + ifelse(interaction, length(adj.covs), 0)
  crit.val.t <- qt(1 - alpha/2, n - 2 - n.adj)

  if (n1 <= n0) {
    stop("The number of current treatment patients is lower than the number of
           current control patients. In order to use PSM the number of current controls
           should be lower than the number of current treatment patients.")
  }
  if (n_hist < n1 - n0) {
    stop("There is not enough historical control partients to match with the current
           treatment group patients to get 1:1 group sizes.")
  }

  # Calculate propensity score
  formula.psm <- formula(paste0("w ~ ", paste0(colnames(data.match)[!colnames(data.match) %in% c("w", "y", "HC")], collapse = "+")))
  md <- MatchIt::matchit(f = formula.psm, data = data.match, ...)
  data.match$prop_score <- md$distance

  # Find CC:AT group
  CC_AT <- data.match[data.match$HC == 0, ]
  # Swap treatment indicator, so treat = 1 for control, in order to match each patient in CC with one patient in AT
  m <- Matching::Match(Tr = 1 - CC_AT$w,
                       X = CC_AT$prop_score,
                       replace = F,
                       M = 1,
                       distance.tolerance = 0)

  CC.AT <- CC_AT[c(m$index.treated, m$index.control), ]
  AT <- CC_AT[-c(m$index.treated, m$index.control), ]

  # Find CC:HC group
  CC_HC <- data.match[data.match$HC == 1 | (data.match$HC == 0 & data.match$w == 0), ]
  m <- Matching::Match(Tr = 1 - CC_HC$HC,
                       X = CC_HC$prop_score,
                       replace = F,
                       M = 1,
                       distance.tolerance = 0)

  CC.HC <- CC_HC[c(m$index.treated, m$index.control), ]

  # Find AT:HC group (using leftout treated, not matched to CC)
  AT_HC <- AT %>%
    rbind(data.match[data.match$HC == 1, ])
  m <- Matching::Match(Tr = AT_HC$w,
                       X = AT_HC$prop_score,
                       replace = F,
                       M = 1,
                       distance.tolerance = 0)

  AT.HC <- AT_HC[c(m$index.treated, m$index.control), ]

  # Estimate bias in HC
  formula.input <- list(...)$formula
  if (!is.null(formula.input)) {
    my_terms <- attr(terms(formula.input), "term.labels")
  } else {
    my_terms <- attr(terms(formula.psm), "term.labels")
  }
  formula.bias  <- formula(paste0("y ~ HC +", paste0(my_terms, collapse = "+")))
  bias_sum <- lm(formula.bias, data = CC.HC) %>% summary
  sigma2 <- bias_sum$sigma^2

  n_c <- nrow(CC.HC)
  p <- length(my_terms)

  # Sampling estimates s^2 of sigma^2
  s2 <- extraDistr::rinvchisq(n = B, nu = n_c - p - 2, tau = sigma2)
  # Finding conditional variance of bias estimate
  eta_var <- coef(bias_sum)["HC", "Std. Error"]^2 / sigma2 * s2
  # Sample bias estimates using conditional variances
  bias_est <- rnorm(B, coef(bias_sum)["HC", "Estimate"], eta_var)

  # Subtract bias from response of HC patients
  data_m <- data_m.b <- rbind(AT.HC, CC.AT)

  formula.lm <- formula(paste0("y ~ w + ",dplyr::case_when(is.null(adj.covs) ~ "1",
                                                           !is.null(adj.covs) & interaction ~ paste0(paste0(adj.covs, collapse = " + "), " + ", paste0(adj.covs, "*w", collapse = " + ")),
                                                           T ~ paste0(adj.covs, collapse = " + "))))

  boot_est <- function(x){
    data_m.b$y[data_m.b$HC == 1] <- data_m$y[data_m$HC == 1] - x
    fit_b <- lm(formula.lm, data = data_m.b) %>% summary %>% coef
    list(estimate.b = fit_b["w", "Estimate"], std.err.b = fit_b["w", "Std. Error"])
  }

  l <- lapply(bias_est, FUN = boot_est)

  dat <- matrix(unlist(l), ncol = 2, byrow = T) %>%
    as.data.frame()
  colnames(dat) <- c("estimate.b", "std.err.b")

  estimate <- mean(dat$estimate.b)
  std.err <- sqrt(mean(dat$std.err.b^2) + (1 + 1/B) * 1/(B - 1) * sum((estimate - dat$estimate.b)^2))
  test_stat <- (estimate - margin)/std.err
  test <- test_stat > crit.val.t

  res <- list('estimate' = estimate, 'std.err' = std.err, 'test_stat' = test_stat, 'test_result' = test)


  return(res)
}

