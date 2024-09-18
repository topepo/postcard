#' Generate pairs of historical and current data
#'
#' Wrapper of [simulate_collection] where covariates and the outcome in both historical and current data is modelled using a normal linear distribution as \eqn{Y(w) \sim N(a^\top X\!\!\!:\!\! \!X + b^\top X + c^{\top} X W + \text{ATE}\cdot W,  \sigma^2)}
#'
#' @param ATE               The average treatment effect \eqn{ATE = \mathbb{E}[Y(1)] - \mathbb{E}[Y(0)] } data is generated from
#' @param ATE.shift         ATE shift in the historical data, when historical data contains treatment group patients
#' @param N.covs            Number of covariates in the data generating process, indexed as x1,...,x_N.covs
#' @param N.overspec        Number of overspecified covariates (generates covariates not used in the data generating process, but can be used in model specifications to simulate overspecification). They are indexed as x_\{N.covs+1\},..., x_\{N.covs+N.overspec\}.
#' @param coefs             Coefficients for a, b and c in \eqn{Y(w) \sim N(aX^TX+bX+cXW+ATE\cdot W)}
#' @param mu.shift          Mean shift in the historical data "away" from mean 0 in rct data (to simulate skewed historical distribution relative to RCT data).
#' @param cov               Covariance structure of normally distributed covariates (simultaneous between "true" and overspecified covariates)
#' @param noise             Std. err of error term in generated data
#' @param N.hist.control    Number of patients in historical control groups
#' @param N.hist.treatment  Number of patients in historical treatment groups
#' @param N.test.control    Additional number of historical control group patients having same distribution as "hist" (if provided, these can be used for preliminary power estimation; see power.est argument for lm.hist)
#' @param N.test.treatment  Additional number of historical treatment group patients having same distribution as "hist" (if provided, these can be used for preliminary power estimation; see power.est argument for lm.hist)
#' @param N.control         Number of patients in current RCT control groups
#' @param N.treatment       Number of patients in current RCT treatment groups
#' @param N.sim             Number of generated historical and RCT datasets
#' @param parallel          Logical value. If TRUE the simulations are done using parallelisation using future::plan(future::multicore), which resolves futures asynchronously (in parallel) in separate forked R processes running in the background on the same machine.This is NOT supported on Windows. Reason for not using future::multisession is that this creates and error, probably stemming from a bug in future::multisession.
#' @param workers           Number of cores to use for parallelisation.
#'
#' @return
#' Same as for [simulate_collection].
#'
#' @examples
#' sim.lm(N.sim = 1, N.hist.control = 200, N.hist.treatment = 200,
#'               N.control = 100, N.treatment = 100)
#'
#' @importFrom future plan availableCores multicore
#' @importFrom future.apply future_lapply
#' @importFrom magrittr "%>%"
#' @importFrom MASS mvrnorm
#' @importFrom stats formula model.matrix
#'
#' @export
sim.lm <- function(ATE = 3,
                   ATE.shift = 0,
                   N.covs = 10,
                   N.overspec = 0,
                   coefs = c(0.5, 1, 1),
                   mu.shift = 0,
                   cov = diag(1, N.covs + N.overspec),
                   noise = 1,
                   N.hist.control = 5000,
                   N.hist.treatment = 100,
                   N.test.control = 0,
                   N.test.treatment = 0,
                   N.control = 200,
                   N.treatment = 300,
                   N.sim = 10,
                   parallel = TRUE,
                   workers = future::availableCores() - 1){

  ####### Check if variables are defined correctly ##########
  stopifnot(is.numeric(ATE), length(ATE) == 1L,
            is.numeric(ATE.shift), length(ATE.shift) == 1L,
            is.numeric(N.covs), length(N.covs) == 1L,
            is.numeric(N.overspec), length(N.overspec) == 1L,
            is.numeric(coefs), length(coefs) == 3L,
            is.matrix(cov), length(cov) == (N.covs + N.overspec)^2,
            is.numeric(noise), length(noise) == 1L,
            is.numeric(N.hist.control), length(N.hist.control) == 1L,
            is.numeric(N.hist.treatment), length(N.hist.treatment) == 1L,
            is.numeric(N.test.control), length(N.test.control) == 1L,
            is.numeric(N.test.treatment), length(N.test.treatment) == 1L,
            is.numeric(N.control), length(N.control) == 1L,
            is.numeric(N.treatment), length(N.treatment) == 1L,
            is.numeric(N.sim), length(N.sim) == 1L)

  out <- simulate_collection(
    covariate_model =
      function(n_treatment, n_control) {
        covariate_model_multinorm(
          n_treatment = n_treatment,
          n_control = n_control,
          n_covars = N.covs + N.overspec,
          mean_covars = 0,
          covariance_covars = cov
        )},
    outcome_model =
      function(data) {
        outcome_model_linear(
          data = data,
          ATE = ATE,
          coef_covars = coefs[2],
          coef_covars_interactions = coefs[1],
          coef_treat_interaction = coefs[3],
          noise = noise
        )
      },
    hist_covariate_model =
      function(n_treatment, n_control) {
        covariate_model_multinorm(
          n_treatment = n_treatment,
          n_control = n_control,
          n_covars = N.covs + N.overspec,
          mean_covars = mu.shift,
          covariance_covars = cov
        )},
    hist_outcome_model =
      function(data) {
        outcome_model_linear(
          data = data,
          ATE = ATE + ATE.shift,
          coef_covars = coefs[2],
          coef_covars_interactions = coefs[1],
          coef_treat_interaction = coefs[3],
          noise = noise
        )
      },
    n_sim = N.sim,
    n_control = N.control,
    n_treatment = N.treatment,
    n_hist_control = N.hist.control,
    n_hist_treatment = N.hist.treatment,
    n_test_control = N.test.control,
    n_test_treatment = N.test.treatment,
    parallel = parallel,
    workers = workers)

  attr(out, "ATE") <- ATE
  attr(out, "ATE.shift") <- ATE.shift
  attr(out, "N.covs") <- N.covs
  attr(out, "coefs") <- coefs

  return(out)
}

covariate_model_multinorm <- function(n_treatment, n_control, n_covars,
                                      mean_covars = 0,
                                      covariance_covars = diag(nrow = n_covars)) {

  data_covars <- MASS::mvrnorm(n = sum(n_treatment, n_control),
                               mu = rep(mean_covars, n_covars),
                               Sigma = covariance_covars) %>%
    as.data.frame()
  colnames(data_covars) <- paste0("x", 1:n_covars)

  final <- data_covars |>
    mutate(w = c(rep(0, n_control), rep(1, n_treatment)))

  return(final)
}

outcome_model_linear <- function(data,
                                 ATE,
                                 coef_covars,
                                 coef_covars_interactions,
                                 coef_treat_interaction,
                                 noise = 1) {

  w_with_noise <- ATE*data$w + stats::rnorm(nrow(data), 0, noise)

  # Taking number of columns of data minus 1 to exclude the treatment variable
  n_covars <- ncol(data) - 1
  X <- stats::model.matrix(stats::formula(paste0("~ -1 + (", paste0("x", 1:n_covars, collapse = "+"),")^2+",
                                                 paste0("I(x", 1:n_covars, "^2)", collapse = "+"),
                                                 "+",
                                                 paste0("I(x", 1:n_covars, "*w)", collapse = "+"))),
                           data = data)

  # Find indices of columns of X
  covars <- grep("^x\\d+$", colnames(X))
  covars_squared <- grep("\\^2", colnames(X))
  treat_interaction <- grep("\\*\\s?w", colnames(X))
  covars_interactions <- grep("x\\d+\\:x\\d+", colnames(X))

  # Create vector from given coefficients to multiply by correct columns of X
  coef_vector <- vector("numeric")
  coef_vector[covars] <- coef_covars
  coef_vector[treat_interaction] <- coef_treat_interaction
  coef_vector[c(covars_squared, covars_interactions)] <- coef_covars_interactions

  final <- data.frame(y = w_with_noise + X %*% coef_vector)

  return(final)
}
