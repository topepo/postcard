#' Simulate a collection of pairs of "historical" and "current" RCT data
#'
#' Provide number of pairs of studies to simulate, the sample sizes in groups of the studies together with functions to model covariates and outcomes in the studies to generate a collection of simulated studies.
#'
#' @param n_sim                Number of collections of historical and
#' @param n_control            Number of patients in current RCT control groups
#' @param n_treatment          Number of patients in current RCT treatment groups
#' @param n_hist_control       Number of patients in historical control groups
#' @param n_hist_treatment     Number of patients in historical treatment groups
#' @param n_test_control       Additional number of historical control group patients having same distribution as "hist" (if provided, these can be used for preliminary power estimation; see power.est argument for lm.hist)
#' @param n_test_treatment     Additional number of historical treatment group patients having same distribution as "hist" (if provided, these can be used for preliminary power estimation; see power.est argument for lm.hist)
#' @param covariate_model      A `function` taking arguments `n_treat` and `n_control` which specifies how to generate covariates (including the treatment variable) in the "current" data
#' @param outcome_model        A `function` taking argument `data` (which will be evaluated with the result of `covariate_model`) and models the outcome from covariates in the data. Output should be a data set containing a column with the output
#' @param hist_covariate_model Same as `covariate_model` but specify how to model the covariates in the historical (and historical test) data sets
#' @param hist_outcome_model   Same as `outcome_model` but specify how to model the outcome in the historical (and historical test) data sets
#' @param parallel             Logical value. If TRUE the simulations are done using parallelisation using future::plan(future::multicore), which resolves futures asynchronously (in parallel) in separate forked R processes running in the background on the same machine.This is NOT supported on Windows. Reason for not using future::multisession is that this creates and error, probably stemming from a bug in future::multisession.
#' @param workers              Number of cores to use for parallelisation.
#'
#' @return
#'  `list` object with `n_sim` elements.
#' Each element in the list contains `$hist` (historical data), `$hist_test` (test data
#' with same distribution of the historical data), `$rct` (current RCT data).
#'
#' @examples
#' # Nonsense example to showcase you can do whatever you want.
#'
#' # Define the function to generate covariates and model the outcome
#' cov_mod <- function(n_treatment, n_control,
#'                     shape = 2, scale = 1/2) {
#'  n_tot <- sum(n_treatment, n_control)
#'
#'  data.frame(a = rgamma(n = n_tot,
#'                        shape = shape,
#'                        scale = 1/2),
#'             b = rnorm(n_tot),
#'             w = c(rep(0, n_control), rep(1, n_treatment)))
#'}
#'
#' out_mod <- function(data, coef = c(2, 1, 3, 1)) {
#'   if (length(coef) != 4) {
#'     stop("`coef` argument needs to have length 4 to match dimensions of design matrix")
#'   }
#'
#'   X <- stats::model.matrix(
#'     formula("~ -1 + a*b + w"),
#'     data = data)
#'
#'   data.frame(y = X %*% coef)
#' }
#'
#' # Use the defined functions to simulate your collection of studies
#' simulate_collection(n_treatment = 50,
#'                     n_control = 25,
#'                     n_hist_treatment = 500,
#'                     n_hist_control = 5000,
#'                     n_test_treatment = 100,
#'                     n_test_control = 500,
#'                     covariate_model = cov_mod,
#'                     outcome_model = out_mod,
#'                     hist_covariate_model = cov_mod,
#'                     hist_outcome_model = out_mod)
#'
#' @importFrom future plan availableCores multicore
#' @importFrom future.apply future_lapply
#' @importFrom magrittr "%>%"
#' @importFrom MASS mvrnorm
#' @importFrom stats formula model.matrix
#'
#' @export
simulate_collection <- function(
    covariate_model,
    outcome_model,
    hist_covariate_model,
    hist_outcome_model,
    n_sim = 10,
    n_control = 200,
    n_treatment = 300,
    n_hist_control = 5000,
    n_hist_treatment = 100,
    n_test_control = 0,
    n_test_treatment = 0,
    parallel = TRUE,
    workers = future::availableCores() - 1){

  ####### Check if variables are defined correctly ##########
  stopifnot(is.numeric(n_hist_control), length(n_hist_control) == 1L,
            is.numeric(n_hist_treatment), length(n_hist_treatment) == 1L,
            is.numeric(n_test_control), length(n_test_control) == 1L,
            is.numeric(n_test_treatment), length(n_test_treatment) == 1L,
            is.numeric(n_control), length(n_control) == 1L,
            is.numeric(n_treatment), length(n_treatment) == 1L,
            is.numeric(n_sim), length(n_sim) == 1L)

  sim <- function(k){

    res <- list()

    if (!is.null(n_hist_control) & !is.null(n_hist_treatment)) {

      res$hist <- simulate_study(n_treat = n_hist_treatment,
                                 n_control = n_hist_control,
                                 covariate_model = hist_covariate_model,
                                 outcome_model = hist_outcome_model)
    }

    if (n_test_control + n_test_treatment > 0) {

      res$hist_test <- simulate_study(n_treat = n_test_treatment,
                                      n_control = n_test_control,
                                      covariate_model = hist_covariate_model,
                                      outcome_model = hist_outcome_model)
    }


    if (!is.null(n_control) & !is.null(n_treatment)) {

      res$rct <- simulate_study(n_treat = n_treatment,
                                n_control = n_control,
                                covariate_model = covariate_model,
                                outcome_model = outcome_model)
    }

    res
  }


  if (parallel) {
    oplan <-  future::plan(future::multicore, workers = workers)
    on.exit(plan(oplan))
    out <- future.apply::future_lapply(X = 1:n_sim, future.seed = TRUE, FUN = sim)
  } else {
    out <- lapply(1:n_sim, FUN = sim)
  }

  out <- as.list(out)
  attr(out, "n_hist_control") <- n_hist_control
  attr(out, "n_hist_treatment") <- n_hist_treatment
  attr(out, "n_control") <- n_control
  attr(out, "n_treatment") <- n_treatment
  attr(out, "n_test_control") <- n_test_control
  attr(out, "n_test_treatment") <- n_test_treatment

  out
}

simulate_study <- function(n_treat, n_control,
                           covariate_model,
                           outcome_model) {

  covariate_data <- covariate_model(
    n_treat = n_treat,
    n_control = n_control
  )

  if (!inherits(covariate_data, "data.frame")) {
    stop("The return value of the `covariate_model` has to be a `data.frame` with the covaraites as (named) columns")
  }

  outcome <- outcome_model(
    data = covariate_data
  )

  if (!inherits(outcome, "data.frame")) {
    stop("The return value of the `outcome_model` has to be a `data.frame` with the outcome as a (named) column")
  }

  return(dplyr::bind_cols(covariate_data, outcome))
}
