#' Generate pairs of historical and current data
#'
#' Current and historical data simulated from a normal linear distribution as \eqn{Y(w) \sim N(aX^TX+bX+cXW+ATE\cdot W)}
#'
#'
#' @param ATE               The average treatment effect \eqn{ATE = \mathbb{E}[Y(1)] - \mathbb{E}[Y(0)] } data is generated from
#' @param ATE.shift         ATE shift in the historical data, when historical data contains treatment group patients
#' @param N.covs            Number of covariates in the data generating process, indexed as x1,...,x_N.covs
#' @param N.overspec        Number of overspecified covariates (generates covariates not used in the data generating process, but can be used in model specifications to simulate overspecification). They are indexed as x_{N.covs+1},..., x_{N.covs+N.overspec}.
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
#' List object consisting of 1, 2, 3, ..., N.sim list each containing $hist (historical data), $hist_test (test data
#' with same distribution of the historical data), $rct (current RCT data)
#' and multiple attributes.
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
                   ATE.shift = stats::rnorm(1, 0, 0.1),
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
                   workers = future::availableCores()){

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

  varnames <- paste0("x", 1:(N.covs + N.overspec))
  ATE_new <- ATE + ATE.shift

  sim <- function(k){

    res <- list()

    if (!is.null(N.hist.control) & !is.null(N.hist.treatment)) {

      data.hist <- MASS::mvrnorm(n = N.hist.control + N.hist.treatment + N.test.control + N.test.treatment,
                                 mu = rep(mu.shift, N.covs + N.overspec),
                                 Sigma = cov) %>% as.data.frame()

      colnames(data.hist) <- varnames
      rownames(data.hist) <- 1:nrow(data.hist)

      data.hist$w <- c(rep(0, N.hist.control), rep(1, N.hist.treatment), rep(0, N.test.control), rep(1, N.test.treatment))
      data.hist$y <- ATE_new*data.hist$w + stats::rnorm(N.hist.control + N.hist.treatment + N.test.control + N.test.treatment, 0, noise)

      X <- stats::model.matrix(stats::formula(paste0("y ~ ",
                                                     paste0("(", paste0("x", 1:N.covs, collapse = "+"),")^2"),
                                                     "+",
                                                     paste0("I(x", 1:N.covs, "^2)", collapse = "+"),
                                                     "+",
                                                     paste0("I(x", 1:N.covs, "*w)", collapse = "+"))),
                               data = data.hist)[,-1]

      data.hist$y <- data.hist$y + X %*% c(rep(coefs[2], N.covs), rep(coefs[1], N.covs),
                                           rep(coefs[3], N.covs), rep(coefs[1], ncol(X) - 3*N.covs))
      res$hist <- data.hist
    }

    if (N.test.control + N.test.treatment > 0) {
      res$hist_test <- res$hist[(N.hist.control + N.hist.treatment + 1):(N.hist.control + N.hist.treatment + N.test.control + N.test.treatment), ]
      res$hist <- res$hist[1:(N.hist.control + N.hist.treatment), ]
    }


    if (!is.null(N.control) & !is.null(N.treatment)) {

      data.rct <- MASS::mvrnorm(n = N.control + N.treatment, mu = rep(0, N.covs + N.overspec), Sigma = cov) %>% as.data.frame()

      colnames(data.rct) <- varnames
      data.rct$w <- c(rep(0, N.control), rep(1, N.treatment))

      data.rct$y <- ATE*data.rct$w + stats::rnorm(N.control + N.treatment, 0, noise)

      X <- stats::model.matrix(stats::formula(paste0("y ~ ",
                                                     paste0("(", paste0("x", 1:N.covs, collapse = "+"), ")^2"),
                                                     "+",
                                                     paste0("I(x", 1:N.covs, "^2)", collapse = "+"),
                                                     "+",
                                                     paste0("I(x", 1:N.covs, "*w)", collapse = "+"))),
                               data = data.rct)[,-1]

      data.rct$y <- data.rct$y + X %*% c(rep(coefs[2], N.covs), rep(coefs[1], N.covs),
                                         rep(coefs[3], N.covs), rep(coefs[1], ncol(X) - 3*N.covs))

      res$rct <- data.rct
    }

    res
  }


  if (parallel) {
    oplan <-  future::plan(future::multicore, workers = workers)
    on.exit(plan(oplan))
    out <- future.apply::future_lapply(X = 1:N.sim, future.seed = TRUE, FUN = sim)
  } else {
    out <- lapply(1:N.sim, FUN = sim)
  }

  out <- as.list(out)
  attr(out, "ATE") <- ATE
  attr(out, "ATE.shift") <- ATE.shift
  attr(out, "N.covs") <- N.covs
  attr(out, "coefs") <- coefs
  attr(out, "N.hist.control") <- N.hist.control
  attr(out, "N.hist.treatment") <- N.hist.treatment
  attr(out, "N.control") <- N.control
  attr(out, "N.treatment") <- N.treatment
  attr(out, "N.test.control") <- N.test.control
  attr(out, "N.test.treatment") <- N.test.treatment

  out
}


