#' Guenther-Schouten Power Approximation
#'
#' @description
#' This function calculates the Guenther-Schouten power approximation for ANOVA or ANCOVA.
#' The approximation is based in (Guenther WC. Sample Size Formulas for Normal Theory T Tests.
#' The American Statistician. 1981;35(4):243–244) and (Schouten HJA. Sample size formula with
#' a continuous outcome for unequal group sizes and unequal variances. Statistics in Medicine.
#' 1999;18(1):87–91).
#'
#' @param n           Number of participants in total. From this number of participants in the treatment group is \eqn{n1=(r/(1+r))n} and the control group is \eqn{n1=(1/(1+r))n}.
#' @param r           Allocation ratio \eqn{r=n1/n0}. For one-to-one randomisation r=1.
#' @param sigma       Standard deviation of \eqn{Y(w)}, where w is the treatment indicator, and we assume homoskedasticity.
#' @param rho         Correlation between outcome and adjustment covariate in univariable covariate adjustment.
#' @param R2          The estimated pooled multiple correlation coefficient between outcome and covariates.
#' @param ate         Minimum effect size that we should be able to detect.
#' @param margin      Superiority margin (for non-inferiority margin, a negative value can be provided).
#' @param alpha       Significance level. Due to regulatory guidelines when using a one-sided test, half the specified significance level is used. Thus, for standard alpha = .05, a significance level of 0.025 is used.
#'
#' @details
#' The formula in the case of an ANCOVA model with multiple covariate adjustement is:
#'
#' \eqn{
#' n=\frac{(1+r)^2}{r}\frac{(z_{1-\alpha/2}+z_{1-\beta})^2\sigma^2(1-R^2)}{(\beta_1-\beta_0-\Delta_s)^2}+\frac{(z_{1-\alpha/2})^2}{2}
#' }
#'
#' where \eqn{\widehat{R}^2\coloneqq \frac{\widehat{\sigma}_{XY}^\top \widehat{\Sigma}_X^{-1}\widehat{\sigma}_{XY}}{\widehat{\sigma}^2}},
#' we denote by \eqn{\sigma^2} the variance of the outcome, \eqn{\Sigma_X} the covariance matrix of the
#' covariates, and \eqn{\sigma_{XY}} a \eqn{p}-dimensional column vector consisting of the covariance
#' between the outcome variable and each covariate. In the univariate case \eqn{R^2} is replaced by \eqn{\rho^2}
#'
#' As a default, both `R2` and `rho` are `NULL`, meaning that power is approximated in the case
#' of an ANOVA with no adjustment covariates are present besides the binary group indicator. In
#' this case, the formula above applies but where \eqn{R^2} is replaced with \eqn{0}.
#'
#' @return
#' a `numeric` with power approximation based on the Guenther-Schouten approximation
#' @export
#'
#' @examples
#' power_gs(rho = 0.7)
#'
power_gs <- function(n = 153,
                     r = 1,
                     sigma = sqrt(2),
                     rho = NULL,
                     R2 = NULL,
                     ate = 0.6,
                     margin = 0,
                     alpha = 0.05) {

  var <- inflation*sigma^2

  no_covariate_correlation <- is.null(rho) & is.null(R2)
  if (!no_covariate_correlation) {
    if (!is.null(rho) & !is.null(R2)) {
      cli::cli_abort("Specify either {.arg rho} or {.arg R2}. If you adjust by multiple covariates use {.arg R2} otherwise use {.arg rho}")
    }

    if (!is.null(rho) & is.null(R2)) {
      var <- var*(1 - rho^2)
    }
    if (is.null(rho) & !is.null(R2)) {
      var <- var*(1 - R2)
    }
  }

  power <- stats::pnorm(
    sqrt(
      r/(1 + r)^2 *
        (ate - margin)^2/var *
        (n - stats::qnorm(1 - alpha/2)^2/2)
    ) - stats::qnorm(1 - alpha/2)
  )
  return(power)
}
