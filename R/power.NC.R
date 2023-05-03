#' Power Approximation based on non-centrality parameter
#'
#' @param n               Number of participants in total. From this number of participants in the treatment group is \eqn{n1=(r/(1+r))n} and the control group is \eqn{n1=(1/(1+r))n}.
#' @param r               Allocation ratio \eqn{r=n1/n0}. For one-to-one randomisation r=1.
#' @param sigma           Variance of \eqn{Y(w)}, where w is the treatment indicator, and we assume homoskedasticity.
#' @param rho             Correlation between outcome and adjustment covariate in univariable covariate adjustment.
#' @param R2              The estimated pooled multiple correlation coefficient between outcome and covariates.
#' @param n.adj           Number of adjustment covariates.
#' @param ATE             Minimum effect size that we should be able to detect.
#' @param margin          Superiority margin (for non-inferiority margin, a negative value can be provided).
#' @param alpha           Significance level. Due to regulatory guidelines when using a one-sided test, half the specified significance level is used. Thus, for standard alpha = .05, a significance level of 0.025 is used.
#' @param method          Method is specified as either ANOVA, where no adjustment covariates are used, or ANCOVA where either one or multiple covariates are adjusted for.
#' @param deflation       Deflation parameter for decreasing rho or R2.
#' @param inflation       Inflation parameter for increasing sigma.
#'
#' @description
#' This function calculates the power for ANOVA or ANCOVA based on the non-centrality parameter and the exact t-distributions.
#'
#'
#' @details
#' The prospective power estimations are based on (Kieser M. Methods and Applications of Sample Size Calculation and Recalculation in Clinical Trials. Springer; 2020).
#' The ANOVA power is calculated based on the non-centrality parameter given as
#'
#' \deqn{nc =\sqrt{\frac{r}{(1+r)^2}\cdot n}\cdot\frac{ATE-margin}{\sigma},}
#'
#' where we denote by \eqn{\sigma^2} the variance of the outcome, such that the power can be estimated as
#'
#' \deqn{1-\beta = 1 - F_{t,n-2,nc}\left(F_{t, n-2, 0}^{-1}(1-\alpha/2)\right).}
#'
#' The power of ANCOVA with univariate covariate adjustment and no interaction is calculated based on the non-centrality parameter given as
#'
#' \deqn{nc =\sqrt{\frac{rn}{(1+r)^2}}\frac{ATE-margin}{\sigma\sqrt{1-\rho^2}},}
#'
#' such that the power can be estimated as
#'
#' \deqn{1-\beta = 1 - F_{t,n-3,nc}\left(F_{t, n-3, 0}^{-1}(1-\alpha/2)\right).}
#'
#' The power of ANCOVA with either univariate covariate adjustment and interaction or multiple covariate adjustement with or without interaction is calculated based on the non-centrality parameter given as
#'
#' \deqn{nc =\frac{ATE-margin}{\sqrt{\left(\frac{1}{n_1}+\frac{1}{n_0} + X_d^\top\left((n-2)\Sigma_X\right)^{-1}X_d \right)\sigma^2\left(1-\frac{\sigma_{XY}^\top \Sigma_X^{-1}\sigma_{XY}}{\sigma^2}\right)}}.}
#'
#' where \eqn{X_d \coloneqq \left(\overline{X}_1^1-\overline{X}_0^1, \ldots, \overline{X}_1^p-\overline{X}_0^p\right)^\top}, \eqn{\widehat{R}^2\coloneqq \frac{\widehat{\sigma}_{XY}^\top \widehat{\Sigma}_X^{-1}\widehat{\sigma}_{XY}}{\widehat{\sigma}^2}},
#' \eqn{\Sigma_X} the covariance matrix of the covariates, and \eqn{\sigma_{XY}} a \eqn{p}-dimensional column vector consisting of the covariance
#' between the outcome variable and each covariate. Since we are in the case of randomized trials the expected difference between the covariate
#' values between the to groups is 0. Furthermore, the elements of \eqn{\Sigma_X^{-1}} will be small, unless the variances are close to 0, or the covariates exhibit strong linear dependencies, so that the correlations are close to 1.
#' These scenarios are excluded since they could lead to potentially serious problems regarding inference either way. These arguments are used by Zimmermann et. al
#' (Zimmermann G, Kieser M, Bathke AC. Sample Size Calculation and Blinded Recalculation for Analysis of Covariance Models with Multiple Random Covariates. Journal of Biopharmaceutical Statistics. 2020;30(1):143–159.) to approximate
#' the non-centrality parameter as in the univariate case where \eqn{\rho^2} is replaced by \eqn{R^2}.
#' Then the power for ANCOVA with k adjustment covariates can be estimated as
#'
#' \deqn{1-\beta = 1 - F_{t,n - 2 - n.adj,nc}\left(F_{t, n - 2 - n.adj,0), 0}^{-1}(1-\alpha/2)\right).}
#'
#' @return
#' The function returns a power approximation based on the non-centrality parameter and the exact t-distribution.
#'
#' @examples
#' power.NC(method = "ANCOVA", rho = 0.7)
#'
#' @importFrom stats qt pt
#'
#' @export
power.NC <- function(n = 153,
                     r = 1,
                     sigma = 2,
                     rho = NULL,
                     R2 = NULL,
                     n.adj = 0,
                     ATE = 0.6,
                     margin = 0,
                     alpha = 0.05,
                     method = c("ANOVA", "ANCOVA"),
                     deflation = 1,
                     inflation = 1){

  sigma <- inflation*sigma

  if (!is.null(rho)) {
    rho <- deflation*rho
  }

  if (!is.null(R2)) {
    R2 <- deflation*R2
  }


  if (method == "ANOVA") {
    nc <- sqrt(r/(1 + r)^2*(n)) * (ATE - margin)/sqrt(sigma)
    power <- 1 - stats::pt(q = stats::qt(1 - alpha/2, n - 2), df = n - 2, ncp = nc)
  }

  if (method == "ANCOVA") {


    if (!is.null(rho) & is.null(R2)) {
      nc <- sqrt(r/(1 + r)^2 * (n)) * (ATE - margin)/(sqrt(sigma * (1 - rho^2)))
      power <- 1 - stats::pt(q = stats::qt(1 - alpha/2, n - 3), df = n - 3, ncp = nc)
    }


    if (is.null(rho) & !is.null(R2)) {
      nc <- sqrt(r/(1 + r)^2 * (n)) * (ATE - margin)/(sqrt(sigma * (1 - R2)))
      power <- 1 - stats::pt(q = stats::qt(1 - alpha/2, n - 2 - n.adj), df = n - 2 - n.adj, ncp = nc)
    }

    if (!is.null(rho) & !is.null(R2)) {

      stop("Specify either rho or R2. If you adjust by multiple covariates use R2 otherwise use rho.")
    }


    if (is.null(rho) & is.null(R2)) {

      stop("Both rho and R2 are not specified. Either specify one of these or use method ANOVA.")
    }

  }

  return(power)

}
