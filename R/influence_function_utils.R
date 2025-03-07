#' Evaluate the influence function of a counterfactual mean in a GLM
#'
#' @inheritParams rctglm
#'
#' @param response_variable a `numeric vector` with the response variable in the model
#' @param counterfactual_pred a `numeric vector` with predictions of the response variable for all
#' observations a hypothetic scenario of all observations being in the same group defined by
#' `group_indicator`. See more in details.
#'
#' @details
#' Assuming we have a response variable \eqn{Y}, design matrix \eqn{X} and link function \eqn{g}, a GLM
#' models the conditional mean of the response given covariates as:
#' \deqn{
#' E[Y|X]=g^{-1}(Xβ)
#' }
#' Assuming that \eqn{X} contains a binary variable \eqn{A} defining groups that were created by random
#' allocation, we can denote by \eqn{X_{-A}} the design matrix without \eqn{A}. Then, the counterfactual
#' mean of group a is
#' \deqn{
#' E[Y|X_{-A}, A=a]
#' }
#' The `counterfactual_pred` is the vector of these predictions for all observations.
#'
#' The evaluation of the influence function of these counterfactual means can be used to find the
#' influence function of any specified marginal effect in the GLM.
#'
#' @return a `numeric vector` with the value of the influence function of the counterfactual mean for
#' all observations
#'
#' @examples
#' # Generate some data
#' n <- 100
#' x1 <- rnorm (n)
#' a <- rbinom (n, 1, .5)
#' b0 <- 1
#' b1 <- 1.5
#' b2 <- 2
#' lin_pred <- b0+b1*x1+b2*a
#'
#' y_norm <- rnorm(n, mean = lin_pred, sd = 1)
#' dat_norm <- data.frame(Y = y_norm, X = x1, A = a)
#'
#' mod <- glm(formula = Y ~ .,
#'            data = dat_norm,
#'            family = gaussian())
#'
#' dat0 <- dat_norm %>% dplyr::mutate(A = 0)
#' pred0 <- predict(mod, type = "response", newdata = dat0)
#' if_countmean0 <- if_counterfactual_mean_glm(
#'   response_variable = dat0$Y,
#'   group_indicator = dat0$A,
#'   counterfactual_pred = pred0)
#'
#' @noRd
if_counterfactual_mean_glm <- function(response_variable,
                                       group_indicator,
                                       counterfactual_pred,
                                       group_allocation_prob = 1/2
) {
  counterfactual_mean <- mean(counterfactual_pred)

  group_indicator/group_allocation_prob *
    (response_variable - counterfactual_pred) +
    (counterfactual_pred - counterfactual_mean)
}

#' Evaluate the influence function of a marginal effect in a GLM model
#'
#' @inheritParams if_counterfactual_mean_glm
#'
#' @param counterfactual_pred0 a `numeric vector` with predictions of \eqn{E[Y|X_{-A}, A=0]}.
#' See more in details.
#' @param counterfactual_pred1 a `numeric vector` with predictions of \eqn{E[Y|X_{-A}, A=1]}.
#' See more in details.
#' @param estimand_fun_deriv0 a `function` with arguments `psi0` and `psi1` being the derivative
#' of the estimand function wrt. `psi0`
#' @param estimand_fun_deriv1 a `function` with arguments `psi0` and `psi1` being the derivative
#' of the estimand function wrt. `psi1`
#'
#' @inherit if_counterfactual_mean_glm details
#'
#' @return a `numeric vector` with the value of the influence function of the marginal effect for
#' all observations
#'
#' @noRd
if_marginaleffect <- function(response_variable,
                              group_indicator,
                              counterfactual_pred0,
                              counterfactual_pred1,
                              group_allocation_prob,
                              estimand_fun_deriv0,
                              estimand_fun_deriv1) {

  counterfactual_mean_IF0 <- if_counterfactual_mean_glm(
    response_variable = response_variable,
    group_indicator = 1 - group_indicator,
    group_allocation_prob = 1 - group_allocation_prob,
    counterfactual_pred = counterfactual_pred0
  )
  counterfactual_mean_IF1 <- if_counterfactual_mean_glm(
    response_variable = response_variable,
    group_indicator = group_indicator,
    group_allocation_prob = group_allocation_prob,
    counterfactual_pred = counterfactual_pred1
  )

  counterfactual_mean0 <- mean(counterfactual_pred0)
  counterfactual_mean1 <- mean(counterfactual_pred1)

  if_estimand <- estimand_fun_deriv1(
    psi1 = counterfactual_mean1,
    psi0 = counterfactual_mean0
  ) * counterfactual_mean_IF1 +
    estimand_fun_deriv0(
      psi1 = counterfactual_mean1,
      psi0 = counterfactual_mean0
    ) * counterfactual_mean_IF0

  return(if_estimand)
}

