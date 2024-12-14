IF_counterfactual_mean_glm <- function(response_variable,
                                       group_indicator,
                                       group_allocation_prob,
                                       counterfactual_mean,
                                       counterfactual_pred) {
  group_indicator/group_allocation_prob *
     (response_variable - counterfactual_mean) +
     (counterfactual_mean - counterfactual_pred)
}

IF_marginaleffect <- function(response_variable,
                              group_indicator,
                              group_allocation_prob,
                              counterfactual_pred0,
                              counterfactual_mean0,
                              counterfactual_pred1,
                              counterfactual_mean1,
                              estimand_fun_deriv0,
                              estimand_fun_deriv1) {

  counterfactual_mean_IF0 <- IF_counterfactual_mean_glm(
    response_variable = response_variable,
    group_indicator = 1 - group_indicator,
    group_allocation_prob = 1 - group_allocation_prob,
    counterfactual_mean = counterfactual_mean0,
    counterfactual_pred = counterfactual_pred0
  )
  counterfactual_mean_IF1 <- IF_counterfactual_mean_glm(
    response_variable = response_variable,
    group_indicator = group_indicator,
    group_allocation_prob = group_allocation_prob,
    counterfactual_mean = counterfactual_mean1,
    counterfactual_pred = counterfactual_pred1
  )

  IF_estimand <- estimand_fun_deriv1(counterfactual_mean0,
                                     counterfactual_mean1) * counterfactual_mean_IF1 +
    estimand_fun_deriv0(counterfactual_mean0,
                        counterfactual_mean1) * counterfactual_mean_IF0

  return(IF_estimand)
}

