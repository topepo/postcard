causalglm <- function(formula,
                      family,
                      data,
                      formula_causal,
                      group_allocation_prob = 1/2,
                      estimand_fun = function(psi1, psi0) psi1/psi0,
                      estimand_fun_deriv0 = NULL, estimand_fun_deriv1 = NULL,
                      predict_counterfactual_means = NULL) {

  if (is.null(estimand_fun_deriv0) | is.null(estimand_fun_deriv1)) {
    args <- get_estimand_fun_args(estimand_fun)

    if (is.null(estimand_fun_deriv0)) {
      estimand_fun_deriv0 <- print_symbolic_differentiation(
        arg = args[["arg_with0"]],
        fun = estimand_fun,
        add_string = "Alternatively, specify the derivative through the argument {.var estimand_fun_deriv0}\n")
    }
    if (is.null(estimand_fun_deriv1)) {
      estimand_fun_deriv1 <- print_symbolic_differentiation(
        arg = args[["arg_with1"]],
        fun = estimand_fun,
        add_string = "Alternatively, specify the derivative through the argument {.var estimand_fun_deriv1}\n")
    }
  }

  model <- glm(formula = formula,
               family = family,
               data = data,
               control = list(maxit = 150))

  response_var_name <- get_response_from_formula(formula_causal)
  group_allocation_var <- get_explanatory_from_formula(formula_causal)


  predict_counterfactual_means <- function(group_val) {
    predict(model,
            type = "response",
            newdata = data |>
              dplyr::mutate("{group_allocation_var}" := group_val))
  }

  counterfactual_pred0 <- predict_counterfactual_means(group_val = 0)
  counterfactual_pred1 <- predict_counterfactual_means(group_val = 1)

  counterfactual_mean0 <- mean(counterfactual_pred0)
  counterfactual_mean1 <- mean(counterfactual_pred1)

  estimand <- estimand_fun(counterfactual_mean1, counterfactual_mean0)

  counterfactual_mean_IF0 <- IF_counterfactual_mean_glm(
    response_variable = data[, response_var_name],
    treatment_indicator = 1 - data[, group_allocation_var],
    group_allocation_prob = 1 - group_allocation_prob,
    counterfactual_mean = counterfactual_mean0,
    counterfactual_pred = counterfactual_pred0
  )
  counterfactual_mean_IF1 <- IF_counterfactual_mean_glm(
    response_variable = data[, response_var_name],
    treatment_indicator = 1 - data[, group_allocation_var],
    group_allocation_prob = 1 - group_allocation_prob,
    counterfactual_mean = counterfactual_mean1,
    counterfactual_pred = counterfactual_pred1
  )

  IF_estimand <- estimand_fun_deriv1(counterfactual_mean0,
                                     counterfactual_mean1) * counterfactual_mean_IF1 +
    estimand_fun_deriv0(counterfactual_mean0,
                        counterfactual_mean1) * counterfactual_mean_IF0

  var_IF_estimand <- as.numeric(var(IF_estimand))
  se_IF_estimand <- sqrt(var_IF_estimand/nrow(data))

  out <- list(
    counterfactual_mean0 = counterfactual_mean0,
       counterfactual_mean1 = counterfactual_mean1,
       estimand_fun = estimand_fun,
       estimand = estimand,
       se_IF_estimand = se_IF_estimand)

  return(structure(out, class = c("causalglm", class(out))))
}

# rate_ratio <- causalglm(formula = Y ~ .,
#                             family = MASS::negative.binomial(3, link = "log"),
#                             data = dat,
#                             formula_causal = Y ~ A,
#                             estimand_fun = function(psi1, psi0) psi1/psi0)
