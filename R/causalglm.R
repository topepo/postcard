causalglm <- function(formula,
                      family,
                      data,
                      group_indicator,
                      group_allocation_prob = 1/2,
                      estimand_fun = function(psi1, psi0) psi1/psi0,
                      estimand_fun_deriv0 = NULL, estimand_fun_deriv1 = NULL,
                      predict_counterfactual_means = NULL,
                      ...
) {

  group_indicator <- rlang::enquo(group_indicator)
  args <- as.list(environment())
  call <- match.call()

  if (is.null(estimand_fun_deriv0) | is.null(estimand_fun_deriv1)) {
    args01 <- get01args(fun = estimand_fun)

    if (is.null(estimand_fun_deriv0)) {
      estimand_fun_deriv0 <- print_symbolic_differentiation(
        arg = args01[["arg0"]],
        fun = estimand_fun,
        add_string = "Alternatively, specify the derivative through the argument {.var estimand_fun_deriv0}\n")
    }
    if (is.null(estimand_fun_deriv1)) {
      estimand_fun_deriv1 <- print_symbolic_differentiation(
        arg = args01[["arg1"]],
        fun = estimand_fun,
        add_string = "Alternatively, specify the derivative through the argument {.var estimand_fun_deriv1}\n")
    }
  }

  args_glm <- c(args[names(args) %in% names(formals(glm))], list(...))
  model <- do.call(glm, args = args_glm)

  response_var <- model$y
  group_indicator_var <- data %>%
    dplyr::pull(!!group_indicator)
  group_indicator_name <- as.character(rlang::quo_get_expr(group_indicator))

  counterfactual_pred0 <- predict_counterfactual_means(group_val = 0,
                                                       model = model,
                                                       data = data,
                                                       group_indicator_name = group_indicator_name)
  counterfactual_pred1 <- predict_counterfactual_means(group_val = 1,
                                                       model = model,
                                                       data = data,
                                                       group_indicator_name = group_indicator_name)

  counterfactual_mean0 <- mean(counterfactual_pred0)
  counterfactual_mean1 <- mean(counterfactual_pred1)

  estimand <- estimand_fun(counterfactual_mean1, counterfactual_mean0)

  IF_marginaleffect_val <- IF_marginaleffect(
    response_variable = response_var,
    group_indicator = group_indicator_var,
    group_allocation_prob = group_allocation_prob,
    counterfactual_pred0 = counterfactual_pred0,
    counterfactual_mean0 = counterfactual_mean0,
    counterfactual_pred1 = counterfactual_pred1,
    counterfactual_mean1 = counterfactual_mean1,
    estimand_fun_deriv0 = estimand_fun_deriv0,
    estimand_fun_deriv1 = estimand_fun_deriv1)

  var_estimand <- as.numeric(var(IF_marginaleffect_val))
  se_estimand <- sqrt(var_estimand/nrow(data))

  out <- list(
    counterfactual_pred0 = counterfactual_pred0,
    counterfactual_pred1 = counterfactual_pred1,
    counterfactual_mean0 = counterfactual_mean0,
    counterfactual_mean1 = counterfactual_mean1,
    estimand_fun = estimand_fun,
    estimand = estimand,
    se_estimand = se_estimand,
    glm = model,
    call = call)

  return(structure(out, class = c("causalglm", class(out))))
}
