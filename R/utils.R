validate_formulas = function() {
  if (get_response_from_formula(self$model$formula) !=
      get_response_from_formula(self$formula_causal)) {
    cli::cli_abort("Reponse variable is different in the 2 provided formulas")
  }
}

#################
# Estimand stuff
get_estimand_fun_args = function(estimand_fun) {

  arg_with0 <- grep("0", get_fun_args(estimand_fun), value = TRUE)
  arg_with1 <- grep("1", get_fun_args(estimand_fun), value = TRUE)

  if (length(arg_with0) == 0 | length(arg_with1) == 0) {
    cli::cli_abort("Arguments of the {.var estimand_fun} need {.code 0} and {.code 1} in their names to be able to perform automatic symbolic differentiation. Alternatively, specify the partial derivatives, {.var estimand_fun_deriv0} and {.var estimand_fun_deriv1}, manually.")
  }

  return(list(arg_with0 = arg_with0, arg_with1 = arg_with1))
}

estimand_fun_derivative = function(fun, arg) {
  print_symbolic_differentiation(
    arg = arg,
    fun = fun,
    add_string = "Alternatively, specify the derivative through the argument {.var {fun_name}}\n")
}


# IFs
IF_counterfactual_mean_glm <- function(response_variable,
                                       treatment_indicator,
                                       group_allocation_prob,
                                       counterfactual_mean,
                                       counterfactual_pred) {
  (treatment_indicator/group_allocation_prob *
     (response_variable - counterfactual_mean) +
     (counterfactual_mean - counterfactual_pred))
}
