####################################################################################
# -------------------------------------------------------------------------------- #
####################################################################################
# R6

library(R6)

get_fun_args <- function(fun) {
  names(formals(fun))
}

deparse_fun_body <- function(fun) {
  deparse(body(fun))
}

print_symbolic_differentiation <- function(arg, fun, add_string = "") {
  derivative <- Deriv::Deriv(fun, arg)

  body_of_fun <- deparse_fun_body(fun)
  body_of_derivative <- deparse_fun_body(derivative)

  cli::cli_alert_info("Symbolically deriving partial derivative of the function '{body_of_fun}' with respect to '{arg}' as: '{body_of_derivative}'.\n")
  if (stringr::str_length(add_string) > 0) cli::cli_ul(add_string)

  return(derivative)
}

glmCausal <- R6Class("glmCausal",
                     public = list(
                       initialize = function(data,
                                             estimand_function = function(psi1, psi0) psi1/psi0,
                                             response_var_name = "Y",
                                             treatment_var_name = "A", treatment_allocation_prob = 1/2,
                                             estimand_func_dm0 = NULL, estimand_func_dm1 = NULL) {
                         self$data <- data
                         self$estimand_function <- estimand_function
                         self$response_var_name <- response_var_name
                         self$treatment_var_name <- treatment_var_name
                         self$treatment_allocation_prob <- treatment_allocation_prob

                         if (is.null(estimand_func_dm0) | is.null(estimand_func_dm1)) {
                           arg_with0 <- grep("0",
                                             get_fun_args(self$estimand_function),
                                             value = TRUE)
                           arg_with1 <- grep("1",
                                             get_fun_args(self$estimand_function),
                                             value = TRUE)

                           if (length(arg_with0) == 0 | length(arg_with1) == 0) {
                             cli::cli_abort("Arguments of the {.var estimand_function} need {.code 0} and {.code 1} in their names to be able to perform automatic symbolic differentiation. Alternatively, specify the partial derivatives, {.var estimand_func_dm0} and {.var estimand_func_dm1}, manually.")
                           }

                           if (is.null(estimand_func_dm0)) {
                             self$estimand_func_dm0 <- print_symbolic_differentiation(
                               arg = arg_with0,
                               fun = self$estimand_function,
                               add_string = "Alternatively, specify the derivative through the argument {.var estimand_func_dm0}\n")
                           }
                           if (is.null(estimand_func_dm1)) {
                             self$estimand_func_dm1 <- print_symbolic_differentiation(
                               arg = arg_with1,
                               fun = self$estimand_function,
                               add_string = "Alternatively, specify the derivative through the argument {.var estimand_func_dm1}\n")
                           }
                         }

                         self$model <- glm(formula(paste0(response_var_name, " ~ .")),
                                           family = MASS::negative.binomial(r, link = "log"),
                                           data = self$data,
                                           control = list(maxit = 150))
                       },

                       model = NULL,
                       data = NULL,
                       response_var_name = NULL,
                       treatment_var_name = NULL,
                       treatment_allocation_prob = NA,
                       pred = function(model, newdata) {
                         predict(model,
                                 type = "response",
                                 newdata = newdata)
                       },
                       estimand_function = NULL,
                       estimand_func_dm0 = NULL,
                       estimand_func_dm1 = NULL,

                       print = function(...) {
                         cat("<glmCausal>:\n")
                         cat("  - Counterfactual control mean (Psi_0=E[Y|W, A=0]) estimate: ",
                             self$counterfactual_mean0,
                             "\n",
                             sep = "")
                         cat("  - Counterfactual control mean (Psi_1=E[Y|W, A=1]) estimate: ",
                             self$counterfactual_mean1,
                             "\n",
                             sep = "")
                         cat("  - Estimand function r: ",
                             deparse_fun_body(self$estimand_function),
                             "\n",
                             sep = "")
                         cat("  - Marginal effect (r(Psi_1, Psi_0)) estimate (SE): ",
                             self$marginal_effect,
                             " (",
                             self$se_IF_marginal_effect_glm,
                             ")\n",
                             sep = "")

                         invisible(self)
                       }

                     ),
                     active = list(
                       counterfactual_pred0 = function() {
                         self$pred(model = self$model,
                                   newdata = self$data %>%
                                     ####### TODO: Generalise this part
                                     mutate("{self$treatment_var_name}" := 0))
                       },

                       counterfactual_pred1 = function() {
                         self$pred(model = self$model,
                                   newdata = self$data %>%
                                     ####### TODO: Generalise this part
                                     mutate("{self$treatment_var_name}" := 1))
                       },
                       counterfactual_mean0 = function() {
                         mean(self$counterfactual_pred0)
                       },
                       counterfactual_mean1 = function() {
                         mean(self$counterfactual_pred1)
                       },
                       marginal_effect = function() {
                         self$estimand_function(self$counterfactual_mean1, self$counterfactual_mean0)
                       },
                       counterfactual_mean_IF0 = function() {
                         private$IF_counterfactual_mean_glm(response_variable = self$data[, self$response_var_name],
                                                            treatment_indicator = 1 - self$data[, self$treatment_var_name],
                                                            treatment_allocation_prob = 1 - self$treatment_allocation_prob,
                                                            counterfactual_mean = self$counterfactual_mean0,
                                                            counterfactual_pred = self$counterfactual_pred0
                         )
                       },
                       counterfactual_mean_IF1 = function() {
                         private$IF_counterfactual_mean_glm(response_variable = self$data[, self$response_var_name],
                                                            treatment_indicator = self$data[, self$treatment_var_name],
                                                            treatment_allocation_prob = self$treatment_allocation_prob,
                                                            counterfactual_mean = self$counterfactual_mean1,
                                                            counterfactual_pred = self$counterfactual_pred1
                         )
                       },

                       IF_marginal_effect_glm = function() {
                         self$estimand_func_dm1(self$counterfactual_mean0,
                                                self$counterfactual_mean1) * self$counterfactual_mean_IF1 +
                           self$estimand_func_dm0(self$counterfactual_mean0,
                                                  self$counterfactual_mean1) * self$counterfactual_mean_IF0
                       },
                       var_IF_marginal_effect_glm = function() {
                         as.numeric(var(self$IF_marginal_effect_glm))
                       },
                       se_IF_marginal_effect_glm = function() {
                         sqrt(self$var_IF_marginal_effect_glm/nrow(self$data))
                       }
                     ),
                     private = list(
                       IF_counterfactual_mean_glm = function(response_variable,
                                                             treatment_indicator, treatment_allocation_prob,
                                                             counterfactual_mean, counterfactual_pred) {
                         treatment_indicator / treatment_allocation_prob * (response_variable - counterfactual_mean) +
                           counterfactual_mean - counterfactual_pred
                       }
                     ))

# rate_ratio <- glmCausal$new(data = dat,
#                             estimand_function = function(psi1, psi0) psi1/psi0)
#
# ate <- glmCausal$new(data = dat,
#                      estimand_function = function(psi1, psi0) psi1-psi0)
