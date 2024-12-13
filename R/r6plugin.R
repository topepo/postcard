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
  if (stringr::str_length(add_string) > 0) cli::cli_ul(add_string, .envir = sys.frame(-1))

  return(derivative)
}

get_response_from_formula <- function(formula) {
  if (inherits(formula, "formula")) {
    formula <- deparse(formula)
  }
  gsub("\\s*~.*", "", formula)
}

get_explanatory_from_formula <- function(formula) {
  if (inherits(formula, "formula")) {
    formula <- deparse(formula)
  }
  gsub(".*~\\s*", "", formula)
}

modelCausal <- R6Class("modelCausal",
                       public = list(
                         model = NULL,
                         data = NULL,
                         formula_causal = NULL,
                         response_var_name = NULL,
                         group_allocation_var = NULL,
                         group_allocation_prob = NA,
                         predict_counterfactual_means = NULL,
                         estimand_fun = NULL,
                         estimand_fun_deriv0 = NULL,
                         estimand_fun_deriv1 = NULL,
                         IF_counterfactual_mean = NULL,

                         initialize = function(model, data,
                                               formula_causal,
                                               group_allocation_prob = 1/2,
                                               estimand_fun = function(psi1, psi0) psi1/psi0,
                                               estimand_fun_deriv0 = NULL, estimand_fun_deriv1 = NULL,
                                               predict_counterfactual_means = NULL,
                                               IF_counterfactual_mean = NULL) {

                           self$model <- model
                           self$data <- data
                           self$estimand_fun <- estimand_fun
                           self$formula_causal <- formula_causal
                           self$response_var_name <- get_response_from_formula(self$formula_causal)
                           self$group_allocation_var <- get_explanatory_from_formula(self$formula_causal)
                           self$group_allocation_prob <- group_allocation_prob

                           if (is.null(IF_counterfactual_mean)) stop("You need to specify the influence function")
                           self$IF_counterfactual_mean = IF_counterfactual_mean

                           if (is.null(predict_counterfactual_means)) {
                             private$set_default_pred_fun()
                           }

                           # Deriving derivatives of estimand function if not set by the user
                           if (is.null(estimand_fun_deriv0) | is.null(estimand_fun_deriv1)) {
                             private$check_estimand_fun_args(self$estimand_fun)

                             if (is.null(estimand_fun_deriv0)) {
                               private$estimand_fun_derivative(fun_name = "estimand_fun_deriv0",
                                                               deriv_arg = private$estimand_fun_args[["arg_with0"]])
                             }
                             if (is.null(estimand_fun_deriv1)) {
                               private$estimand_fun_derivative(fun_name = "estimand_fun_deriv1",
                                                               deriv_arg = private$estimand_fun_args[["arg_with1"]])
                             }
                           }
                         },

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
                               deparse_fun_body(self$estimand_fun),
                               "\n",
                               sep = "")
                           cat("  - Estimand (r(Psi_1, Psi_0)) estimate (SE): ",
                               self$estimand,
                               " (",
                               self$se_IF_estimand,
                               ")\n",
                               sep = "")

                           invisible(self)
                         }
                       ),

                       active = list(
                         counterfactual_pred0 = function() {
                           self$predict_counterfactual_means(group_val = 0)
                         },

                         counterfactual_pred1 = function() {
                           self$predict_counterfactual_means(group_val = 1)
                         },
                         counterfactual_mean0 = function() {
                           mean(self$counterfactual_pred0)
                         },
                         counterfactual_mean1 = function() {
                           mean(self$counterfactual_pred1)
                         },
                         estimand = function() {
                           self$estimand_fun(self$counterfactual_mean1, self$counterfactual_mean0)
                         },
                         counterfactual_mean_IF0 = function() {
                           self$IF_counterfactual_mean(response_variable = self$data[, self$response_var_name],
                                                       treatment_indicator = 1 - self$data[, self$group_allocation_var],
                                                       group_allocation_prob = 1 - self$group_allocation_prob,
                                                       counterfactual_mean = self$counterfactual_mean0,
                                                       counterfactual_pred = self$counterfactual_pred0
                           )
                         },
                         counterfactual_mean_IF1 = function() {
                           self$IF_counterfactual_mean(response_variable = self$data[, self$response_var_name],
                                                       treatment_indicator = self$data[, self$group_allocation_var],
                                                       group_allocation_prob = self$group_allocation_prob,
                                                       counterfactual_mean = self$counterfactual_mean1,
                                                       counterfactual_pred = self$counterfactual_pred1
                           )
                         },

                         IF_estimand = function() {
                           self$estimand_fun_deriv1(self$counterfactual_mean0,
                                                    self$counterfactual_mean1) * self$counterfactual_mean_IF1 +
                             self$estimand_fun_deriv0(self$counterfactual_mean0,
                                                      self$counterfactual_mean1) * self$counterfactual_mean_IF0
                         },
                         var_IF_estimand = function() {
                           as.numeric(var(self$IF_estimand))
                         },
                         se_IF_estimand = function() {
                           sqrt(self$var_IF_estimand/nrow(self$data))
                         }
                       ),

                       private = list(
                         set_default_pred_fun = function() {
                           self$predict_counterfactual_means <- function(group_val) {
                             predict(self$model,
                                     type = "response",
                                     newdata = self$data |>
                                       mutate("{self$group_allocation_var}" := group_val))
                           }
                         },
                         estimand_fun_args = NULL,
                         check_estimand_fun_args = function(estimand_fun) {

                           arg_with0 <- grep("0", get_fun_args(estimand_fun), value = TRUE)
                           arg_with1 <- grep("1", get_fun_args(estimand_fun), value = TRUE)

                           if (length(arg_with0) == 0 | length(arg_with1) == 0) {
                             cli::cli_abort("Arguments of the {.var estimand_fun} need {.code 0} and {.code 1} in their names to be able to perform automatic symbolic differentiation. Alternatively, specify the partial derivatives, {.var estimand_fun_deriv0} and {.var estimand_fun_deriv1}, manually.")
                           }

                           private$estimand_fun_args <- list(arg_with0 = arg_with0, arg_with1 = arg_with1)

                           return(invisible(self))
                         },
                         estimand_fun_derivative = function(fun_name, deriv_arg) {
                           self[[fun_name]] <- print_symbolic_differentiation(
                             arg = deriv_arg,
                             fun = self$estimand_fun,
                             add_string = "Alternatively, specify the derivative through the argument {.var {fun_name}}\n")

                           return(invisible(self))
                         }
                       ))

modelCausal$new(model = glm(Y ~ .,
                            family = MASS::negative.binomial(3, link = "log"),
                            data = dat,
                            control = list(maxit = 150)),
                data = dat,
                formula_causal = Y~A,
                estimand_fun = function(psi1, psi0) psi1/psi0,
                IF_counterfactual_mean = IF_counterfactual_mean_glm
)

glmCausal <- R6Class("glmCausal",
                     inherit = modelCausal,
                     public = list(
                       initialize = function(formula,
                                             family,
                                             data,
                                             formula_causal,
                                             group_allocation_prob = 1/2,
                                             estimand_fun = function(psi1, psi0) psi1/psi0,
                                             estimand_fun_deriv0 = NULL, estimand_fun_deriv1 = NULL,
                                             predict_counterfactual_means = NULL,
                                             IF_counterfactual_mean = NULL) {

                         model <- glm(formula = formula,
                                      family = family,
                                      data = data,
                                      control = list(maxit = 150))

                         IF_counterfactual_mean <- IF_counterfactual_mean_glm

                         args_model_IF <- as.list(environment())
                         args_to_super_initialize <- args_model_IF[!names(args_model_IF) %in% c("formula", "family")]

                         do.call(super$initialize, args_to_super_initialize)

                         private$validate()
                       }
                     ),

                     private = list(
                       validate = function() {
                         if (get_response_from_formula(self$model$formula) !=
                             get_response_from_formula(self$formula_causal)) {
                           cli::cli_abort("Reponse variable is different in the 2 provided formulas")
                         }
                       }
                     ))

fit_glmCausal <- function(formula,
                          family,
                          data,
                          formula_causal,
                          group_allocation_prob = 1/2,
                          estimand_fun = function(psi1, psi0) psi1/psi0,
                          estimand_fun_deriv0 = NULL, estimand_fun_deriv1 = NULL,
                          predict_counterfactual_means = NULL) {

  args <- as.list(environment())

  do.call(glmCausal$new, args)
}

rate_ratio <- fit_glmCausal(formula = Y ~ .,
                            family = MASS::negative.binomial(3, link = "log"),
                            data = dat,
                            formula_causal = Y ~ A,
                            estimand_fun = function(psi1, psi0) psi1/psi0)

unadjusted <- modelCausal$new(model = lm(Y ~ A, data = dat),
                              data = dat,
                              formula_causal = Y ~ A,
                              estimand_fun = function(psi1, psi0) psi1/psi0)

ate <- fit_glmCausal(formula = Y ~ .,
                     family = MASS::negative.binomial(3, link = "log"),
                     data = dat,
                     formula_causal = Y ~ A,
                     estimand_fun = function(psi1, psi0) psi1 - psi0)

#
# ate <- glmCausal$new(data = dat,
#                      estimand_fun = function(psi1, psi0) psi1-psi0)


