get_fun_args <- function(fun) {
  names(formals(fun))
}

deparse_fun_body <- function(fun) {
  deparse(body(fun))
}

get01args = function(fun) {

  arg0 <- grep("0", get_fun_args(fun), value = TRUE)
  arg1 <- grep("1", get_fun_args(fun), value = TRUE)

  if (length(arg0) == 0 | length(arg1) == 0) {
    cli::cli_abort("Arguments of the {.var fun} need {.code 0} and {.code 1} in their names to be able to perform automatic symbolic differentiation. Alternatively, specify the partial derivatives, {.var estimand_fun_deriv0} and {.var estimand_fun_deriv1}, manually.")
  }

  return(list(arg0 = arg0, arg1 = arg1))
}

print_symbolic_differentiation <- function(arg, fun, add_string = "") {
  derivative <- Deriv::Deriv(fun, arg)

  body_of_fun <- deparse_fun_body(fun)
  body_of_derivative <- deparse_fun_body(derivative)

  cli::cli_alert_info("Symbolically deriving partial derivative of the function '{body_of_fun}' with respect to '{arg}' as: '{body_of_derivative}'.\n")
  if (stringr::str_length(add_string) > 0) cli::cli_ul(add_string, .envir = sys.frame(-1))

  return(derivative)
}

predict_counterfactual_means <- function(model, data, group_indicator_name, group_val) {
  predict(model,
          type = "response",
          newdata = data |>
            dplyr::mutate("{group_indicator_name}" := group_val))
}

default_estimand_funs <- function(defaults = c("ate", "rate_ratio")) {
  switch(defaults,
    ate = function(psi1, psi0) psi1-psi0,
    rate_ratio = function(psi1, psi0) psi/psi0
  )
}
