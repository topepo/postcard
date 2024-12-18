get_fun_args <- function(fun) {
  names(formals(fun))
}

deparse_fun_body <- function(fun) {
  deparsed_body <- deparse(body(fun))
  len_dep <- length(deparsed_body)
  if (len_dep > 1) {
    deparsed_body <- gsub("\\s*", "", deparsed_body[-c(1, len_dep)])
  }

  return(deparsed_body)
}

get_response_from_formula <- function(formula) {
  if (inherits(formula, "formula")) {
    formula <- deparse(formula)
  }
  gsub("\\s*~.*", "", formula)
}

# Get names of arguments containing 0 and 1 from function
get01args = function(fun) {

  arg0 <- grep("0", get_fun_args(fun), value = TRUE)
  arg1 <- grep("1", get_fun_args(fun), value = TRUE)

  if (length(arg0) == 0 | length(arg1) == 0) {
    cli::cli_abort("Arguments of the {.var fun} need {.code 0} and {.code 1} in their names to be able to perform automatic symbolic differentiation. Alternatively, specify the partial derivatives, {.var estimand_fun_deriv0} and {.var estimand_fun_deriv1}, manually.")
  }

  return(list(arg0 = arg0, arg1 = arg1))
}

# Perform symbolic differentiation of function and print message
print_symbolic_differentiation <- function(arg, fun, add_string = "", verbose = options::opt("verbose")) {
  derivative <- Deriv::Deriv(fun, arg)

  body_of_fun <- deparse_fun_body(fun)
  body_of_derivative <- deparse_fun_body(derivative)

  if (verbose >= 1) {
    cli::cli_alert_info("Symbolically deriving partial derivative of the function '{body_of_fun}' with respect to '{arg}' as: '{body_of_derivative}'.\n")
    if (stringr::str_length(add_string) > 0)
      cli::cli_ul(add_string)
  }

  return(derivative)
}

# Modify data to have the same value of a group identifier and then predict
predict_counterfactual_means <- function(model, data, group_indicator_name, group_val) {
  predict(model,
          type = "response",
          newdata = data |>
            dplyr::mutate("{group_indicator_name}" := group_val))
}

# Default estimand functions
default_estimand_funs <- function(default = c("ate", "rate_ratio")) {
  default <- match.arg(default)

  switch(default,
         ate = function(psi1, psi0) psi1-psi0,
         rate_ratio = function(psi1, psi0) psi1/psi0
  )
}

# Create formula that is function of
formula_everything <- function(orig_formula, data, verbose = options::opt("verbose")) {
  response_var_name <- get_response_from_formula(orig_formula)
  if (!response_var_name %in% colnames(data))
    cli::cli_abort("Tried to create formula to fit prognostic model but did not find the response variable {.var {response_var_name}} specified in the primary formula.\nProvide a formula manually through the argument {.arg prog_formula}.")

  prog_formula_str <- paste0(response_var_name, " ~ .")
  if (verbose >= 1)
    cli::cli_alert_info("Created formula for fitting prognostic model as: {prog_formula_str}")

  return(formula(prog_formula_str))
}
