#' Fit GLM and find any estimand (marginal effect) using plug-in estimation with variance estimation using
#' influence functions
#'
#' @inheritParams stats::glm
#'
#' @param group_indicator (name of) the variable in data that identifies randomisation groups
#' @param group_allocation_prob a `numeric` with the probabiliy of being assigned "group 1" (rather than group 0)
#' @param estimand_fun a `character` specifying a default estimand function, or `function` with
#' arguments `psi0` and `psi1` specifying the estimand.
#' @param estimand_fun_deriv0 a `function` specifying the derivative of `estimand_fun` wrt. `psi0`
#' @param estimand_fun_deriv1 a `function` specifying the derivative of `estimand_fun` wrt. `psi1`
#' @param ... Additional arguments passed to [stats::glm()]
#'
#' @details
#' Default estimand functions can be specified via `"ate"` and `"rate_ratio"`.
#'
#' As a default, the `Deriv` package to perform symbolic differentiation to find the derivatives of
#' the `estimand_fun`.
#'
#' @return an `rctglm` object
#' @export
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
#' ate <- rctglm(formula = Y ~ .,
#'               group_indicator = A,
#'               data = dat_norm,
#'               family = gaussian(),
#'               estimand_fun = "ate")
rctglm <- function(formula,
                   family,
                   data,
                   group_indicator,
                   group_allocation_prob = 1/2,
                   estimand_fun = "ate",
                   estimand_fun_deriv0 = NULL, estimand_fun_deriv1 = NULL,
                   ...
) {

  # TODO: Right now, estimand_fun needs to have arguments with 0 and 1 in them, and the group_indicator
  # needs to take values 0 and 1. Generalise this to work for factors and be able to set reference level

  group_indicator <- rlang::enquo(group_indicator)
  args <- as.list(environment())
  call <- match.call()

  if (is.character(estimand_fun)) estimand_fun <- default_estimand_funs(estimand_fun)

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
  group_indicator_name <- as.character(rlang::quo_get_expr(rlang::eval_tidy(group_indicator)))
  group_indicator_var <- data %>%
    dplyr::pull(group_indicator_name)


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

  return(structure(out, class = c("rctglm", class(out))))
}
