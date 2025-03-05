# Modify data to have the same value of a group identifier and then predict
predict_counterfactual_mean <- function(model,
                                         group_indicator_name,
                                         group_val,
                                         newdata = NULL,
                                         data = NULL) {
  if (is.null(data)) data <- model$data
  if (is.null(newdata)) newdata <- data
  newdata_same_group_val <- newdata |>
    dplyr::mutate("{group_indicator_name}" := group_val)

  group_indicator_in_model <- group_indicator_name %in% names(coef(model))
  if (!group_indicator_in_model)
    cli::cli_abort("{.arg {group_indicator_name}} is not in {.arg {model}}. Specify name of a binary predictor in the {.arg {model}}")
  predict(model,
          type = "response",
          newdata = newdata_same_group_val)
}

predict_counterfactual_means <- function(model,
                                         group_indicator_name,
                                         newdata = NULL,
                                         data = NULL) {
  args <- as.list(environment())
  counterfactual_preds <- sapply(c(0,1), function(x) {
    do.call(
      predict_counterfactual_mean,
      c(args, list(group_val = x))
    )
  }) %>%
    as.data.frame() %>%
    stats::setNames(c("psi0", "psi1"))

  return(counterfactual_preds)
}

# Default estimand functions
default_estimand_funs <- function(default = c("ate", "rate_ratio")) {
  default <- match.arg(default)

  switch(default,
         ate = function(psi1, psi0) psi1-psi0,
         rate_ratio = function(psi1, psi0) psi1/psi0
  )
}
