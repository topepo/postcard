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
