#' Find the best learner in terms of RMSE among specified learners using cross validation
#'
#' @inheritParams rctglm
#'
#' @param n_folds a `numeric` with the number of cross-validation folds used when fitting and
#' evaluating models
#' @param learners a `list` of `tidymodels`. Default uses a combination of MARS, linear
#' regression and boosted trees
#'
#' @return a trained `workflow`
#' @export
#'
#' @examples
#' # Generate some "historical" data of a control treatment
#' n <- 100
#' w1 <- runif(n, min = -2, max = 2)
#' x1 <- abs(sin(w1))
#' a <- rbinom (n, 1, .5)
#' b0 <- 1
#' b1 <- 1.5
#' b2 <- 2
#'
#' truemean_notreat <- b0+b1*x1
#' y_notreat <- rnorm(n, mean = truemean_notreat, sd = 1)
#' dat_hist <- data.frame(Y = y_notreat, W = w1)
#'
#' # Fit a learner to the data with default learners
#' fit <- fit_best_learner(Y ~ ., data = dat_hist)
fit_best_learner <- function(data, formula, n_folds = 5, learners = default_learners()) {
  cv_folds <- rsample::vfold_cv(data, v = n_folds)
  lrnr <- cv_folds %>%
    get_best_learner(learners = learners,
                     formula = formula)
  lrnr_fit <- generics::fit(lrnr, data)

  return(lrnr_fit)
}

# Default learners used for searching among
default_learners <- function() {
  list(
    mars = list(
      model = parsnip::mars(
        mode = "regression", prod_degree = 3) %>%
        parsnip::set_engine("earth"),
      grid = NULL
    ),
    lm = list(
      model = parsnip::linear_reg() %>%
        parsnip::set_engine("lm"),
      grid = NULL
    ),
    gbt = list(
      model = parsnip::boost_tree(
        mode = "regression",
        trees = parsnip::tune("trees"),
        tree_depth = parsnip::tune("tree_depth"),
        learn_rate = 0.1
      ) %>%
        parsnip::set_engine("xgboost"),
      grid = purrr::cross_df(list(
        trees = seq.int(25, 500, by=25),
        tree_depth = c(3)
      ))
    )
  )
}

# set up the pre-processing of the work flow
get_preproc_names <- function(wf) {
  wf %>%
    dplyr::pull(.data$wflow_id) %>%
    stringr::str_split("_") %>%
    purrr::map_chr(~.[1]) %>%
    unique()
}

# function to add learners with the pre-processing
add_learners <- function(preproc, learners) {
  wf <- workflowsets::workflow_set(
    preproc = preproc,
    models = learners %>%
      purrr::map(~.$model)
  )
  for (learner_name in names(learners)){
    for (preproc_name in get_preproc_names(wf)) {
      wf %<>%
        workflowsets::option_add(
          id = stringr::str_c(preproc_name, "_", learner_name),
          grid = learners[[learner_name]]$grid
        )
    }
  }
  wf
}

# K fold cross validation with recipe -------------------------------------
get_best_learner <- function(
    resamples,
    learners = default_learners(),
    formula,
    verbose = T
) {

  if (is.character(formula)) formula <- formula(formula)

  wfs <- add_learners(preproc = list(mod = formula),
                      learners = learners)

  fit_learners <- wfs %>%
    workflowsets::workflow_map(
      resamples = resamples,
      metrics = yardstick::metric_set(yardstick::rmse)
    )

  best_learner_name <- fit_learners %>%
    workflowsets::rank_results(rank_metric = 'rmse') %>%
    dplyr::select(.data$wflow_id, .data$model, .data$.config, rmse=mean, rank) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::pull(.data$wflow_id)
  if (verbose){
    print(best_learner_name)
  }

  best_params <- fit_learners %>%
    workflowsets::extract_workflow_set_result(best_learner_name) %>%
    tune::select_best(metric='rmse')

  fit_learners %>%
    tune::extract_workflow(best_learner_name) %>%
    tune::finalize_workflow(best_params)
}
