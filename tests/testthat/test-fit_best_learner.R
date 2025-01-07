# default_learners
test_that("`default_learners` returns a list of models with class model_spec", {
  learners <- default_learners()
  expect_type(
    learners,
    "list"
  )

  models <- lapply(learners, \(x) x$model)
  all_are_model_spec <- all(sapply(models, \(x) inherits(x, "model_spec")))
  expect_equal(all_are_model_spec, TRUE)
})

# get_preproc_names
test_that("`get_preproc_names` returns vector of model names from wflow_id column", {
  create_data <- function(preproc_names, mod_names = c("name1", "name2")) {
    dat <- data.frame(
      wflow_id = paste(
        preproc_names,
        "_",
        mod_names,
        sep = ""
      )
    )
  }

  preproc_names <- "mod"
  dat <- create_data(preproc_names)
  expect_equal(
    get_preproc_names(dat),
    "mod"
  )

  preproc_names2 <- paste(1:5, "mod", sep = "")
  dat2 <- create_data(preproc_names2)
  expect_equal(
    get_preproc_names(dat2),
    preproc_names2
  )
})
