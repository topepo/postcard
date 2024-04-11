test_that("lm.hist.sim", {

  select_rct.rows <- function(data, N.control, N.treatment){
    data.ctrl <- data %>% filter(w == 0)
    data.ctrl <- data.ctrl[1:N.control,]
    data.treat <- data %>% filter(w == 1)
    data.treat <- data.treat[1:N.treatment,]
    rbind(data.ctrl, data.treat)
  }

  select_hist.rows <- function(data, N.hist){
    data[1:N.hist,]
  }

  select_sim <- function(data, N.control, N.treatment, N.hist){
    data$rct <- select_rct.rows(data$rct, N.control, N.treatment)
    data$hist <- select_hist.rows(data$hist, N.hist)
    return(data)
  }

  workers = 52
  char_vector <- paste0("x", 1:2)
  data_test <- sim.lm(N.hist.treatment = 0, N.test.control = 100)
  test <- lapply(data_test, select_sim, N.control = (2/5)*30, N.treatment = (3/5)*30, N.hist = 5*30)
  attributes(test) <- attributes(data_test)

  res <- lm.hist.sim(test, method = "PROCOVA", pred.model = random.hist, adj.covs = char_vector, workers = workers, L2 = FALSE, est.power = FALSE, type = "HC3")

  expected_cols <- 7
  expected_column_names <- c("estimate", "std.err", "test_stat", "power", "coverage", "MSE", "type1.err")

  expect_equal(ncol(res), expected_cols,
               info = "Number of columns does not match the expected value.")
  expect_identical(colnames(res), expected_column_names,
                   info = "Column names do not match the expected value.")

})
