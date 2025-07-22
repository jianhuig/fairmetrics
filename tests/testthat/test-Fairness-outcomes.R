test_that('eval_acc_parity ratio+diff outcome test', {
  expect_equal({
    dt <- data.frame(
      y_true = c(0, 1, 0, 1),
      y_pred = c(1, 1, 1, 1),
      group = c("A", "A", "B", "B")
    )
    result <- eval_acc_parity(data = dt,
                              outcome = "y_true",
                              group = "group",
                              probs = "y_pred",
                              digits = 1,
                              confint = TRUE,
                              message = FALSE)
    list(result$Ratio, result$Difference)
  },
  list(1,0))
})

test_that('eval_acc_parity CI outcome test', {
  expect_equal({
    dt <- data.frame(
      y_true = c(0, 1, 0, 1),
      y_pred = c(1,1,1,1),
      group = c("A", "A", "B", "B")
    )
    result <- eval_acc_parity(data = dt,
                              outcome = "y_true",
                              group = "group",
                              probs = "y_pred",
                              digits = 1,
                              confint = TRUE,
                              message = FALSE)
    list(result$`95% Ratio CI`, result$`95% Diff CI`)
  },
  list("[NA, NA]", "[-1, 1]"))
})


test_that('eval_bs_parity ratio+diff outcome test', {
  expect_equal({
    dt <- data.frame(
      y_true = c(0, 1, 0, 1),
      y_pred = c(1, 1, 1, 1),
      group = c("A", "A", "B", "B")
    )
    result <- eval_bs_parity(data = dt,
                              outcome = "y_true",
                              group = "group",
                              probs = "y_pred",
                              digits = 1,
                              confint = TRUE,
                              message = FALSE)
    list(result$Ratio, result$Difference)
  },
  list(1,0))
})

test_that('eval_bs_parity CI outcome test', {
  expect_equal({
    dt <- data.frame(
      y_true = c(0, 1, 0, 1),
      y_pred = c(1,1,1,1),
      group = c("A", "A", "B", "B")
    )
    result <- eval_bs_parity(data = dt,
                              outcome = "y_true",
                              group = "group",
                              probs = "y_pred",
                              digits = 1,
                              confint = TRUE,
                              message = TRUE)
    list(result$`95% Ratio CI`, result$`95% Diff CI`)
  },
  list("[NA, NA]", "[-1, 1]"))
})


test_that('eval_eq_opp ratio+diff outcome test', {
  expect_equal({
    dt <- data.frame(
      y_true = c(0, 1, 0, 1),
      y_pred = c(1, 1, 1, 1),
      group = c("A", "A", "B", "B")
    )
    result <- eval_eq_opp(data = dt,
                             outcome = "y_true",
                             group = "group",
                             probs = "y_pred",
                             digits = 1,
                             confint = TRUE,
                             message = TRUE)
    list(result$Ratio, result$Difference)
  },
  list(NaN,0))
})

test_that('eval_eq_opp CI outcome test', {
  expect_equal({
    dt <- data.frame(
      y_true = c(0, 1, 0, 1),
      y_pred = c(1,1,1,1),
      group = c("A", "A", "B", "B")
    )
    result <- eval_eq_opp(data = dt,
                             outcome = "y_true",
                             group = "group",
                             probs = "y_pred",
                             digits = 1,
                             confint = TRUE,
                             message = TRUE)
    list(result$`95% Ratio CI`, result$`95% Diff CI`)
  },
  list("[NaN, NaN]", "[0, 0]"))
})



test_that('eval_pred_equality ratio+diff outcome test', {
  expect_equal({
    dt <- data.frame(
      y_true = c(0, 1, 0, 1),
      y_pred = c(1, 1, 1, 1),
      group = c("A", "A", "B", "B")
    )
    result <- eval_pred_equality(data = dt,
                          outcome = "y_true",
                          group = "group",
                          probs = "y_pred",
                          digits = 1,
                          confint = TRUE,
                          message = TRUE)
    list(result$Ratio, result$Difference)
  },
  list(1,0))
})

test_that('eval_pred_equality CI outcome test', {
  expect_equal({
    dt <- data.frame(
      y_true = c(0, 1, 0, 1),
      y_pred = c(1,1,1,1),
      group = c("A", "A", "B", "B")
    )
    result <- eval_pred_equality(data = dt,
                          outcome = "y_true",
                          group = "group",
                          probs = "y_pred",
                          digits = 1,
                          confint = TRUE,
                          message = TRUE)
    list(result$`95% Ratio CI`, result$`95% Diff CI`)
  },
  list("[NA, NA]", "[0, 0]"))
})



test_that('eval_stats_parity ratio+diff outcome test', {
  expect_equal({
    dt <- data.frame(
      y_true = c(0, 1, 0, 1),
      y_pred = c(1, 1, 1, 1),
      group = c("A", "A", "B", "B")
    )
    result <- eval_stats_parity(data = dt,
                                 outcome = "y_true",
                                 group = "group",
                                 probs = "y_pred",
                                 digits = 1,
                                 confint = TRUE,
                                 message = TRUE)
    list(result$Ratio, result$Difference)
  },
  list(1,0))
})

test_that('eval_stats_parity CI outcome test', {
  expect_equal({
    dt <- data.frame(
      y_true = c(0, 1, 0, 1),
      y_pred = c(1,1,1,1),
      group = c("A", "A", "B", "B")
    )
    result <- eval_stats_parity(data = dt,
                                 outcome = "y_true",
                                 group = "group",
                                 probs = "y_pred",
                                 digits = 1,
                                 confint = TRUE,
                                 message = TRUE)
    list(result$`95% Ratio CI`, result$`95% Diff CI`)
  },
  list("[1, 1]", "[0, 0]"))
})


test_that('eval_treatment_equality ratio+diff outcome test', {
  expect_equal({
    dt <- data.frame(
      y_true = c(0, 1, 0, 1),
      y_pred = c(1, 1, 1, 1),
      group = c("A", "A", "B", "B")
    )
    result <- eval_treatment_equality(data = dt,
                                outcome = "y_true",
                                group = "group",
                                probs = "y_pred",
                                digits = 1,
                                confint = TRUE,
                                message = TRUE)
    list(result$Ratio, result$Difference)
  },
  list(NaN,0))
})

test_that('eval_treatment_equality CI outcome test', {
  expect_equal({
    dt <- data.frame(
      y_true = c(0, 1, 0, 1),
      y_pred = c(1,1,1,1),
      group = c("A", "A", "B", "B")
    )
    result <- eval_treatment_equality(data = dt,
                                outcome = "y_true",
                                group = "group",
                                probs = "y_pred",
                                digits = 1,
                                confint = TRUE,
                                message = TRUE)
    list(result$`95% Ratio CI`, result$`95% Diff CI`)
  },
  list("[NaN, NaN]", "[0, 0]"))
})













