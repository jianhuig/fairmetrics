test_that('get_fairness_metrics CI test', {
  expect_no_error({
    source("helper.R")
    get_fairness_metrics(
      data = test_data,
      outcome = "day_28_flg",
      group = "gender",
      probs = "pred",
      confint = TRUE,
      cutoff = 0.41,
      bootstraps = 2500,
      alpha = 0.05,
      digits = 2
    )

  })
})

test_that('get_fairness_metrics no CI test', {
  expect_no_error({
    source("helper.R")
    get_fairness_metrics(
      data = test_data,
      outcome = "day_28_flg",
      group = "gender",
      probs = "pred",
      confint = FALSE,
      cutoff = 0.41,
      bootstraps = 2500,
      alpha = 0.05,
      digits = 2
    )

  })
})
