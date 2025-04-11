test_that('get_fairness_metrics test', {
  expect_no_error({
    source("helper.R")
    get_fairness_metrics(
      dat = test_data,
      outcome = "day_28_flg",
      group = "gender",
      group2 = "age",
      condition = ">= 60",
      probs = "pred",
      cutoff = 0.41,
      bootstraps = 2500,
      alpha = 0.05,
      digits =2
    )

  })
})
