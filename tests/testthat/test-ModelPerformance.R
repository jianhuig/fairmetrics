test_that('get_all_metrics test', {
  expect_no_error({
    source("helper.R")
    get_all_metrics(
      dat = test_data,
      outcome = "day_28_flg",
      group = "gender",
      probs = "pred",
      cutoff = 0.41,
      digits = 2
    )
  })

})

