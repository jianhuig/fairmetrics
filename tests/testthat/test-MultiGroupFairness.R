test_that('eval_generalized_entropy_index test', {
  expect_no_error({
  source("helper.R")
  eval_generalized_entropy_index(
    dat = test_data,
    outcome = "day_28_flg",
    group = "gender",
    probs = "pred",
    alpha = 2,
    cutoff = 0.5,
    digits = 2
  )
  })
})

test_that('eval_max_abs_diff test', {
  expect_no_error({
    source("helper.R")
    eval_max_abs_diff(
      dat = test_data,
      outcome = "day_28_flg",
      group = "gender",
      probs = "pred",
      cutoff = 0.41,
      digits = 2
    )
  })

})


test_that('eval_max_min_diff test', {
  expect_no_error({
    source("helper.R")
    eval_max_min_diff(
      dat = test_data,
      outcome = "day_28_flg",
      group = "gender",
      probs = "pred",
      cutoff = 0.41,
      digits = 2
    )
  })

})

test_that('eval_max_min_ratio test', {
  expect_no_error({
    source("helper.R")
    eval_max_min_ratio(
      dat = test_data,
      outcome = "day_28_flg",
      group = "gender",
      probs = "pred",
      cutoff = 0.41,
      digits = 2
    )
  })
})

test_that('eval_mean_abs_dev test', {
  expect_no_error({
    eval_mean_abs_dev(
      dat = test_data,
      outcome = "day_28_flg",
      group = "gender",
      probs = "pred",
      cutoff = 0.41,
      digits = 2
    )
  })
})


test_that('eval_variance test', {
  expect_no_error({
    source("helper.R")
    eval_variance(
      dat = test_data,
      outcome = "day_28_flg",
      group = "gender",
      probs = "pred",
      cutoff = 0.41,
      digits = 2
    )
  })

})
