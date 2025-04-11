test_that('eval_acc_parity test', {
  expect_no_error({
    source("helper.R")
    eval_acc_parity(
      dat = test_data,
      outcome = "day_28_flg",
      group = "gender",
      probs = "pred",
      confint = TRUE,
      cutoff = 0.41,
      alpha = 0.05,
      digits = 2,
      bootstraps = 2,
      message = FALSE
    )
  })
})

test_that('eval_bs_parity test', {
  expect_no_error({
  source("helper.R")
  eval_bs_parity(
    dat = test_data,
    outcome = "day_28_flg",
    group = "gender",
    probs = "pred",
    confint = TRUE,
    alpha = 0.05,
    bootstraps = 2500,
    digits = 2,
    message = FALSE
  )
  })
})

test_that('eval_cond_stats_parity test', {
  expect_no_error({
  source("helper.R")
  eval_cond_stats_parity(
    dat = test_data,
    outcome = "day_28_flg",
    group = "gender",
    group2 = "service_unit",
    condition = "MICU",
    probs = "pred",
    cutoff = 0.41,
    bootstraps = 2500,
    alpha = 0.05,
    digits = 2,
    message = FALSE
  )
})
})

test_that('eval_eq_odds test',{
  expect_no_error({
  source("helper.R")
  eval_eq_odds(
    dat = test_data,
    outcome = "day_28_flg",
    group = "gender",
    probs = "pred",
    cutoff = 0.41,
    bootstraps = 2500,
    alpha = 0.05,
    digits = 2,
    message = FALSE
  )})
})

test_that('eval_eq_opp test', {
  expect_no_error({
  source("helper.R")
  eval_eq_opp(
    dat = test_data,
    outcome = "day_28_flg",
    group = "gender",
    probs = "pred",
    cutoff = 0.41,
    bootstraps = 2500,
    alpha =0.05,
    digits =,
    message = FALSE
  )})
})




test_that('eval_neg_class_bal test', {
  expect_no_error({
    source("helper.R")
    eval_neg_class_bal(
      dat = test_data,
      outcome = "day_28_flg",
      group = "gender",
      probs = "pred",
      confint = TRUE,
      alpha = 0.05,
      bootstraps = 2500,
      digits = 2,
      message = FALSE
    )
  })
})

test_that('eval_pos_class_bal test', {
  expect_no_error({
    source("helper.R")
    eval_pos_class_bal(
      dat = test_data,
      outcome = "day_28_flg",
      group = "gender",
      probs = "pred",
      confint = TRUE,
      alpha = 0.05,
      bootstraps = 2500,
      digits = 2,
      message = FALSE
    )
  }
  )
})

test_that('eval_pred_equality test', {
  expect_no_error({
    source("helper.R")
    eval_pred_equality(
      dat = test_data,
      outcome = "day_28_flg",
      group = "gender",
      probs = "pred",
      cutoff = 0.41,
      confint = TRUE,
      alpha =0.05,
      bootstraps = 2500,
      digits = 2,
      message = FALSE
    )
  })
})

test_that('eval_stats_parity test', {
  expect_no_error({
    source("helper.R")
    eval_stats_parity(
      dat = test_data,
      outcome = "day_28_flg",
      group = "gender",
      probs = "pred",
      cutoff = 0.41,
      confint = TRUE,
      alpha =0.05,
      bootstraps = 2500,
      digits = 2,
      message = FALSE
    )
  })

})


test_that('eval_treatment_equality test', {
  expect_no_error({
    source("helper.R")
    eval_treatment_equality(
      dat = test_data,
      outcome = "day_28_flg",
      group = "gender",
      probs = "pred",
      cutoff = 0.41,
      confint =TRUE,
      alpha = 0.05,
      bootstraps = 2500,
      digits = 2,
      message = FALSE
    )
  })

})






