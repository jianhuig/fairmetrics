test_that('eval_acc_parity match metrics test', {
  expect_equal({
    source("helper-match-metrics.R")
    res <- eval_acc_parity(
      data = test_data,
      outcome = "day_28_flg",
      group = "gender",
      probs = "pred",
      confint = FALSE,
      message = FALSE
    )[1:3]
    rownames(res)<- 1
  },
  {
    res <- subset(performance, Metric =="Accuracy")
    rownames(res)<- 1

  })
})

test_that('eval_bs_parity match metrics test', {
  expect_equal({
    source("helper-match-metrics.R")
    res <- eval_bs_parity(
      data = test_data,
      outcome = "day_28_flg",
      group = "gender",
      probs = "pred",
      confint = FALSE,
      message = FALSE
    )[1:3]
    rownames(res)<- 1
  },
  {
    res <- subset(performance, Metric =="Brier Score")
    rownames(res)<- 1
  })
})

test_that('eval_cond_stats_parity metrics test', {
  expect_equal({
    source("helper-match-metrics.R")
    res <- eval_cond_stats_parity(
      data = test_data,
      outcome = "day_28_flg",
      group = "gender",
      group2 = "service_unit",
      condition = "MICU",
      probs = "pred",
      confint = FALSE,
      message = FALSE
    )[1:3]
    rownames(res)<- 1
  },
  {
    res <- subset(performance, Metric =="Positive Prediction Rate")[2,]
    rownames(res)<- 1
  })
})


test_that('eval_eq_opp match metrics test', {
  expect_equal({
    source("helper-match-metrics.R")
    res <- eval_eq_opp(
      dat = test_data,
      outcome = "day_28_flg",
      group = "gender",
      probs = "pred",
      confint = FALSE,
      message = FALSE
      )[1:3]
    rownames(res)<- 1
    },
    {
      res <- subset(performance, Metric =="False Negative Rate")
      rownames(res)<- 1
    }
    )
})




test_that('eval_neg_class_bal match metrics test', {
  expect_equal({
    source("helper-match-metrics.R")
    res <- eval_neg_class_bal(
      data = test_data,
      outcome = "day_28_flg",
      group = "gender",
      probs = "pred",
      confint = FALSE,
      message = FALSE
    )[1:3]
    rownames(res) <- 1
  },
  {
    res <- subset(performance, Metric == "Avg. Predicted Prob.")[2,]
    rownames(res) <- 1
  }
  )
})

test_that('eval_pos_class_bal match metrics test', {
  expect_equal({
    source("helper-match-metrics.R")
    res <- eval_pos_class_bal(
      data = test_data,
      outcome = "day_28_flg",
      group = "gender",
      probs = "pred",
      confint = FALSE,
      message = FALSE
    )[1:3]
    rownames(res) <- 1
  },
  {
    res <- subset(performance, Metric =="Avg. Predicted Prob.")[1,]
    rownames(res) <- 1
  }
  )
})

test_that('eval_pred_equality metrics test', {
  expect_equal({
    source("helper-match-metrics.R")
    res <- eval_pred_equality(
      data = test_data,
      outcome = "day_28_flg",
      group = "gender",
      probs = "pred",
      cutoff = 0.41,
      confint = FALSE,
      message = FALSE
    )[1:3]
    rownames(res) <- 1
  },
  {
    res <- subset(performance, Metric =="False Positive Rate")
    rownames(res) <- 1
  }
  )
})

test_that('eval_stats_parity metrics test', {
  expect_equal({
    source("helper-match-metrics.R")
    res <- eval_stats_parity(
      data = test_data,
      outcome = "day_28_flg",
      group = "gender",
      probs = "pred",
      cutoff = 0.41,
      confint = FALSE,
      message = FALSE
    )[1:3]
    rownames(res) <- 1
  },
  {
    res <- subset(performance, Metric =="Positive Prediction Rate")[1,]
    rownames(res) <- 1
  }
  )
})


test_that('eval_treatment_equality metrics test', {
  expect_equal({
    source("helper-match-metrics.R")
    res <- eval_treatment_equality(
      data = test_data,
      outcome = "day_28_flg",
      group = "gender",
      probs = "pred",
      cutoff = 0.41,
      confint = FALSE,
      message = FALSE
    )[1:3]
    rownames(res) <- 1
  },
  {
    res <- subset(performance, Metric =="(False Negative)/(False Positive) Ratio")[1,]
    rownames(res) <- 1
  }
  )
})
