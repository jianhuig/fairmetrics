library(fairmetrics)
library(dplyr)
library(randomForest)
# Data for tests
data("mimic_preprocessed")
set.seed(123)
train_data <- mimic_preprocessed |>
  dplyr::filter(dplyr::row_number() <= 700)
# Fit a random forest model
rf_model <- randomForest::randomForest(factor(day_28_flg) ~ ., data = train_data, ntree = 1000)
# Test the model on the remaining data
test_data <- mimic_preprocessed |>
  dplyr::mutate(gender = ifelse(gender_num == 1, "Male", "Female"))|>
  dplyr::filter(dplyr::row_number() > 700)

test_data$pred <- predict(rf_model, newdata = test_data, type = "prob")[, 2]

get_fairness_metrics(
  data = test_data,
  outcome = "day_28_flg",
  group = "gender",
  group2 = "service_unit",
  condition = "MICU",
  probs = "pred",
  cutoff = 0.41,
  confint = FALSE
) |> list2env(envir = environment())
