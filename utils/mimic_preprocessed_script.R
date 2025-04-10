# Preprocessing script for making mimic_preprocessed
# SUMMARY:
# Remove columns with more than 10% missing data and impute the rest with median.
# Remove columns that are highly correlated with the outcome variable.
library(FairnessTutorial)
library(dplyr)
data("mimic")
# Calculate the number of missing values per column
missing_values <- sapply(mimic, function(x) sum(is.na(x)))
# Calculate the percentage of missing values per column
missing_values_percentage <- sapply(mimic, function(x) sum(is.na(x)) / length(x) * 100)
# Identify columns with more than 10% missing values
columns_to_remove <- names(missing_values_percentage[missing_values_percentage > 10])
# Remove these columns
mimic <- select(mimic, -one_of(columns_to_remove))
# Impute remaining missing values with median
mimic<- mimic %>% mutate(across(where(~any(is.na(.))), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))
# Identify columns that have only one unique value
cols_with_one_value <- sapply(mimic, function(x) length(unique(x)) == 1)
# Subset the dataframe to remove these columns
mimic_preprocessed <- mimic[, !cols_with_one_value]
# Remove columns that are highly correlated with the outcome variable
# corrplot::corrplot(cor(select_if(mimic, is.numeric)), method = "color", tl.cex = 0.5)
mimic <- mimic %>%
  dplyr::select(-c("hosp_exp_flg", "icu_exp_flg", "mort_day_censored", "censor_flg"))

save(mimic_preprocessed, file = "./data/mimic_preprocessed.rda")
