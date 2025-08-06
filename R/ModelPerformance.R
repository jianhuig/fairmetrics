# -----------------------------------------------------------------------------
# Model Performance Metrics
# -----------------------------------------------------------------------------

#' Calculate the true positive rate
#' @param data Data frame containing the outcome, predicted outcome, and
#' protected attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the protected attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @param digits the number of digits to round the result to, default is 2
#' @return a vector of true positive rates
#' @noRd

get_tpr <- function(data, outcome, group, probs, cutoff = 0.5, digits = 2) {
  tpr <- c() # Initialize empty vector for true positive rates
  groups <- sort(unique(data[, group])) # Get sorted unique groups

  for (i in groups) {
    tp <- sum(data[, outcome] == 1 & data[, group] == i & data[, probs] >= cutoff, na.rm=TRUE)
    p <- sum(data[, outcome] == 1 & data[, group] == i, na.rm = TRUE)
    tpr <- c(tpr, round(tp / p, digits)) # Calculate TPR and add to vector
  }

  return(tpr)
}


#' Calculate the false positive rate
#' @param data Data frame containing the outcome, predicted outcome, and
#' protected attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the protected attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @param digits the number of digits to round the result to, default is 2
#' @return a vector of false positive rates
#' @noRd

get_fpr <- function(data, outcome, group, probs, cutoff = 0.5, digits = 2) {
  fpr <- c()
  groups <- sort(unique(data[, group]))
  for (i in groups) {
    fp <- sum(data[, outcome] == 0 &
      data[, group] == i &
      data[, probs] >= cutoff, na.rm = TRUE)
    n <- sum(data[, outcome] == 0 & data[, group] == i, na.rm=TRUE)
    fpr <- c(fpr, round(fp / n, digits))
  }
  return(fpr)
}

#' Calculate the probability of positive prediction
#' @param data Data frame containing the outcome, predicted outcome, and
#' protected attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the protected attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @param digits the number of digits to round the result to, default is 2
#' @return a vector of probability of positive prediction
#' @noRd

get_ppr <- function(data, outcome, group, probs, cutoff = 0.5, digits = 2) {
  ppr <- c()
  groups <- sort(unique(data[, group]))
  for (i in groups) {
    pp <- sum(
      data[, group] == i &
        data[, probs] >= cutoff,
      na.rm = TRUE
    )
    n <- sum(data[, group] == i, na.rm=TRUE)
    ppr <- c(ppr, round(pp / n, digits))
  }
  return(ppr)
}

#' Calculate the positive predictive value
#' @param data Data frame containing the outcome, predicted outcome, and
#' protected attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the protected attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @param digits the number of digits to round the result to, default is 2
#' @return a vector of positive predictive value
#' @noRd

get_ppv <- function(data, outcome, group, probs, cutoff = 0.5, digits = 2) {
  ppv <- c()
  groups <- sort(unique(data[, group]))
  for (i in groups) {
    tp <- sum(data[, outcome] == 1 &
      data[, group] == i &
      data[, probs] >= cutoff, na.rm=TRUE)
    pp <- sum(data[, group] == i &
      data[, probs] >= cutoff, na.rm=TRUE)
    ppv <- c(ppv, round(tp / pp, digits))
  }
  return(ppv)
}

#' Calculate the negative predictive value
#' @param data Data frame containing the outcome, predicted outcome, and
#' protected attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the protected attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @param digits the number of digits to round the result to, default is 2
#' @return  a vector of negative predictive value
#' @noRd

get_npv <- function(data, outcome, group, probs, cutoff = 0.5, digits = 2) {
  npv <- c()
  groups <- sort(unique(data[, group]))
  for (i in groups) {
    tn <- sum(data[, outcome] == 0 &
      data[, group] == i &
      data[, probs] < cutoff, na.rm =TRUE)
    nn <- sum(data[, group] == i &
      data[, probs] < cutoff, na.rm = TRUE)
    npv <- c(npv, round(tn / nn, digits))
  }
  return(npv)
}

#' Calculate the accuracy
#' @param data Data frame containing the outcome, predicted outcome, and
#' protected attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the protected attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @param digits the number of digits to round the result to, default is 2
#' @return a vector of accuracy
#' @noRd

get_acc <- function(data, outcome, group, probs, cutoff = 0.5, digits = 2) {
  acc <- c()
  groups <- sort(unique(data[, group]))
  for (i in groups) {
    groups <- sort(unique(data[[group]]))
    tp <- sum(data[, outcome] == 1 & data[,group] == i & data[,probs] >= cutoff, na.rm=TRUE)
    tn <- sum(data[, outcome] == 0 & data[,group] == i & data[,probs] < cutoff, na.rm=TRUE)
    p <- sum(data[,group] == i, na.rm = TRUE)
    acc <- c(acc, round((tp + tn) / p, digits))
  }
  return(acc)
}

#' Calculate the brier score
#' @param data Data frame containing the outcome, predicted outcome, and
#' protected attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the protected attribute
#' @param probs the name of the predicted outcome variable
#' @param digits the number of digits to round the result to, default is 2
#' @return a vector of brier score
#' @noRd

get_brier_score <- function(data, outcome, group, probs, digits = 2) {
  brier_score <- c()
  groups <- sort(unique(data[, group]))
  for (i in groups) {
    sub_data <- data[data[, group] == i, ]
    brier_score <- c(brier_score, round(mean((sub_data[, outcome] - sub_data[, probs])^2, na.rm=TRUE), digits))
  }
  return(brier_score)
}

#' Calculate the the ratio of false negative to false positive
#' @param data Data frame containing the outcome, predicted outcome, and
#' protected attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the protected attribute
#' @param probs the name of the predicted outcome variable
#' @param cutoff the threshold for the predicted outcome, default is 0.5
#' @param digits the number of digits to round the result to, default is 2
#' @return a vector of the ratio of false negative to false positive
#' @noRd

get_err_ratio <- function(data, outcome, group, probs, cutoff = 0.5,
                          digits = 2) {
  err_ratio <- c()
  groups <- sort(unique(data[, group]))
  for (i in groups) {
    sub_data <- data[data[, group] == i, ]
    fp <- sum(sub_data[, outcome] == 0 &
      sub_data[, probs] >= cutoff, na.rm =TRUE)
    fn <- sum(sub_data[, outcome] == 1 &
      sub_data[, probs] < cutoff, na.rm =TRUE)
    err_ratio <- c(err_ratio, round(fn / fp, digits))
  }
  return(err_ratio)
}

#' Calculate the average predicted probability
#' @param data Data frame containing the outcome, predicted outcome, and
#' protected attribute
#' @param outcome the name of the outcome variable, it must be binary
#' @param group the name of the protected attribute
#' @param probs the name of the predicted outcome variable
#' @param digits the number of digits to round the result to, default is 2
#' @return a vector of average predicted probability
#' @noRd

get_avg_prob <- function(data, outcome, group, probs, digits = 2) {
  avg_prob <- c()
  groups <- sort(unique(data[, group]))
  for (i in groups) {
    sub_data <- data[data[, group] == i, ]
    avg_prob <- c(avg_prob, round(mean(sub_data[, probs],na.rm=TRUE), digits))
  }
  return(avg_prob)
}
