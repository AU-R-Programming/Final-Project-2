#' Confusion Matrix and Metrics
#'
#' @param y_true True binary response values.
#' @param y_pred Predicted probabilities.
#' @param cutoff Cut-off value for classification (default = 0.5).
#' @return A list containing confusion matrix and various metrics.
#' @export
evaluate_model <- function(y_true, y_pred, cutoff = 0.5) {
  y_class <- ifelse(y_pred > cutoff, 1, 0)

  tp <- sum(y_class == 1 & y_true == 1)
  fp <- sum(y_class == 1 & y_true == 0)
  fn <- sum(y_class == 0 & y_true == 1)
  tn <- sum(y_class == 0 & y_true == 0)

  prevalence <- mean(y_true)
  accuracy <- (tp + tn) / length(y_true)
  sensitivity <- tp / (tp + fn)
  specificity <- tn / (tn + fp)
  false_discovery_rate <- fp / (fp + tp)
  diagnostic_odds_ratio <- (tp / fn) / (fp / tn)

  confusion_matrix <- matrix(c(tp, fn, fp, tn), nrow = 2,
                             dimnames = list(Predicted = c("Positive", "Negative"),
                                             True = c("Positive", "Negative")))

  metrics <- list(
    ConfusionMatrix = confusion_matrix,
    Prevalence = prevalence,
    Accuracy = accuracy,
    Sensitivity = sensitivity,
    Specificity = specificity,
    FDR = false_discovery_rate,
    DOR = diagnostic_odds_ratio
  )
  return(metrics)
}
