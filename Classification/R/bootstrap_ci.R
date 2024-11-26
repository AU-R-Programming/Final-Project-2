#' Bootstrap Confidence Intervals
#'
#' @param X A matrix of predictors (including intercept column).
#' @param y A vector of binary responses (0/1).
#' @param n_boot Number of bootstrap samples (default = 20).
#' @param alpha Significance level (default = 0.05).
#' @return A matrix of confidence intervals for each coefficient.
#' @export
bootstrap_ci <- function(X, y, n_boot = 20, alpha = 0.05) {
  beta_estimates <- replicate(n_boot, {
    idx <- sample(1:nrow(X), replace = TRUE)
    logistic_regression(X[idx, ], y[idx])
  })

  lower <- apply(beta_estimates, 1, quantile, probs = alpha / 2)
  upper <- apply(beta_estimates, 1, quantile, probs = 1 - alpha / 2)
  ci <- cbind(lower, upper)
  return(ci)
}
