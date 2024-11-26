#' Logistic Regression Optimization
#'
#' This function performs logistic regression using numerical optimization.
#' @param X A matrix of predictors (including intercept column).
#' @param y A vector of binary responses (0/1).
#' @return A named vector of coefficients (β).
#' @export
logistic_regression <- function(X, y) {
  n <- nrow(X)

  # Log-likelihood function
  log_likelihood <- function(beta) {
    p <- 1 / (1 + exp(-X %*% beta))
    -sum(y * log(p) + (1 - y) * log(1 - p))
  }

  # Initial values using least squares
  beta_init <- solve(t(X) %*% X, t(X) %*% y)

  # Optimization
  opt <- optim(par = beta_init, fn = log_likelihood, method = "BFGS")
  return(opt$par)
}
