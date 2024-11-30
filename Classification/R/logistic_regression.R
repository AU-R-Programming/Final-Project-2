#' Logistic Regression Using Numerical Optimization
#'
#' This function performs logistic regression using numerical optimization with optional ridge regularization
#' for numerical stability.
#'
#' @param X A matrix of predictors (including the intercept column, if required).
#' @param y A binary response vector (0/1).
#' @param ridge_penalty A small constant for ridge regularization to stabilize matrix inversion
#' (default is \code{1e-5}).
#'
#' @return A named vector of optimized coefficients (\eqn{\beta}).
#' @examples
#' # Example usage:
#' # Simulated data
#' set.seed(123)
#' X <- cbind(1, matrix(rnorm(100 * 2), ncol = 2))  # Add intercept and 2 features
#' y <- rbinom(100, 1, 0.5)  # Binary response
#' beta <- logistic_regression(X, y)
#' print(beta)
#'
#' @export
logistic_regression <- function(X, y, ridge_penalty = 1e-5) {
  # Regularized initial values for stability
  beta_init <- solve(t(X) %*% X + diag(ridge_penalty, ncol(X)), t(X) %*% y)

  # Log-likelihood function
  log_likelihood <- function(beta) {
    p <- 1 / (1 + exp(-X %*% beta))  # Predicted probabilities
    epsilon <- 1e-8                 # Small constant to prevent log(0)
    p <- pmax(pmin(p, 1 - epsilon), epsilon)
    -sum(y * log(p) + (1 - y) * log(1 - p))
  }

  # Optimization using L-BFGS-B
  opt <- optim(par = beta_init, fn = log_likelihood, method = "L-BFGS-B")
  return(opt$par)
}
