# Load all package functions for testing
devtools::load_all()

adult_data <- read.csv("adult.csv")

# Ensure the response variable is 'income'
y <- adult_data$income  # Response variable

# Use columns 1 to 12 as features
X <- model.matrix(~., data = adult_data[, 1:4])  # Select columns 1 to 12 as features

# Test logistic regression function
beta <- logistic_regression(X, y)
print(beta)

# Test bootstrap confidence intervals
ci <- bootstrap_ci(X, y)
print(ci)

# Test model evaluation
predictions <- 1 / (1 + exp(-X %*% beta))
metrics <- evaluate_model(y, predictions)
print(metrics)
