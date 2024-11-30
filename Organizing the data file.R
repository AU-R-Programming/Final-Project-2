# Load the dataset
adult_data <- read.csv("adult.csv")

# Example: Remove columns 3 and 5
adult_data <- adult_data[, -c(6, 7)]

# Save the updated dataset to a CSV file
write.csv(adult_data, "adult.csv", row.names = FALSE)


