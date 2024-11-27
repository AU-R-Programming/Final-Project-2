


adult_data <- adult_data[, -c(3, 5)]


# Save the cleaned adult_data to a CSV file
write.csv(adult_data, "adult.csv", row.names = FALSE)


