# Assuming the dataset is in a CSV format, load the dataset
data <- read.csv("pml-training.csv")

# Remove variables with missing values
threshold <- 0.9 * nrow(data)
data_clean <- data[, colSums(is.na(data)) < threshold]

# Remove unnecessary variables
unrelated_vars <- c("X","user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "new_window", "num_window")
data_clean <- data_clean[ , !(names(data_clean) %in% unrelated_vars)]

# Ensure the response variable is a factor
data_clean$classe <- as.factor(data_clean$classe)






