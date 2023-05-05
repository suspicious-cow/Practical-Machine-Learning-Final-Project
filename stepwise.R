# set a seed in case we use any random values
set.seed(1337)

# Set the names of the packages and libraries you want to install
required_libraries <- c("magrittr", "randomForest", "caret", "gbm", "xgboost", "kernlab", "e1071", "combinat")

# Install missing packages and load all required libraries
for (lib in required_libraries) {
  if (!requireNamespace(lib, quietly = TRUE)) {
    install.packages(lib)
  }
  library(lib, character.only = TRUE)
}

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


cat("Dimensions of original data:\n", dim(data))

cat("Dimensions of clean data:\n", dim(data_clean))

# Feature selection using randomForest
set.seed(1337)
rf <- randomForest(classe ~ ., data = data_clean, importance = TRUE)


# Extract variable importance
importance_matrix <- as.data.frame(rf$importance)
rownames(importance_matrix) <- colnames(data_clean)[!colnames(data_clean) %in% "classe"]

# Order variables by importance
importance_order <- order(importance_matrix$MeanDecreaseGini, decreasing = TRUE)

# Select top N most important variables
N <- 20
selected_vars <- rownames(importance_matrix)[importance_order[1:N]]
data_selected <- data_clean[, c(selected_vars, "classe")]

# Check the selected variables
names(data_selected)

# create a graph to show importance visually
# Sort the importance scores in decreasing order
importance_sorted <- importance_matrix[order(importance_matrix$MeanDecreaseGini, decreasing = TRUE),]

# Limit the number of features displayed to the top 20 most important features
N <- 20
importance_topN <- importance_sorted[1:N, ]

# Adjust the margins of the plot 
par(mar = c(5, 8, 2, 2) + 0.1)

# Create a horizontal bar plot of the sorted variable importance scores 
barplot(importance_topN$MeanDecreaseGini, names.arg = rownames(importance_topN),
        horiz = TRUE, las = 2, cex.names = 0.7,
        xlab = "Importance Score",
        ylab = "",
        col = "#66b3ff",
        border = "black")

# Add a title to the plot
title("Top 20 Feature Importance in the Model", font.main = 2)

# Reset the plot parameters to default
par(mar = c(5, 4, 4, 2) + 0.1)



# Split the dataset into training (70%) and testing (30%) sets
set.seed(1337)
trainIndex <- createDataPartition(data_selected$classe, p = 0.7, list = FALSE)
train <- data_selected[trainIndex, ]
test <- data_selected[-trainIndex, ]

# Set up cross-validation
trControl <- trainControl(method = "cv", number = 5)

# Train models and evaluate performance
model_names <- c("rf", "xgb", "knn", "nb")
methods <- c("rf", "xgbTree", "knn", "naive_bayes")

results <- list()
resampling_results <- list()

for (i in 1:length(model_names)) {
  set.seed(1337)
  
  # Check if the current model is "xgb" and apply the unique setting
  if (model_names[i] == "xgb") {
    model <- caret::train(classe ~ ., data = train, method = methods[i], trControl = trControl, verbose = FALSE, verbosity = 0)
  } else {
    model <- caret::train(classe ~ ., data = train, method = methods[i], trControl = trControl)
  }
  
  pred <- predict(model, test)
  accuracy <- mean(pred == test$classe)
  results[[model_names[i]]] <- list(model = model, accuracy = accuracy)
  
  # Store resampling results for each model
  resampling_results[[model_names[i]]] <- model$resample
}


# Calculate expected out-of-sample error for each model
expected_out_of_sample_error <- sapply(resampling_results, function(x) mean(1 - x$Accuracy))

# Print expected out-of-sample error for each model
cat("Expected Out of Sample Error", "\n", expected_out_of_sample_error)

# Function to create ensemble for a given combination of models
get_ensemble_accuracy <- function(models, results, test) {
  ensemble_preds <- list()
  
  for (model_name in models) {
    prob_preds <- predict(results[[model_name]]$model, test, type = "prob")
    # Ensure that the columns are ordered consistently
    prob_preds <- prob_preds[, sort(colnames(prob_preds))]
    ensemble_preds[[model_name]] <- prob_preds
  }
  
  avg_probs <- Reduce("+", ensemble_preds) / length(ensemble_preds)
  final_pred <- colnames(avg_probs)[max.col(avg_probs, ties.method = "first")]
  ensemble_accuracy <- mean(final_pred == test$classe)
  
  return(ensemble_accuracy)
}

library(ggplot2)

# Create an empty data frame to store ensemble results
ensemble_results <- data.frame()

# Iterate through all possible combinations of models
for (num_models in 2:length(model_names)) {
  combinations <- combn(model_names, num_models, simplify = FALSE)
  
  for (combination in combinations) {
    ensemble_accuracy <- get_ensemble_accuracy(combination, results, test)
    
    # Store the combination and accuracy in the ensemble_results data frame
    ensemble_results <- rbind(ensemble_results, data.frame(
      Combination = paste(combination, collapse = ", "),
      Accuracy = ensemble_accuracy
    ))
  }
}

# Find the best combination and accuracy
best_accuracy <- max(ensemble_results$Accuracy)
best_combination <- ensemble_results$Combination[which.max(ensemble_results$Accuracy)]

cat("Best combination:", best_combination, "\n",
    "Best accuracy:", best_accuracy, "\n")

# Create a bar plot of ensemble accuracies
ggplot(ensemble_results, aes(x = Combination, y = Accuracy, fill = Combination)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  labs(
    title = "Ensemble Accuracy for Each Combination",
    x = "Combinations",
    y = "Accuracy"
  )



# load the final test dataset
new_test_data <- read.csv("pml-testing.csv")

# Get probability predictions for each class using the trained models
rf_probs <- predict(results[["rf"]]$model, new_test_data, type = "prob")
xgb_probs <- predict(results[["xgb"]]$model, new_test_data, type = "prob")

# Ensure that the columns are ordered consistently
rf_probs <- rf_probs[, sort(colnames(rf_probs))]
xgb_probs <- xgb_probs[, sort(colnames(xgb_probs))]

# Average the class probabilities
avg_probs <- (rf_probs + xgb_probs) / 2

# Select the class with the highest probability as the final prediction
final_pred <- colnames(avg_probs)[max.col(avg_probs, ties.method = "first")]

# The variable final_pred now contains the final predictions for the new_test_data
cat("Predictions for final test set: ", final_pred)
