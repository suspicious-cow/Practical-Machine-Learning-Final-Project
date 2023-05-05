# Load required libraries
library(caret)
library(randomForest)
library(gbm)
library(xgboost)
library(kernlab)
library(e1071)


# Feature selection using randomForest
set.seed(123)
rf <- randomForest(classe ~ ., data = data_clean, importance = TRUE)
importance_order <- order(rf$importance, decreasing = TRUE)

# Select top N most important variables
N <- 20
selected_vars <- colnames(rf$importance)[importance_order[1:N]]
data_selected <- data_clean[, c(selected_vars, "classe")]

# Split the dataset into training (70%) and testing (30%) sets
set.seed(123)
trainIndex <- createDataPartition(data_selected$classe, p = 0.7, list = FALSE)
train <- data_selected[trainIndex, ]
test <- data_selected[-trainIndex, ]

# Set up cross-validation
trControl <- trainControl(method = "cv", number = 5)

# Train models and evaluate performance
model_names <- c("rf", "gbm", "xgb", "svmRadial", "knn", "nb")
methods <- c("rf", "gbm", "xgbTree", "svmRadial", "knn", "naive_bayes")

results <- list()

for (i in 1:length(model_names)) {
  set.seed(123)
  model <- caret::train(classe ~ ., data = train, method = methods[i], trControl = trControl)
  pred <- predict(model, test)
  accuracy <- mean(pred == test$classe)
  results[[model_names[i]]] <- list(model = model, accuracy = accuracy)
}

# Print the results
results
