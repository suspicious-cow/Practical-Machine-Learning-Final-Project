# Assuming 'importance_matrix' contains the variable importance scores
barplot(importance_matrix$MeanDecreaseGini, names.arg = rownames(importance_matrix),
        main = "Variable Importance Scores", xlab = "Features", ylab = "Importance")

# Assuming 'expected_out_of_sample_error' contains the expected out-of-sample error for each model
barplot(expected_out_of_sample_error, names.arg = names(expected_out_of_sample_error),
        main = "Expected Out-of-Sample Error", xlab = "Models", ylab = "Error")


# Assuming 'ensemble_accuracies' is a named vector containing the accuracy of different combinations
barplot(ensemble_accuracies, names.arg = names(ensemble_accuracies),
        main = "Ensemble Model Accuracies", xlab = "Model Combinations", ylab = "Accuracy")



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



# Convert 'expected_out_of_sample_error' to a data frame for ggplot
expected_error_df <- data.frame(
  Model = names(expected_out_of_sample_error),
  Error = as.numeric(expected_out_of_sample_error)
)

# Create a bar plot using ggplot2
ggplot(expected_error_df, aes(x = Model, y = Error, fill = Model)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  scale_fill_manual(values = c("rf" = "#66b3ff", "xgb" = "#ff9999", "knn" = "#99ff99", "nb" = "#ffcc99")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  labs(
    title = "Expected Out-of-Sample Error",
    x = "Models",
    y = "Error"
  )

# Note: You can customize the colors by changing the values in 'scale_fill_manual'.

