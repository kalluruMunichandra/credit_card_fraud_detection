# Load required libraries
library(randomForest)
library(caret)
library(ROCR)

# Load the dataset (you need to replace 'your_dataset.csv' with your actual dataset)
data <- read.csv("your_dataset.csv")

# Split data into features and target variable
features <- data[, -c("Class", "Time")]  # Exclude 'Class' and 'Time' columns
target <- data$Class

# Split data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(target, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Train a Random Forest model
model <- randomForest(Class ~ ., data = trainData)

# Make predictions on the test set
predictions <- predict(model, newdata = testData)

# Convert predictions to binary format
binary_predictions <- as.numeric(predictions == "fraud")

# Evaluate the model
confusion_matrix <- table(binary_predictions, testData$Class)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))

# Generate ROC curve
ROCR_pred <- prediction(predictions, testData$Class)
ROCR_perf <- performance(ROCR_pred, "tpr", "fpr")
plot(ROCR_perf, colorize = TRUE, print.cutoffs.at = seq(0, 1, by = 0.1), text.adj = c(-0.2,1.7))

# Print Area Under the Curve (AUC)
auc <- as.numeric(performance(ROCR_pred, "auc")@y.values)
print(paste("AUC:", auc))
