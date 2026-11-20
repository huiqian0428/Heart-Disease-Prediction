
# ============================================================
# STS4412 GROUP ASSIGNMENT
# DECISION TREE MODELLING
# Dataset: Heart Disease
# ============================================================


# ============================================================
# 1. LOAD REQUIRED PACKAGES
# ============================================================

packages <- c(
  "tidyverse",
  "readr",
  "caret",
  "rpart",
  "rpart.plot",
  "pROC",
  "ggplot2"
)

for (package in packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(paste("Please install the package:", package))
  }
  library(package, character.only = TRUE)
}


# ============================================================
# 2. IMPORT CLEANED DATASET
# ============================================================

cleaned_data <- read.csv("heart_disease.csv")

cat("\nDATASET DIMENSIONS\n")
print(dim(cleaned_data))

cat("\nTARGET VARIABLE DISTRIBUTION\n")
print(table(cleaned_data$target_binary))



# ============================================================
# 3. DATA PREPARATION
# ============================================================

model_data <- cleaned_data %>%
  mutate(
    age = as.numeric(age),
    trestbps = as.numeric(trestbps),
    chol = as.numeric(chol),
    thalach = as.numeric(thalach),
    oldpeak = as.numeric(oldpeak),
    
    ca = factor(ca),
    thal = factor(thal),
    
    target_binary = factor(
      target_binary,
      levels = c(0, 1),
      labels = c("Healthy", "Heart_Disease")
    )
  )%>%
  select(-num)

cat("\nPREPARED DATASET STRUCTURE\n")
str(model_data)

cat("\nTARGET VARIABLE DISTRIBUTION\n")
print(table(model_data$target_binary, useNA = "ifany"))

cat("\nMISSING TARGET VALUES\n")
print(sum(is.na(model_data$target_binary)))



# ============================================================
# 4. SPLIT DATA INTO TRAINING AND TESTING SETS
# ============================================================

set.seed(123)

train_index <- createDataPartition(
  model_data$target_binary,
  p = 0.80,
  list = FALSE
)

train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

cat("\nTRAINING DATA DIMENSIONS\n")
print(dim(train_data))

cat("\nTESTING DATA DIMENSIONS\n")
print(dim(test_data))


# ============================================================
# 5. CONSTRUCT INITIAL DECISION TREE
# ============================================================

tree_model <- rpart(
  target_binary ~ .,
  data = train_data,
  method = "class",
  control = rpart.control(
    cp = 0.001,
    minsplit = 20,
    xval = 10
  )
)

cat("\nINITIAL DECISION TREE\n")
print(tree_model)


# ============================================================
# 6. CROSS-VALIDATION AND CP SELECTION
# ============================================================

# Plot cross-validation error against CP
plotcp(
  tree_model,
  main = "Cross-Validation Error vs Complexity Parameter"
)

# Display CP table
cat("\nCOMPLEXITY PARAMETER TABLE\n")
printcp(tree_model)


# ============================================================
# 7. INITIAL DECISION TREE VISUALISATION
# ============================================================

rpart.plot(
  tree_model,
  type = 2,
  extra = 104,
  fallen.leaves = TRUE,
  main = "Initial Decision Tree for Heart Disease Classification"
)


# ============================================================
# 8. PRUNE DECISION TREE
# ============================================================

# Selected CP based on cross-validation results
pruned_tree <- prune(
  tree_model,
  cp = 0.0132979
)

cat("\nPRUNED DECISION TREE\n")
print(pruned_tree)

cat("\nPRUNED TREE CP TABLE\n")
printcp(pruned_tree)


# ============================================================
# 9. PRUNED DECISION TREE VISUALISATION
# ============================================================

rpart.plot(
  pruned_tree,
  type = 2,
  extra = 104,
  fallen.leaves = TRUE,
  main = "Pruned Decision Tree for Heart Disease Classification"
)


# ============================================================
# 10. MODEL EVALUATION
# ============================================================

# Predict classification on testing data
tree_predictions <- predict(
  pruned_tree,
  newdata = test_data,
  type = "class"
)

# Confusion matrix and classification metrics
confusion_result <- confusionMatrix(
  tree_predictions,
  test_data$target_binary,
  positive = "Heart_Disease"
)

cat("\nCONFUSION MATRIX AND MODEL PERFORMANCE\n")
print(confusion_result)


# ============================================================
# 11. PREDICTED PROBABILITIES
# ============================================================

tree_probabilities <- predict(
  pruned_tree,
  newdata = test_data,
  type = "prob"
)

cat("\nFIRST SIX PREDICTED PROBABILITIES\n")
print(head(tree_probabilities))


# ============================================================
# 12. ROC CURVE AND AUC
# ============================================================

roc_tree <- roc(
  response = test_data$target_binary,
  predictor = tree_probabilities[, "Heart_Disease"],
  levels = c("Healthy", "Heart_Disease"),
  direction = "<"
)

auc_tree <- as.numeric(auc(roc_tree))

cat("\nAREA UNDER THE CURVE (AUC)\n")
print(auc_tree)

# Plot ROC curve
plot(
  roc_tree,
  main = "ROC Curve - Decision Tree",
  col = "blue",
  lwd = 2
)

abline(
  a = 0,
  b = 1,
  lty = 2,
  col = "red"
)


# ============================================================
# 13. FEATURE IMPORTANCE
# ============================================================

# Extract importance from the initial fitted tree
importance <- tree_model$variable.importance

importance_df <- data.frame(
  Variable = names(importance),
  Importance = as.numeric(importance)
) %>%
  arrange(desc(Importance))

cat("\nFEATURE IMPORTANCE\n")
print(importance_df)

# Plot feature importance
ggplot(
  importance_df,
  aes(
    x = reorder(Variable, Importance),
    y = Importance
  )
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Feature Importance - Initial Decision Tree",
    x = "Predictor Variables",
    y = "Importance Score"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    )
  )


# ============================================================
# 14. NEW PATIENT PREDICTION DEMONSTRATION
# ============================================================

# Example patient characteristics

new_patient <- data.frame(
  age = 55,
  sex = 1,
  cp = 4,
  trestbps = 140,
  chol = 250,
  fbs = 0,
  restecg = 0,
  thalach = 150,
  exang = 0,
  oldpeak = 2.3,
  slope = 2,
  ca = 0,
  thal = 3
)


# Match categorical variables to training data
for (variable in setdiff(names(train_data), "target_binary")) {
  if (is.factor(train_data[[variable]])) {
    new_patient[[variable]] <- factor(
      new_patient[[variable]],
      levels = levels(train_data[[variable]])
    )
  }
}

# Predict patient classification
patient_prediction <- predict(
  pruned_tree,
  newdata = new_patient,
  type = "class"
)

# Predict patient probabilities
patient_probability <- predict(
  pruned_tree,
  newdata = new_patient,
  type = "prob"
)

# Display prediction results
prediction_result <- data.frame(
  Predicted_Class = as.character(patient_prediction),
  Healthy_Probability = patient_probability[, "Healthy"],
  Heart_Disease_Probability = patient_probability[, "Heart_Disease"]
)

cat("\nNEW PATIENT PREDICTION RESULT\n")
print(prediction_result)


# ============================================================
# END OF DECISION TREE ANALYSIS
# ============================================================


