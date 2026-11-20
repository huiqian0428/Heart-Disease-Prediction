
# STS4412 Group Assignment
# Dataset: heart_disease.csv

# Task 1: Dataset Understanding & Data Preprocessing

library(tidyverse)
raw_data <- read.csv("heart_disease.csv")

cat("1. DATASET DIMENSIONS")
print(dim(raw_data))

cat("2. VARIABLE NAMES & DATA TYPES")
str(raw_data)

cat("3. TARGET VARIABLE INSPECTION")
# Target variable is 'target_binary' (1 = Heart Disease, 0 = Healthy)
print(table(raw_data$target_binary))
print(prop.table(table(raw_data$target_binary)) * 100)

# Check Missing Values & Duplicate Records
cat("4. MISSING VALUES CHECK")
missing_counts <- colSums(is.na(raw_data))
print(missing_counts)

cat("5. DUPLICATE RECORDS CHECK")
cat("Number of duplicate rows:", sum(duplicated(raw_data)), "\n")

# Check Categorical Variables
cat("6. CATEGORICAL VARIABLES SUMMARY")

# Checking unique values for categorical features
categorical_cols <- c("sex", "cp", "fbs", "restecg", "exang", "slope", "ca", "thal")
for (col in categorical_cols) {
  cat("Unique values for", col, ":\n")
  print(table(raw_data[[col]]))
}

# Data Cleaning & Preprocessing
# Handle duplicates if any exist (remove them)
cleaned_data <- raw_data %>% distinct()

# Convert categorical variables into factors with appropriate labels 
cleaned_data <- cleaned_data %>%
  mutate(
    sex = factor(sex, levels = c(0, 1), labels = c("Female", "Male")),
    cp = factor(cp),
    fbs = factor(fbs, levels = c(0, 1), labels = c("False", "True")),
    restecg = factor(restecg),
    exang = factor(exang, levels = c(0, 1), labels = c("No", "Yes")),
    slope = factor(slope),
    target_binary = factor(target_binary, levels = c(0, 1), labels = c("Healthy", "Heart_Disease"))
  )

cat("7. CLEANED DATASET STRUCTURE")
str(cleaned_data)

# Prepare and Export the Final Cleaned Dataset
write.csv(cleaned_data, "heart_disease_cleaned.csv", row.names = FALSE)
