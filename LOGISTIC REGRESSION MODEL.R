library(tidyverse)
library(caret)
library(pROC)
# ======================================================================
# Load the Dataset
# ======================================================================
data <- read.csv("heart_disease.csv")

# Remove the variable "num"
data <- data[, !names(data) %in% "num"]
dim(data)
names(data)


# Convert category variable into character 
data <- data %>%
  mutate(
    sex = factor(sex),
    cp = factor(cp),
    fbs = factor(fbs),
    restecg = factor(restecg),
    exang = factor(exang),
    slope = factor(slope),
    ca = factor(ca),
    thal = factor(thal),
    target_binary = factor(
      target_binary,
      levels = c(0, 1)
    )
  )

# Check dataset
str(data)
dim(data)
# ======================================================================
# Model Preparation
# ======================================================================
set.seed(123)

# Split training and testing data
train_index <- createDataPartition(
  data$target_binary,
  p = 0.80,
  list = FALSE
)

train_data <- data[train_index, ]
test_data <- data[-train_index, ]

cat("Training observations:", nrow(train_data), "\n")
cat("Testing observations:", nrow(test_data), "\n")

# Check class distribution
prop.table(table(train_data$target_binary))
prop.table(table(test_data$target_binary))

# ======================================================================
# StandardiZe numerical variables
# ======================================================================
numeric_cols <- c(
  "age", "trestbps", "chol",
  "thalach", "oldpeak"
)

# Calculate parameters using training data only
train_means <- sapply(
  train_data[numeric_cols], mean
)

train_sds <- sapply(
  train_data[numeric_cols], sd
)

# Standardise training data
train_data[numeric_cols] <- scale(
  train_data[numeric_cols],
  center = train_means,
  scale = train_sds
)

# Apply the same parameters to testing data
test_data[numeric_cols] <- scale(
  test_data[numeric_cols],
  center = train_means,
  scale = train_sds
)


# ======================================================================
# Build Logistic Regression Model
# ======================================================================
logistic_model <- glm(
  target_binary ~ .,
  data = train_data,
  family = binomial(link = "logit")
)

# Model summary
summary(logistic_model)



# ======================================================================
# Assumption Checking for Logistic Regression
# ======================================================================

# A. Binary Outcome Variable
# Check whether the dependent variable has two categories

table(train_data$target_binary)

cat("Number of outcome categories:",
    nlevels(train_data$target_binary), "\n")


# ======================================================================
# B. Multicollinearity (VIF)
# ======================================================================

library(car)

vif_values <- car::vif(logistic_model)

print(vif_values)

# Interpretation:
# VIF < 5  : Generally acceptable
# VIF >= 5 : Potential multicollinearity problem
# VIF >= 10: Serious multicollinearity problem

# For categorical variables with multiple degrees of freedom,
# car::vif() may return GVIF.
# Adjusted GVIF should be used for interpretation.

if (is.matrix(vif_values)) {
  
  adjusted_vif <- vif_values[, "GVIF"] ^
    (1 / (2 * vif_values[, "Df"]))
  
  print(adjusted_vif)
  
} else {
  
  print(vif_values)
}


# ======================================================================
# C. Linearity of the Logit (Box-Tidwell Test)
# ======================================================================

# Continuous independent variables
continuous_vars <- c(
  "age", "trestbps", "chol",
  "thalach", "oldpeak"
)

# Box-Tidwell requires positive continuous predictors.
# Shift variables if necessary.

bt_data <- train_data

for (var in continuous_vars) {
  
  if (any(bt_data[[var]] <= 0)) {
    
    bt_data[[var]] <- bt_data[[var]] -
      min(bt_data[[var]]) + 0.001
  }
}

# Construct interaction terms between each predictor
# and its natural logarithm.

for (var in continuous_vars) {
  
  bt_data[[paste0(var, "_log")]] <-
    bt_data[[var]] * log(bt_data[[var]])
}

# Fit Box-Tidwell model
bt_formula <- as.formula(
  paste(
    "target_binary ~",
    paste(
      c(
        setdiff(names(train_data),
                "target_binary"),
        paste0(continuous_vars, "_log")
      ),
      collapse = " + "
    )
  )
)

box_tidwell_model <- glm(
  bt_formula,
  data = bt_data,
  family = binomial(link = "logit")
)

# Examine interaction term p-values
bt_summary <- summary(box_tidwell_model)

bt_results <- coef(
  bt_summary
)[paste0(continuous_vars, "_log"),
  , drop = FALSE]

print(bt_results)

# Interpretation:
# p-value > 0.05: No significant evidence against linearity
# p-value < 0.05: Possible violation of linearity assumption


# ======================================================================
# D. Influential Observations (Cook's Distance)
# ======================================================================

# Calculate Cook's Distance
cooks_distance <- cooks.distance(logistic_model)

# Plot Cook's Distance
plot(
  cooks_distance,
  type = "h",
  main = "Cook's Distance - Logistic Regression",
  xlab = "Observation",
  ylab = "Cook's Distance"
)

# Reference threshold
cook_threshold <- 4 / nrow(train_data)

abline(
  h = cook_threshold,
  col = "red",
  lty = 2,
  lwd = 2
)

# Identify potentially influential observations
influential_obs <- which(
  cooks_distance > cook_threshold
)

cat("Cook's Distance threshold:",
    cook_threshold, "\n")

cat("Number of potentially influential observations:",
    length(influential_obs), "\n")

print(influential_obs)


# ======================================================================
# E. Independence of Observations
# ======================================================================

# Independence cannot be confirmed through a statistical
# test alone. It should be justified based on data collection.

cat("Independence of observations should be assessed",
    "based on the dataset's sampling design.\n")


# ======================================================================
# Predict using Testing data
# ======================================================================
# Predict probabilities
predicted_prob <- predict(
  logistic_model,
  newdata = test_data,
  type = "response"
)

# Convert probabilities into classes
predicted_class <- ifelse(
  predicted_prob >= 0.5,
  1, 0
)

predicted_class <- factor(
  predicted_class,
  levels = c(0, 1)
)

# Compare predictions
results <- data.frame(
  Actual = test_data$target_binary,
  Predicted = predicted_class,
  Probability = predicted_prob
)

head(results)

# ======================================================================
# Model Evaluation
# ======================================================================
# A. Confusion Matrix
conf_matrix <- confusionMatrix(
  data = predicted_class,
  reference = test_data$target_binary,
  positive = "1"
)

print(conf_matrix)

# B. Evaluation Metrics
accuracy <- conf_matrix$overall["Accuracy"]
precision <- conf_matrix$byClass["Pos Pred Value"]
recall <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
f1_score <- conf_matrix$byClass["F1"]

cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("Specificity:", specificity, "\n")
cat("F1 Score:", f1_score, "\n")

# C. ROC Curve and AUC
roc_curve <- roc(
  response = test_data$target_binary,
  predictor = predicted_prob,
  levels = c("0", "1"),
  direction = "<"
)

plot(
  roc_curve,
  main = "ROC Curve - Logistic Regression",
  col = "blue",
  lwd = 2
)

auc_value <- auc(roc_curve)

cat("AUC:", as.numeric(auc_value), "\n")

# SUMMARY
# Regression coefficients
summary(logistic_model)

# Calculate odds ratios
odds_ratios <- exp(coef(logistic_model))

print(odds_ratios)

# Confidence intervals for odds ratios
exp(confint(logistic_model))


# ======================================================================
# Save File for nex part can be use in Shiny app
# ======================================================================
model_bundle <- list(
  model = logistic_model,
  means = train_means,
  sds = train_sds,
  factor_levels = lapply(
    train_data[c(
      "sex", "cp", "fbs", "restecg",
      "exang", "slope", "ca", "thal"
    )],
    levels
  )
)

# ==========================================================
# NEW PATIENT PREDICTION TEST
# ==========================================================

new_patient <- data.frame(
  age = 55,
  sex = factor(1, levels = levels(train_data$sex)),
  cp = factor(4, levels = levels(train_data$cp)),
  trestbps = 140,
  chol = 250,
  fbs = factor(0, levels = levels(train_data$fbs)),
  restecg = factor(0, levels = levels(train_data$restecg)),
  thalach = 150,
  exang = factor(0, levels = levels(train_data$exang)),
  oldpeak = 2.3,
  slope = factor(2, levels = levels(train_data$slope)),
  ca = factor(0, levels = levels(train_data$ca)),
  thal = factor(3, levels = levels(train_data$thal))
)

# Standardise numerical variables using training parameters
new_patient[numeric_cols] <- scale(
  new_patient[numeric_cols],
  center = train_means,
  scale = train_sds
)

# Predict
new_patient_probability <- predict(
  logistic_model,
  newdata = new_patient,
  type = "response"
)

cat("\nNEW PATIENT LOGISTIC REGRESSION RESULT\n")
cat("Healthy Probability:",
    round(1 - new_patient_probability, 4), "\n")
cat("Heart Disease Probability:",
    round(new_patient_probability, 4), "\n")

saveRDS(
  model_bundle,
  file = "~/HeartDiseaseApp/heart_disease_model.rds"
)
