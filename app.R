# heart disease shiny app
# users upload a complete heart disease CSV containing target_binary.

# 1. app paths

app_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

clean_script <- file.path(app_dir, "EDA Assignment.R")
logistic_script <- file.path(app_dir, "LOGISTIC REGRESSION MODEL.R")
tree_script <- file.path(app_dir, "DecisionTreeAnalysis.R")
eda_module_script <- file.path(app_dir, "heart_eda_module.R")
default_data_file <- file.path(app_dir, "heart_disease.csv")

required_scripts <- c(
  clean_script,
  logistic_script,
  tree_script,
  eda_module_script
)

missing_scripts <- required_scripts[!file.exists(required_scripts)]

if (length(missing_scripts) > 0) {
  stop(
    "The following required R scripts are missing from the application folder:\n",
    paste(missing_scripts, collapse = "\n")
  )
}

# 2. packages

required_packages <- c(
  "shiny", "tidyverse", "readr", "caret", "pROC",
  "rpart", "rpart.plot", "ggplot2", "DT"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Please install these packages before running the application: ",
    paste(missing_packages, collapse = ", ")
  )
}

library(shiny)
library(tidyverse)
library(caret)
library(pROC)
library(rpart)
library(rpart.plot)
library(DT)

# 3. load eda module once

eda_env <- new.env(parent = globalenv())

source(
  eda_module_script,
  local = eda_env,
  echo = FALSE,
  print.eval = FALSE
)

# 4. validate and analyse an uploaded dataset

required_data_columns <- c(
  "age", "sex", "cp", "trestbps", "chol", "fbs",
  "restecg", "thalach", "exang", "oldpeak",
  "slope", "ca", "thal", "target_binary"
)

numeric_data_columns <- c(
  "age", "trestbps", "chol", "thalach", "oldpeak"
)

validate_heart_data <- function(data) {
  
  missing_columns <- setdiff(required_data_columns, names(data))
  
  if (length(missing_columns) > 0) {
    stop(
      "The uploaded CSV is missing these required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  
  if (nrow(data) < 20) {
    stop("The dataset must contain at least 20 observations.")
  }
  
  # original scripts do not perform missing-value imputation.
  missing_values <- vapply(
    data[required_data_columns],
    function(x) sum(is.na(x) | trimws(as.character(x)) == ""),
    numeric(1)
  )
  
  if (any(missing_values > 0)) {
    bad <- names(missing_values)[missing_values > 0]
    stop(
      "Missing values were found in required columns: ",
      paste(bad, collapse = ", "),
      ". Please clean these columns before uploading."
    )
  }
  
  # confirm that numeric variables can be read as numeric.
  for (variable in numeric_data_columns) {
    converted <- suppressWarnings(as.numeric(as.character(data[[variable]])))
    if (any(is.na(converted))) {
      stop("Column '", variable, "' must contain numeric values only.")
    }
    data[[variable]] <- converted
  }
  
  # original preprocessing and eda scripts expect these encodings.
  expected_categories <- list(
    sex = c("0", "1"),
    cp = c("1", "2", "3", "4"),
    fbs = c("0", "1"),
    restecg = c("0", "1", "2"),
    exang = c("0", "1"),
    target_binary = c("0", "1")
  )
  
  for (variable in names(expected_categories)) {
    values <- unique(as.character(data[[variable]]))
    invalid <- setdiff(values, expected_categories[[variable]])
    
    if (length(invalid) > 0) {
      stop(
        "Column '", variable, "' contains unsupported value(s): ",
        paste(invalid, collapse = ", "),
        ". Expected values: ",
        paste(expected_categories[[variable]], collapse = ", "),
        "."
      )
    }
  }
  
  target_values <- unique(as.character(data$target_binary))
  if (!all(c("0", "1") %in% target_values)) {
    stop("target_binary must contain both classes: 0 and 1.")
  }
  
  data
}

run_uploaded_analysis <- function(upload_path) {
  
  # separate directory is created for each analysis run.
  # original scripts write/read relative filenames, so they are executed while this directory is the working directory.
  session_dir <- tempfile("heart_disease_run_")
  dir.create(session_dir, recursive = TRUE)
  
  uploaded_file <- file.path(session_dir, "heart_disease.csv")
  cleaned_file <- file.path(session_dir, "heart_disease_cleaned.csv")
  model_bundle_file <- file.path(session_dir, "heart_disease_model.rds")
  
  if (!file.copy(upload_path, uploaded_file, overwrite = TRUE)) {
    stop("The uploaded dataset could not be copied for processing.")
  }
  
  uploaded_data <- read.csv(
    uploaded_file,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  uploaded_data <- validate_heart_data(uploaded_data)
  
  # keep original working directory and restore it after sourcing.
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(session_dir)
  
  # 4a. ori preprocessing script 
  # redirect file.choose() to the current user's uploaded csv
  env1 <- new.env(parent = globalenv())
  env1$file.choose <- function() uploaded_file
  
  source(
    clean_script,
    local = env1,
    echo = FALSE,
    print.eval = FALSE
  )
  
  if (!file.exists(cleaned_file)) {
    stop("The original preprocessing script did not create heart_disease_cleaned.csv.")
  }
  
  # 4b. original logistic regression script 
  # redirect file.choose() and saveRDS() without editing script.
  env3 <- new.env(parent = globalenv())
  env3$file.choose <- function() uploaded_file
  env3$saveRDS <- function(object, file, ...) {
    base::saveRDS(object, file = model_bundle_file, ...)
  }
  
  source(
    logistic_script,
    local = env3,
    echo = FALSE,
    print.eval = FALSE
  )
  
  if (!file.exists(model_bundle_file)) {
    stop("The original logistic regression script did not create its model bundle.")
  }
  
  bundle <- base::readRDS(model_bundle_file)
  
  # 4c. original decision tree script
  # reads heart_disease_cleaned.csv from the current run directory.
  env4 <- new.env(parent = globalenv())
  
  source(
    tree_script,
    local = env4,
    echo = FALSE,
    print.eval = FALSE
  )
  
  list(
    ok = TRUE,
    raw_data = uploaded_data,
    cleaned_data = env1$cleaned_data,
    preprocessing_env = env1,
    logistic_env = env3,
    tree_env = env4,
    bundle = bundle,
    session_dir = session_dir,
    rows = nrow(uploaded_data),
    columns = ncol(uploaded_data)
  )
}

# ============================================================
# 5. User Interface (ui)
# ============================================================

ui <- fluidPage(
  
  titlePanel("Heart Disease Analytics and Prediction"),
  
  p(
    "STS4412 Group Assignment | Exploratory Data Analysis ",
    "and Statistical Modelling"
  ),
  
  wellPanel(
    
    h3("Welcome to Heart Disease Analytics and Prediction"),
    p(
      "This application provides an interactive platform ",
      "for conducting exploratory data analysis and statistical ",
      "modelling of heart disease data."
    ),
    
    p(
      "Users can explore demographic and clinical characteristics ",
      "through descriptive statistics, frequency distributions, ",
      "histograms, boxplots, correlation analysis, and categorical ",
      "association analysis. The application also enables users ",
      "to evaluate and compare Logistic Regression and Decision Tree ",
      "models, as well as obtain predicted probabilities of heart ",
      "disease based on individual patient information."
    )
  ),
  
  tabsetPanel(
    
    # ------------------------------------------------------------
    # Tab 1: Exploratory Data Analysis
    # ------------------------------------------------------------
    
    tabPanel(
      title = "Exploratory Data Analysis",
      eda_env$heart_eda_ui("heart_eda")
    ),
    
    # ------------------------------------------------------------
    # Tab 2: Modelling
    # ------------------------------------------------------------
    tabPanel(
      title = "Modelling",
      
      tabsetPanel(
        
        # logistic regression
        tabPanel(
          title = "Logistic Regression",
          
          h3("Logistic Regression Model"),
          
          h4("Model Summary"),
          verbatimTextOutput("logistic_summary"),
          
          h4("Regression Coefficients and Odds Ratios"),
          DTOutput("odds_ratio_table"),
        ),
        
        # decision tree
        tabPanel(
          title = "Decision Tree",
          
          h3("Pruned Decision Tree"),
          plotOutput("decision_tree_plot", height = "600px"),
          
          h4("Complexity Parameter Table"),
          DTOutput("tree_cp_table"),
          
          h4("Feature Importance"),
          plotOutput("tree_importance_plot", height = "400px"),
        ),
        
        # model comparison
        tabPanel(
          title = "Model Comparison",
          
          h3("Logistic Regression vs Decision Tree"),
          
          p(
            "Both models use the training/testing procedures ",
            "specified in the original member scripts."
          ),
          
          DTOutput("model_comparison_table"),
          
          br(),
          
          fluidRow(
            column(
              6,
              h4("Logistic Regression: ROC Curve"),
              plotOutput("logistic_roc", height = "350px")
            ),
            column(
              6,
              h4("Decision Tree: ROC Curve"),
              plotOutput("tree_roc", height = "350px")
            )
          )
        ),
        
        # Prediction Part
        tabPanel(
          title = "Heart Disease Prediction",
          
          sidebarLayout(
            
            sidebarPanel(
              h4("Patient Information"),
              p("Enter the patient's clinical information for heart disease prediction."),
              
              numericInput("age", "Age (years)",value = 50, min = 1, max = 100),
              helpText("The patient's age in years."),
              
              selectInput("sex", "Sex", choices = character(0)),
              helpText("Biological sex of the patient."),
              
              selectInput("cp", "Chest Pain Type", choices = character(0)),
              helpText("Type of chest pain experienced by the patient."),
              
              numericInput("trestbps", "Resting Blood Pressure (mm Hg)",value = 120, min = 50, max = 200),
              helpText("Blood pressure measured while the patient is at rest."),
              
              numericInput("chol", "Serum Cholesterol (mg/dl)",value = 200, min = 50, max = 600),
              helpText("Serum cholesterol level measured in mg/dl."),
              
              selectInput("fbs", "Fasting Blood Sugar", choices = character(0)),
              helpText("Indicates whether fasting blood sugar is greater than 120 mg/dl."),
              
              selectInput("restecg", "Resting ECG Result", choices = character(0)),
              helpText("Results of the resting electrocardiogram."),
              
              numericInput("thalach", "Maximum Heart Rate",value = 150, min = 50, max = 250),
              helpText("Maximum heart rate achieved during exercise."),
              
              selectInput("exang", "Exercise-Induced Angina",choices = character(0)),
              helpText("Indicates whether exercise causes angina (chest pain)."),
              
              numericInput("oldpeak", "ST Depression (Oldpeak)",value = 1, min = 0, max = 10, step = 0.1),
              helpText("ST depression induced by exercise relative to rest."),
              
              selectInput("slope", "ST Segment Slope", choices = character(0)),
              helpText("Slope of the peak exercise ST segment."),
              
              selectInput("ca", "Number of Major Vessels",choices = character(0)),
              helpText("Number of major vessels visualised by fluoroscopy."),
              
              selectInput("thal", "Thalassemia", choices = character(0)),
              helpText("Thalassemia test result category."),
              
              actionButton("predict","Predict Heart Disease",class = "btn-primary",width = "100%")
            ),
            
            mainPanel(
              
              h2("Heart Disease Prediction"),
              
              p(
                class = "text-muted",
                "Logistic Regression Classification and Probability Analysis"
              ),
              p(
                "This application is for educational purposes only. ",
                "It is not a substitute for professional medical advice."
              ),
              
              hr(),
              
              # Prediction Outcome
              h3("Prediction Outcome"),
              
              uiOutput("prediction_outcome"),
              
              br(),
              
              # Model Interpretation
              h3("Interpretation"),
              uiOutput("prediction_explanation"),
              br()
            )  
            
          )  
          
        )  
        
      )  
      
    )  
    
  )
  
) 

# ============================================================
# 6. Server
# ============================================================

server <- function(input, output, session) {
  
  analysis_result <- reactiveVal(NULL)
  
  observeEvent(TRUE, {
    
    result <- tryCatch(
      {
        run_uploaded_analysis(default_data_file)
      },
      error = function(e) {
        list(
          ok = FALSE,
          message = conditionMessage(e)
        )
      }
    )
    analysis_result(result)
  }, once = TRUE)
  
  # Update factor choices for the prediction form
  observeEvent(analysis_result(), {
    result <- analysis_result()
    if (is.null(result) || !isTRUE(result$ok)) {
      return()
    }
    factor_levels <- result$bundle$factor_levels
    
    updateSelectInput(session, "sex", choices = c("Female" = "0","Male"= "1"))
    updateSelectInput(session, "cp", choices = c("Typical Angina" = "1",
                                                 "Atypical Angina" = "2",
                                                 "Non-Anginal Pain" = "3",
                                                 "Asymptomatic" = "4"))
    updateSelectInput(session, "fbs", choices = c( "No (≤ 120 mg/dl)" = "0",
                                                   "Yes (> 120 mg/dl)" = "1"))
    updateSelectInput(session, "restecg", choices = c( "Normal" = "0",
                                                       "ST-T Wave Abnormality" = "1",
                                                       "Left Ventricular Hypertrophy" = "2"))
    updateSelectInput(session, "exang", choices = c("No" = "0",
                                                    "Yes" = "1"))
    updateSelectInput(session, "slope", choices = c("Upsloping" = "1",
                                                    "Flat" = "2",
                                                    "Downsloping" = "3"))
    updateSelectInput(session, "ca", choices = c( "0 Major Vessels" = "0",
                                                  "1 Major Vessel" = "1",
                                                  "2 Major Vessels" = "2",
                                                  "3 Major Vessels" = "3"))
    updateSelectInput(session, "thal", choices = c( "Normal" = "3",
                                                    "Fixed Defect" = "6",
                                                    "Reversible Defect" = "7"))
    
  })
  
  # ------------------------------------------------------------
  # Tab 1: Exploratory Data Analysis Server
  # ------------------------------------------------------------
  eda_env$heart_eda_server(
    "heart_eda",
    data = reactive({
      result <- analysis_result()
      req(!is.null(result), isTRUE(result$ok))
      result$raw_data
    })
  )
  
  # helper: require a successful analysis before rendering model output.
  current_analysis <- reactive({
    result <- analysis_result()
    req(!is.null(result), isTRUE(result$ok))
    result
  })
  
  # ------------------------------------------------------------
  # Tab 2: Model Comparison
  # ------------------------------------------------------------
  
  output$model_comparison_table <- renderDT({
    
    result <- current_analysis()
    logistic_cm <- result$logistic_env$conf_matrix
    tree_cm <- result$tree_env$confusion_result
    
    comparison <- data.frame(
      Model = c("Logistic Regression", "Decision Tree"),
      Accuracy = c(
        unname(logistic_cm$overall["Accuracy"]),
        unname(tree_cm$overall["Accuracy"])
      ),
      Sensitivity = c(
        unname(logistic_cm$byClass["Sensitivity"]),
        unname(tree_cm$byClass["Sensitivity"])
      ),
      Specificity = c(
        unname(logistic_cm$byClass["Specificity"]),
        unname(tree_cm$byClass["Specificity"])
      ),
      Precision = c(
        unname(logistic_cm$byClass["Pos Pred Value"]),
        unname(tree_cm$byClass["Pos Pred Value"])
      ),
      F1_Score = c(
        unname(logistic_cm$byClass["F1"]),
        unname(tree_cm$byClass["F1"])
      ),
      AUC = c(
        as.numeric(result$logistic_env$auc_value),
        as.numeric(result$tree_env$auc_tree)
      )
    )
    
    comparison[-1] <- lapply(comparison[-1], round, 4)
    
    datatable(
      comparison,
      rownames = FALSE,
      options = list(dom = "t", scrollX = TRUE)
    )
  })
  # ------------------------------------------------------------
  # Tab 2: Model (Logistic Regression)
  # ------------------------------------------------------------

  output$logistic_summary <- renderPrint({
    result <- current_analysis()
    summary(result$logistic_env$logistic_model)
  })
  
  output$odds_ratio_table <- renderDT({
    
    result <- current_analysis()
    logistic_model <- result$logistic_env$logistic_model
    
    odds <- exp(stats::coef(logistic_model))
    confidence <- exp(stats::confint(logistic_model))
    
    odds_table <- data.frame(
      Variable = names(odds),
      Odds_Ratio = as.numeric(odds),
      Lower_95_CI = confidence[, 1],
      Upper_95_CI = confidence[, 2],
      row.names = NULL
    )
    
    odds_table[, -1] <- lapply(odds_table[, -1], round, 4)
    
    datatable(
      odds_table,
      rownames = FALSE,
      options = list(pageLength = 15, scrollX = TRUE)
    )
  })
  
  output$logistic_roc <- renderPlot({
    result <- current_analysis()
    plot(
      result$logistic_env$roc_curve,
      main = "ROC Curve - Logistic Regression",
      col = "blue",
      lwd = 2
    )
    abline(a = 0, b = 1, lty = 2, col = "red")
  })

  # ------------------------------------------------------------
  # Tab 2: Model (Decision Tree)
  # ------------------------------------------------------------

  output$decision_tree_plot <- renderPlot({
    result <- current_analysis()
    rpart.plot(
      result$tree_env$pruned_tree,
      type = 2,
      extra = 104,
      fallen.leaves = TRUE,
      main = "Pruned Decision Tree for Heart Disease Classification",roundint = FALSE
    )
  })
  
  output$tree_cp_table <- renderDT({
    result <- current_analysis()
    cp_table <- as.data.frame(result$tree_env$tree_model$cptable)
    cp_table$Complexity_Parameter <- rownames(cp_table)
    rownames(cp_table) <- NULL
    
    datatable(
      cp_table,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$tree_importance_plot <- renderPlot({
    
    result <- current_analysis()
    importance_data <- result$tree_env$importance_df
    
    ggplot(
      importance_data,
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
      theme_minimal()
  })
  
  output$tree_roc <- renderPlot({
    result <- current_analysis()
    plot(
      result$tree_env$roc_tree,
      main = "ROC Curve - Decision Tree",
      col = "blue",
      lwd = 2
    )
    abline(a = 0, b = 1, lty = 2, col = "red")
  })
  
  # ------------------------------------------------------------
  # Tab 2: Model (Prediction)
  # ------------------------------------------------------------
  prediction_result <- eventReactive(input$predict, {
    
    result <- current_analysis()
    bundle <- result$bundle
    
    # Retrieve Decision Tree training data
    tree_train <- result$tree_env$train_data
    
    req(
      input$age,
      input$sex,
      input$cp,
      input$trestbps,
      input$chol,
      input$fbs,
      input$restecg,
      input$thalach,
      input$exang,
      input$oldpeak,
      input$slope,
      input$ca,
      input$thal
    )
    
    # ==========================================================
    # A. Prepare input for Logistic Regression
    # ==========================================================
    
    logistic_patient <- data.frame(
      
      age = input$age,
      
      sex = factor(
        input$sex,
        levels = bundle$factor_levels$sex
      ),
      
      cp = factor(
        input$cp,
        levels = bundle$factor_levels$cp
      ),
      
      trestbps = input$trestbps,
      
      chol = input$chol,
      
      fbs = factor(
        input$fbs,
        levels = bundle$factor_levels$fbs
      ),
      
      restecg = factor(
        input$restecg,
        levels = bundle$factor_levels$restecg
      ),
      
      thalach = input$thalach,
      
      exang = factor(
        input$exang,
        levels = bundle$factor_levels$exang
      ),
      
      oldpeak = input$oldpeak,
      
      slope = factor(
        input$slope,
        levels = bundle$factor_levels$slope
      ),
      
      ca = factor(
        input$ca,
        levels = bundle$factor_levels$ca
      ),
      
      thal = factor(
        input$thal,
        levels = bundle$factor_levels$thal
      )
    )
    
    if (anyNA(logistic_patient)) {
      stop("Invalid patient information for Logistic Regression.")
    }
    
    # Standardise numeric variables using Logistic Regression
    # training data statistics
    
    numeric_cols <- c(
      "age", "trestbps", "chol",
      "thalach", "oldpeak"
    )
    
    logistic_patient[numeric_cols] <- sweep(
      as.matrix(logistic_patient[numeric_cols]),
      2,
      bundle$means[numeric_cols],
      "-"
    )
    
    logistic_patient[numeric_cols] <- sweep(
      logistic_patient[numeric_cols],
      2,
      bundle$sds[numeric_cols],
      "/"
    )
    
    logistic_probability <- as.numeric(
      predict(
        bundle$model,
        newdata = logistic_patient,
        type = "response"
      )
    )
    
    # ==========================================================
    # B. Prepare input for Decision Tree
    # ==========================================================
    tree_model <- result$tree_env$pruned_tree
    
    # Get the actual variable types recorded by the fitted model
    model_classes <- attr(tree_model$terms, "dataClasses")
    
    # Get factor levels directly from the Decision Tree training data
    model_levels <- lapply(
      tree_train,
      function(x) {
        if (is.factor(x)) levels(x) else NULL
      }
    )
    
    # Collect raw Shiny input values
    tree_patient <- data.frame(
      age = input$age,
      sex = input$sex,
      cp = input$cp,
      trestbps = input$trestbps,
      chol = input$chol,
      fbs = input$fbs,
      restecg = input$restecg,
      thalach = input$thalach,
      exang = input$exang,
      oldpeak = input$oldpeak,
      slope = input$slope,
      ca = input$ca,
      thal = input$thal,
      check.names = FALSE
    )
    
    # Convert each variable according to the fitted model
    for (col in names(tree_patient)) {
      
      expected_type <- model_classes[col]
      
      if (is.na(expected_type)) {
        stop(paste("Cannot identify the model type for:", col))
      }
      
      if (expected_type == "factor") {
        
        tree_patient[[col]] <- factor(
          as.character(tree_patient[[col]]),
          levels = model_levels[[col]]
        )
        
      } else if (expected_type == "numeric") {
        
        tree_patient[[col]] <- as.numeric(
          as.character(tree_patient[[col]])
        )
        
      } else if (expected_type == "integer") {
        
        tree_patient[[col]] <- as.integer(
          as.character(tree_patient[[col]])
        )
        
      } else if (expected_type == "logical") {
        
        tree_patient[[col]] <- as.logical(
          as.character(tree_patient[[col]])
        )
        
      } else {
        stop(
          paste(
            "Unsupported variable type:",
            expected_type,
            "for",
            col
          )
        )
      }
    }
    
    # Check missing or invalid values
    if (anyNA(tree_patient)) {
      stop("Invalid patient information for Decision Tree.")
    }
    
    # Predict using the pruned Decision Tree
    tree_probabilities <- predict(
      tree_model,
      newdata = tree_patient,
      type = "prob"
    )
    
    tree_probability <- as.numeric(
      tree_probabilities[, "Heart_Disease"]
    )
    # ==========================================================
    # C. Return both model predictions
    # ==========================================================
    
    list(
      logistic_probability = logistic_probability,
      logistic_no_disease_probability = 1 - logistic_probability,
      
      tree_probability = tree_probability
    )
    
  }, ignoreInit = TRUE)
  
  
  # ------------------------------------------------------------
  # 1. Prediction Outcome
  # ------------------------------------------------------------
  output$prediction_outcome <- renderUI({
    
    result <- prediction_result()
    req(result)
    
    logistic_probability <- result$logistic_probability * 100
    tree_probability <- result$tree_probability * 100
    
    fluidRow(
      
      column(
        width = 6,
        
        wellPanel(
          
          h4("Logistic Regression"),
          
          h1(
            paste0(round(logistic_probability, 2), "%"),
            style = "color:#2878B5; font-weight:bold;"
          ),
          
          p("Predicted Probability of Heart Disease")
          
        )
      ),
      
      column(
        width = 6,
        
        wellPanel(
          
          h4("Decision Tree"),
          
          h1(
            paste0(round(tree_probability, 2), "%"),
            style = "color:#27845B; font-weight:bold;"
          ),
          
          p("Predicted Probability of Heart Disease")
          
        )
      )
      
    )
    
  })
  
  # ------------------------------------------------------------
  # Model Interpretation
  # ------------------------------------------------------------
  
  output$prediction_explanation <- renderUI({
    
    result <- prediction_result()
    req(result)
    
    logistic_probability <- result$logistic_probability * 100
    tree_probability <- result$tree_probability * 100
    
    difference <- abs(
      logistic_probability - tree_probability
    )
    
    wellPanel(
      
      h4("Prediction Comparison"),
      
      p(
        paste0(
          "The Logistic Regression model estimates a ",
          round(logistic_probability, 2),
          "% probability of heart disease."
        )
      ),
      
      p(
        paste0(
          "The Decision Tree model estimates a ",
          round(tree_probability, 2),
          "% probability of heart disease."
        )
      ),
      
      p(
        paste0(
          "The difference between the two predicted probabilities is ",
          round(difference, 2),
          " percentage points."
        )
      ),
      
      hr(),
      
      p(
        "Both predictions are model-generated estimates. ",
        "Differences may occur because the two models use ",
        "different modelling approaches and decision structures."
      ),
      
      p(
        "These results should not be interpreted as a confirmed ",
        "medical diagnosis."
      )
      
    )
    
  })
}

# 7. run app

shinyApp(ui = ui, server = server)
