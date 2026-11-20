# Heart Disease EDA Shiny module
# Five selected categorical predictors for EDA: sex, cp, fbs, restecg, exang,slope,vessels,thal
# Numeric fields for EDA : age, trestbps, chol, thalach，oldpeak

heart_eda_prepare <- function(x) {
  required <- c("sex", "cp", "fbs", "restecg", "exang","slope","ca","thal", "target_binary",
                "age", "trestbps", "chol", "thalach","oldpeak")
  missing_cols <- setdiff(required, names(x))
  if (length(missing_cols) > 0) {
    stop("Missing columns: ", paste(missing_cols, collapse = ", "))
  }

  # Preserve the highlighted five categorical predictors + target,
  # and the four continuous variables needed for EDA plots.
  # 'num' and other unselected features are dropped.
  x <- x[, required, drop = FALSE]
  for (v in c("age", "trestbps", "chol", "thalach","oldpeak")) {
    x[[v]] <- suppressWarnings(as.numeric(as.character(x[[v]])))
  }

  recode_factor <- function(values, levels, labels, name) {
    original <- as.character(values)
    result <- factor(original, levels = levels, labels = labels)
    invalid <- !is.na(original) & is.na(result)
    if (any(invalid)) {
      stop("Unexpected value(s) in ", name, ": ",
           paste(unique(original[invalid]), collapse = ", "))
    }
    result
  }

  x$sex <- recode_factor(x$sex, c("0", "1"), c("Female", "Male"), "sex")
  x$cp <- recode_factor(
    x$cp, c("1", "2", "3", "4"),
    c("Typical angina", "Atypical angina", "Non-anginal pain", "Asymptomatic"), "cp"
  )
  x$fbs <- recode_factor(
    x$fbs, c("0", "1"), c("<=120 mg/dL", ">120 mg/dL"), "fbs"
  )
  x$restecg <- recode_factor(
    x$restecg, c("0", "1", "2"),
    c("Normal", "ST-T Wave Abnormality", "Left Ventricular Hypertrophy"), "restecg"
  )
  x$exang <- recode_factor(x$exang, c("0", "1"), c("No", "Yes"), "exang")
  x$slope <- recode_factor(x$slope, c("1", "2","3"), c("upsloping", "flat","downsloping"), "slope")
  x$ca <- recode_factor(x$ca, c("0","1", "2","3"), c("0 major vessels", "1 major vessels","2 major vessels","3 major vessels"), "ca")
  x$thal <- recode_factor(x$thal, c("3", "6","7"), c("Normal", "Fixed Defect","Reversible Defect"), "thal")
  x$target_binary <- recode_factor(
    x$target_binary, c("0", "1"),
    c("No heart disease", "Heart disease"), "target_binary"
  )
  x
}

heart_eda_numeric_choices <- c(
  "Age (years)" = "age",
  "Resting blood pressure (mmHg)" = "trestbps",
  "Cholesterol (mg/dL)" = "chol",
  "Maximum heart rate (bpm)" = "thalach",
  "ST Depression" = "oldpeak"
)

heart_eda_choices <- c(
  "Sex" = "sex",
  "Chest pain type" = "cp",
  "Fasting blood sugar" = "fbs",
  "Resting ECG" = "restecg",
  "Exercise-induced angina" = "exang",
  "ST Segment Slope" = "slope",
  "Number of Major Vessels" = "ca",
  "Thalassemia" = "thal"
)

# A descriptive measure of association for two categorical variables.
# Cramer's V ranges from 0 to 1 (no direction or sign).
heart_eda_cramers_v <- function(a, b) {
  good <- !is.na(a) & !is.na(b)
  tab <- table(droplevels(a[good]), droplevels(b[good]))
  tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
  if (nrow(tab) < 2 || ncol(tab) < 2) return(NA_real_)
  n <- sum(tab)
  statistic <- suppressWarnings(stats::chisq.test(tab, correct = FALSE)$statistic)
  sqrt(as.numeric(statistic) / (n * min(nrow(tab) - 1, ncol(tab) - 1)))
}

heart_eda_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Exploratory Data Analysis - Heart Disease"),
    shiny::p(
      "An exploratory analysis of categorical and numerical variables ",
      "to identify patterns and relationships associated with heart disease status."
    ),
    shiny::fluidRow(
      shiny::column(6, shiny::selectInput(
        ns("sex_filter"), "Filter by sex", c("All", "Female", "Male")
      )),
      shiny::column(6, shiny::selectInput(
        ns("status_filter"), "Filter by heart disease status",
        c("All", "No heart disease", "Heart disease")
      ))
    ),
    shiny::tabsetPanel(
      shiny::tabPanel("Overview", shiny::br(),
        shiny::verbatimTextOutput(ns("overview")),
        shiny::plotOutput(ns("status_plot"), height = "320px")
      ),
      shiny::tabPanel(
        "Descriptive Statistics", shiny::br(),
        
        shiny::tabsetPanel(
          
          # 1. Numerical Summary
          shiny::tabPanel(
            "Numerical Summary", shiny::br(),
            shiny::h4("Numerical Summary"),
            DT::DTOutput(ns("numeric_desc_table"))
          ),
          
          # 2. Categorical Summary
          shiny::tabPanel(
            "Categorical Summary", shiny::br(),
            shiny::h4("Categorical Summary"),
            DT::DTOutput(ns("desc_table")),
            
            shiny::hr(),
            
            shiny::h4("Frequency Distribution"),
            
            shiny::selectInput(
              ns("frequency_var"),
              "Select Variable",
              choices = heart_eda_choices,
              selected = "cp"
            ),
            
            DT::DTOutput(ns("frequency_table"))
          )
        )
      ),
      
      shiny::tabPanel("Histograms & density", shiny::br(),
        shiny::selectInput(ns("numeric_var"), "Choose numerical variable",
          choices = heart_eda_numeric_choices
        ),
        shiny::radioButtons(ns("distribution_type"), "Plot type",
          choices = c("Histogram", "Density"), inline = TRUE
        ),
        shiny::plotOutput(ns("numeric_distribution_plot"), height = "410px")
      ),
      shiny::tabPanel("Boxplots", shiny::br(),
        shiny::selectInput(ns("boxplot_var"), "Choose numerical variable",
          choices = heart_eda_numeric_choices
        ),
        shiny::plotOutput(ns("boxplot"), height = "410px"),
        shiny::helpText("Boxplots compare a continuous measurement by heart disease status. ",
                        "Use the 'All' status filter to compare both outcomes.")
      ),
      
      shiny::tabPanel("Category Analysis", shiny::br(),
                      shiny::selectInput(
                        ns("category_var"),"Choose categorical variable",
                        choices = heart_eda_choices),
                      shiny::radioButtons(
                        ns("category_plot_type"),
                        "Choose plot type",choices = c(
                          "Frequency plot" = "frequency",
                          "Percentage bar plot" = "percentage",
                          "Pie chart (percentage)" = "pie"
                        ), selected = "frequency",inline = TRUE), 
                      shiny::plotOutput(
                        ns("category_plot"),height = "430px"),
                      shiny::helpText("Use the 'All' status filter to compare both outcomes.")
      ),
      shiny::tabPanel("Correlation", shiny::br(),
        shiny::selectInput(ns("cor_method"), "Correlation method",
          choices = c("Pearson", "Spearman")
        ),
        shiny::plotOutput(ns("correlation_plot"), height = "440px"),
        shiny::helpText(
          "Correlation is calculated only for the five continuous ",
          "measurements. Categorical associations are in a separate tab."
        )
      ),
      shiny::tabPanel("Categorical association", shiny::br(),
        shiny::plotOutput(ns("association_plot"), height = "480px"),
        shiny::p("Cramer's V measures strength of association between categorical ",
                 "variables, from 0 (none) to 1 (strong). It is not Pearson correlation ",
                 "and does not indicate a positive or negative direction.")
      )
    )
  )
}

heart_eda_server <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    prepared <- shiny::reactive({
      df <- data()
      shiny::validate(shiny::need(is.data.frame(df), "Provide a data frame."))
      heart_eda_prepare(df)
    })

    filtered <- shiny::reactive({
      df <- prepared()
      if (!identical(input$sex_filter, "All")) {
        df <- df[!is.na(df$sex) & df$sex == input$sex_filter, , drop = FALSE]
      }
      if (!identical(input$status_filter, "All")) {
        df <- df[!is.na(df$target_binary) &
                   df$target_binary == input$status_filter, , drop = FALSE]
      }
      shiny::validate(shiny::need(nrow(df) > 0,
                                  "No records match the selected filters."))
      droplevels(df)
    })

    output$overview <- shiny::renderPrint({
      df <- filtered()
      cat("Observations:", nrow(df), "\n")
      cat("Variables available for EDA:", ncol(df), "\n")
      cat("Missing values:", sum(is.na(df)), "\n\n")
      cat("Sex frequencies:\n")
      print(table(df$sex, useNA = "ifany"))
      cat("\nHeart disease frequencies:\n")
      print(table(df$target_binary, useNA = "ifany"))
    })

    output$status_plot <- shiny::renderPlot({
      df <- filtered()
      ggplot2::ggplot(df, ggplot2::aes(x = target_binary, fill = target_binary)) +
        ggplot2::geom_bar() +
        ggplot2::scale_fill_manual(values = c("#34a9a1", "#dd795b")) +
        ggplot2::labs(title = "Heart disease status distribution",
                      x = "Outcome", y = "Number of observations") +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "none")
    })

    output$desc_table <- DT::renderDT({
      df <- filtered()
      cols <- c(names(heart_eda_choices), "Heart disease outcome")
      vars <- c(unname(heart_eda_choices), "target_binary")
      rows <- lapply(seq_along(vars), function(i) {
        v <- vars[i]
        counts <- sort(table(df[[v]]), decreasing = TRUE)
        total <- sum(counts)
        data.frame(
          Variable = cols[i],
          `Valid N` = total,
          Missing = sum(is.na(df[[v]])),
          Categories = sum(counts > 0),
          Mode = if (length(counts)) names(counts)[1] else NA_character_,
          `Mode count` = if (length(counts)) as.integer(counts[1]) else NA_integer_,
          `Mode percent` = if (total > 0) round(100 * counts[1] / total, 1) else NA_real_,
          check.names = FALSE
        )
      })
      DT::datatable(do.call(rbind, rows), rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE))
    })

    # Desciptive Statistic 
    # Numeric variable
    output$numeric_desc_table <- DT::renderDT({
      
      df <- filtered()
      
      rows <- lapply(names(heart_eda_numeric_choices), function(label) {
        
        v <- unname(heart_eda_numeric_choices[[label]])
        
        z <- df[[v]]
        z <- z[is.finite(z)]
        
        data.frame(
          Variable = label,
          N = length(z),
          Mean = if (length(z)) round(mean(z), 2) else NA_real_,
          SD = if (length(z) > 1) round(sd(z), 2) else NA_real_,
          Median = if (length(z)) round(median(z), 2) else NA_real_,
          Min = if (length(z)) round(min(z), 2) else NA_real_,
          Max = if (length(z)) round(max(z), 2) else NA_real_,
          check.names = FALSE
        )
      })
      
      DT::datatable(
        do.call(rbind, rows),
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE
        )
      )
    })
    
    output$frequency_table <- DT::renderDT({
      
      df <- filtered()
      v <- input$frequency_var
      
      shiny::req(v)
      
      # Same frequency calculation as the pie chart
      tab <- table(df[[v]], useNA = "no")
      
      total <- sum(tab)
      
      result <- data.frame(
        Category = names(tab),
        Frequency = as.integer(tab),
        Percentage = round(100 * as.numeric(tab) / total, 1),
        check.names = FALSE
      )
      
      DT::datatable(
        result,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          searching = FALSE,
          lengthChange = FALSE,
          scrollX = TRUE
        )
      )
    })
    
    # Category Analysis: Three Plot Types
     output$category_plot <- shiny::renderPlot({
      
      df <- filtered()
      
      v <- input$category_var
      plot_type <- input$category_plot_type
      
      shiny::req(v, plot_type)
      var_label <- names(heart_eda_choices)[
        match(v, unname(heart_eda_choices))
      ]
      # 1. Frequency Plot
      if (identical(plot_type, "frequency")) {
        
        p <- ggplot2::ggplot(
          df,
          ggplot2::aes(
            x = .data[[v]],
            fill = target_binary
          )
        ) +
          ggplot2::geom_bar(position = "dodge") +
          ggplot2::labs(
            title = paste("Category counts by heart disease status:", var_label),
            x = var_label,
            y = "Number of observations",
            fill = "Outcome"
          ) +
          ggplot2::theme_minimal()
        
         # 2. Percentage Bar Plot
      } else if (identical(plot_type, "percentage")) {
        
        p <- ggplot2::ggplot(
          df,
          ggplot2::aes(
            x = .data[[v]],
            fill = target_binary
          )
        ) +
          ggplot2::geom_bar(position = "fill") +
          ggplot2::scale_y_continuous(
            labels = function(y) paste0(round(100 * y), "%")
          ) +
          ggplot2::labs(
            title = paste("Outcome percentages within", var_label, "categories"),
            x = var_label,
            y = "Percentage within category",
            fill = "Outcome"
          ) +
          ggplot2::theme_minimal()
        
        # 3. Pie Chart (Overall Percentage)
      } else if (identical(plot_type, "pie")) {
        
        pie_data <- as.data.frame(
          table(df[[v]], useNA = "no")
        )
        
        names(pie_data) <- c("Category", "Count")
        
        pie_data <- pie_data[pie_data$Count > 0, ]
        
        pie_data$Percentage <-
          100 * pie_data$Count / sum(pie_data$Count)
        
        p <- ggplot2::ggplot(
          pie_data,
          ggplot2::aes(
            x = "",
            y = Percentage,
            fill = Category
          )
        ) +
          ggplot2::geom_col(
            width = 1,
            colour = "white"
          ) +
          ggplot2::coord_polar(theta = "y") +
          ggplot2::geom_text(
            ggplot2::aes(
              label = paste0(round(Percentage, 1), "%")
            ),
            position = ggplot2::position_stack(vjust = 0.5),
            size = 4
          ) +
          ggplot2::labs(
            title = paste("Overall percentage distribution of", var_label),
            fill = var_label
          ) +
          ggplot2::theme_void()
      }
      # Common Theme
      p +
        ggplot2::theme(
          plot.title = ggplot2::element_text(
            hjust = 0.5,
            face = "bold"
          ),
          axis.text.x = ggplot2::element_text(
            angle = 20,
            hjust = 1
          ),
          legend.position = if (identical(plot_type, "pie")) {
            "right"
          } else {
            "right"
          }
        )
    })

    output$numeric_distribution_plot <- shiny::renderPlot({
      df <- filtered()
      v <- input$numeric_var
      shiny::req(v)
      df <- df[is.finite(df[[v]]), , drop = FALSE]
      shiny::validate(shiny::need(nrow(df) > 1, "Not enough numeric records to plot."))
      p <- ggplot2::ggplot(
        df, ggplot2::aes(x = .data[[v]], fill = target_binary,
                         colour = target_binary)
      )
      if (identical(input$distribution_type, "Density")) {
        group_ok <- tapply(df[[v]], df$target_binary, function(z) {
          length(z) > 1 && length(unique(z)) > 1
        })
        shiny::validate(shiny::need(all(group_ok[!is.na(group_ok)]),
          "Not enough distinct values in one outcome group for density estimation."))
        p <- p + ggplot2::geom_density(alpha = 0.3, na.rm = TRUE)
      } else {
        p <- p + ggplot2::geom_histogram(bins = 30, alpha = 0.55,
                                         position = "identity", na.rm = TRUE)
      }
      p + ggplot2::labs(
        title = paste(input$distribution_type, "of", names(heart_eda_numeric_choices)[match(v, heart_eda_numeric_choices)]),
        x = names(heart_eda_numeric_choices)[match(v, heart_eda_numeric_choices)],
        y = if (identical(input$distribution_type, "Density")) "Density" else "Count",
        fill = "Outcome", colour = "Outcome"
      ) + ggplot2::theme_minimal()
    })

    output$boxplot <- shiny::renderPlot({
      df <- filtered()
      v <- input$boxplot_var
      shiny::req(v)
      shiny::validate(shiny::need(sum(is.finite(df[[v]])) > 0,
                                  "No numeric observations for this selection."))
      ggplot2::ggplot(df, ggplot2::aes(x = target_binary, y = .data[[v]],
                                      fill = target_binary)) +
        ggplot2::geom_boxplot(alpha = 0.7, outlier.alpha = 0.5, na.rm = TRUE) +
        ggplot2::labs(
          title = paste("Boxplot of", names(heart_eda_numeric_choices)[match(v, heart_eda_numeric_choices)],
                        "by heart disease"),
          x = "Heart disease status",
          y = names(heart_eda_numeric_choices)[match(v, heart_eda_numeric_choices)]
        ) + ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "none")
    })

    output$correlation_plot <- shiny::renderPlot({
      df <- filtered()
      nums <- unname(heart_eda_numeric_choices)
      shiny::validate(shiny::need(nrow(df) >= 3, "At least three records required."))
      cm <- suppressWarnings(stats::cor(df[, nums, drop = FALSE],
        method = tolower(input$cor_method), use = "pairwise.complete.obs"))
      vals <- as.data.frame(as.table(cm), responseName = "Correlation")
      names(vals)[1:2] <- c("Variable1", "Variable2")
      ggplot2::ggplot(vals, ggplot2::aes(x = Variable1, y = Variable2,
                                        fill = Correlation)) +
        ggplot2::geom_tile(color = "white") +
        ggplot2::geom_text(ggplot2::aes(
          label = ifelse(is.finite(Correlation), sprintf("%.2f", Correlation), "N/A")
        ), size = 4) +
        ggplot2::scale_fill_gradient2(low = "#3477b9", mid = "white",
          high = "#bb544e", midpoint = 0, limits = c(-1, 1), na.value = "grey90") +
        ggplot2::coord_equal() + ggplot2::theme_minimal() +
        ggplot2::labs(title = paste(input$cor_method,
                                   "correlations: numerical measurements"),
                      x = NULL, y = NULL)
    })
    output$association_plot <- shiny::renderPlot({
      df <- filtered()
      vars <- c(unname(heart_eda_choices), "target_binary")
      labels <- c(names(heart_eda_choices), "Heart disease outcome")
      mat <- matrix(NA_real_, nrow = length(vars), ncol = length(vars),
                    dimnames = list(labels, labels))
      for (i in seq_along(vars)) {
        for (j in seq_along(vars)) {
          mat[i, j] <- heart_eda_cramers_v(df[[vars[i]]], df[[vars[j]]])
        }
      }
      vals <- as.data.frame(as.table(mat), responseName = "CramersV")
      names(vals)[1:2] <- c("Variable1", "Variable2")
      ggplot2::ggplot(vals, ggplot2::aes(x = Variable1, y = Variable2,
                                        fill = CramersV)) +
        ggplot2::geom_tile(color = "white") +
        ggplot2::geom_text(ggplot2::aes(
          label = ifelse(is.na(CramersV), "N/A", sprintf("%.2f", CramersV))
        ), size = 3.6) +
        ggplot2::scale_fill_gradient(low = "white", high = "red",
                                     limits = c(0, 1), na.value = "grey90") +
        ggplot2::coord_equal() +
        ggplot2::labs(title = "Cramer's V: categorical associations",
                      x = NULL, y = NULL, fill = "V") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(
          angle = 35, hjust = 1))
    })

    output$patterns_table <- DT::renderDT({
      # Use the full sample for pattern comparisons, regardless of UI filters.
      df <- prepared()
      rows <- lapply(seq_along(heart_eda_choices), function(i) {
        var <- unname(heart_eda_choices[i])
        tab <- table(df[[var]], df$target_binary)
        denom <- rowSums(tab)
        yes <- if ("Heart disease" %in% colnames(tab)) {
          tab[, "Heart disease"]
        } else {
          rep(0, nrow(tab))
        }
        data.frame(
          Predictor = names(heart_eda_choices)[i],
          Category = rownames(tab),
          N = as.integer(denom),
          `Heart disease count` = as.integer(yes),
          `Heart disease percent` = ifelse(
            denom > 0, round(100 * yes / denom, 1), NA_real_),
          check.names = FALSE
        )
      })
      result <- do.call(rbind, rows)
      result <- result[result$N > 0, , drop = FALSE]
      DT::datatable(result, rownames = FALSE,
                    options = list(pageLength = 15, scrollX = TRUE))
    })
  })
}
