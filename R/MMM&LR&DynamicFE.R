# ==============================
# Robust Marketing Mix Modeling with Robyn and Linear Regression Comparison
# ==============================

# ==========================================
# PART 1: SETUP ENVIRONMENT
# ==========================================

# Configure multicore processing
Sys.setenv(R_FUTURE_FORK_ENABLE = "true")
options(future.fork.enable = TRUE)
library(reticulate)
library(Robyn)
library(readxl)
library(dplyr)
library(tidyverse)
library(magrittr)
library(caret)
library(Cairo)
use_condaenv("r-reticulate")

# Create directory for outputs
create_mmm_directory <- function(dir_path = "~/MMM") {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    message("Created directory: ", dir_path)
  }
  return(dir_path)
}


# Function to load data from various sources
load_marketing_data <- function(file_path) {
  if (is.null(file_path)) {
    # Ask for file path if not provided
    file_path <- file.choose()
  }
  
  # Determine file type and read accordingly
  if (grepl("\\.xls[xm]?$", file_path)) {
    data <- suppressWarnings(read_excel(file_path))
  } else if (grepl("\\.csv$", file_path)) {
    data <- suppressWarnings(read.csv(file_path))
  } else if (grepl("\\.rds$", file_path)) {
    data <- readRDS(file_path)
  } else {
    stop("Unsupported file format. Please provide an Excel, CSV, or RDS file.")
  }
  
  # Basic validation
  required_columns <- c("DATE")
  if (!all(required_columns %in% names(data))) {
    stop("Dataset must contain at least a DATE column")
  }
  
  # Convert dates
  if (!inherits(data$DATE, "Date")) {
    data$DATE <- as.Date(data$DATE)
  }
  
  # Convert all non-date columns to numeric, handling commas
  col_names <- names(data)
  for (col in col_names) {
    if (col != "DATE") {  # Skip the date column
      # First convert to character to handle factors
      char_values <- as.character(data[[col]])
      
      # Remove commas and handle potential thousand separators
      clean_values <- gsub(",", "", char_values)
      
      # Try to convert to numeric
      data[[col]] <- as.numeric(clean_values)
      
      # Check if conversion created NAs and warn if it did
      na_count <- sum(is.na(data[[col]]))
      original_na_count <- sum(is.na(char_values))
      if (na_count > original_na_count) {
        warning(paste("Converting column", col, "to numeric created", 
                      na_count - original_na_count, "new NA values"))
      }
    }
  }
  
  # Remove rows with NAs
  data <- data[complete.cases(data), ]
  
  # Apply feature engineering 
  data <- enhance_features(data)
  
  return(data)
}

# Function to estimate weights for combined features
estimate_feature_weights <- function(data) {
  numeric_data <- data %>%
    select(-DATE) %>%
    select(where(is.numeric))
  
  # lagged SEM variables 
  numeric_data <- numeric_data %>%
    mutate(
      SEM_lag1 = lag(SEM, 1),
      SEM_lag2 = lag(SEM, 2)
    ) %>%
    filter(!is.na(SEM_lag2))  
  # Normalization of the data
  preproc <- preProcess(numeric_data, method = c("range"))
  scaled_data <- predict(preproc, numeric_data)
  
  # extract normalized weights
  fit_weighted_model <- function(X_cols, y_col, data = scaled_data) {
    formula_str <- paste(y_col, "~", paste(X_cols, collapse = " + "), "- 1")  # -1 removes intercept
    formula_obj <- as.formula(formula_str)
    model <- lm(formula_obj, data = data)
    coefs <- coef(model)
    abs_coefs <- abs(coefs)
    norm_coefs <- abs_coefs / sum(abs_coefs)
    weights <- as.list(norm_coefs)
    return(weights)
  }
  
  # Estimate weights
  digital_weights <- fit_weighted_model(
    X_cols = c("SEM", "SNS", "AFF", "DOUGA"),
    y_col = "CV"
  )
  
  tv_like_weights <- fit_weighted_model(
    X_cols = c("ADNW1", "ADNW2", "ADNW3"),
    y_col = "CV"
  )
  
  sem_time_weights <- fit_weighted_model(
    X_cols = c("SEM", "SEM_lag1", "SEM_lag2"),
    y_col = "CV"
  )
  
  #the results
  cat("Digital Channel Weights:\n")
  print(digital_weights)
  
  cat("\nTV-Like Channel Weights:\n")
  print(tv_like_weights)
  
  cat("\nSEM Time Weights:\n")
  print(sem_time_weights)
  
  # Apply weights to create combined features
  data <- data %>%
    mutate(
      SEM_lag1 = lag(SEM, 1),
      SEM_lag2 = lag(SEM, 2)
    ) %>%
    filter(!is.na(SEM_lag2)) %>%
    mutate(
      Digital_Weighted = 
        digital_weights$SEM * SEM + 
        digital_weights$SNS * SNS + 
        digital_weights$AFF * AFF + 
        digital_weights$DOUGA * DOUGA,
      
      TV_Like_Weighted = 
        tv_like_weights$ADNW1 * ADNW1 + 
        tv_like_weights$ADNW2 * ADNW2 + 
        tv_like_weights$ADNW3 * ADNW3,
      
      SEM_TimeWeighted = 
        sem_time_weights$SEM * SEM + 
        sem_time_weights$SEM_lag1 * SEM_lag1 + 
        sem_time_weights$SEM_lag2 * SEM_lag2
    )
  
  return(list(
    enhanced_data = data,
    digital_weights = digital_weights,
    tv_like_weights = tv_like_weights,
    sem_time_weights = sem_time_weights
  ))
}

# Enhanced feature engineering function 
enhance_features <- function(data) {
  # First, get the estimated weights and enhanced data
  weight_results <- estimate_feature_weights(data)
  
  # Extract the weights
  digital_weights <- weight_results$digital_weights
  tv_like_weights <- weight_results$tv_like_weights
  sem_time_weights <- weight_results$sem_time_weights
  
  # Create weighted combinations of media channels using estimated weights
  data <- data %>%
    mutate(
      SEM_lag1 = lag(SEM, 1),
      SEM_lag2 = lag(SEM, 2)
    ) %>%
    filter(!is.na(SEM_lag2)) %>%
    mutate(
      Digital_Weighted = 
        digital_weights$SEM * SEM + 
        digital_weights$SNS * SNS + 
        digital_weights$AFF * AFF + 
        digital_weights$DOUGA * DOUGA,
      
      TV_Like_Weighted = 
        tv_like_weights$ADNW1 * ADNW1 + 
        tv_like_weights$ADNW2 * ADNW2 + 
        tv_like_weights$ADNW3 * ADNW3,
      
      SEM_TimeWeighted = 
        sem_time_weights$SEM * SEM + 
        sem_time_weights$SEM_lag1 * SEM_lag1 + 
        sem_time_weights$SEM_lag2 * SEM_lag2
    )
  
  # Create dummy variables for campaigns/events if they don't exist
  if (!("GAMEPR230324" %in% names(data))) {
    data <- data %>%
      mutate(
        GAMEPR230324 = ifelse(DATE == as.Date("2023-03-24"), 1, 0),
        GAMEPR230727 = ifelse(DATE == as.Date("2023-07-27"), 1, 0),
        INFLU230402 = ifelse(DATE == as.Date("2023-04-02"), 1, 0),
        INFLU230404 = ifelse(DATE == as.Date("2023-04-04"), 1, 0),
        INFLU230408 = ifelse(DATE == as.Date("2023-04-08"), 1, 0),
        INFLU230821 = ifelse(DATE == as.Date("2023-08-21"), 1, 0),
        CP230317 = ifelse(DATE == as.Date("2023-03-17"), 1, 0),
        CP230421 = ifelse(DATE == as.Date("2023-04-21"), 1, 0),
        CP230811 = ifelse(DATE == as.Date("2023-08-11"), 1, 0),
        obon230812 = ifelse(DATE == as.Date("2023-08-12"), 1, 0),
        GW230429 = ifelse(DATE == as.Date("2023-04-29"), 1, 0),
        GW230430 = ifelse(DATE == as.Date("2023-04-30"), 1, 0)
      )
  }
  
  # Group campaigns by type
  data <- data %>%
    mutate(
      INFLU_campaigns = INFLU230402 + INFLU230404 + INFLU230408 + INFLU230821,
      GAMEPR_campaigns = GAMEPR230324 + GAMEPR230727,
      CP_campaigns = CP230317 + CP230421 + CP230811
    )
  
  
  
  return(data)
}

# Function to categorize variables based on our enhanced dataset
categorize_variables <- function(data) {
  # Check if we're using the enhanced feature set
  if (all(c("Digital_Weighted", "TV_Like_Weighted", "SEM_TimeWeighted") %in% names(data))) {
    return(list(
      dep_var = "CV",
      paid_media_vars = c(
        "Digital_Weighted", 
        "TV_Like_Weighted", 
        "SEM_TimeWeighted"
      ),
      paid_media_spends = c(
        "Digital_Weighted", 
        "TV_Like_Weighted", 
        "SEM_TimeWeighted"
      ),
      context_vars = c(
        "GT",
        "CP_campaigns",
        "obon230812",
        "GW230429",
        "GW230430"
      ),
      organic_vars = c(
        "BRAND",
        "INFLU_campaigns",
        "GAMEPR_campaigns"
      )
    ))
  } else {
    # Default to original categorization if enhanced features aren't available
    return(list(
      dep_var = "CV",
      paid_media_vars = c(
        "SEM",
        "ADNW1",
        "ADNW2",
        "ADNW3",
        "SNS",
        "AFF",
        "DOUGA"
      ),
      paid_media_spends = c(
        "SEM",
        "ADNW1",
        "ADNW2",
        "ADNW3",
        "SNS",
        "AFF",
        "DOUGA"
      ),
      context_vars = c(
        "CP230317",
        "CP230421",
        "CP230811",
        "obon230812",
        "GW230429",
        "GW230430",
        "GT"
      ),
      organic_vars = c(
        "BRAND",
        "GAMEPR230324",
        "GAMEPR230727",
        "INFLU230402",
        "INFLU230404",
        "INFLU230408",
        "INFLU230821"
      )
    ))
  }
}

display_hyper_names <- function(data, var_categories, adstock = "geometric") {
  if (all(c("Digital_Weighted", "TV_Like_Weighted", "SEM_TimeWeighted") %in% names(data))) {
    InputCollect <- robyn_inputs(
      dt_input = data,
      date_var = "DATE", 
      dep_var = "CV", 
      dep_var_type = "conversion", 
      prophet_vars = c("weekday"),
      prophet_country = "DE", 
      context_vars = c("GT", "CP_campaigns", "obon230812", "GW230429", "GW230430"),
      paid_media_spends = c("Digital_Weighted", "TV_Like_Weighted", "SEM_TimeWeighted"),
      paid_media_vars = c("Digital_Weighted", "TV_Like_Weighted", "SEM_TimeWeighted"),
      organic_vars = c("BRAND", "INFLU_campaigns", "GAMEPR_campaigns"),
      window_start = "2023-3-08",
      window_end = "2023-12-02",
      adstock = adstock
    )
  } else {
    # Use original variables
    InputCollect <- robyn_inputs(
      dt_input = data,
      date_var = "DATE", 
      dep_var = "CV", 
      dep_var_type = "conversion", 
      prophet_vars = c("weekday","holiday"),
      prophet_country = "DE", 
      context_vars = c("GT","CP230317","CP230421","CP230811","obon230812",
                       "GW230429","GW230430"),
      paid_media_spends = c("SEM","ADNW1","ADNW2","ADNW3","SNS",
                            "AFF","DOUGA"),
      paid_media_vars = c("SEM","ADNW1","ADNW2","ADNW3","SNS",
                          "AFF","DOUGA"),
      organic_vars = c("BRAND","GAMEPR230324","GAMEPR230727","INFLU230402","INFLU230404",
                       "INFLU230408","INFLU230821"),
      window_start = "2023-3-08",
      window_end = "2023-12-02",
      adstock = adstock
    )
  }
  
  # Get hyperparameter names
  hyper_names <- hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)
  
  print(hyper_names)
  
  return(hyper_names)
}


# ==========================================
# PART 3: ROBYN MMM IMPLEMENTATION
# ==========================================
# Function to prepare and run Robyn MMM 
run_robyn_mmm <- function(data, var_categories,
                          window_start = "2023-3-08", window_end = "2023-12-02",
                          adstock = "geometric", 
                          iterations = 2000, trials = 5,
                          create_files = TRUE, 
                          directory = "~/MMM") {
  
  # Load required packages
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    install.packages("dplyr")
  }
  library(dplyr)
  
  # Load holiday data
  data("dt_prophet_holidays")
  
  # To make sure column names in the data are clean and match expected format
  names(data) <- gsub(" ", "_", names(data))
  names(data) <- gsub("\\*", "", names(data))  
  names(data) <- gsub("__", "_", names(data))  
  
  # Check if we're using enhanced features
  if (all(c("Digital_Weighted", "TV_Like_Weighted", "SEM_TimeWeighted") %in% names(data))) {
    # Prepare input collection with enhanced features
    InputCollect <- robyn_inputs(
      dt_input = data,
      date_var = "DATE",
      dep_var = var_categories$dep_var,
      dep_var_type = "conversion", 
      prophet_vars = c("weekday"), 
      prophet_country = "DE",
      context_vars = c("GT", "CP_campaigns", "obon230812", "GW230429", "GW230430"),
      paid_media_spends = c("Digital_Weighted", "TV_Like_Weighted", "SEM_TimeWeighted"),
      paid_media_vars = c("Digital_Weighted", "TV_Like_Weighted", "SEM_TimeWeighted"),
      organic_vars = c("BRAND", "INFLU_campaigns", "GAMEPR_campaigns"),
      window_start = window_start,
      window_end = window_end,
      adstock = adstock
    )
    
    # Establishing hyperparameter ranges for enhanced features
    hyperparameters <- list(
      Digital_Weighted_alphas = c(0.5, 3),
      Digital_Weighted_gammas = c(0.8, 1.0),
      Digital_Weighted_thetas = c(0.0, 0.3),
      
      TV_Like_Weighted_alphas = c(0.5, 3),  
      TV_Like_Weighted_gammas = c(0.8, 1.0),
      TV_Like_Weighted_thetas = c(0.3, 0.8),
      
      SEM_TimeWeighted_alphas = c(0.5, 3),
      SEM_TimeWeighted_gammas = c(0.8, 1.0),
      SEM_TimeWeighted_thetas = c(0.1, 0.8),
      
      BRAND_alphas = c(0.5, 3),
      BRAND_gammas = c(0.8, 1.0),
      BRAND_thetas = c(0.1, 0.8),
      
      INFLU_campaigns_alphas = c(0.5, 3),  
      INFLU_campaigns_gammas = c(0.8, 1.0),
      INFLU_campaigns_thetas = c(0.1, 0.8),
      
      GAMEPR_campaigns_alphas = c(0.5, 3),
      GAMEPR_campaigns_gammas = c(0.8, 1.0),
      GAMEPR_campaigns_thetas = c(0.1, 0.8),
      
      train_size = c(0.85, 0.98)
    )
    
  } else {
    #original
    InputCollect <- robyn_inputs(
      dt_input = data,
      date_var = "DATE",
      dep_var = var_categories$dep_var,
      dep_var_type = "conversion", 
      prophet_vars = c("weekday"), 
      prophet_country = "DE",
      context_vars = c("GT","CP230317","CP230421","CP230811","obon230812",
                       "GW230429","GW230430"),
      paid_media_spends = c("SEM","ADNW1","ADNW2","ADNW3","SNS",
                            "AFF","DOUGA"),
      paid_media_vars = c("SEM","ADNW1","ADNW2","ADNW3","SNS",
                          "AFF","DOUGA"),
      organic_vars = c("BRAND","GAMEPR230324","GAMEPR230727","INFLU230402","INFLU230404",
                       "INFLU230408","INFLU230821"),
      window_start = window_start,
      window_end = window_end,
      adstock = adstock
    )
    
    # Original
    hyperparameters <- list(
      SEM_alphas = c(1, 2),
      SEM_gammas = c(0.95, 1),
      SEM_thetas = c(0, 0.799),
      ADNW1_alphas = c(1, 2),
      ADNW1_gammas = c(0.95, 1),
      ADNW1_thetas = c(0, 0.799),
      ADNW2_alphas = c(1, 2),
      ADNW2_gammas = c(0.95, 1),
      ADNW2_thetas = c(0, 0.799),
      ADNW3_alphas = c(1, 2),
      ADNW3_gammas = c(0.95, 1),
      ADNW3_thetas = c(0, 0.799),
      SNS_alphas = c(1, 2),
      SNS_gammas = c(0.95, 1),
      SNS_thetas = c(0, 0.799),
      AFF_alphas = c(1, 2),
      AFF_gammas = c(0.95, 1),
      AFF_thetas = c(0, 0.799),
      DOUGA_alphas = c(1, 2),
      DOUGA_gammas = c(0.95, 1),
      DOUGA_thetas = c(0, 0.799),
      BRAND_alphas = c(1, 2),
      BRAND_gammas = c(0.95, 1),
      BRAND_thetas = c(0, 0.799),
      GAMEPR230324_alphas = c(1, 2),
      GAMEPR230324_gammas = c(0.95, 1),
      GAMEPR230324_thetas = c(0, 0.799),
      GAMEPR230727_alphas = c(1, 2),
      GAMEPR230727_gammas = c(0.95, 1),
      GAMEPR230727_thetas = c(0, 0.799),
      INFLU230402_alphas = c(1, 2),
      INFLU230402_gammas = c(0.95, 1),
      INFLU230402_thetas = c(0, 0.799),
      INFLU230404_alphas = c(1, 2),
      INFLU230404_gammas = c(0.95, 1),
      INFLU230404_thetas = c(0, 0.799),
      INFLU230408_alphas = c(1, 2),
      INFLU230408_gammas = c(0.95, 1),
      INFLU230408_thetas = c(0, 0.799),
      INFLU230821_alphas = c(1, 2),
      INFLU230821_gammas = c(0.95, 1),
      INFLU230821_thetas = c(0, 0.799),
      train_size = c(0.85, 0.98)
    )
  }
  
  # Update input collection with hyperparameters
  InputCollect <- robyn_inputs(InputCollect = InputCollect, hyperparameters = hyperparameters)
  
  set.seed(123)
  
  # Run model
  OutputModels <- robyn_run(
    InputCollect = InputCollect,
    cores = NULL,
    iterations = iterations,
    trials = trials,
    ts_validation = TRUE,
    add_penalty_factor = FALSE
  )
  
  # Generate outputs 
  OutputCollect <- robyn_outputs(
    InputCollect = InputCollect,
    OutputModels = OutputModels,
    pareto_fronts = "auto",
    min_candidates = 100,
    calibration_constraint = 0.1,
    csv_out = "pareto",
    clusters = TRUE,
    export = create_files,
    plot_folder = directory,
    plot_pareto = FALSE
    
  )
  
  
  
  if ("solID" %in% colnames(OutputCollect$resultHypParam)) {
    OutputCollect$resultHypParam$model <- OutputCollect$resultHypParam$solID
  }
  
  # Load and score pareto CSV to find best model
  pareto_file_path <- file.path(OutputCollect$plot_folder, "pareto_hyperparameters.csv")
  
  if (file.exists(pareto_file_path)) {
    message("Found pareto_hyperparameters.csv file - analyzing...")
    pareto_data <- tryCatch({
      read.csv(pareto_file_path)
    }, error = function(e) {
      message("Error reading pareto CSV: ", e$message)
      return(NULL)
    })
  }
  
  best_model <- NULL
  
  if (file.exists(pareto_file_path)) {
    pareto_data <- read.csv(pareto_file_path)
    
    model_id_col <- intersect(c("solID", "modelID", "model"), colnames(pareto_data))[1]
    
    if (!is.null(model_id_col) && all(c("nrmse", "rsq_train") %in% colnames(pareto_data))) {
      pareto_data <- pareto_data %>%
        dplyr::mutate( 
          nrmse_norm = (nrmse_test - min(nrmse_test, na.rm = TRUE)) / 
            (max(nrmse_test, na.rm = TRUE) - min(nrmse_test, na.rm = TRUE)),
          
          decomp_norm = (decomp.rssd - min(decomp.rssd, na.rm = TRUE)) / 
            (max(decomp.rssd, na.rm = TRUE) - min(decomp.rssd, na.rm = TRUE)),
          
          rsq_norm = (rsq_test - min(rsq_test, na.rm = TRUE)) / 
            (max(rsq_test, na.rm = TRUE) - min(rsq_test, na.rm = TRUE)),
          
          # Score: you can adjust the weights here
          score = 0.4 * rsq_norm + 0.3 * (1 - nrmse_norm) + 0.3 * (1 - decomp_norm)
        ) %>%
        dplyr::arrange(desc(score))
      
      best_model <- as.character(pareto_data[1, model_id_col])
      message("Best model from pareto CSV: ", best_model)
    } else {
      stop("Pareto CSV missing required columns.")
    }
  } else {
    stop("Pareto CSV not found at path: ", pareto_file_path)
  }
  
  # Export best model
  if (!is.null(best_model) && best_model %in% OutputCollect$resultHypParam$model) {
    ExportedModel <- robyn_write(InputCollect, OutputCollect, best_model, export = create_files)
    
    OnePager <- tryCatch({
      robyn_onepagers(InputCollect, OutputCollect, best_model, export = create_files)
    }, error = function(e) NULL)
    
    # Create allocation scenarios with constraints
    AllocatorCollect1 <- tryCatch({
      robyn_allocator(
        InputCollect = InputCollect,
        OutputCollect = OutputCollect,
        select_model = best_model,
        channel_constr_low = 0.7,
        channel_constr_up = c(
          Digital_Weighted = 1.5,
          TV_Like_Weighted = 0.8,
          SEM_TimeWeighted = 2.0
        ),
        scenario = "max_response",
        export = create_files
      )
    }, error = function(e) NULL)
    
    AllocatorCollect2 <- tryCatch({
      robyn_allocator(
        InputCollect = InputCollect,
        OutputCollect = OutputCollect,
        select_model = best_model,
        total_budget = 10000,
        channel_constr_low = 0.7,
        channel_constr_up = c(3,3,3,3,3,3,3),
        channel_constr_multiplier = 2,
        scenario = "max_response",
        export = create_files
      )
    }, error = function(e) NULL)
  } else {
    stop("Best model ID not found in resultHypParam.")
  }
  
  # Return key objects
  return(list(
    InputCollect = InputCollect,
    OutputModels = OutputModels,
    OutputCollect = OutputCollect,
    best_model = best_model,
    ExportedModel = ExportedModel,
    OnePager = OnePager,
    AllocatorCollect1 = AllocatorCollect1,
    AllocatorCollect2 = AllocatorCollect2
  ))
}

# ==========================================
# PART 4: LINEAR REGRESSION MODEL
# ==========================================

# Function to run linear regression model
run_linear_regression <- function(data, var_categories) {
  # Prepare data for linear regression model
  lm_data <- data %>%
    select(-DATE) %>%
    select(where(is.numeric))
  
  # Set dependent variable
  dep_var <- var_categories$dep_var
  
  # Create formula with only the variables that are in var_categories
  predictor_vars <- c(
    var_categories$paid_media_vars,
    var_categories$organic_vars,
    var_categories$context_vars
  )
  
  # Ensure all predictor variables exist in lm_data
  predictor_vars <- predictor_vars[predictor_vars %in% names(lm_data)]
  
  # Create formula string
  formula_str <- paste(dep_var, "~", paste(predictor_vars, collapse = " + "))
  formula_obj <- as.formula(formula_str)
  
  # Split data into training and testing sets
  set.seed(123)
  trainIndex <- createDataPartition(lm_data[[dep_var]], p = 0.7, list = FALSE)
  train_data <- lm_data[trainIndex, ]
  test_data <- lm_data[-trainIndex, ]
  
  # Linear Regression
  message("Fitting Linear Regression model...")
  lm_model <- lm(formula_obj, data = train_data)
  lm_predictions <- predict(lm_model, test_data)
  
  # Calculate metrics
  y_test <- test_data[[dep_var]]
  rmse <- sqrt(mean((y_test - lm_predictions)^2))
  mae <- mean(abs(y_test - lm_predictions))
  r2 <- 1 - sum((y_test - lm_predictions)^2) / sum((y_test - mean(y_test))^2)
  
  # Format metrics
  metrics <- data.frame(
    model = "Linear Regression",
    rmse = rmse,
    mae = mae,
    r2 = r2
  )
  
  # Extract variable importance (coefficients)
  lm_summary <- summary(lm_model)
  lm_coef <- lm_summary$coefficients[, 1]
  lm_coef <- lm_coef[!names(lm_coef) %in% "(Intercept)"]
  
  # Create variable importance dataframe
  variable_importance <- data.frame(
    variable = names(lm_coef),
    importance = abs(lm_coef),
    model = "Linear Regression"
  ) %>%
    arrange(desc(importance))
  
  # Return results
  return(list(
    model = lm_model,
    predictions = lm_predictions,
    metrics = metrics,
    variable_importance = variable_importance,
    test_data = test_data,
    actual = y_test,
    summary = lm_summary
  ))
}
# ==========================================
# PART 5: COMPARISON AND VISUALIZATION
# ==========================================
compare_models <- function(robyn_results, lr_results, var_categories) {
  # Check for required elements
  if (is.null(robyn_results) || is.null(robyn_results$best_model) || is.null(robyn_results$OutputCollect)) {
    message("WARNING: Robyn results incomplete, creating metrics with LR model only")
    all_metrics <- lr_results$metrics
    
    return(list(
      metrics = all_metrics,
      metrics_plot = ggplot(all_metrics, aes(x = model, y = rmse)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        coord_flip() +
        labs(title = "Model Metrics (LR only)",
             x = "Model", y = "RMSE") +
        theme_minimal(),
      r2_plot = NULL,
      importance_plot = NULL,
      variable_importance = lr_results$variable_importance
    ))
  }
  
  # Extract Robyn metrics from resultHypParam
  best_model <- robyn_results$best_model
  result_df <- robyn_results$OutputCollect$resultHypParam
  
  # Validate model existence
  model_col <- intersect(c("solID", "modelID", "model"), colnames(result_df))[1]
  if (is.na(model_col) || !(best_model %in% result_df[[model_col]])) {
    message("WARNING: Best model ID '", best_model,
            "' not found in OutputCollect$resultHypParam.", model_col, ".")
    message("Available IDs: ", paste(head(result_df[[model_col]], 10), collapse = ", "))
    return(NULL)
  }
  robyn_row <- result_df[result_df[[model_col]] == best_model, ]
  
  if (nrow(robyn_row) == 0) {
    message("WARNING: Best model not found in OutputCollect$resultHypParam, skipping Robyn comparison")
    return(NULL)
  }
  
  # Extract NRMSE and Rsq
  robyn_metrics <- data.frame(
    model = "Robyn MMM",
    rmse = robyn_row$nrmse,
    r2 = robyn_row$rsq_train,
    mae = NA  # Robyn doesn't directly provide MAE
  )
  
  # Combine with Linear Regression metrics
  all_metrics <- bind_rows(robyn_metrics, lr_results$metrics)
  
  # Create RMSE and R2 plots
  metrics_plot <- ggplot(all_metrics, aes(x = model, y = rmse)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(title = "Model Comparison: RMSE (lower is better)",
         x = "Model", y = "RMSE") +
    theme_minimal()
  
  r2_plot <- ggplot(all_metrics, aes(x = model, y = r2)) +
    geom_bar(stat = "identity", fill = "darkgreen") +
    coord_flip() +
    labs(title = "Model Comparison: R² (higher is better)",
         x = "Model", y = "R²") +
    theme_minimal()
  
  # Extract Robyn decomposition safely
  robyn_decomp <- tryCatch({
    robyn_results$OutputCollect$decomposition
  }, error = function(e) {
    message("Error getting decomposition: ", e$message)
    return(NULL)
  })
  
  if (is.null(robyn_decomp)) {
    return(list(
      metrics = all_metrics,
      metrics_plot = metrics_plot,
      r2_plot = r2_plot,
      importance_plot = NULL,
      variable_importance = lr_results$variable_importance
    ))
  }
  
  robyn_decomp_model <- robyn_decomp[[best_model]]
  robyn_imp <- data.frame(
    variable = names(robyn_decomp_model),
    importance = unlist(robyn_decomp_model),
    model = "Robyn MMM"
  )
  
  # Filter variables
  media_vars <- c(var_categories$paid_media_vars, var_categories$organic_vars)
  robyn_imp <- robyn_imp[robyn_imp$variable %in% media_vars, ]
  
  # Linear Regression importance filtered
  lr_imp <- lr_results$variable_importance
  lr_imp_filtered <- lr_imp[lr_imp$variable %in% media_vars, ]
  
  # Combine and normalize
  combined_imp <- bind_rows(robyn_imp, lr_imp_filtered)
  combined_imp <- combined_imp %>%
    group_by(model) %>%
    mutate(norm_importance = importance / sum(importance, na.rm = TRUE)) %>%
    ungroup()
  
  imp_plot <- ggplot(combined_imp, aes(x = reorder(variable, norm_importance), 
                                       y = norm_importance, fill = model)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    labs(title = "Variable Importance Comparison",
         x = "Variable", y = "Normalized Importance") +
    theme_minimal() +
    facet_wrap(~ model, scales = "free_y")
  
  return(list(
    metrics = all_metrics,
    metrics_plot = metrics_plot,
    r2_plot = r2_plot,
    importance_plot = imp_plot,
    variable_importance = combined_imp
  ))
}

# ==========================================
# PART 6: MAIN EXECUTION FUNCTION 
# ==========================================

run_mmm_analysis <- function(file_path = NULL) {
  
  # Create MMM directory
  robyn_directory <- create_mmm_directory()
  
  # Load data
  message("Loading marketing data...")
  dt_marketing <- load_marketing_data(file_path)
  
  # Categorize variables
  message("Categorizing variables...")
  var_categories <- categorize_variables(dt_marketing)
  hyper_params <- display_hyper_names(dt_marketing, var_categories)
  print(hyper_params)
  
  message("Variable categories identified:")
  print(var_categories)
  
  # Set up error handling for the entire process
  tryCatch({
    # Configure graphics settings for stability
    options(bitmapType = "cairo")
    
    # Run Robyn MMM
    message("Running Robyn MMM model ...")
    robyn_results <- run_robyn_mmm(
      data = dt_marketing,
      var_categories = var_categories,
      iterations = 2000,  
      trials = 5,       
      create_files = TRUE,
      directory = robyn_directory
    )
    
    # Show models in resultHypParam
    cat("DEBUG: Available model IDs in OutputCollect$resultHypParam:\n")
    print(robyn_results$OutputCollect$resultHypParam$model)
    
    # What is the selected best_model
    cat("DEBUG: Selected best_model: ", robyn_results$best_model, "\n")
    
    
    message("Robyn MMM model complete. Best model: ", robyn_results$best_model)
    
    
    # Run Linear Regression model
    message("Running Linear Regression model...")
    lr_results <- run_linear_regression(dt_marketing, var_categories)
    message("Linear Regression model complete")
    
    # Compare models
    message("Comparing Robyn MMM and Linear Regression models...")
    comparison <- compare_models(robyn_results, lr_results, var_categories)
    
    # Save comparison plots
    tryCatch({
      Cairo::CairoPNG(filename = file.path(robyn_directory, "metrics_comparison.png"), 
                      width = 800, height = 600, units = "px")
      print(comparison$metrics_plot)
      dev.off()
      
      Cairo::CairoPNG(filename = file.path(robyn_directory, "r2_comparison.png"), 
                      width = 800, height = 600, units = "px")
      print(comparison$r2_plot)
      dev.off()
      
      Cairo::CairoPNG(filename = file.path(robyn_directory, "importance_comparison.png"), 
                      width = 1000, height = 800, units = "px")
      print(comparison$importance_plot)
      dev.off()
    }, error = function(e) {
      message("Error saving plots: ", e$message)
      message("Will save data files only.")
    })
    
    # Write metrics to CSV
    write.csv(comparison$metrics, file.path(robyn_directory, "model_metrics_comparison.csv"), row.names = FALSE)
    
    # Write variable importance comparison to CSV
    write.csv(comparison$variable_importance, file.path(robyn_directory, "variable_importance_comparison.csv"), row.names = FALSE)
    
    # Return results
    return(list(
      robyn_results = robyn_results,
      lr_results = lr_results,
      comparison = comparison,
      directory = robyn_directory
    ))
    
  }, error = function(e) {
    message("Error in MMM analysis: ", e$message)
    
    # Try to run at least the linear regression if Robyn fails
    tryCatch({
      message("Attempting to run Linear Regression model only...")
      lr_results <- run_linear_regression(dt_marketing, var_categories)
      message("Linear Regression model complete")
      
      # Write LR results to CSV
      write.csv(lr_results$metrics, file.path(robyn_directory, "lr_model_metrics.csv"), row.names = FALSE)
      write.csv(lr_results$variable_importance, file.path(robyn_directory, "lr_variable_importance.csv"), row.names = FALSE)
      
      return(list(
        robyn_results = NULL,
        lr_results = lr_results,
        comparison = NULL,
        directory = robyn_directory
      ))
    }, error = function(e2) {
      message("Error in Linear Regression analysis: ", e2$message)
      return(NULL)
    })
  })
}

# ==========================================
# PART 7: EXECUTION & USER INTERFACE
# ==========================================

# Create simple UI for file selection
if (interactive()) {
  cat("===================================================\n")
  cat("   Marketing Mix Modeling Analysis Suite           \n")
  cat("   Robyn MMM + Linear Regression Comparison        \n")
  cat("===================================================\n\n")
  
  cat("Please select your marketing data file (Excel, CSV, or RDS)...\n")
  results <- run_mmm_analysis()
  
  cat("\n===================================================\n")
  cat("Analysis complete! Results saved to:", results$directory, "\n")
  cat("===================================================\n")
  
  # Show Linear Regression results
  cat("\nLinear Regression model results:\n")
  cat("RMSE: ", round(results$lr_results$metrics$rmse, 4), "\n")
  cat("R²: ", round(results$lr_results$metrics$r2, 4), "\n\n")
  
  # Show top 5 most important variables in Linear Regression
  cat("Top 5 most important variables in Linear Regression:\n")
  top_vars <- head(results$lr_results$variable_importance[order(-results$lr_results$variable_importance$importance), ], 5)
  for (i in 1:nrow(top_vars)) {
    cat(i, ". ", top_vars$variable[i], " (importance: ", round(top_vars$importance[i], 4), ")\n", sep="")
  }
  cat("\n")
  
  # Show Robyn MMM results
  if (!is.null(results$comparison)) {
    cat("Robyn MMM model: ", results$robyn_results$best_model, "\n")
    cat("NRMSE: ", round(results$comparison$metrics$rmse[results$comparison$metrics$model == "Robyn MMM"], 4), "\n")
    cat("R²: ", round(results$comparison$metrics$r2[results$comparison$metrics$model == "Robyn MMM"], 4), "\n\n")
  } else {
    cat("Robyn MMM model results unavailable due to error.\n\n")
  }
  cat("See full comparison in:", file.path(results$directory, "model_metrics_comparison.csv"), "\n")
  cat("See variable importance comparison in:", file.path(results$directory, "variable_importance_comparison.csv"), "\n")
} else {
  # results <- run_mmm_analysis(path)
}