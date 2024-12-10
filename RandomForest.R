library(arrow)
library(dplyr)
library(lubridate)
library(scales)
library(zoo)
library(tidyr)
library(caret)
library(ggplot2)
library(grf)
library(DALEX)
library(parallel)

options(cores = detectCores() - 1) 



# Function to set seed (similar to Python's set_seed)
set_seed <- function(seed = 42) {
  set.seed(seed)  # Set R's random seed
  Sys.setenv("R_SEED" = seed)  # Set environment variable (optional for reproducibility)
}

set_seed()

# Function to split data into train, validation, and test sets
split_data <- function(df) {
  train <- df %>% filter(index <= "2024-04-01 00:00:00+0000")
  val <- df %>% filter(index > "2024-04-02 00:00:00+0000" & index <= "2024-09-01 00:00:00+0000")
  test <- df %>% filter(index > "2024-09-02 00:00:00+0000")
  
  return(list(train = train, val = val, test = test))
}

add_descriptive_columns <- function(df, basic = TRUE, seasons = TRUE, weekday = TRUE) {
  # Ensure the index is a datetime object
  if (!inherits(df$index, "POSIXct")) {
    stop("The 'index' column must be a POSIXct datetime object.")
  }
  
  # Add basic descriptive columns
  if (basic) {
    df <- df %>%
      mutate(
        year = year(index),          # Extract year
        month = month(index),        # Extract month
        week = isoweek(index),       # Extract ISO week
        dayOfWeek = wday(index) - 1, # Extract day of week (0 = Monday, 6 = Sunday)
        dayOfMonth = mday(index),    # Extract day of the month
        dayOfYear = yday(index),     # Extract day of the year
        time = format(index, "%H:%M:%S"), # Extract time as a string
        hour = hour(index),          # Extract hour
        minute = minute(index)       # Extract minute
      )
  }
  
  # Map months to seasons (1: Winter, 2: Spring, 3: Summer, 4: Autumn)
  if (seasons) {
    df <- df %>%
      mutate(
        season = case_when(
          month %in% c(12, 1, 2) ~ 1, # Winter
          month %in% c(3, 4, 5) ~ 2,  # Spring
          month %in% c(6, 7, 8) ~ 3,  # Summer
          month %in% c(9, 10, 11) ~ 4 # Autumn
        )
      )
  }
  
  # Add weekday and weekend flags
  if (weekday) {
    df <- df %>%
      mutate(
        weekday = ifelse(dayOfWeek <= 4, 1, 0), # 1 = Weekday (Monday to Friday)
        weekend = ifelse(dayOfWeek >= 5, 1, 0) # 1 = Weekend (Saturday, Sunday)
      )
  }
  
  return(df)
}


# Function to create shifted and rolling features
shift_data <- function(df, lag) {
  df <- df %>% mutate(
    # Shift 'kWh' by 1 step
    shifted_faktas = lag(kWh, 96),
    # Difference between shifted values
    difference = shifted_faktas - lag(shifted_faktas, 1)
  )
  
  # Generate rolling statistics
  for (offset in c(4, 8, 12, 16)) {
    prefix <- paste0("kWh_", offset, "_rolling_")
    df <- df %>% mutate(
      !!paste0(prefix, "mean") := zoo::rollmean(shifted_faktas, offset, fill = NA, align = "right"),
      !!paste0(prefix, "median") := zoo::rollmedian(shifted_faktas, offset, fill = NA, align = "right"),
      !!paste0(prefix, "min") := zoo::rollapply(shifted_faktas, offset, min, fill = NA, align = "right"),
      !!paste0(prefix, "max") := zoo::rollapply(shifted_faktas, offset, max, fill = NA, align = "right")
    )
  }

  # Generate lag features
  for (i in 1:lag) {
    df <- df %>% mutate(!!paste0("kWh_lag_", i) := lag(shifted_faktas, i))
  }
  
  # Drop the 'shifted_faktas' column
  df <- df %>% select(-shifted_faktas)
  
  return(df)
}

# Read the Parquet file
merged_df <- read_parquet("data/tr_visi_duomenys_updated.parquet")

# Ensure your data has a POSIXct index column
merged_df <- merged_df %>%
  mutate(index = as.POSIXct(date_time)) %>% # Convert date_time to POSIXct
  select(-date_time)                       # Remove the original column if needed

# Add descriptive columns
merged_df <- add_descriptive_columns(merged_df)

# Drop unnecessary columns
df <- merged_df %>%
  select(-c(feelsLikeTemperature, windSpeed, week, dayOfYear, Šventė, weekday, date, time))

# Interpolate missing values in the 'kWh' column
df <- df %>%
  mutate(kWh = zoo::na.approx(kWh))

# Split the data
split_sets <- split_data(df)
train_data <- split_sets$train
val_data <- split_sets$val
test_data <- split_sets$test

# Apply shift_data function and remove rows with NA values
train_data <- shift_data(train_data, lag = 48) %>% drop_na()
val_data <- shift_data(val_data, lag = 48) %>% drop_na()
test_data <- shift_data(test_data, lag = 48) %>% drop_na()

# Separate target and features
train_target <- train_data %>% select(kWh)
train_data <- train_data %>% select(-kWh)

val_target <- val_data %>% select(kWh)
val_data <- val_data %>% select(-kWh)

test_target <- test_data %>% select(kWh)
test_data <- test_data %>% select(-kWh)

# Encode cyclical features
encode_cyclical <- function(df) {
  df <- df %>%
    mutate(
      month_x = sin(month / 12 * 2 * pi),
      month_y = cos(month / 12 * 2 * pi),
      season_x = sin(season / 4 * 2 * pi),
      season_y = cos(season / 4 * 2 * pi),
      dayOfWeek_x = sin(dayOfWeek / 7 * 2 * pi),
      dayOfWeek_y = cos(dayOfWeek / 7 * 2 * pi),
      dayOfMonth_x = sin(dayOfMonth / 31 * 2 * pi),
      dayOfMonth_y = cos(dayOfMonth / 31 * 2 * pi),
      hour_x = sin(hour / 24 * 2 * pi),
      hour_y = cos(hour / 24 * 2 * pi),
      minute_x = sin(minute / 60 * 2 * pi),
      minute_y = cos(minute / 60 * 2 * pi)
    ) %>%
    select(-c(month, season, dayOfWeek, dayOfMonth, hour, minute))
  
  return(df)
}

train_data <- encode_cyclical(train_data)
val_data <- encode_cyclical(val_data)
test_data <- encode_cyclical(test_data)
train_data <- train_data %>% select(-index)
val_data <- val_data %>% select(-index)
test_data <- test_data %>% select(-index)

# Standardize features
scale_data <- function(train, val, test) {
  scaler <- caret::preProcess(train, method = c("center", "scale"))
  train_scaled <- predict(scaler, train)
  val_scaled <- predict(scaler, val)
  test_scaled <- predict(scaler, test)
  
  return(list(train = train_scaled, val = val_scaled, test = test_scaled))
}

scaled_data <- scale_data(train_data, val_data, test_data)
train_data_scaled <- scaled_data$train
val_data_scaled <- scaled_data$val
test_data_scaled <- scaled_data$test


mean_absolute_percentage_error <- function(y_true, y_pred) {
  return(mean(abs((y_true - y_pred) / y_true)) * 100)
}


# Function to plot model results
plot_model_results <- function(test_target, prediction, time_index, plot_intervals = FALSE, plot_anomalies = FALSE, log_scale = FALSE) {
  # Calculate residuals for confidence intervals
  residuals <- test_target - prediction
  error_std <- sd(residuals)
  scale <- 1.96
  lower <- prediction - (error_std * scale)
  upper <- prediction + (error_std * scale)
  
  # Prepare data for plotting
  plot_data <- data.frame(
    Time = time_index,  # Use a date or POSIXct column for x-axis
    Actual = test_target,
    Predicted = prediction,
    Lower_CI = lower,
    Upper_CI = upper
  )
  
  # Base plot
  p <- ggplot(plot_data, aes(x = Time)) +
    geom_line(aes(y = Actual, color = "Actual"), size = 1) +
    geom_line(aes(y = Predicted, color = "Predicted"), size = 1) +
    labs(
      title = "RandomForest Forecast vs. Actual October's Consumption",
      y = "kWh",
      x = "",
      color = "Legend"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center and style title
      axis.title = element_text(size = 14),                              # Style axis titles
      axis.text = element_text(size = 12),                               # Style axis text
      legend.title = element_blank(),                                    # Remove legend title
      legend.text = element_text(size = 12),                             # Style legend text
      legend.position = "top",                                           # Place legend at the top
      legend.spacing.x = unit(0.5, "cm"),                                # Add space between legend items
      axis.text.x = element_text(angle = 30, hjust = 1)                  # Rotate x-axis labels
    ) +
    scale_color_manual(
      values = c("Actual" = "blue", "Predicted" = "red")                 # Set custom colors
    )
  
  # Add confidence intervals
  if (plot_intervals) {
    p <- p +
      geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), fill = "grey80", alpha = 0.4)
  }
  
  # Highlight anomalies
  if (plot_anomalies) {
    anomalies <- which(test_target < lower | test_target > upper)
    anomaly_data <- data.frame(Time = time_index[anomalies], Value = test_target[anomalies])
    p <- p +
      geom_point(data = anomaly_data, aes(x = Time, y = Value), color = "purple", size = 3)
  }
  
  # Apply log scale if needed
  if (log_scale) {
    p <- p + scale_y_log10()
  }
  
  # Format x-axis with specific date steps and rotated ticks
  p <- p + 
    scale_x_datetime(
      date_labels = "%Y-%m-%d",      # Format for date labels
      date_breaks = "3 days"         # Interval between ticks
    )
  
  print(p)
}


# Function to plot model results
plot_model_results_one_day <- function(test_target, prediction, time_index, plot_intervals = FALSE, plot_anomalies = FALSE, log_scale = FALSE) {
  # Calculate residuals for confidence intervals
  residuals <- test_target - prediction
  error_std <- sd(residuals)
  scale <- 1.96
  lower <- prediction - (error_std * scale)
  upper <- prediction + (error_std * scale)
  
  # Prepare data for plotting
  plot_data <- data.frame(
    Time = time_index,  # Use a date or POSIXct column for x-axis
    Actual = test_target,
    Predicted = prediction,
    Lower_CI = lower,
    Upper_CI = upper
  )
  
  # Base plot
  p <- ggplot(plot_data, aes(x = Time)) +
    geom_line(aes(y = Actual, color = "Actual"), size = 1) +
    geom_line(aes(y = Predicted, color = "Predicted"), size = 1) +
    labs(
      title = "RandomForest Forecast vs. 2024-09-05 Consumption",
      y = "kWh",
      x = "",
      color = "Legend"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center and style title
      axis.title = element_text(size = 14),                              # Style axis titles
      axis.text = element_text(size = 12),                               # Style axis text
      legend.title = element_blank(),                                    # Remove legend title
      legend.text = element_text(size = 12),                             # Style legend text
      legend.position = "top",                                           # Place legend at the top
      legend.spacing.x = unit(0.5, "cm"),                                # Add space between legend items
      axis.text.x = element_text(angle = 30, hjust = 1)                  # Rotate x-axis labels
    ) +
    scale_color_manual(
      values = c("Actual" = "blue", "Predicted" = "red")                 # Set custom colors
    )
  
  # Add confidence intervals
  if (plot_intervals) {
    p <- p +
      geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), fill = "grey80", alpha = 0.4)
  }
  
  # Highlight anomalies
  if (plot_anomalies) {
    anomalies <- which(test_target < lower | test_target > upper)
    anomaly_data <- data.frame(Time = time_index[anomalies], Value = test_target[anomalies])
    p <- p +
      geom_point(data = anomaly_data, aes(x = Time, y = Value), color = "purple", size = 3)
  }
  
  # Apply log scale if needed
  if (log_scale) {
    p <- p + scale_y_log10()
  }
  
  # Format x-axis with specific date steps and rotated ticks
  p <- p + 
    scale_x_datetime(
      date_labels = "%H:%M",      # Use time format for 24-hour data
      date_breaks = "2 hours"     # Ticks every 2 hours
    )
  
  print(p)
}

# Function to calculate forecast scores
forecast_results <- function(model_name, target_kwh, predictions) {
  # Calculate metrics
  mean_absolute_error <- mean(abs(target_kwh - predictions))
  root_mean_squared_error <- sqrt(mean((target_kwh - predictions)^2))
  mean_absolute_percentage_error <- mean(abs((target_kwh - predictions) / target_kwh)) * 100
  
  # Create a results data frame
  results <- data.frame(
    Regressor = model_name,
    Mean_Absolute_Error = mean_absolute_error,
    Root_Mean_Squared_Error = root_mean_squared_error,
    Mean_Absolute_Percentage_Error = mean_absolute_percentage_error
  )
  
  return(results)
}


# Custom prediction wrapper for grf
grf_pred_wrapper <- function(object, newdata) {
  # Convert newdata to matrix (grf requires matrices)
  newdata_matrix <- as.matrix(newdata)
  # Use predict() from grf and return predictions
  predict(object, newdata_matrix)$predictions
}

# Function to compute and plot permutation importance
plot_and_return_permutation_importance <- function(model, X, y, dataset_name) {
  # Ensure X is a matrix (grf works with matrices)
  X_matrix <- as.matrix(X)
  
  # Create an explainer object using DALEX
  explainer <- explain(
    model = model,
    data = X_matrix,
    y = y,
    predict_function = grf_pred_wrapper,
    label = "grf_model"
  )
  
  # Calculate permutation importance
  importance <- model_parts(
    explainer = explainer,
    loss_function = loss_root_mean_square, # Use RMSE as the loss function
    B = 5,                                 # Number of permutations
    type = "difference",                   # Calculate importance as difference in loss
    n_sample = NULL                        # Use all samples
  )
  
  # Convert importance data to a data frame
  importance_df <- as.data.frame(importance)
  
  # Sort importance values from biggest impact to smallest
  sorted_importance <- importance_df %>%
    filter(variable != "_baseline_") %>% # Exclude baseline row
    arrange(desc(dropout_loss))
  
  # Create a boxplot of importances
  p <- ggplot(sorted_importance, aes(x = dropout_loss, y = reorder(variable, dropout_loss))) +
    geom_boxplot(color = "blue", fill = "lightblue", outlier.color = "red") +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
    labs(
      title = paste("Permutation Importances (", dataset_name, " set)", sep = ""),
      x = "Decrease in RMSE",
      y = "Features"
    ) +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      axis.text = element_text(size = 8),
      plot.title = element_text(size = 12, face = "bold")
    )
  print(p)
  
  # Return sorted importance data frame
  return(sorted_importance)
}


# Combine features and target into one data frame
train_data <- cbind(train_data_scaled, kWh = train_target$kWh)
val_data <- cbind(val_data_scaled, kWh = val_target$kWh)
test_data <- cbind(test_data_scaled, kWh = test_target$kWh)

# Prepare data for grf
X <- as.matrix(train_data[, setdiff(names(train_data), "kWh")])  # Exclude target column by name
Y <- train_data$kWh

# Train a regression forest
model <- regression_forest(X, Y, num.trees = 100)

# Predict on validation set
X_val <- as.matrix(val_data[, setdiff(names(val_data), "kWh")])
X_test <- as.matrix(test_data[, setdiff(names(test_data), "kWh")])
val_preds <- predict(model, X_val)$predictions
test_preds <- predict(model, X_test)$predictions

# Calculate forecast metrics for the ranger model
val_results <- forecast_results(
  model_name = "Ranger.RandomForestRegressor",
  target_kwh  = val_data$kWh, 
  predictions = val_preds
)

test_results <- forecast_results(
  model_name = "Ranger.RandomForeslentRegressor",
  target_kwh  = test_data$kWh, 
  predictions = test_preds
)
# Print results
print(val_results)
print(test_results)


start_time <- as.POSIXct("2024-09-03 09:15:00", tz = "UTC")
end_time <- as.POSIXct("2024-11-11 22:00:00", tz = "UTC")
time_index <- seq(from = start_time, to = end_time, by = "15 mins")

# Call the function
plot_model_results(test_target$kWh, test_preds, time_index)
subset_time_index <- as.POSIXct(time_index[1:96], tz = "UTC")

plot_model_results_one_day(test_target$kWh[60:156], test_preds[60:156], time_index[60:156])

################################################################################
# Plot and get sorted permutation importance
train_sorted_importance <- plot_and_return_permutation_importance(
  model = model,
  X = X,
  y = train_target$kWh,
  dataset_name = "Train set"
)

# View sorted importances
print(train_sorted_importance)

# Plot and get sorted permutation importance
val_sorted_importance <- plot_and_return_permutation_importance(
  model = model,
  X = val_data_scaled,
  y = val_target$kWh,
  dataset_name = "Validation set"
)

# View sorted importances
print(val_sorted_importance)


top_features <- train_sorted_importance %>%
  group_by(variable) %>%  # Handle duplicate variables
  summarize(max_importance = max(dropout_loss), .groups = "drop_last") %>%
  arrange(desc(max_importance)) %>%
  slice(1:36) %>%
  pull(variable)


train_data_selected <- train_data[, top_features, drop = FALSE]
val_data_selected <- val_data[, top_features, drop = FALSE]
test_data_selected <- test_data[, top_features, drop = FALSE]

scaler <- preProcess(train_data_selected, method = c("center", "scale"))

# Transform the datasets
train_data_tr_sel <- predict(scaler, train_data_selected)
val_data_tr_sel <- predict(scaler, val_data_selected)
test_data_tr_sel <- predict(scaler, test_data_selected)

# Expanded parameter grid
param_grid <- expand.grid(
  min.node.size = c(1, 5, 10),
  num.trees = c(100, 200),
  alpha = c(0.05, 0.1, NULL),   # Include NULL for confidence intervals
  sample.fraction = c(0.4),     # Avoid edge cases for honesty
  honesty.fraction = c(0.5),    # Simpler honesty settings
  honesty = c(TRUE, FALSE)
)


# Random search: Sample a subset of the grid
n_random_samples <- 20
random_grid <- param_grid[sample(1:nrow(param_grid), n_random_samples), ]

# Function for random search with GRF
random_search_grf <- function(random_grid, train_data, train_target, val_data, val_target) {
  best_rmse <- Inf
  best_params <- NULL
  final_model <- NULL
  
  for (i in seq_len(nrow(random_grid))) {
    params <- random_grid[i, ]
    
    cat("Training model with parameters:\n")
    print(params)
    
    # Train the model
    model <- regression_forest(
      X = as.matrix(train_data),
      Y = train_target,
      min.node.size = 5,
      num.trees = 100,
      alpha = 0.1,
      sample.fraction = 0.4,
      honesty.fraction = 0.5,
      honesty = TRUE,
      seed = 42
    )
    
    # Predict on validation set
    val_preds <- predict(model, as.matrix(val_data))$predictions
    
    # Calculate RMSE
    rmse <- caret::RMSE(val_preds, val_target)
    
    # Update best model if this one is better
    if (rmse < best_rmse) {
      best_rmse <- rmse
      best_params <- params
      final_model <- model
    }
  }
  
  return(list(best_model = final_model, best_params = best_params, best_rmse = best_rmse))
}

# Run random search
search_results <- random_search_grf(
  random_grid = random_grid,
  train_data = train_data_tr_sel,
  train_target = train_target$kWh,
  val_data = val_data_tr_sel,
  val_target = val_target$kWh
)

# Extract results
final_model <- search_results$best_model
best_params <- search_results$best_params
cat("Best Parameters:\n")
print(best_params)
cat("Best Validation RMSE:", search_results$best_rmse, "\n")


# Predictions on the validation and test sets
val_preds <- predict(final_model, as.matrix(val_data_tr_sel))$predictions
test_preds <- predict(final_model, as.matrix(test_data_tr_sel))$predictions

# Evaluate on validation and test sets
val_rmse <- caret::RMSE(val_preds, val_target$kWh)
test_rmse <- caret::RMSE(test_preds, test_target$kWh)

# Output results
cat("Validation RMSE:", val_rmse, "\n")
cat("Test RMSE:", test_rmse, "\n")


# Calculate forecast metrics for the ranger model
val_results <- forecast_results(
  model_name = "RandomForestRegressor",
  target_kwh  = val_data$kWh, 
  predictions = val_preds
)

test_results <- forecast_results(
  model_name = "RandomForeslentRegressor",
  target_kwh  = test_data$kWh, 
  predictions = test_preds
)
# Print results
print(val_results)
print(test_results)

plot_model_results(test_target$kWh[2652:5628], test_preds[2652:5628], time_index[2652:5628])

plot_model_results_one_day(test_target$kWh[156:252], test_preds[156:252], time_index[156:252])
