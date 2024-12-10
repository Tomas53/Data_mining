library(arrow)
library(dplyr)
library(lubridate)
library(scales)
library(zoo)
library(tidyr)


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
  
  # Drop the 'shifted_faktas' column
  #df <- df %>% select(-shifted_faktas)
  
  # Generate lag features
  for (i in 1:lag) {
    df <- df %>% mutate(!!paste0("kWh_lag_", i) := lag(shifted_faktas, i))
  }
  # Drop the 'shifted_faktas' column
  df <- df %>% select(-shifted_faktas)
  
  return(df)
}

# Read the Parquet file
merged_df <- read_parquet("~/Documents/R projektai/data mining/tr_visi_duomenys_updated.parquet")

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

scaled_data
train_data_scaled
val_data_scaled
test_data_scaled






# Load required libraries
library(xgboost)
library(dplyr)
library(ggplot2)

# Define the top 10 features based on importance
top_10_features <- c(
  "kWh_4_rolling_median", "kWh_4_rolling_mean", "kWh_lag_1", 
  "kWh_4_rolling_max", "kWh_4_rolling_min", "airTemperature", 
  "hour_x", "hour_y", "dayOfWeek_x", "difference"
)

# Add a timestamp column assuming 15-minute intervals
start_time <- as.POSIXct("2024-09-03 09:15:00")  
test_data_scaled$timestamp <- seq(
  from = start_time,
  by = "15 min",
  length.out = nrow(test_data_scaled)
)

# Filter the datasets to include only the selected top 10 features
train_data_selected <- train_data_scaled %>% select(all_of(top_10_features))
val_data_selected <- val_data_scaled %>% select(all_of(top_10_features))
test_data_selected <- test_data_scaled %>% select(all_of(top_10_features))

# Convert data to matrix format for xgboost
train_matrix <- as.matrix(train_data_selected)
val_matrix <- as.matrix(val_data_selected)
test_matrix <- as.matrix(test_data_selected)

# Convert target variables to numeric vectors
train_target_vector <- as.numeric(unlist(train_target))
val_target_vector <- as.numeric(unlist(val_target))
test_target_vector <- as.numeric(unlist(test_target))

# Create DMatrix objects
dtrain <- xgb.DMatrix(data = train_matrix, label = train_target_vector)
dval <- xgb.DMatrix(data = val_matrix, label = val_target_vector)
dtest <- xgb.DMatrix(data = test_matrix, label = test_target_vector)

# Define XGBoost parameters
params <- list(
  booster = "gbtree",
  objective = "reg:squarederror",  # Regression task
  eta = 0.1,                      # Learning rate
  max_depth = 6,                  # Maximum depth of trees
  subsample = 0.8,                # Subsample ratio of the training instance
  colsample_bytree = 0.8          # Subsample ratio of columns for each tree
)


set.seed(42)  
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 500,                   # Maximum number of boosting rounds
  watchlist = list(
    train = dtrain,
    validation = dval
  ),
  early_stopping_rounds = 100,      
  print_every_n = 10               
)

# Filter data for September 5th, 2024
september_5th_indices <- which(format(test_data_scaled$timestamp, "%Y-%m-%d") == "2024-09-05")

# Ensure there are at least 96 rows (24 hours with 15-minute intervals)
if (length(september_5th_indices) < 96) {
  stop("Not enough data for 24 hours of September 5th (96 rows needed).")
}

september_5th_features <- test_data_scaled[september_5th_indices, top_10_features, drop = FALSE]
september_5th_actual <- test_target_vector[september_5th_indices]

# Predict for September 5th
september_5th_predictions <- predict(xgb_model, as.matrix(september_5th_features))


time_index <- test_data_scaled$timestamp[september_5th_indices]


plot_model_results_one_day <- function(test_target, prediction, time_index) {
  residuals <- test_target - prediction
  error_std <- sd(residuals)
  scale <- 1.96
  lower <- prediction - (error_std * scale)
  upper <- prediction + (error_std * scale)
  
  plot_data <- data.frame(
    Time = time_index,
    Actual = test_target,
    Predicted = prediction,
    Lower_CI = lower,
    Upper_CI = upper
  )
  
  ggplot(plot_data, aes(x = Time)) +
    geom_line(aes(y = Actual, color = "Actual"), size = 1) +
    geom_line(aes(y = Predicted, color = "Predicted"), size = 1) +
    labs(
      title = "Gradient Boosting Forecast vs. 2024-09-05 Consumption",
      y = "kWh",
      x = "",
      color = "Legend"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_blank(),
      legend.text = element_text(size = 12),
      legend.position = "top",
      axis.text.x = element_text(angle = 30, hjust = 1)
    ) +
    scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
    scale_x_datetime(
      date_labels = "%H:%M",
      date_breaks = "2 hours"
    )
}


plot_model_results_one_day(
  test_target = september_5th_actual,
  prediction = september_5th_predictions,
  time_index = time_index
)




# Load required libraries
library(xgboost)
library(dplyr)
library(ggplot2)

# Define the top 10 features based on importance
top_10_features <- c(
  "kWh_4_rolling_median", "kWh_4_rolling_mean", "kWh_lag_1", 
  "kWh_4_rolling_max", "kWh_4_rolling_min", "airTemperature", 
  "hour_x", "hour_y", "dayOfWeek_x", "difference"
)

# Add a timestamp column assuming 15-minute intervals
start_time <- as.POSIXct("2024-09-01 09:15:00")  
test_data_scaled$timestamp <- seq(
  from = start_time,
  by = "15 min",
  length.out = nrow(test_data_scaled)
)


train_data_selected <- train_data_scaled %>% select(all_of(top_10_features))
val_data_selected <- val_data_scaled %>% select(all_of(top_10_features))
test_data_selected <- test_data_scaled %>% select(all_of(top_10_features))

# Convert data to matrix format for xgboost
train_matrix <- as.matrix(train_data_selected)
val_matrix <- as.matrix(val_data_selected)
test_matrix <- as.matrix(test_data_selected)

# Convert target variables to numeric vectors
train_target_vector <- as.numeric(unlist(train_target))
val_target_vector <- as.numeric(unlist(val_target))
test_target_vector <- as.numeric(unlist(test_target))

# Create DMatrix objects
dtrain <- xgb.DMatrix(data = train_matrix, label = train_target_vector)
dval <- xgb.DMatrix(data = val_matrix, label = val_target_vector)
dtest <- xgb.DMatrix(data = test_matrix, label = test_target_vector)

# Define XGBoost parameters
params <- list(
  booster = "gbtree",
  objective = "reg:squarederror",  # Regression task
  eta = 0.1,                      # Learning rate
  max_depth = 6,                  # Maximum depth of trees
  subsample = 0.8,                # Subsample ratio of the training instance
  colsample_bytree = 0.8          # Subsample ratio of columns for each tree
)

set.seed(42)
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 500,                   # Maximum number of boosting rounds
  watchlist = list(
    train = dtrain,
    validation = dval
  ),
  early_stopping_rounds = 100,     
  print_every_n = 10              
)

# Filter data for the entire month of October 2024
october_indices <- which(format(test_data_scaled$timestamp, "%Y-%m") == "2024-10")


if (length(october_indices) < 1) {
  stop("Not enough data for the month of October.")
}

october_features <- test_data_scaled[october_indices, top_10_features, drop = FALSE]
october_actual <- test_target_vector[october_indices]

# Predict for October
october_predictions <- predict(xgb_model, as.matrix(october_features))


october_time_index <- test_data_scaled$timestamp[october_indices]

# Plot function for a full month
plot_model_results <- function(test_target, prediction, time_index) {
  residuals <- test_target - prediction
  error_std <- sd(residuals)
  scale <- 1.96
  lower <- prediction - (error_std * scale)
  upper <- prediction + (error_std * scale)
  
  plot_data <- data.frame(
    Time = time_index,
    Actual = test_target,
    Predicted = prediction,
    Lower_CI = lower,
    Upper_CI = upper
  )
  
  ggplot(plot_data, aes(x = Time)) +
    geom_line(aes(y = Actual, color = "Actual"), size = 1) +
    geom_line(aes(y = Predicted, color = "Predicted"), size = 1) +
    labs(
      title = "Gradient Boosting Forecast vs Actual October's Consumption",
      y = "kWh",
      x = "",
      color = "Legend"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_blank(),
      legend.text = element_text(size = 12),
      legend.position = "top",
      axis.text.x = element_text(angle = 30, hjust = 1)
    ) +
    scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
    scale_x_datetime(
      date_labels = "%Y-%m-%d",
      date_breaks = "3 days"
    )
}

# Plot the results for October
plot_model_results(
  test_target = october_actual,
  prediction = october_predictions,
  time_index = october_time_index
)




















#comparison of two models

# Load required libraries
library(xgboost)
library(dplyr)

# Ensure all datasets are numeric
train_data_scaled <- train_data_scaled %>% mutate_if(is.character, as.numeric)
val_data_scaled <- val_data_scaled %>% mutate_if(is.character, as.numeric)
test_data_scaled <- test_data_scaled %>% mutate_if(is.character, as.numeric)

# Define the top 10 features based on importance
top_10_features <- c(
  "kWh_4_rolling_median", "kWh_4_rolling_mean", "kWh_lag_1", 
  "kWh_4_rolling_max", "kWh_4_rolling_min", "airTemperature", 
  "hour_x", "hour_y", "dayOfWeek_x", "difference"
)

# Add a timestamp column assuming 15-minute intervals
start_time <- as.POSIXct("2024-09-01 00:00:00")  # Replace with correct start time
test_data_scaled$timestamp <- seq(
  from = start_time,
  by = "15 min",
  length.out = nrow(test_data_scaled)
)

# Function to calculate evaluation metrics
calculate_metrics <- function(actual, predicted) {
  rmse <- sqrt(mean((actual - predicted)^2))  # Root Mean Square Error
  mse <- mean((actual - predicted)^2)        # Mean Squared Error
  mape <- mean(abs((actual - predicted) / actual)) * 100  # Mean Absolute Percentage Error
  mae <- mean(abs(actual - predicted))       # Mean Absolute Error
  return(list(RMSE = rmse, MSE = mse, MAPE = mape, MAE = mae))
}

# Model with feature selection
train_data_selected <- train_data_scaled %>% select(all_of(top_10_features))
val_data_selected <- val_data_scaled %>% select(all_of(top_10_features))
test_data_selected <- test_data_scaled %>% select(all_of(top_10_features))

train_matrix_fs <- as.matrix(train_data_selected)
val_matrix_fs <- as.matrix(val_data_selected)
test_matrix_fs <- as.matrix(test_data_selected)

train_target_vector <- as.numeric(unlist(train_target))
val_target_vector <- as.numeric(unlist(val_target))
test_target_vector <- as.numeric(unlist(test_target))

dtrain_fs <- xgb.DMatrix(data = train_matrix_fs, label = train_target_vector)
dval_fs <- xgb.DMatrix(data = val_matrix_fs, label = val_target_vector)
dtest_fs <- xgb.DMatrix(data = test_matrix_fs, label = test_target_vector)

params <- list(
  booster = "gbtree",
  objective = "reg:squarederror",  
  eta = 0.1,                      
  max_depth = 6,                  
  subsample = 0.8,                
  colsample_bytree = 0.8          
)

set.seed(42)
xgb_model_fs <- xgb.train(
  params = params,
  data = dtrain_fs,
  nrounds = 500,
  watchlist = list(train = dtrain_fs, validation = dval_fs),
  early_stopping_rounds = 100,
  print_every_n = 10
)

predictions_fs <- predict(xgb_model_fs, dtest_fs)
metrics_fs <- calculate_metrics(test_target_vector, predictions_fs)

# Model without feature selection
train_matrix_nofs <- as.matrix(select_if(train_data_scaled, is.numeric))
val_matrix_nofs <- as.matrix(select_if(val_data_scaled, is.numeric))
test_matrix_nofs <- as.matrix(select_if(test_data_scaled, is.numeric))

dtrain_nofs <- xgb.DMatrix(data = train_matrix_nofs, label = train_target_vector)
dval_nofs <- xgb.DMatrix(data = val_matrix_nofs, label = val_target_vector)
dtest_nofs <- xgb.DMatrix(data = test_matrix_nofs, label = test_target_vector)

set.seed(42)
xgb_model_nofs <- xgb.train(
  params = params,
  data = dtrain_nofs,
  nrounds = 500,
  watchlist = list(train = dtrain_nofs, validation = dval_nofs),
  early_stopping_rounds = 100,
  print_every_n = 10
)

predictions_nofs <- predict(xgb_model_nofs, dtest_nofs)
metrics_nofs <- calculate_metrics(test_target_vector, predictions_nofs)

# Print comparison of metrics
cat("Model with Feature Selection:\n")
cat("RMSE:", metrics_fs$RMSE, "\n")
cat("MSE:", metrics_fs$MSE, "\n")
cat("MAPE:", metrics_fs$MAPE, "%\n")
cat("MAE:", metrics_fs$MAE, "\n\n")

cat("Model without Feature Selection:\n")
cat("RMSE:", metrics_nofs$RMSE, "\n")
cat("MSE:", metrics_nofs$MSE, "\n")
cat("MAPE:", metrics_nofs$MAPE, "%\n")
cat("MAE:", metrics_nofs$MAE, "\n")



#testing feature selection model againt validation data
# Test the feature selection model against validation data
predictions_val_fs <- predict(xgb_model_fs, dval_fs)
metrics_val_fs <- calculate_metrics(val_target_vector, predictions_val_fs)

# Print validation metrics for the feature selection model
cat("Validation Data Metrics (Model with Feature Selection):\n")
cat("RMSE:", metrics_val_fs$RMSE, "\n")
cat("MSE:", metrics_val_fs$MSE, "\n")
cat("MAPE:", metrics_val_fs$MAPE, "%\n")
cat("MAE:", metrics_val_fs$MAE, "\n")



