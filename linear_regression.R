library(arrow)
library(dplyr)
library(lubridate)
library(scales)
library(zoo)
library(tidyr)
library(caret)

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
  df <- df %>% mutate(                    # Shift 'kWh' by 96 step (before was 1)
    shifted_faktas = lag(kWh, 96),         # Difference between shifted values
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




####### Linear Regression Model

initial_linear_model <- lm(kWh ~ ., data = cbind(train_target, train_data_scaled))
# summary(initial_linear_model)
val_predictions <- predict(initial_linear_model, newdata = val_data_scaled)
# test_predictions <- predict(initial_linear_model, newdata = test_data_scaled)



# Function to calculate evaluation metrics
library(Metrics)
evaluate_model <- function(true_values, predictions) {
  mae <- mae(true_values, predictions)  # Mean Absolute Error
  rmse <- rmse(true_values, predictions)  # Root Mean Square Error
  mape <- mape(true_values, predictions)*100  # Mean Absolute Percentage Error
  
  return(list( MAE = mae, RMSE = rmse, MAPE = mape))
}

# Evaluate on validation set
val_metrics <- evaluate_model(val_target$kWh, val_predictions)
print(val_metrics)

# Evaluate on test set
# test_metrics <- evaluate_model(test_target$kWh, test_predictions)
# print(test_metrics)



########## Hyperparameter Tuning - LASSO

library(glmnet)

# Data prep for glmnet
x_train <- as.matrix(train_data_scaled)  # Convert to matrix format for glmnet
y_train <- train_target$kWh  # Target variable

x_test <- as.matrix(test_data_scaled) 
y_test <- test_target$kWh

x_val <- as.matrix(val_data_scaled)
y_val <- val_target$kWh

lasso_model <- glmnet(x_train, y_train, alpha = 1) # alpha = 1 for Lasso (L1 regularization)

# Plot the Lasso path to see how coefficients shrink with increasing lambda (regularization strength)
# plot(lasso_model, xvar = "lambda", label = TRUE)

# Choose the best lambda using cross-validation (CV)
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1)

# Best lambda from CV
best_lambda <- cv_lasso$lambda.min
# best_lambda  # 0.005221571

# Plot the cross-validation curve to find the optimal lambda
# plot(cv_lasso)

# coef(cv_lasso, s = "lambda.min")

# Fit the Lasso model with the best lambda
best_lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = best_lambda)

# View the coefficients of the best Lasso model
coef(best_lasso_model)


# Predict on validation data
val_predictions <- predict(best_lasso_model, s = best_lambda, newx = x_val)

# Evaluate on validation data
val_metrics <- evaluate_model(y_val, val_predictions)
print(val_metrics)


# Predict on test data
test_predictions <- predict(best_lasso_model, s = best_lambda, newx = x_test)

# Evaluate on test data
test_metrics <- evaluate_model(y_test, test_predictions)
print(test_metrics)



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
      title = "LinearRegression Forecast vs. Actual October's Consumption",
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
  # if (plot_intervals) {
  #   p <- p +
  #     geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), fill = "grey80", alpha = 0.4)
  # }
  
  # Highlight anomalies
  # if (plot_anomalies) {
  #   anomalies <- which(test_target < lower | test_target > upper)
  #   anomaly_data <- data.frame(Time = time_index[anomalies], Value = test_target[anomalies])
  #   p <- p +
  #     geom_point(data = anomaly_data, aes(x = Time, y = Value), color = "purple", size = 3)
  # }
  
  # Apply log scale if needed
  if (log_scale) {
    p <- p + scale_y_log10()
  }
  
  # Format x-axis with specific date steps and rotated ticks
  p <- p + 
    scale_x_datetime(
      date_labels = "%Y-%m-%d",      # Format for date labels
      date_breaks = "7 days"         # Interval between ticks
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
      title = "LinearRegression Forecast vs. Actual one day Consumption",
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


start_time <- as.POSIXct("2024-09-03 09:15:00", tz = "UTC")
end_time <- as.POSIXct("2024-11-11 22:00:00", tz = "UTC")
time_index <- seq(from = start_time, to = end_time, by = "15 mins")


october_start <- as.POSIXct("2024-10-01 00:00:00", tz = "UTC")
october_end <- as.POSIXct("2024-10-31 23:59:59", tz = "UTC")
october_indices <- which(time_index >= october_start & time_index <= october_end)
october_test_target <- test_target$kWh[october_indices]
october_test_predictions <- test_predictions[october_indices, 1]  # Extract the first column if needed
october_time_index <- time_index[october_indices]

# Plot October data
plot_model_results(october_test_target, october_test_predictions, october_time_index)


# Call the function
# plot_model_results(test_target$kWh, test_predictions, time_index)
# time_index <- as.POSIXct(time_index[1:96], tz = "UTC")


plot_model_results_one_day(test_target$kWh[60:156], test_predictions[60:156], time_index[60:156])
