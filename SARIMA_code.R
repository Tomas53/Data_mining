library(arrow)
library(dplyr)
library(lubridate)
library(scales)
library(zoo)
library(tidyr)
library(forecast)
library(parallel)

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

# Function to add descriptive columns
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
    # Shift 'kWh' by 96 steps (assuming 15-minute intervals, 96 steps in one day)
    shifted_faktas = lag(kWh, 96)
  )
  
  # Generate rolling statistics on the shifted values
  for (offset in c(4, 8, 12, 16)) {
    prefix <- paste0("kWh_", offset, "_rolling_")
    df <- df %>% mutate(
      !!paste0(prefix, "mean") := zoo::rollmean(shifted_faktas, offset, fill = NA, align = "right"),
      !!paste0(prefix, "median") := zoo::rollmedian(shifted_faktas, offset, fill = NA, align = "right"),
      !!paste0(prefix, "min") := zoo::rollapply(shifted_faktas, offset, min, fill = NA, align = "right"),
      !!paste0(prefix, "max") := zoo::rollapply(shifted_faktas, offset, max, fill = NA, align = "right")
    )
  }
  
  # Generate lag features using 'shifted_faktas'
  for (i in 1:lag) {
    df <- df %>% mutate(!!paste0("kWh_lag_", i) := lag(shifted_faktas, i))
  }
  
  # Drop the 'shifted_faktas' column after creating lag features
  df <- df %>% select(-shifted_faktas)
  
  return(df)
}

# Read the Parquet file
merged_df <- read_parquet("/Users/studentas/Desktop/tr_visi_duomenys_updated.parquet")

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

# Add time indices to train_target, val_target, and test_target
train_target <- train_target %>%
  mutate(time = train_data$index)

val_target <- val_target %>%
  mutate(time = val_data$index)

test_target <- test_target %>%
  mutate(time = test_data$index)

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


# Count rows of each target set
cat("Rows in train_target:", nrow(train_target), "\n")
cat("Rows in val_target:", nrow(val_target), "\n")
cat("Rows in test_target:", nrow(test_target), "\n")


######sationary/diferencijavimo testai

# Load necessary library
library(urca)
install.packages("vars")
library(vars)

ts_data <- ts(train_target$kWh, frequency = 96)  # Replace 96 with your data's frequency

optimal_lags <- VARselect(ts_data, lag.max = 48, type = "const")
print(optimal_lags$selection)  # This will show optimal lags based on AIC, BIC, etc.
selected_lag <- optimal_lags$selection["AIC(n)"]  # Use the lag with the lowest AIC

# Function for ADF Test
check_stationarity_simple <- function(df, target_column = "kWh") {
  # Ensure the target column exists
  if (!(target_column %in% names(df))) {
    stop(paste("Column", target_column, "not found in the dataframe"))
  }
  
  # Convert the target column to a time series
  ts_data <- ts(df[[target_column]], frequency = 96)  # Adjust frequency if needed
  
  # Perform the ADF test
  adf_test <- ur.df(ts_data, type = "none", lags = 43)  # Default lag = 10; adjust as needed
  
  # Extract results
  adf_stat <- adf_test@teststat[1]
  critical_value <- adf_test@cval[2]  # Critical value at 5%
  
  # Print results
  print(paste("ADF Test Statistic:", adf_stat))
  print(paste("Critical Value (5%):", critical_value))
  
  if (adf_stat < critical_value) {
    print("The series is stationary.")
  } else {
    print("The series is NOT stationary.")
  }
}


check_stationarity_simple(train_target)

##############################SARIMA


library(forecast)

sarima_model <- auto.arima(
  ts(train_target$kWh, frequency = 96),  # Assuming 96 periods (15-minute intervals)
  seasonal = TRUE,
  stepwise = TRUE,     # Use stepwise search to speed up model fitting
  approximation = TRUE,  # More accurate but slower
  trace = TRUE        # Display progress
)


###########validation

# Generate forecast on the validation set using the trained model
sarima_forecast_val <- forecast(sarima_model, h = length(val_target$kWh))

# Extract the predicted values from the forecast
predicted_values <- sarima_forecast_val$mean

# Actual values from the validation target
actual_values <- val_target$kWh

# Compute RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((actual_values - predicted_values)^2))

# Compute MAE (Mean Absolute Error)
mae <- mean(abs(actual_values - predicted_values))

# Compute MAPE (Mean Absolute Percentage Error)
mape <- mean(abs((actual_values - predicted_values) / actual_values)) * 100

# Print the evaluation metrics
cat("Validation Metrics:\n")
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")
cat("MAPE:", mape, "%\n")

# Plot the forecast vs actuals for visualization
library(ggplot2)

# Ensure we align the forecast time with the actual time index
forecast_comparison <- data.frame(
  Time = as.POSIXct(val_target$time),  # Ensure it's in POSIXct format for time-based plotting
  Actual = actual_values,
  Predicted = predicted_values
)

# Plotting the actual vs predicted values with time index
ggplot(forecast_comparison, aes(x = Time)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +
  geom_line(aes(y = Predicted, color = "Predicted"), size = 1) +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  ggtitle("ARIMA Forecast vs Actuals (Validation Set)") +
  xlab("Time") +
  ylab("Energy Consumption (kWh)") +
  theme_minimal() +
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))


################test

# Step 1: Generate forecast for the test_target data using the SARIMA model
test_forecast <- forecast(sarima_model, h = nrow(test_target))

# Step 2: Extract the predicted values from the forecast
predicted_test_values <- test_forecast$mean

# Actual values from test_target
actual_test_values <- test_target$kWh

# Step 3: Compute RMSE, MAE, and MAPE for the forecast on test_target
rmse_test <- sqrt(mean((actual_test_values - predicted_test_values)^2))
mae_test <- mean(abs(actual_test_values - predicted_test_values))
mape_test <- mean(abs((actual_test_values - predicted_test_values) / actual_test_values)) * 100

# Print the evaluation metrics for the test forecast
cat("Test Forecast Evaluation Metrics:\n")
cat("RMSE (Test):", rmse_test, "\n")
cat("MAE (Test):", mae_test, "\n")
cat("MAPE (Test):", mape_test, "%\n")

# Step 4: Plot the forecasted vs actual values for test_target
test_comparison <- data.frame(
  Time = test_target$time,
  Actual = actual_test_values,
  Predicted = predicted_test_values
)

# Plot using ggplot2
library(ggplot2)
ggplot(test_comparison, aes(x = Time)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +
  geom_line(aes(y = Predicted, color = "Predicted"), size = 1) +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  ggtitle("SARIMA Forecast vs Actuals: Test Data") +
  xlab("Time") +
  ylab("Energy Consumption (kWh)") +
  theme_minimal() +
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

################################################OCTOBER
# Step 1: Extract October data from test_target (or val_target, depending on your needs)
october_data <- test_target %>% 
  filter(format(time, "%Y-%m") == "2024-10")

# Step 2: Forecast for October using the SARIMA model
october_forecast <- forecast(sarima_model, h = nrow(october_data))  # Forecast for October data

# Step 3: Prepare actual values for October
actual_october_values <- october_data$kWh

# Step 4: Prepare predicted values for October
predicted_october_values <- october_forecast$mean

# Step 5: Create a comparison data frame for October
october_comparison <- data.frame(
  Time = october_data$time,
  Actual = actual_october_values,
  Predicted = predicted_october_values
)

# Step 6: Plot forecasted vs actual values for October with 2-hour ticks on x-axis
library(ggplot2)
ggplot(october_comparison, aes(x = Time)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +
  geom_line(aes(y = Predicted, color = "Predicted"), size = 1) +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  ggtitle("SARIMA Forecast vs Actuals: October 2024") +
  xlab("Time") +
  ylab("Energy Consumption (kWh)") +
  scale_x_datetime(
    breaks = seq(min(october_data$time), max(october_data$time), by = "2 hours"),  # Set breaks to 2 hours
    labels = scales::date_format("%H:%M")  # Format labels as hour:minute
  ) +
  theme_minimal() +
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))


###########################SEPTEMBER 5

library(ggplot2)

# Filter actuals for September 5
september5_actuals <- test_target %>%
  filter(format(time, "%Y-%m-%d") == "2024-09-05") %>%
  pull(kWh)

# Generate the forecast for the same period
september5_predictions <- forecast(sarima_model, h = length(september5_actuals))$mean

# Align forecast time with actual timestamps
september5_time <- test_target %>%
  filter(format(time, "%Y-%m-%d") == "2024-09-05") %>%
  pull(time)

# Create a data frame for ggplot
september5_comparison <- data.frame(
  Time = september5_time,
  Actual = september5_actuals,
  Predicted = september5_predictions
)

# Plot using ggplot2
ggplot(september5_comparison, aes(x = Time)) +
  geom_line(aes(y = Actual, color = "Actual"), linewidth = 1) +
  geom_line(aes(y = Predicted, color = "Predicted"), linewidth = 1) +
  scale_color_manual(
    name = "",
    values = c("Actual" = "blue", "Predicted" = "red")
  ) +
  scale_x_datetime(
    date_breaks = "2 hours", 
    date_labels = "%H:%M"
  ) +
  ggtitle("SARIMA Forecast vs. Actual: 2024-09-05") +
  xlab("") +
  ylab("Energy Consumption (kWh)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )




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
      title = "SARIMA Forecast vs. Actual October's Consumption",
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
      title = "RandomForest Forecast vs. Actual one day Consumption",
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


####################################################################PLOT BENDRAS KODAS


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
      title = "SARIMA Forecast vs. Actual October's Consumption",
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
      title = "SARIMA Forecast vs. Actual one day Consumption",
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

plot_model_results_one_day(test_target$kWh[156:252], predicted_test_values[156:252], test_target$time[156:252])
plot_model_results(test_target$kWh[2652:5628], predicted_test_values[2652:5628], test_target$time[2652:5628])




