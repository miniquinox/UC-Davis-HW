# Clear the environment
rm(list=ls(all=TRUE))

# Load required libraries
library(forecast)
library(readxl)

# Load the data
data <- read_excel("2020_Covid_Data.xlsx", sheet = "Cases")
data$Date <- as.Date(data$Date, origin = "1899-12-30")

# Define the period of interest for the initial analysis
start_date <- as.Date("2020-01-22")
end_date <- as.Date("2020-12-31")

# Extract the cases for the initial period
ga_cases <- data$GA[data$Date >= start_date & data$Date <= end_date]

# Create a time series object for the initial period
ga_ts <- ts(ga_cases, start = c(2020, as.numeric(format(start_date, "%j"))), frequency = 365)

# Fit auto ARIMA model
auto_model <- auto.arima(ga_ts, trace = TRUE, max.p = 10, max.d = 10, max.q = 10)

# Forecast future values for auto ARIMA
forecast_length <- as.numeric(as.Date("2021-02-10") - as.Date("2021-01-01")) + 1
auto_forecast <- forecast(auto_model, h = forecast_length)

# Fit additional models as specified
models <- list(
  arima121 = Arima(ga_ts, order = c(1,2,1)),
  arima221 = Arima(ga_ts, order = c(2,2,1)),
  arima122 = Arima(ga_ts, order = c(1,2,2))
)

# Forecast future values for additional models
forecasts <- lapply(models, forecast, h = forecast_length)

# Calculate AIC for each model and extract forecasted means
aic_values <- sapply(models, AIC)
forecast_means <- lapply(forecasts, function(x) x$mean)

# Calculate the weights based on AIC
weights <- exp(-0.5 * (aic_values - min(aic_values)))
weights <- weights / sum(weights)  # Normalize so that weights sum to 1

# Calculate the weighted average forecast
avg_forecast <- Reduce(`+`, Map(function(forecast, weight) forecast * weight, forecast_means, weights))

# Extract real values for Jan 1st to Feb 10th, 2021
forecast_start_date <- as.Date("2021-01-01")
forecast_end_date <- as.Date("2021-02-10")
real_values <- data$GA[data$Date >= forecast_start_date & data$Date <= forecast_end_date]

# Calculate MAPE for auto.arima and combined forecasts
auto_mape <- mean(abs((auto_forecast$mean - real_values) / real_values), na.rm = TRUE) * 100
avg_mape <- mean(abs((avg_forecast - real_values) / real_values), na.rm = TRUE) * 100

# Print MAPEs
print(paste("MAPE for auto.arima:", auto_mape))
print(paste("MAPE for Combined Forecast:", avg_mape))

# Save the results to a CSV file
results <- data.frame(
  Date = seq(forecast_start_date, by = "day", length.out = forecast_length),
  Actual_Cases = real_values,
  Auto_ARIMA = round(auto_forecast$mean, 0),
  Avg_Forecast = round(avg_forecast, 0)
)
write.csv(results, "forecast_vs_actual_cases_with_combined.csv", row.names = FALSE)
