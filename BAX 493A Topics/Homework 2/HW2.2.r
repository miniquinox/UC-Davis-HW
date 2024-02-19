# Clear the environment
rm(list=ls(all=TRUE))

# Load required libraries
library(forecast)
library(readxl)

# Load the data
data <- read_excel("2020_Covid_Data.xlsx", sheet = "Cases")
data$Date <- as.Date(data$Date, origin = "1899-12-30")

# Define the period of interest
start_date <- as.Date("2020-02-29")
end_date <- as.Date("2020-12-31")

# Extract the cases for the period
ga_cases <- data$AL[data$Date >= start_date & data$Date <= end_date]
# cat(ga_cases)

# Create a time series object
ga_ts <- ts(ga_cases, start = c(2020, as.numeric(format(start_date, "%j"))), frequency = 365)

# Fit auto ARIMA model
auto_model <- auto.arima(ga_ts, trace = TRUE)

models <- list(
  arima121 = Arima(ga_ts, order = c(1,2,1)), # Best model from auto.arima
  arima221 = Arima(ga_ts, order = c(2,2,1)),
  arima022 = Arima(ga_ts, order = c(0,2,2)),
  arima122 = Arima(ga_ts, order = c(1,2,2))
)

# Calculate AIC for each model
aic_values <- sapply(models, AIC)

# Sort models by AIC
sorted_models <- sort(aic_values)

# Print the sorted AIC values to identify the best models
print(sorted_models)

# Forecast future values
forecast_length <- as.numeric(as.Date("2021-02-10") - as.Date("2021-01-01")) + 1
auto_forecast <- forecast(auto_model, h = forecast_length)

# Real values for comparison
real_values <- c(677589, 685122, 690900, 696063, 706154, 713840, 723630, 736926, 748852, 757045,
                 764499, 773692, 782288, 791324, 801130, 809663, 815995, 820952, 828444, 836649,
                 844799, 853173, 862158, 866911, 870828, 879221, 885605, 892957, 899515, 905858,
                 909445, 912479, 917440, 922364, 928070, 932912, 937402, 940991, 943695, 947416, 950906)

# Prepare the forecast and actual values for export
results <- data.frame(
  Date = seq(as.Date("2021-01-01"), by = "day", length.out = forecast_length),
  Actual_Cases = real_values,
  Auto_ARIMA = round(auto_forecast$mean, 0)
)

# Calculate MAPE for each model
auto_mape <- mean(abs((auto_forecast$mean - real_values) / real_values), na.rm = TRUE) * 100

# Print MAPEs
print(paste("MAPE for auto.arima:", auto_mape))

# Save the results to a CSV file
write.csv(results, "forecast_vs_actual_cases.csv", row.names = FALSE)

