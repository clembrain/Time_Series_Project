install.packages("readxl")

# Install required libraries if not already installed
install.packages(c("readxl", "dplyr", "ggplot2", "forecast", "tseries"))

# Load the libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(forecast)
library(tseries)
library(TTR)

# Load the dataset
data <- read.csv(file = 'A.csv')

# View the structure and summary of the dataset
str(data)
summary(data)

# Check for missing values
sum(is.na(data))

# Subset to include only 'Date' and 'Open' columns
data_subset <- data %>%
  select(Date, Low)

head(data_subset)


# Ensure 'Date' is in Date format
data_subset$Date <- as.Date(data_subset$Date)

# Verify the structure
str(data_subset)
#summary of the dataset
summary(data_subset)


#We can see from this time series
#that there seems to be seasonal variation

# Create a time series object for Open prices
Low_ts <- ts(data_subset$Low, start = c(1999, 11), frequency = 252)  # Adjust start and frequency as needed

# Check the time series
Low_ts


# Plot the time series
autoplot(Low_ts) +
  ggtitle("Time Series of Open Prices") +
  xlab("Time") +
  ylab("Low Price")


# Calculate Simple Moving Average (SMA) with a window of 5
data_subset$SMA_150 <- TTR::SMA(data_subset$Low, n = 150)
plot.ts(Low_ts)

# Filter out rows with NA in the SMA_150 column
data_subset_filtered <- data_subset %>% filter(!is.na(SMA_150))

# Plot original data and SMA
ggplot(data_subset_filtered, aes(x = Date)) +
  geom_line(aes(y = Low, group = 1), color = "blue", alpha = 0.6) +  # Add group aesthetic
  geom_line(aes(y = SMA_150, group = 1), color = "red") +             # Add group aesthetic
  ggtitle("Simple Moving Average (SMA) - Window 5") +
  xlab("Date") +
  ylab("Low Price") +
  theme_minimal()

# ------------------------------------------------------
#-  Holt-Winters Exponential Smoothing and Forecasting
# -----------------------------------------------------

# Using Holt-Winters exponential smoothing
low_ts_smoothing <- HoltWinters(Low_ts)
low_ts_smoothing


# Checking the Sum of Squared Errors (SSE)
low_ts_smoothing$SSE

# Plotting the fitted values
plot(low_ts_smoothing)

HoltWinters(Low_ts, beta=FALSE, gamma=FALSE, l.start= 28.61)


# Forecasting the Low prices for the next 30 days
future_low_ts_smoothing <- forecast(low_ts_smoothing, h = 90)
future_low_ts_smoothing
plot(future_low_ts_smoothing, main = "Low Prices using Holt-Winters Forecast")

# Accuracy of the forecast
accuracy(future_low_ts_smoothing)

# Checking residuals randomness
acf(future_low_ts_smoothing$residuals, lag.max = 20, na.action = na.pass, main = "ACF of Residuals")

Box.test(future_low_ts_smoothing$residuals, lag = 20, type = "Ljung-Box")

# Plot residuals
plot.ts(future_low_ts_smoothing$residuals)

# Remove NA values from residuals
future_low_ts_smoothing$residuals <- future_low_ts_smoothing$residuals[!is.na
                                    (future_low_ts_smoothing$residuals)]
plot(future_low_ts_smoothing$residuals)

# Histogram of Forecast Errors
plotForecastErrors <- function(forecasterrors) {
  mybinsize <- IQR(forecasterrors) / 4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd * 5
  mymax <- max(forecasterrors) + mysd * 3
  mynorm <- rnorm(10000, mean = 0, sd = mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col = "red", freq = FALSE, breaks = mybins, 
       main = "Histogram of Forecast Errors", xlab = "Forecast Errors")
  myhist <- hist(mynorm, plot = FALSE, breaks = mybins)
  points(myhist$mids, myhist$density, type = "l", col = "blue", lwd = 2)
}
plotForecastErrors(future_low_ts_smoothing$residuals)




# --------------------------------------
  # ARIMA AND AUTO ARIMA
# ------------------------------------------

# Step 1: Testing for Stationarity
# Perform ADF test on the original series
adf_test_low_ts <- adf.test(Low_ts)
print(adf_test_low_ts)

# If p-value > 0.05, the series is non-stationary, proceed to differencing
diff_low_ts <- diff(Low_ts)
plot.ts(diff_low_ts, main = "Differenced Time Series of Low Prices")

# Perform ADF test on the differenced series
adf_test_diff_low_ts <- adf.test(diff_low_ts)
print(adf_test_diff_low_ts)

# Step 2: Plot ACF and PACF for the Differenced Series
acf(diff_low_ts, lag.max = 20, main = "ACF of Differenced Series")
pacf(diff_low_ts, lag.max = 20, main = "PACF of Differenced Series")

# Step 3: Auto ARIMA
auto_arima_low_ts <- auto.arima(Low_ts)
summary(auto_arima_low_ts)

# Step 4: Manually Fit an ARIMA Model
# From ACF, PACF, or auto.arima results, decide (p, d, q)
low_ts_arima <- arima(Low_ts, order = c(0, 1, 1))  # Example: ARIMA(0,1,1)
summary(low_ts_arima)

# Step 5: Forecasting
# Forecast for the next 30 periods
forecast_low_ts_arima <- forecast(low_ts_arima, h = 90)
plot(forecast_low_ts_arima, main = "ARIMA FORECAST")


# Step 6: Evaluate Accuracy
accuracy(forecast_low_ts_arima)

# Step 7: Residual Analysis for ARIMA
# Check ACF of residuals to confirm randomness
acf(forecast_low_ts_arima$residuals, lag.max = 20, main = "ACF of ARIMA Residuals")

# Perform Ljung-Box Test on residuals
Box.test(forecast_low_ts_arima$residuals, lag = 20, type = "Ljung-Box")

# Plot residuals
plot.ts(forecast_low_ts_arima$residuals, main = "Residuals from ARIMA Forecast")

# Step 8: Histogram of Forecast Errors
plotForecastErrors <- function(forecasterrors) {
  mybinsize <- IQR(forecasterrors) / 4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd * 5
  mymax <- max(forecasterrors) + mysd * 3
  mynorm <- rnorm(10000, mean = 0, sd = mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col = "red", freq = FALSE, breaks = mybins, 
       main = "Histogram of Forecast Errors", xlab = "Forecast Errors")
  myhist <- hist(mynorm, plot = FALSE, breaks = mybins)
  points(myhist$mids, myhist$density, type = "l", col = "blue", lwd = 2)
}
plotForecastErrors(forecast_low_ts_arima$residuals)

# Step 9: Adjusting the ARIMA Model
low_ts_arima2 <- arima(Low_ts, order = c(1, 1, 1))  # Example: ARIMA(1,1,1)
summary(low_ts_arima2)

# Forecasting with the adjusted model
forecast_low_ts_arima2 <- forecast(low_ts_arima2, h = 90)
plot(forecast_low_ts_arima2, main = "ARIMA Forecast for Low Prices (Adjusted Model)")

# Check accuracy of the adjusted model
accuracy(forecast_low_ts_arima2)

# ACF of residuals for the adjusted model
acf(forecast_low_ts_arima2$residuals, lag.max = 20, main = "ACF of Adjusted ARIMA Residuals")

# Ljung-Box Test for adjusted model residuals
Box.test(forecast_low_ts_arima2$residuals, lag = 20, type = "Ljung-Box")





# ----------------------------------------------
  # SARIMA
# ----------------------------------------------

# SARIMA Model: Auto ARIMA with Seasonal Adjustment
sarima_low_ts <- auto.arima(Low_ts, seasonal = TRUE)
summary(sarima_low_ts)

# Making Predictions
forecast_sarima_low_ts <- forecast(sarima_low_ts, h = 30)
print(forecast_sarima_low_ts)

# Plotting the SARIMA forecast
plot(forecast_sarima_low_ts, main = "SARIMA Forecast for Low Prices")

# Checking the accuracy metrics
accuracy(forecast_sarima_low_ts)

# ACF residual plot for the SARIMA forecast
acf(forecast_sarima_low_ts$residuals, lag.max = 20, main = "ACF of SARIMA Residuals")

# Box-Ljung Test on Residuals
Box.test(forecast_sarima_low_ts$residuals, lag = 20, type = "Ljung-Box")

# ---------------------------------
# ---------------------------------
# ---------------------------------
