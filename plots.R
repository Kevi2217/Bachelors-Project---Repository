# Create run-sequence, ACF, and PACF plots for original time series
run1 <- autoplot(esp_ts) +
  xlab("Time") +
  ylab("Daily Mean Spot Price (EUR)") +
  ggtitle("Daily Mean Spot Prices in EUR") +
  theme_options

acf1 <- autoplot(Acf(esp_ts, lag.max = 50, plot = FALSE)) +
  xlab("Lags") +
  ylab("ACF") +
  ggtitle("ACF of Daily Mean Spot Prices") +
  theme_options

pacf1 <- autoplot(Pacf(esp_ts, lag.max = 50, plot = FALSE)) +
  xlab("Lags") +
  ylab("PACF") +
  ggtitle("PACF of Daily Mean Spot Prices") +
  theme_options

# Create run-sequence, ACF, and PACF plots for differenced time series
run2 <- autoplot(esp_diff) +
  xlab("Time") +
  ylab("Differenced Mean Spot Price (EUR)") +
  ggtitle("Differenced Mean Spot Prices in EUR") +
  theme_options

acf2 <- autoplot(Acf(esp_diff, lag.max = 50, plot = FALSE)) +
  xlab("Lags") +
  ylab("ACF") +
  ggtitle("ACF of Differenced Mean Spot Prices") +
  theme_options

pacf2 <- autoplot(Pacf(esp_diff, lag.max = 50, plot = FALSE)) +
  xlab("Lags") +
  ylab("PACF") +
  ggtitle("PACF of Differenced Mean Spot Prices") +
  theme_options

# Arrange the plots using grid.arrange
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol = 3)

#######################################
##### Testing with subset of data #####
#######################################

# Create subset
esp_subset <- window(esp_ts, start = 2019, end = 2021)

# Create run-sequence plot
autoplot(esp_subset) +
  xlab("Time") +
  ylab("Daily Mean Spot Price (EUR)") +
  ggtitle("Daily Mean Spot Prices in EUR (2019-2024)") +
  theme_options

# Convert to multiple seasonality time series
esp_msts1 <- msts(esp_subset, seasonal.periods = c(31, 365))

# Decompose the time series with multiple seasonality
esp_mstl1 <- mstl(esp_msts1, iterate = 100)

# Plot the decomposed components
autoplot(esp_mstl1) +
  ggtitle("Multiple STL Decomposition of Daily Mean Spot Prices in EUR (2019-2024)") +
  labs(x = "Time", y = "Daily Mean Spot Price (EUR)") +
  theme_options