# Specify the theme
theme_options <- theme(
  plot.title = element_text(size = 11),
  axis.title.x = element_text(size = 9),
  axis.title.y = element_text(size = 9),
  axis.text = element_text(size = 7),
  legend.title = element_blank(),
  legend.text = element_text(size = 7))

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

# Create run-sequence, ACF, and PACF plots for log-differenced time series
run3 <- autoplot(esp_logdiff) +
  xlab("Time") +
  ylab("Log-Differenced Mean Spot Price (EUR)") +
  ggtitle("Log-Differenced Mean Spot Prices in EUR") +
  theme_options

acf3 <- autoplot(Acf(esp_logdiff, lag.max = 50, plot = FALSE)) +
  xlab("Lags") +
  ylab("ACF") +
  ggtitle("ACF of Log-Differenced Mean Spot Prices") +
  theme_options

pacf3 <- autoplot(Pacf(esp_logdiff, lag.max = 50, plot = FALSE)) +
  xlab("Lags") +
  ylab("PACF") +
  ggtitle("PACF of Log-Differenced Mean Spot Prices") +
  theme_options

# Arrange the plots using grid.arrange
grid.arrange(run1, acf1, pacf1, run2, acf2, pacf2, run3, acf3, pacf3, ncol = 3)

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

autoplot(esp_logdiff) +
  xlab("Time") +
  ylab("Differenced Mean Spot Price (EUR)") +
  ggtitle("Differenced Mean Spot Prices in EUR") +
  theme_options