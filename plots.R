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