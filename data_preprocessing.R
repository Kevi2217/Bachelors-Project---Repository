library(readxl)
library(zoo)
library(dplyr)
library(forecast)
library(ggplot2)
library(tseries)
library(xts)
library(lubridate)
library(stats)
library(ggfortify)

# Specify the theme
theme_options <- theme(
  plot.title = element_text(size = 14),
  axis.title.x = element_text(size = 12),
  axis.title.y = element_text(size = 12),
  axis.text = element_text(size = 10),
  legend.title = element_blank(),
  legend.text = element_text(size = 10))

# Read the Excel file
esp_raw <- read_excel("C:/Users/aranu/Desktop/Elspotprices.xlsx")

# Preprocess the data
esp <- esp_raw %>%
  rename_all(tolower) %>%
  select(-c(hourutc,spotpricedkk,pricearea)) %>%
  mutate(hourdk = as.Date(hourdk)) %>%
  group_by(hourdk) %>%
  summarise(daily_mean_spotpriceeur = round(mean(spotpriceeur), 2))

# Convert to time series object
esp_ts <- ts(esp$daily_mean_spotpriceeur, frequency = 365, start = 2019)

# Create run-sequence plot
autoplot(esp_ts) +
  xlab("Time") +
  ylab("Daily Mean Spot Price (EUR)") +
  ggtitle("Daily Mean Spot Prices in EUR (2019-2024)") +
  theme_options

# Convert to multiple seasonality time series
esp_msts <- msts(esp_ts, seasonal.periods = c(31, 365))

# Decompose the time series with multiple seasonality
esp_mstl <- mstl(esp_msts, iterate = 50)

# Plot the decomposed components
autoplot(esp_mstl) +
  ggtitle("Multiple STL Decomposition of Daily Mean Spot Prices in EUR (2019-2024)") +
  labs(x = "Time", y = "Daily Mean Spot Price (EUR)") +
  theme_options

autoplot(acf(esp_ts, lag.max = 730))



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
