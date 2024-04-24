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
library(gridExtra)
library(MASS)

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

# Convert to multiple seasonality time series
esp_msts <- msts(esp_ts, seasonal.periods = c(7, 365))

# Decompose the time series with multiple seasonality
esp_mstl <- mstl(esp_msts, iterate = 50)

# Specify the theme
theme_options <- theme(
  plot.title = element_text(size = 14),
  axis.title.x = element_text(size = 12),
  axis.title.y = element_text(size = 12),
  axis.text = element_text(size = 10),
  legend.title = element_blank(),
  legend.text = element_text(size = 10))

# Plot the decomposed components
autoplot(esp_mstl) +
  ggtitle("Multiple STL Decomposition of Daily Mean Spot Prices in EUR (2019-2024)") +
  labs(x = "Time", y = "Daily Mean Spot Price (EUR)") +
  theme_options

# Difference the time series
esp_diff <- diff(esp_ts)

# Log-transform and difference the time series
esp_logdiff <- log(esp_ts + abs(5*min(esp_ts))) %>% diff() %>% diff(lag = 7)
