library(dplyr)
library(scales)
library(tidyr)
library(lubridate)
library(tseries)
library(MASS)
library(forecast)
library(ggplot2)
library(gridExtra)
library(LSTS)

# Training data
elspotprices_19_24 <- elspotprices_19_24 %>% 
  rename_all(tolower)

esp <- elspotprices_19_24 %>%
  dplyr::mutate(hourdk = as.Date(hourdk)) %>%
  dplyr::group_by(pricearea, hourdk) %>%
  dplyr::summarise(DAP_DKK = round(mean(spotpricedkk), 2),
                   DAP_EUR = round(mean(spotpriceeur), 2)) %>% 
  dplyr::filter(pricearea == "DK1")


# Format the newer data points.
elspotprices_2024_to_may <- elspotprices_2024_to_may %>% 
  rename_all(tolower)

elspot_new <- elspotprices_2024_to_may %>% 
  dplyr::mutate(spotpricedkk = as.numeric(gsub(",",".",spotpricedkk, fixed = TRUE))) %>% 
  dplyr::mutate(spotpriceeur = as.numeric(gsub(",",".",spotpriceeur, fixed = TRUE))) %>% 
  dplyr::mutate(hourdk = as.Date(hourdk))

elspot_new <- elspot_new %>% 
  dplyr::group_by(pricearea, hourdk) %>%
  dplyr::summarise(DAP_DKK = round(mean(spotpricedkk), 2),
                   DAP_EUR = round(mean(spotpriceeur), 2)) %>% 
  dplyr::filter(pricearea == "DK1")


# Convert into time series object
espnew_ts <- ts(elspot_new$DAP_EUR, start = end(esp_ts), frequency = 365)
esp_ts <- ts(esp$DAP_EUR, start = 2019, frequency = 365)


# Box Cox-transform and difference the time series
lambda <- BoxCox.lambda(esp_ts + abs(1.5*min(esp_ts)))
esp_boxcox_diff <- BoxCox(esp_ts + abs(1.5*min(esp_ts)), lambda) %>% diff() %>% diff(lag = 7)

esp_only <- BoxCox(esp_ts + abs(1.5*min(esp_ts)), lambda)
# Changing the 'outliers'
for (i in seq_along(esp_only)) {
  if (esp_only[i] < 0.985) {
    esp_only[i] <- mean(esp_only[max(1, i - 6):i])
  }
}
