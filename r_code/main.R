library(dplyr)
library(tidyr)
library(lubridate)
library(openxlsx)
library(ggplot2)
library(leaflet)
library(ggmap)
library(mapview)
library(sp)
library(mapdeck)
library(RColorBrewer)
library(ggcorrplot)
library(MASS)
library(stringr)
library(car)
library(olsrr)
library(stabledist)
library(readxl)
library(stats)
library(forecast)
library(zoo)
library(tseries)

# OR Operator
# |

# Henter data fra 2022 og 2023
elspotprices_14_19_data <- read_excel("elspotprices_14-19.xlsx")
elspotprices_19_24_data <- read_excel("elspotprices_19-24.xlsx")
# Sammensætter dataframes så vi har én for begge år
# esp_data <- esp_22_data %>%
#   rbind(esp_23_data)


# Combines data
esp <- elspotprices_14_19_data %>% 
  rbind(elspotprices_19_24_data) %>%
  rename_all(tolower)

esp <- elspotprices_14_19 %>% 
  rbind(elspotprices_19_24) %>%
  rename_all(tolower)

# source("seasonal_plots.R")

esp <- elspotprices_19_24 %>%
  rename_all(tolower)

esp_daily <- esp %>%
  dplyr::mutate(hourdk = as.Date(hourdk)) %>%
  dplyr::group_by(pricearea, hourdk) %>%
  dplyr::summarise(daily_mean_spotpricedkk = round(mean(spotpricedkk), 2),
                   daily_mean_spotpriceeur = round(mean(spotpriceeur), 2)) %>% 
  dplyr::filter(pricearea == "DK1")
  #dplyr::filter(year(hourdk) == 2020 & 2021 & 2022 & 2023 & 2024)


# Daily price plot for all time
ggplot(esp_daily, aes(x = hourdk,
                       y = daily_mean_spotpriceeur)) +
  geom_line() +
  labs(title = "DAILY Price (ALL TIME)", x = "Time", y = "Daily Price") +
  theme_minimal()

esp_30day_rolling_avg_daily <- esp %>%
  dplyr::mutate(hourdk = as.Date(hourdk)) %>%
  dplyr::group_by(pricearea, hourdk) %>%
  dplyr::summarise(daily_mean_spotpricedkk = round(mean(spotpricedkk), 2),
                   daily_mean_spotpriceeur = round(mean(spotpriceeur), 2)) %>% 
  dplyr::filter(pricearea == "DK1") %>% 
  dplyr::mutate(MA_30 = zoo::rollmeanr(daily_mean_spotpriceeur, k = 30, fill = NA))

# 30 day-rolling Daily price plot for all time
ggplot(esp_30day_rolling_avg_daily, aes(x = hourdk,
                      y = MA_30)) +
  geom_line() +
  labs(title = "DAILY Price 30 MA (ALL TIME)", x = "Time", y = "Daily Price") +
  theme_minimal()

# Converts dataframe to time series
esp_ts <- ts(esp_daily$daily_mean_spotpriceeur, frequency = 1,
             start = c(year(esp_daily$hourdk[1]), month(esp_daily$hourdk[1])))
plot(esp_ts)

price_diff_ts <- ts(diff(esp_daily$daily_mean_spotpriceeur), frequency = 1,
             start = c(year(esp_daily$hourdk[1]), month(esp_daily$hourdk[1])))
plot(price_diff_ts)


# Augmented Dicket Fuller Test
adf.test(esp_ts)

# Jarque Bera Test
jarque.bera.test(esp_ts)

# ACF plot
ggAcf(esp_ts, main = "ACF Plot")

# PACF plot
ggPacf(esp_ts, main = "PACF Plot")




# Fit ARIMA model
# CREATES arima model
arima_model <- arima(esp_ts, order = c(2, 1, 2))

ts.plot(esp_ts, main = "Daily 2015 model test")
arima_fit <- esp_ts - residuals(arima_model)
points(arima_fit, type = "l", col = "red", lty = 2)


# Creates AR model
# par(mfrow=c(2,1))
# plot(arima.sim(list(order=c(1,0,0), ar=.9), n=100), ylab="",
#        main=(expression(AR(1)~~~phi==+.9)))
# plot(arima.sim(list(order=c(1,0,0), ar=-.9), n=100), ylab="",
#        main=(expression(AR(1)~~~phi==-.9)))

# Creates MA model
# par(mfrow = c(2,1))
# plot(arima.sim(list(order=c(0,0,1), ma=.5), n=100), ylab = "",
#        main=(expression(MA(1)~~~theta==+.5)))
# plot(arima.sim(list(order=c(0,0,1), ma=-.5), n=100), ylab = "",
#        main=(expression(MA(1)~~~theta==-.5)))

