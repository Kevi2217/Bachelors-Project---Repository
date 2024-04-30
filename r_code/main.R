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
library(gridExtra)
library(pracma)

# OR Operator
# |

# Henter data fra 2022 og 2023
elspotprices_14_19_data <- read_excel("elspotprices_14-19.xlsx")
elspotprices_19_24_data <- read_excel("elspotprices_19-24.xlsx")

# Theme definition
theme_options <- theme(
  plot.title = element_text(size = 14),
  axis.title.x = element_text(size = 12),
  axis.title.y = element_text(size = 12),
  axis.text = element_text(size = 10),
  legend.title = element_blank(),
  legend.text = element_text(size = 10))

# Combines data
esp <- elspotprices_14_19_data %>% 
  rbind(elspotprices_19_24_data) %>%
  rename_all(tolower)

# source("seasonal_plots.R")



esp_daily <- esp %>%
  dplyr::mutate(hourdk = as.Date(hourdk)) %>%
  dplyr::filter(hourdk >= "2019-01-01" & hourdk <= "2023-12-31") %>% 
  dplyr::filter(pricearea == "DK1") %>% 
  dplyr::group_by(hourdk) %>%
  dplyr::summarise(daily_mean_spotpriceeur = round(mean(spotpriceeur), 2)) %>% 
  dplyr::mutate(rolling_mean = cummean(daily_mean_spotpriceeur),
                rolling_sd = sqrt(cumsum((daily_mean_spotpriceeur - rolling_mean)^2) /
                                    seq_along((daily_mean_spotpriceeur - rolling_mean)^2)))


# Daily price plot for all time
ggplot(esp_daily, aes(x = hourdk)) +
  geom_line(aes(y = daily_mean_spotpriceeur, color = "Daily prices"), size = 0.3) +
  geom_line(aes(y = rolling_mean, color = "Rolling mean"), size = 0.3) +
  geom_line(aes(y = rolling_sd, color = "Rolling sd"), size = 0.3) +
  scale_color_manual(values = c("Daily prices" = "black",
                                "Rolling mean" = "red",
                                "Rolling sd" = "blue")) +
  labs(title = "Average daily Price (2019-2024)", x = "Time", y = "Average Daily Price")




esp_30day_rolling_avg_daily <- esp %>%
  dplyr::mutate(hourdk = as.Date(hourdk)) %>%
  dplyr::group_by(pricearea, hourdk) %>%
  dplyr::summarise(daily_mean_spotpricedkk = round(mean(spotpricedkk), 2),
                   daily_mean_spotpriceeur = round(mean(spotpriceeur), 2)) %>% 
  dplyr::filter(pricearea == "DK1") %>% 
  dplyr::mutate(MA_30 = zoo::rollmeanr(daily_mean_spotpriceeur, k = 30, fill = NA)) 
  # dplyr::filter(hourdk >= "2020-01-01")

# 30 day-rolling Daily price plot for all time
ggplot(esp_30day_rolling_avg_daily, aes(x = hourdk,
                      y = MA_30)) +
  geom_line() +
  labs(title = "DAILY Price 30 MA (ALL TIME)", x = "Time", y = "Daily Price") +
  theme_minimal()

# Converts dataframe to time series
esp_ts <- ts(esp_daily$daily_mean_spotpriceeur, frequency = 7,
             start = 2019)
autoplot(esp_ts)

############################# STL of time series ###############################
esp_mstl2 <- mstl(msts(esp_ts, seasonal.periods = c(7, 365)), iterate = 50)
theme_options <- theme(
  plot.title = element_text(size = 14),
  axis.title.x = element_text(size = 12),
  axis.title.y = element_text(size = 12),
  axis.text = element_text(size = 10),
  legend.title = element_blank(),
  legend.text = element_text(size = 10))
# Plot the decomposed components
autoplot(esp_mstl2) +
  ggtitle("(normal) Multiple STL Decomposition of Daily Mean Spot Prices (2019-2024)") +
  labs(x = "Time", y = "Daily Mean Spot Price (EUR)") +
  theme_options
################################################################################
# Transforming data 
# log_diff_ts <- log(esp_ts + abs(1.5*min(esp_ts))) %>% diff() %>% diff(lag = 7)
# autoplot(log_diff_ts)

lambda <- BoxCox.lambda(esp_ts + abs(1.5*min(esp_ts)))
esp_boxcox <- BoxCox(esp_ts + abs(1.5*min(esp_ts)), lambda) %>% diff() %>% diff(lag = 7)

for (i in which(abs(esp_boxcox) >= 0.05)) {
  # Assign the value of the previous observation to the current index
  esp_boxcox[i] <- esp_boxcox[i - 1]
}
autoplot(esp_boxcox)


############################# STL of transformed time series ###############################
esp_mstl2 <- mstl(msts(esp_boxcox, seasonal.periods = c(7, 365)), iterate = 50)
theme_options <- theme(
  plot.title = element_text(size = 14),
  axis.title.x = element_text(size = 12),
  axis.title.y = element_text(size = 12),
  axis.text = element_text(size = 10),
  legend.title = element_blank(),
  legend.text = element_text(size = 10))
# Plot the decomposed components
autoplot(esp_mstl2) +
  ggtitle("(BC transformed) Multiple STL Decomposition of Daily Mean Spot Prices (2019-2024)") +
  labs(x = "Time", y = "Daily Mean Spot Price (EUR)") +
  theme_options
################################################################################


# Augmented Dicket Fuller Test
adf.test(esp_ts)

adf.test(esp_boxcox)


# ACF plot
ggAcf(esp_ts, main = "ACF Plot")

# PACF plot
ggpPacf(esp_ts, main = "PACF Plot")

# ACF plot
ggAcf(esp_boxcox, main = "ACF Plot")

# PACF plot
ggPacf(esp_boxcox, main = "PACF Plot")










# Fit ARIMA model
# CREATES arima model
# arima_model <- arima(esp_ts, order = c(2, 1, 2))
# 
# ts.plot(esp_ts, main = "Daily 2015 model test")
# arima_fit <- esp_ts - residuals(arima_model)
# points(arima_fit, type = "l", col = "red", lty = 2)

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

