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
library(solitude)
library(astsa)
library(rugarch)

# OR Operator
# |

# Henter data fra 2022 og 2023
elspotprices_14_19_data <- read_excel("elspotprices_14-19.xlsx")
elspotprices_19_24_data <- read_excel("elspotprices_19-24.xlsx")
# The below is for later when testing models
elspotprices_24_test_data <- read_excel("elspotprices_2024.xlsx")

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

esp_test <- elspotprices_24_test_data %>%
  rename_all(tolower) %>% 
  dplyr::mutate(hourdk = as.Date(hourdk)) %>%
  # Already filtered for at the website, so its a sanity check
  dplyr::filter(pricearea == "DK1") %>% 
  dplyr::group_by(hourdk) %>%
  dplyr::summarise(daily_mean_spotpriceeur = round(mean(spotpriceeur), 2))

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
esp_ts <- ts(esp_daily$daily_mean_spotpriceeur, frequency = 365,
             start = 2019)
autoplot(esp_ts)

#Also define our esp_test_ts which will be used in forecasting
esp_test_ts <- ts(esp_test$daily_mean_spotpriceeur, frequency = 365,
                  start = 2024)

############################# STL of time series ###############################
esp_mstl2 <- mstl(msts(esp_ts, seasonal.periods = c(7, 365)), iterate = 50)

# Plot the decomposed components
autoplot(esp_mstl2) +
  ggtitle("(normal) Multiple STL Decomposition of Daily Mean Spot Prices (2019-2024)") +
  labs(x = "Time", y = "Daily Mean Spot Price (EUR)") +
  theme_options
################################################################################

# Using boxcox transformation
lambda <- BoxCox.lambda(esp_ts + abs(1.5*min(esp_ts)))
esp_boxcox <- BoxCox(esp_ts + abs(1.5*min(esp_ts)), lambda)
autoplot(esp_boxcox)

esp_test_boxcox <- BoxCox(esp_test_ts + abs(1.5*min(esp_ts)), lambda)
autoplot(esp_test_boxcox)


##################### OUTLIER REMOVAL USING ISOLATIONFOREST #################### 
bc_df <- as.data.frame(esp_boxcox)
iforest<- isolationForest$new()
iforest$fit(bc_df)

#predict outliers within dataset
bc_df$pred <- iforest$predict(bc_df)
bc_df$outlier <- as.factor(ifelse(bc_df$pred$anomaly_score >=0.78, "outlier", "normal"))
 
for (i in which(bc_df$pred$anomaly_score >= 0.78)) {
  # Assign the value of the previous 7 observations
  esp_boxcox[i] <- mean(esp_boxcox[(i - 6):i])
  bc_df$pred$anomaly_score[i] <- 0
}
autoplot(esp_boxcox)
################################################################################

###################### STL of transformed time series ##########################
esp_mstl2 <- mstl(msts(esp_boxcox, seasonal.periods = c(7, 365)), iterate = 50)

# Plot the decomposed components
autoplot(esp_mstl2) +
  ggtitle("(BC transformed) Multiple STL Decomposition of Daily Mean Spot Prices (2019-2024)") +
  labs(x = "Time", y = "Daily Mean Spot Price (EUR)") +
  theme_options
################################################################################
# ACF plot
ggAcf(esp_ts, main = "ACF Plot")
# PACF plot
ggPacf(esp_ts, main = "PACF Plot")

# ACF plot
ggAcf(esp_boxcox, main = "ACF Plot", lag.max = 50)
# PACF plot
ggPacf(esp_boxcox, main = "PACF Plot", lag.max = 50)

##############################################################  GARCH  ########################################################
# seasonal arima = 8, 1, 0
archTest(esp_boxcox)

sarima_model <- stats::arima(esp_boxcox, order = c(1,1,3), seasonal = list(order = c(8,1,0), periods = 7))
sarima_residuals <- sarima_model$residuals
############################# GARCH ORDER ESTIMATION ###########################
results <- data.frame(r = integer(),
                      s = integer(),
                      AIC_value = numeric())

for (r in 1:5) {
  for (s in 1:5) {
# Step 3: Fit GARCH Model
garch_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(r, s)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE)
)
garch_fit <- ugarchfit(spec = garch_spec,
                       data = sarima_residuals)

AIC_value <- -2 * garch_fit@fit$LLH + 2 * length(coef(garch_fit))

results <- rbind(results, list(r = r, s = s, AIC_value = AIC_value))
  }
}
################################################################################
garch_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 2)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE)
)
garch_fit <- ugarchfit(spec = garch_spec,
                       data = sarima_residuals)


sarima_garch_model <- list(sarima_model1 = sarima_model, garch_model1 = garch_fit)


# SARIMA forecast
sarima_forecasts <- forecast(sarima_garch_model$sarima_model1, h = length(esp_test_boxcox))

# GARCH forecast
garch_forecasts <- ugarchforecast(sarima_garch_model$garch_model1, n.ahead = length(esp_test_boxcox))

# LONG FORECAST ALL DAYS
merged_boxcox_ts <- ts(c(esp_boxcox, esp_test_boxcox), start = 2019, frequency = 365)
merged_ts <- ts(c(esp_ts, esp_test_ts), start = 2019, frequency = 365)

autoplot(merged_boxcox_ts) +
  autolayer(sarima_forecasts$mean) +
  geom_ribbon(data = esp_test_boxcox, aes(ymin = sarima_forecasts$mean - 1.96 * sigma(garch_forecasts),
                                          ymax = sarima_forecasts$mean + 1.96 * sigma(garch_forecasts)),
              alpha = 0.3) +
  ylab("EUR/MWh") +
  ggtitle("One-Step SARIMA-GARCH Forecast with Upper and Lower Bounds") +
  coord_cartesian(xlim = c(2023.6, 2024.4))


# One-step forecasting
n <- length(esp_boxcox)
m <- length(esp_test_boxcox)

onestep_pred <- rep(0,m)
onestep_upper <- rep(0,m)
onestep_lower <- rep(0,m)

# for-loop of one-step predictions
for (i in 1:m) {
  current_data <- merged_boxcox_ts[-((n+i):(n+m))]
 
  current_sarima_fit <- stats::arima(current_data, order = c(1, 1, 3), seasonal = list(order = c(8, 1, 0), periods = 7))
  
  current_garch_fit <- ugarchforecast(ugarchfit(spec = garch_spec, data = current_sarima_fit$residuals),
                 n.ahead = 1)
  
  onestep_pred[i] <- forecast(current_sarima_fit, h = 1)$mean 
  
  onestep_upper[i] <- onestep_pred[i] + 1.96 * current_garch_fit@forecast$sigmaFor[1]
  
  onestep_lower[i] <- onestep_pred[i] - 1.96 * current_garch_fit@forecast$sigmaFor[1]
}
# Defines vectors into ts
onestep_pred_ts <- ts(onestep_pred, start = 2024, frequency = 365)
rscale_onestep_pred_ts <- InvBoxCox(onestep_pred_ts, lambda) - abs(1.5*min(esp_ts))

onestep_upper_ts <- ts(onestep_upper, start = 2024, frequency = 365)
rscale_onestep_upper_ts <- InvBoxCox(onestep_upper_ts, lambda) - abs(1.5*min(esp_ts))

onestep_lower_ts <- ts(onestep_lower, start = 2024, frequency = 365)
rscale_onestep_lower_ts <- InvBoxCox(onestep_lower_ts, lambda) - abs(1.5*min(esp_ts))

autoplot(merged_ts) +
  autolayer(rscale_onestep_pred_ts) +
  geom_ribbon(data = esp_test_boxcox, aes(ymin = rscale_onestep_lower_ts,
                                          ymax = rscale_onestep_upper_ts),
              alpha = 0.3) +
  ylab("EUR/MWh") +
  ggtitle("One-Step SARIMA-GARCH Forecast with Upper and Lower Bounds") +
  coord_cartesian(xlim = c(2023.8, 2024.4))


# Count every observation outside the prediction interval.
count <- 0
indekscount <- list()

for(i in seq_along(esp_test_ts)){
  if(rscale_onestep_upper_ts[i] < esp_test_ts[i] | esp_test_ts[i] < rscale_onestep_lower_ts[i]){
    count = count + 1
    indekscount <- append(indekscount, i)
  }
}









