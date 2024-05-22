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
library(LSTS)
library(scales)


########################## Preprocessing for GARCH #############################
# Get data from 2014 to 2024
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

# Using boxcox transformation
lambda <- BoxCox.lambda(esp_ts + abs(1.5*min(esp_ts)))
esp_boxcox <- BoxCox(esp_ts + abs(1.5*min(esp_ts)), lambda)
autoplot(esp_boxcox)

esp_test_boxcox <- BoxCox(esp_test_ts + abs(1.5*min(esp_ts)), lambda)
autoplot(esp_test_boxcox)

############################# OUTLIER REMOVAL ################################## 

# Assign the value of the previous 7 observations
esp_boxcox[1644] <- mean(esp_boxcox[(1644 - 6):1644])
autoplot(esp_boxcox)

################################### !GARCH! ####################################
# seasonal arima = 8, 1, 0
archTest(esp_boxcox)

sarima_model <- stats::arima(esp_boxcox, order = c(1,1,3), seasonal = list(order = c(8,1,0), periods = 7))
sarima_residuals <- sarima_model$residuals

############################# GARCH ORDER ESTIMATION ###########################
results <- data.frame(m = integer(),
                      s = integer(),
                      AIC_value = numeric(),
                      AICC_value = numeric(),
                      BIC_value = numeric())

for (m in 0:2) {
  for (s in 0:2) {
# Step 3: Fit GARCH Model
garch_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(m, s)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE)
)
garch_fit <- ugarchfit(spec = garch_spec,
                       data = sarima_residuals)

AIC_value <- -2 * garch_fit@fit$LLH + 2 * length(coef(garch_fit))

AICC_value <- AIC_value + (2*length(coef(garch_fit))*(length(coef(garch_fit)) + 1)) / (length(sarima$residuals) - length(coef(garch_fit)) + 1)  

BIC_value <- - 2*garch_fit@fit$LLH + log(length(sarima$residuals))*length(coef(garch_fit))

results <- rbind(results, list(m = m, s = s, AIC_value = AIC_value, 
                               AICC_value = AICC_value,
                               BIC_value = BIC_value))
  }
}

######################### Moving on with GARCH (1, 2) ##########################
garch_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 2)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE)
)
garch_fit <- ugarchfit(spec = garch_spec,
                       data = sarima_residuals)


sarima_garch_model <- list(sarima_model1 = sarima_model, garch_model1 = garch_fit)


grid.arrange(ggAcf((garch_fit@fit$residuals / garch_fit@fit$sigma)^2,
                   main = "Standardized Residuals Squared ACF", lag.max = 50),
             ggPacf((garch_fit@fit$residuals / garch_fit@fit$sigma)^2,
                    main = "Standardized Residuals Squared PACF", lag.max = 50),
             ncol = 2)

# Standardized residuals plot
checkresiduals(ts(sarima_residuals / garch_fit@fit$sigma, start = 2019, frequency = 365), lag.max = 30)

# Ljung-Box plot
ljungbox <- function(model, h) {
  p_values <- rep(0,h)
  for (i in 1:h){
    p_values[i] <- Box.test((model@fit$residuals / garch_fit@fit$sigma), lag = i,
                            type = c("Ljung-Box"))$p.value
  }
  ggplot()+
    geom_point(aes(x = 1:h,y = p_values, shape = "p-value")) +
    geom_hline(color = "blue", yintercept = 0.05, linetype = "dashed") +
    xlab("Lag") +
    ylab("p-value") +
    coord_cartesian(ylim = c(0, 1)) +
    labs(shape = "Legend")
}
ljungbox(garch_fit, 14)



############################ 121-step forecast #################################
# SARIMA forecast
long_pred <- forecast(sarima_garch_model$sarima_model1, h = length(esp_test_boxcox))$mean

# GARCH forecast
long_garch_forecasts <- ugarchforecast(sarima_garch_model$garch_model1, n.ahead = length(esp_test_boxcox))

long_upper <- onestep_pred + 1.96 * sigma(long_garch_forecasts)

long_lower <- onestep_pred - 1.96 * sigma(long_garch_forecasts)

long_pred_ts <- ts(long_pred, start = 2024, frequency = 365)
rscale_long_pred_ts <- InvBoxCox(long_pred_ts, lambda) - abs(1.5*min(esp_ts))

long_upper_ts <- ts(long_upper, start = 2024, frequency = 365)
rscale_long_upper_ts <- InvBoxCox(long_upper_ts, lambda) - abs(1.5*min(esp_ts))

long_lower_ts <- ts(long_lower, start = 2024, frequency = 365)
rscale_long_lower_ts <- InvBoxCox(long_lower_ts, lambda) - abs(1.5*min(esp_ts))


merged_boxcox_ts <- ts(c(esp_boxcox, esp_test_boxcox), start = 2019, frequency = 365)
merged_ts <- ts(c(esp_ts, esp_test_ts), start = 2019, frequency = 365)

# Plotting long forecast
autoplot(merged_ts, series = "Observed prices") +
  autolayer(rscale_long_pred_ts, series = "Predicted prices") +
  geom_ribbon(data = esp_test_boxcox, aes(ymin = rscale_long_lower_ts,
                                          ymax = rscale_long_upper_ts),
              alpha = 0.3) +
  ylab("EUR/MWh") +
  ggtitle("SARIMA(1,1,3)(8,1,0)[7]-GARCH(1,2) Forecast of Electricity Prices") +
  coord_cartesian(xlim = c(2023.955, 2024.31), ylim = c(0, 240)) +
  scale_color_manual(values = c("Observed prices" = "black",
                                "Predicted prices" = "orangered1")) +
  labs(color = "Legend")


############################## One-step forecast ###############################
n <- length(esp_boxcox)
m <- length(esp_test_boxcox)

onestep_pred <- rep(0,m)
onestep_upper <- rep(0,m)
onestep_lower <- rep(0,m)

# for-loop of one-step predictions
for (i in 1:m) {
  current_data <- merged_boxcox_ts[-((n+i):(n+m))]
 
  current_sarima_fit <- stats::arima(current_data, order = c(1, 1, 3),
                                     seasonal = list(order = c(8, 1, 0), periods = 7))
  
  current_garch_fit <- ugarchforecast(ugarchfit(spec = garch_spec, data = current_sarima_fit$residuals),
                 n.ahead = 1)
  
  onestep_pred[i] <- forecast(current_sarima_fit, h = 1)$mean 
  
  onestep_upper[i] <- onestep_pred[i] + 1.96 * current_garch_fit@forecast$sigmaFor[1]
  
  onestep_lower[i] <- onestep_pred[i] - 1.96 * current_garch_fit@forecast$sigmaFor[1]
}

onestep_pred_ts <- ts(onestep_pred, start = 2024, frequency = 365)
rscale_onestep_pred_ts <- InvBoxCox(onestep_pred_ts, lambda) - abs(1.5*min(esp_ts))

onestep_upper_ts <- ts(onestep_upper, start = 2024, frequency = 365)
rscale_onestep_upper_ts <- InvBoxCox(onestep_upper_ts, lambda) - abs(1.5*min(esp_ts))

onestep_lower_ts <- ts(onestep_lower, start = 2024, frequency = 365)
rscale_onestep_lower_ts <- InvBoxCox(onestep_lower_ts, lambda) - abs(1.5*min(esp_ts))

autoplot(merged_ts, series = "Observed prices") +
  autolayer(rscale_onestep_pred_ts, series = "Predicted prices") +
  geom_ribbon(data = esp_test_boxcox, aes(ymin = rscale_onestep_lower_ts,
                                          ymax = rscale_onestep_upper_ts),
              alpha = 0.3) +
  ylab("EUR/MWh") +
  ggtitle("One-Step SARIMA(1,1,3)(8,1,0)[7]-GARCH(1,2) Forecast of Electricity Prices") +
  coord_cartesian(xlim = c(2023.955, 2024.31), ylim = c(0, 220)) +
  scale_color_manual(values = c("Observed prices" = "black",
                                "Predicted prices" = "orangered1")) +
  labs(color = "Legend")


# Count every observation outside the prediction interval.
count <- 0
indekscount <- list()

for(i in seq_along(esp_test_ts)){
  if(rscale_onestep_upper_ts[i] < esp_test_ts[i] | esp_test_ts[i] < rscale_onestep_lower_ts[i]){
    count = count + 1
    indekscount <- append(indekscount, i)
  }
}
