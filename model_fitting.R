library(sarima)
library(forecast)

# Format the newer data points.
Elspotprices_1_1_2024_25_4_2024 <- Elspotprices_1_1_2024_25_4_2024 %>% 
  rename_all(tolower)

elspot_new <- Elspotprices_1_1_2024_25_4_2024 %>% 
  dplyr::mutate(spotpricedkk = as.numeric(gsub(",",".",spotpricedkk, fixed = TRUE))) %>% 
  dplyr::mutate(spotpriceeur = as.numeric(gsub(",",".",spotpriceeur, fixed = TRUE))) %>% 
  dplyr::mutate(hourdk = as.Date(hourdk))

elspot_new <- elspot_new %>% 
  dplyr::group_by(pricearea, hourdk) %>%
  dplyr::summarise(DAP_DKK = round(mean(spotpricedkk), 2),
                   DAP_EUR = round(mean(spotpriceeur), 2)) %>% 
  dplyr::filter(pricearea == "DK1")


espnew_ts <- ts(elspot_new$DAP_EUR, start = end(esp_boxcox), frequency = 365)

espnew_ts <- BoxCox(espnew_ts + abs(1.5*min(esp_ts)), lambda) %>% diff() %>% diff(lag = 7)


esp_ts <- ts(esp$DAP_EUR, start = 2019, frequency = 365)


# Difference the time series
esp_diff <- diff(esp_ts)

# Log-transform and difference the time series
esp_logdiff <- log(esp_ts + abs(1.5*min(esp_ts))) %>% diff() %>% diff(lag = 7)

# Box Cox-transform and difference the time series
lambda <- BoxCox.lambda(esp_ts + abs(1.5*min(esp_ts)))
esp_boxcox <- BoxCox(esp_ts + abs(1.5*min(esp_ts)), lambda) %>% diff() %>% diff(lag = 7)

lambda <- BoxCox.lambda(esp_ts + abs(1.5*min(esp_ts)))
esp_boxcox <- BoxCox(esp_ts + abs(1.5*min(esp_ts)), lambda)

# Changing the four 'outliers'
for (i in seq_along(esp_boxcox)) {
  if (abs(esp_boxcox[i]) > 0.02) {
    esp_boxcox[i] <- mean(esp_boxcox[max(1, i - 6):i])
  }
}

## Arima(3,1,2)(4,1,1)[7]

ggmodel <- arima(esp_boxcox, order = c(3,1,2), seasonal = list(order = c(4,1,1), frequency = 7))

autoplot(fitted(ggmodel)) + autolayer(esp_boxcox)
checkresiduals(ggmodel)

gmodel <- auto.arima(esp_boxcox, seasonal = TRUE, method = "ML")



# for 5 bedste
# Define ranges for p, d, q, P, D, Q parameters
p_range <- 0:5
d <- 1
q_range <- 0:5
P_range <- 0:8
D <- 1
Q_range <- 0:3

current_aic <- 0
best_aic <- list(Inf)
best_aicc <- list(Inf)
best_bic <- list(Inf)
optimal_orders <- list()
optimal_seasonal_orders <- list()

for (p in p_range) {
  for (q in q_range) {
    for (P in P_range) {
      for (Q in Q_range) {
        # Fit SARIMA model for each combination
        sarima_model <- tryCatch(
          auto.arima(esp_boxcox, seasonal = TRUE, 
                     max.p = p, max.q = q, max.P = P, max.Q = Q))
        # Check if model fitting was successful and evaluate based on AIC
        if (!is.null(sarima_model)) {
          current_aicc <- sarima_model$aicc
          current_aic <-  sarima_model$aic
          current_bic <- sarima_model$bic
          if (current_aic < tail(best_aic, n=1)) {
            best_aicc <- append(best_aicc, current_aicc)
            best_aic <- append(best_aic, current_aic)
            best_bic <- append(best_bic, current_bic)
            optimal_orders <- append(optimal_orders,c(p, d, q))
            optimal_seasonal_orders <- append(optimal_seasonal_orders,c(P, D, Q))
          }
        }
      }
    }
  }
}


# Best model SARIMA(4,1,2)(5,1,0)[7] ... AIC = -20218.51
# 2.best model SARIMA(2,1,3)(5,1,0)[7] ... AIC = -20218.27
# 3. best model SARIMA(1,1,2)(5,1,0) ... AIC = -20202.51
# 4. best model SARIMA(0,1,4)(5,1,0) ... AIC = -20199.44
# 5. best model SARIMA(0,1,3)(5,1,0) ... AIC = -20196.67

model <- arima(esp_boxcox, order = c(4,0,2), seasonal = list(order = c(5,0,0), periods = 7))
autoplot(esp_boxcox) + autolayer(fitted(model))




