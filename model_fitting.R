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

# Merging the training data with test data
merged_elspot <- merge(esp,elspot_new, all = TRUE)




esp_ts <- ts(esp$DAP_EUR, start = 2019, frequency = 7)
merged_ts <- ts(merged_elspot$DAP_EUR, start = 2019, frequency = 7)


# Difference the time series
esp_diff <- diff(esp_ts)

# Log-transform and difference the time series
esp_logdiff <- log(esp_ts + abs(1.5*min(esp_ts))) %>% diff() %>% diff(lag = 7)

# Box Cox-transform and difference the time series
lambda <- BoxCox.lambda(esp_ts + abs(1.5*min(esp_ts)))
esp_boxcox <- BoxCox(esp_ts + abs(1.5*min(esp_ts)), lambda) %>% diff() %>% diff(lag = 7)


Acf(esp_boxcox, lag.max = 50)
Pacf(esp_boxcox, lag.max = 50)


sarima_model <- auto.arima(esp_boxcox, seasonal = TRUE, stepwise = FALSE ,approximation = FALSE, ic = "aicc")

plot(forecast(sarima_model, h = 50), xlim = c(2270,2300))


# Define ranges for p, d, q, P, D, Q parameters
p_range <- 0:5
d <- 0
q_range <- 0:5
P_range <- 0:5
D <- 0
Q_range <- 0:5

# Initialize variables to store optimal values and corresponding criteria
best_aic <- Inf
optimal_order <- c(0, 0, 0)
optimal_seasonal_order <- c(0, 0, 0)

# Loop through parameter combinations
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
          current_aic <- sarima_model$aic
          if (current_aic < best_aic) {
            best_aic <- current_aic
            optimal_order <- c(p, d, q)
            optimal_seasonal_order <- c(P, D, Q)
          }
        }
      }
    }
  }
}

# optimal_order (1,0,2)
# optimal_seasonal order (5,0,0)

model <- arima(esp_boxcox, order = c(1,0,2), seasonal = list(order = c(5,0,0), periods = 7))
