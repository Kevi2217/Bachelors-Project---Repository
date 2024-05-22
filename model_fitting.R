library(sarima)
library(forecast)

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


<<<<<<< HEAD
=======
#
espnew_ts <- ts(elspot_new$DAP_EUR, start = end(esp_ts), frequency = 365)

espnew_ts <- BoxCox(espnew_ts + abs(1.5*min(esp_ts)), lambda) %>% diff() %>% diff(lag = 7)


>>>>>>> 8d41e23c1d5643d4e67d3220d25f7667dee314aa

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

<<<<<<< HEAD
sarima_model <- auto.arima(esp_boxcox, seasonal = TRUE, stepwise = FALSE ,approximation = FALSE, ic = "aicc")

plot(forecast(sarima_model, h = 50), xlim = c(2270,2300))


# Define ranges for p, d, q, P, D, Q parameters
p_range <- 0:5
d <- 1
q_range <- 0:5
P_range <- 0:5
D <- 1
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
          auto.arima(esp_ts, seasonal = TRUE, 
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

# AIC.
# optimal_order (1,0,2)
# optimal_seasonal order (5,0,0)

model <- arima(esp_ts, order = c(1,1,2), seasonal = list(order = c(5,1,0), periods = 7))

model_auto <- auto.arima(esp_boxcox, seasonal = TRUE, stepwise = FALSE, approximation = FALSE, ic = "aicc")

# Performing model diagnostic
checkresiduals(model)

# Perform Ljung-Box test
ljung_box_test <- Box.test(residuals(model), type = "Ljung-Box")

# Plot Ljung-Box test results
plot(ljung_box_test$p.value, type = "o", ylim = c(0, 1), ylab = "p-value", xlab = "Lag", 
     main = "Ljung-Box Test")

# Add a horizontal line at the significance level (e.g., 0.05)
abline(h = 0.05, col = "red", lty = 2)

# Add legend
legend("topright", legend = c("p-value", "Significance Level"), col = c("black", "red"), 
       lty = c(1, 2), cex = 0.8)

plot(forecast(model_auto, h = 50 ), xlim = c(2270,2300))


# Merging the training data with test data
merged_elspot <- merge(esp,elspot_new, all = TRUE)

merged_ts <- ts(merged_elspot$DAP_EUR, start = 2019, frequency = 7)

merged_elspot <- BoxCox(merged_ts + abs(1.5*min(esp_ts)), lambda) %>% diff() %>% diff(lag = 7)


=======
>>>>>>> 8d41e23c1d5643d4e67d3220d25f7667dee314aa
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

model <- arima(esp_boxcox, order = c(1,0,3), seasonal = list(order = c(8,0,0), periods = 7))
autoplot(model)
checkresiduals(model)
autoplot(forecast(model, h = 100), xlim = c(2275,2300))

model2 <- arima(esp_boxcox, order = c(0,0,4), seasonal = list(order = c(8,0,0), periods = 7))
model3 <- arima(esp_boxcox, order = c(0,0,3), seasonal = list(order = c(8,0,0), periods = 7))
model4 <- arima(esp_boxcox, order = c(0,0,3), seasonal = list(order = c(7,0,0), periods = 7))
model5 <- arima(esp_boxcox, order = c(0,0,2), seasonal = list(order = c(8,0,0), periods = 7))

get_aic <- function(model){
  n <- length(model$residuals)
  k <- length(model$coef)
  aicc <- AIC(model) + (2 * k * (k + 1)) / (n - k - 1)
  metrics <- list(AIC(model),aicc,BIC(model))
  return(metrics)
}

# Best model SARIMA(4,1,2)(5,1,0)[7] ... AIC = -20218.51
# 2.best model SARIMA(2,1,3)(5,1,0)[7] ... AIC = -20218.27
# 3. best model SARIMA(1,1,2)(5,1,0) ... AIC = -20202.51
# 4. best model SARIMA(0,1,4)(5,1,0) ... AIC = -20199.44
# 5. best model SARIMA(0,1,3)(5,1,0) ... AIC = -20196.67

model <- arima(esp_boxcox, order = c(4,0,2), seasonal = list(order = c(5,0,0), periods = 7))
autoplot(esp_boxcox) + autolayer(fitted(model))



# Forecasting
esp_only <- BoxCox(esp_ts + abs(1.5*min(esp_ts)), lambda)
# Changing the 'outliers'
for (i in seq_along(esp_only)) {
  if (esp_only[i] < 0.985) {
    esp_only[i] <- mean(esp_only[max(1, i - 6):i])
  }
}

espnew_only <- ts(elspot_new$DAP_EUR, start = end(esp_only), frequency = 365)
espnew_only <- BoxCox(espnew_only + abs(1.5*min(esp_ts)), lambda)
espnew_ts <- ts(elspot_new$DAP_EUR, start = end(esp_ts), frequency = 365)
merged_ts <- ts(c(esp_ts, espnew_ts), start = 2019, frequency = 365)


testmodel <- arima(esp_only, order = c(1,1,3), seasonal = list(order = c(8,1,0), periods = 7))



forecast_df <- data.frame("mean" = InvBoxCox(forecast(testmodel, h= 121)$mean, lambda)-abs(1.5*min(esp_ts)),
                          "upper" = InvBoxCox(forecast(testmodel, h= 121)$upper, lambda)-abs(1.5*min(esp_ts)),
                          "lower" = InvBoxCox(forecast(testmodel, h= 121)$lower, lambda)-abs(1.5*min(esp_ts)))

mean_ts <- ts(forecast_df$mean, start = end(esp_ts), frequency = 365)
upper_ts <-ts(forecast_df$upper.95., start = end(esp_ts), frequency = 365)
lower_ts <- ts(forecast_df$lower.95., start = end(esp_ts), frequency = 365)


autoplot(merged_ts, series = "Observed prices") +
  autolayer(mean_ts, series = "Predicted prices") +
  geom_ribbon(data = espnew_ts, aes(ymin = lower_ts, ymax = upper_ts), alpha = 0.3) +
  ylab("EUR/MWh") +
  ggtitle("Forecast of Electricity Prices") +
  #scale_x_date(date_labels = "%b %Y", breaks = date_breaks("1 month")) +
  coord_cartesian(xlim = c(2023.95, 2024.3), ylim = c(0,300)) +
  scale_color_manual(values = c("Observed prices" = "black", "Predicted prices" = "orangered")) +
  labs(color = "Legend")

# Calculate the MAPE
library(MLmetrics) 
library(Metrics)
mean(abs((espnew_ts - mean_ts)/espnew_ts))* 100
MAPE(mean_ts, espnew_ts)

# Calculate standard error (mean)
sd(mean_ts)/sqrt(length(mean_ts))

sd(mean_ts)

mae(espnew_ts, mean_ts)

# Counts amount of observed prices are within the prediction interval
x <- 0
for (i in seq_along(espnew_ts)){
    if (onelowerts[i] < espnew_ts[i] & espnew_ts[i] < oneupperts[i]) {
      x = x +1
  }
}


