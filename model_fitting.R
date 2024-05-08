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

# Convert to multiple seasonality time series
esp_msts <- msts(esp_boxcox, seasonal.periods = c(7, 365))

# Decompose the time series with multiple seasonality
esp_mstl <- mstl(esp_msts, iterate = 500)

Acf(esp_boxcox, lag.max = 50)
Pacf(esp_boxcox, lag.max = 50)


# Changing the four 'outliers'
for (i in seq_along(esp_boxcox)) {
  if (abs(esp_boxcox[i]) > 0.05) {
    esp_boxcox[i] <- mean(esp_boxcox[max(1, i - 6):i])
  }
}

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
          current_aic <- sarima_model$aic
          if (current_aic < tail(best_aic, n=1)) {
            best_aic <- append(best_aic, current_aic)
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


library(LSTS)
Box.Ljung.Test(model$residuals, lag = 42)
