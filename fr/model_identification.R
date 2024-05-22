#### MODEL IDENTIFICATION ####
# Define ranges for p, d, q, P, D, Q parameters
p_range <- 0:5
d <- 1
q_range <- 0:5
P_range <- 0:8
D <- 1
Q_range <- 0:3

# List of best AIC,AICC, BIC and SARIMA orders
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
        sarima_model <- arima(esp_only, order = c(p,d,q), method = "ML",
                              seasonal = list(order = c(P,D,Q), periods = 7))
        # Check if model fitting was successful and evaluate based on AIC
        if (!is.null(sarima_model)) {
          n <- length(sarima_model$residuals)
          k <- length(sarima_model$coef)
          current_aic <-  sarima_model$aic
          current_aicc <- sarima_model$aic + (2 * k * (k + 1)) / (n - k - 1)
          current_bic <- BIC(sarima_model)
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


#Given model.
testmodel <- arima(esp_only, order = c(1,1,3), seasonal = list(order = c(8,1,0), periods = 7))

summary(testmodel)

autoplot(testmodel)

#fit plot 
autoplot(esp_ts) + autolayer(fitted(testmodel))
