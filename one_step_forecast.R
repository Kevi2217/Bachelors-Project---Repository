
n <- 5
ts_data <- esp_boxcox

for(i in 1:n){
  
  model <- arima(ts_data, order = c(3,1,2), seasonal = list(order  = c(4,1,1), periods = 7))
  
  # One-step forecast
  forecast_result <-forecast(model, h = 1)
  
  # Extract the one-step forecasted value
  one_step_forecast <- forecast_result$mean[1]
  
  ts_data <- window(esp_boxcox, end = end(esp_boxcox) + 1)
  ts_data[length(ts_data)] <- one_step_forecast
}
