
# One-step forecasting

# Merging training and test data.
all_data <- c(esp_boxcox, espnew_ts)

n = length(esp_boxcox)
m = length(espnew_ts)

one_pred = rep(0,m)
one_upper = rep(0,m)
one_lower = rep(0,m)


for (i in 1:m) {
  current_data = all_data[-((n+i):(n+m))]
  
  current_fit = arima(current_data, order = c(1,0,3), seasonal = list(order = c(8,0,0), periods = 7), include.mean = FALSE)
  
  one_pred[i] = forecast(current_fit, h = 1)$mean 
  
  one_upper[i] = forecast(current_fit, h = 1)$upper
  
  one_lower[i] = forecast(current_fit, h = 1)$lower
}

backto <- one_pred %>% 
  diffinv(lag = 7) %>% 
  diffinv() %>% 
  InvBoxCox(lambda)



onestepts <- ts(one_pred, start = start(espnew_ts), frequency = 365)


autoplot(espnew_ts) +
  autolayer(onestepts, series = "One-Step Forecast") +
  geom_ribbon(aes(ymax = ts(one_upper, start = start(espnew_ts),frequency = 365), ymin = ts(one_lower, start = start(espnew_ts),frequency = 365)), alpha = 0.3) +
  ylab("Your Y-Axis Label") +
  xlab("Your X-Axis Label") +
  ggtitle("One-Step Forecast with Upper and Lower Bounds")



onestep_back <- InvBoxCox(onestepts,  lambda)
onestep_back <- diffinv(onestep_back, lag = 7, difference = 1)
onestep_back <- diffinv(onestep_back, lag = 1, difference = 1)



merged_ts <- ts(c(esp_ts, test_espnew), start = 2019, frequency = 365)

# Test for converting the transformed data back to its original scale.
test_espnew <- ts(elspot_new$DAP_EUR, start = end(esp_ts), frequency = 365)


test_esp <- esp_boxcox %>% 
  InvBoxCox(lambda) %>% 
  diffinv(lag = 7) %>%  
  diffinv()   

autoplot(ts(test_esp, start = start(esp_ts), frequency = 365))


