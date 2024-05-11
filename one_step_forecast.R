
# One-step forecasting

# Merging training and test data.
merged_ts <- ts(c(esp_only, espnew_only), start = 2019, frequency = 365)

n = length(esp_only)
m = length(espnew_only)

one_pred = rep(0,m)
one_upper = rep(0,m)
one_lower = rep(0,m)

# for-loop of one-step predictions
for (i in 1:m) {
  current_data = merged_ts[-((n+i):(n+m))]
  
  current_fit = arima(current_data, order = c(1,1,3), seasonal = list(order = c(8,1,0), periods = 7), include.mean = FALSE)
  
  one_pred[i] = forecast(current_fit, h = 1)$mean 
  
  one_upper[i] = forecast(current_fit, h = 1)$upper
  
  one_lower[i] = forecast(current_fit, h = 1)$lower
}

onestepts <- ts(one_pred, start = start(espnew_ts), frequency = 365)
onestepts <- InvBoxCox(onestepts, lambda)

oneupperts <- ts(one_upper, start = start(espnew_ts), frequency = 365)
oneupperts <- InvBoxCox(oneupperts, lambda)

onelowerts <- ts(one_lower, start = start(espnew_ts), frequency = 365)
onelowerts <- InvBoxCox(onelowerts, lambda)

autoplot(merged_ts) +
  autolayer(onestepts - abs(1.5*min(esp_ts)), series = "One-Step Forecast") +
  geom_ribbon(data =espnew_ts, aes(ymax = oneupperts - abs(1.5*min(esp_ts)), ymin = onelowerts - abs(1.5*min(esp_ts))), alpha = 0.3) +
  ylab("Your Y-Axis Label") +
  xlab("Your X-Axis Label") +
  ggtitle("One-Step Forecast with Upper and Lower Bounds") +
  coord_cartesian(xlim = c(2023.9, 2024.3))
