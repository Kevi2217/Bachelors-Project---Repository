# One-step forecasting
esp_only <- BoxCox(esp_ts + abs(1.5*min(esp_ts)), lambda)
# Changing the four 'outliers'
for (i in seq_along(esp_only)) {
  if (esp_only[i] < 0.985) {
    esp_only[i] <- mean(esp_only[max(1, i - 6):i])
  }
}

espnew_only <- ts(elspot_new$DAP_EUR, start = end(esp_only), frequency = 365)
espnew_only <- BoxCox(espnew_only + abs(1.5*min(esp_ts)), lambda)

espnew_ts <- ts(elspot_new$DAP_EUR, start = end(esp_ts), frequency = 365)

# Merging training and test data.
merged_ts <- ts(c(esp_ts, espnew_ts), start = 2019, frequency = 365)
merged_ts1 <- ts(c(esp_only, espnew_only), start = 2019, frequency = 365)

n = length(esp_only)
m = length(espnew_only)

one_pred1 = rep(0,m)
one_upper1 = rep(0,m)
one_lower1 = rep(0,m)


for (i in 1:m) {
  current_data = merged_ts1[-((n+i):(n+m))]
  
  current_fit = arima(current_data, order = c(1,1,3), seasonal = list(order = c(8,1,0), periods = 7), include.mean = FALSE)
  
  one_pred1[i] = forecast(current_fit, h = 1)$mean 
  
  one_upper1[i] = forecast(current_fit, h = 1)$upper
  
  one_lower1[i] = forecast(current_fit, h = 1)$lower
}

onestepts <- ts(one_pred1, start = start(espnew_ts), frequency = 365)
onestepts <- InvBoxCox(onestepts, lambda)

oneupperts <- ts(one_upper1, start = start(espnew_ts), frequency = 365)
oneupperts <- InvBoxCox(oneupperts, lambda)

onelowerts <- ts(one_lower1, start = start(espnew_ts), frequency = 365)
onelowerts <- InvBoxCox(onelowerts, lambda)

autoplot(merged_ts) +
  autolayer(onestepts- abs(1.5*min(esp_ts)), series = "One-Step Forecast") +
  geom_ribbon(data =espnew_ts, aes(ymax = oneupperts- abs(1.5*min(esp_ts)), ymin = onelowerts- abs(1.5*min(esp_ts))), alpha = 0.3) +
  ylab("Your Y-Axis Label") +
  xlab("Your X-Axis Label") +
  ggtitle("One-Step Forecast with Upper and Lower Bounds") +
  coord_cartesian(xlim = c(2023.9, 2024.3))

mae(espnew_ts, onestepts - abs(1.5*min(esp_ts)))
mape(espnew_ts, onestepts - abs(1.5*min(esp_ts)))



# Count every observation outside the prediction interval.

ggonestep <- rep(0,m)
ggoneupper <- rep(0,m)
ggonelower <- rep(0,m)
count <- 0
indekscount <- list()

for(i in seq_along(onestepts)){
  ggonestep[i] = onestepts[i] - abs(1.5*min(esp_ts))
  ggoneupper[i] = oneupperts[i] - abs(1.5*min(esp_ts))
  ggonelower[i] = onelowerts[i] - abs(1.5*min(esp_ts))
  if(ggoneupper[i] < espnew_ts[i] | espnew_ts[i] < ggonelower[i]){
    count = count + 1
    indekscount <- append(indekscount, i)
  }
}


library(LSTS)

ljungbox <- function(model, h){
  p_values <- rep(0,h)
  for (i in 1:h){
  p_values[i] <- Box.test(testmodel$residuals, lag = i, type = "Ljung", fitdf = i - 1 - 3)$p.value
  }
  ggplot()+
    geom_point(aes(x = 1:h,y = p_values)) +
    geom_hline(yintercept = 0.05, linetype = "dashed", color = "blue") +
    coord_cartesian(ylim = c(0, 1))
}
ljungbox(testmodel, 50)

g <- Arima(esp_only, order = c(0,1,4), seasonal = list(order = c(2,1,3), periods = 7))
