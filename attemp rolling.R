
### Rolling h-step ###

merged_ts1 <- c(esp_only, espnew_only)
test_df <- data.frame()
max.h <- 4
n <- length(esp_ts)
m <- length(espnew_ts)

# Framework of the h-step table
for(h in 1:max.h){
  col_name <- paste("Prediction", h, sep = "")
  col_upper <- paste("Upper", h, sep = "")
  col_lower <- paste("Lower", h, sep = "")
  prediction = list()
  upper = list()
  lower = list()
  for(i in 1:m){
    current_data <- merged_ts1[(n - (365/3) + (i-1)):(n + (i-1))]
    
    current_fit <- arima(current_data, order = c(1,1,3), 
                         seasonal = list(order = c(8,1,0), period = 7), 
                         include.mean = FALSE)
    forecast_result <- forecast(current_fit, h = h)
    for(j in 1:max.h){
      if(h == 1){
        Index = i + (j-1)
        new.row <- data.frame(Index)
        test_df <- rbind(test_df, new.row)
      }
      prediction <- append(prediction, forecast_result$mean[j])
      upper <- append(upper, forecast_result$upper[j+h])
      lower <- append(lower, forecast_result$lower[j+h])
    }
  }
  test_df[[col_name]] <- prediction
  test_df[[col_upper]] <- upper
  test_df[[col_lower]] <- lower
}


