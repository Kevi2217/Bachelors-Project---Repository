library(forecast)
library(dplyr)
library(scales)

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
    current_data <- merged_ts1[-((n+i):(n+m))]
    
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

# Save the data.frame
save(test_df,file="h-step_data.Rda")

# Transform back to the original scale.
test_df1 <- test_df %>% 
  mutate(across(everything(), as.numeric)) %>% 
  group_by(Index) %>% 
  mutate(across(everything(), ~ InvBoxCox(.x, lambda) - abs(1.5*min(esp_ts)))) %>% 
  summarise_all(mean, na.rm = TRUE) %>%
  filter(Index <= length(espnew_ts)) %>%
  as.data.frame()


step_prediction <- data.frame(date = seq(as.Date("2023-12-31"), by = "days", length.out = length(espnew_ts)),
                              espnew_time = as.numeric(espnew_ts),
                              Prediction1 = test_df1$Prediction1,
                              Upper1 = test_df1$Upper1,
                              Lower1 = test_df1$Lower1,
                              Prediction2 = test_df1$Prediction2,
                              Upper2 = test_df1$Upper2,
                              Lower2 = test_df1$Lower2,
                              Prediction3 = test_df1$Prediction3,
                              Upper3 = test_df1$Upper3,
                              Lower3 = test_df1$Lower3,
                              Prediction4 = test_df1$Prediction4,
                              Upper4 = test_df1$Upper4,
                              Lower4 = test_df1$Lower4
                              )

res_step_pred <- step_prediction %>% 
  mutate(Residuals1 = espnew_time - Prediction1,
         Residuals2 = espnew_time - Prediction2,
         Residuals3 = espnew_time - Prediction3,
         Residuals4 = espnew_time - Prediction4) %>% 
  dplyr::select(-Upper1,-Upper2,-Upper3,-Upper4,
                -Lower1,-Lower2,-Lower3,-Lower4,
                -date, - espnew_time)

res31 <- res_step_pred[1:31,]

mean(res31$Residuals3)
res31$Residuals1[which(abs(res31$Residuals1) == min(abs(res31$Residuals1)))]
res31$Residuals1[which(abs(res31$Residuals1) == max(abs(res31$Residuals1)))]
max(abs(res31$Residuals1))

xtable(res31, type = "latex")


# Calculate the MAPE and MAE
library(MLmetrics) 
library(Metrics)
# 121-step 
mean(abs((espnew_ts - mean_ts)/espnew_ts))* 100
MAPE(mean_ts, espnew_ts)
mae(espnew_ts, mean_ts)

#one-step MAPE and MAE
mae(espnew_ts, test_df1$Prediction1)
mape(espnew_ts, test_df1$Prediction1)

#MAE_resisduals
sum(abs(res31$Residuals1))/length(res31$Residuals1)

# Counts amount of observed prices are outside the prediction interval (one-step)
x <- 0
indexcount <- list()
for (i in 1:length(espnew_ts)){
  if (step_prediction$Lower1[i] > espnew_ts[i] | espnew_ts[i] > step_prediction$Upper1[i]) {
    x = x +1
    indexcount <- append(indexcount, i)
  }
}


# Counts amount of observed prices are outside the prediction interval (121-step)
x <- 0
indexcount <- list()
for (i in 1:length(espnew_ts)){
  if (forecast_df$lower.95.[i] > espnew_ts[i] | espnew_ts[i] > forecast_df$upper.95.[i]) {
    x = x +1
    indexcount <- append(indexcount, i)
  }
}


##### PLOTS #####

merged_ts <- ts(c(esp_ts, espnew_ts), start = 2019, frequency = 365)
merged_df <- data.frame(date = seq(as.Date("2019-01-01"), by = "days", length.out = length(merged_ts)), 
                        value = as.numeric(merged_ts))

#One-Step Prediction plot
ggplot(data = step_prediction) +
  geom_line(data = merged_df, aes(x = date, y = value, color = "Observed prices")) +
  geom_line(aes(x = date, y = Prediction1, color = "Predicted prices")) +
  geom_ribbon(aes(x = date, ymin = Lower1, ymax = Upper1), alpha = 0.3) +
  ylab("EUR/MWh") +
  xlab("Time") +
  ggtitle("One-Step SARIMA(1,1,3)(8,1,0)[7] Prediction of Electricity Prices") +
  scale_x_date(date_labels = "%b %Y", breaks = date_breaks("1 month")) +
  coord_cartesian(xlim = c(as.Date("2023-12-15"), as.Date("2024-04-24")), ylim = c(0,200)) +
  scale_color_manual(values = c("Observed prices" = "black", "Predicted prices" = "orangered1")) +
  labs(color = "Legend")


#h-Step Prediction plot
ggplot(data = step_prediction) +
  geom_line(data = merged_df, aes(x = date, y = value, color = "Observed prices")) +
  geom_line(aes(x = date, y = Prediction1, color = "h = 1")) +
  geom_line(aes(x = date, y = Prediction2, color = "h = 2")) +
  geom_line(aes(x = date, y = Prediction3, color = "h = 3")) +
  geom_line(aes(x = date, y = Prediction4, color = "h = 4")) +
  #geom_ribbon(aes(x = date, ymin = Lower1, ymax = Upper1), alpha = 0.3) +
  ylab("EUR/MWh") +
  xlab("Time") +
  ggtitle("h-Step Predictions of Electricity Prices") +
  scale_x_date(date_labels = "%d %b %Y", breaks = date_breaks("7 day")) +
  coord_cartesian(xlim = c(as.Date("2024-01-02"), as.Date("2024-02-01")), ylim = c(0,120)) +
  scale_color_manual(values = c("Observed prices" = "black", "h = 1" = "orangered1",
                                "h = 2" = "blue4", "h = 3" = "goldenrod3",
                                "h = 4" = "orchid3")) +
  labs(color = "Legend")



# Forecast plot (121-step)
forecast_df <- data.frame(date = seq(as.Date("2023-12-31"), by = "days", length.out = length(espnew_ts)),
                          mean = InvBoxCox(as.numeric(forecast(testmodel, h= 121)$mean), lambda)-abs(1.5*min(esp_ts)),
                          upper = InvBoxCox(as.numeric(forecast(testmodel, h= 121)$upper), lambda)-abs(1.5*min(esp_ts)),
                          lower = InvBoxCox(as.numeric(forecast(testmodel, h= 121)$lower), lambda)-abs(1.5*min(esp_ts)))

ggplot() +
  geom_line(data = merged_df, aes(x = date, y = value, color = "Observed prices")) +
  geom_line(data = forecast_df, aes(x = date, y = mean, color = "Predicted prices")) +
  geom_ribbon(aes(x = forecast_df$date, ymin = forecast_df$lower, ymax = forecast_df$upper), alpha = 0.3) +
  ylab("EUR/MWh") +
  xlab("Time") +
  ggtitle("Forecast of Electricity Prices") +
  scale_x_date(date_labels = "%b %Y", breaks = date_breaks("1 month")) +
  coord_cartesian(xlim = c(as.Date("2023-12-15"), as.Date("2024-04-24")), ylim = c(0,300)) +
  scale_color_manual(values = c("Observed prices" = "black", "Predicted prices" = "orangered1")) +
  labs(color = "Legend")

autoplot(merged_ts, series = "Observed prices") +
  autolayer(mean_ts, series = "Predicted prices") +
  geom_ribbon(data = espnew_ts, aes(ymin = lower_ts, ymax = upper_ts), alpha = 0.3) +
  ylab("EUR/MWh") +
  ggtitle("SARIMA(1,1,3)(8,1,0)[7] Forecast of Electricity Prices") +
  #scale_x_date(date_labels = "%b %Y", breaks = date_breaks("1 month")) +
  coord_cartesian(xlim = c(2023.955, 2024.31), ylim = c(0,300)) +
  scale_color_manual(values = c("Observed prices" = "black", "Predicted prices" = "orangered")) +
  labs(color = "Legend")


# Fitted val plot.
autoplot(esp_only, series = "Observed values") +
  autolayer(fitted(testmodel), series = "Fitted values") +
  ylab("Box-Cox-transformed prices") +
  scale_color_manual(values = c("Observed values" = "black", "Fitted values" = "coral1")) +
  labs(title = "Fitted Values of SARIMA(1,1,3)(8,1,0)[7]", color = "Legend")
  
