library(ggplot2)
library(gridExtra)

# Getting the residuals
res_for121 <- espnew_ts - forecast_df$mean

# Plot histogram
his1 <- gghistogram(res_for121, add.normal = TRUE, add.rug = TRUE) + 
  labs(title = "Residuals of 121-Step Forecast", x = "residuals", y = "count")


#Residuals analysis of 1-step forecast
res_for1 <- res_step_pred$Residuals1

# Plot histogram
his2 <- gghistogram(res_for1, add.normal = TRUE, add.rug = TRUE) + 
  labs(title = "Residuals of One-Step Forecast", x = "residuals", y = "count")


combined_plot <- grid.arrange(his1, his2, ncol = 2)


# binwidth <- (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))/bins



