
# Residuals analysis
checkresiduals(testmodel$residuals, lag.max = 30)

ljungbox <- function(model, h){
  p_values <- rep(0,h)
  for (i in 1:h){
    p_values[i] <- Box.test(testmodel$residuals, lag = i, type = "Ljung")$p.value
  }
  ggplot()+
    geom_point(aes(x = 1:h,y = p_values, shape = "p-value")) +
    geom_hline(color = "blue", yintercept = 0.05, linetype = "dashed") +
    xlab("Lag") +
    ylab("p-value") +
    coord_cartesian(ylim = c(0, 1)) +
    labs(title = "Ljung-Box Test for SARIMA(1,1,3)(8,1,0)[7] Residuals", shape = "Legend")
}
ljungbox(testmodel, 14)
