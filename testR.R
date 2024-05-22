library(dplyr)
library(tidyr)
library(lubridate)
library(tseries)
library(MASS)
library(forecast)
library(ggplot2)

elspotprices_19_24 <- elspotprices_19_24 %>% 
  rename_all(tolower)

esp <- elspotprices_19_24 %>%
  dplyr::mutate(hourdk = as.Date(hourdk)) %>%
  dplyr::group_by(pricearea, hourdk) %>%
  dplyr::summarise(DAP_DKK = round(mean(spotpricedkk), 2),
                   DAP_EUR = round(mean(spotpriceeur), 2)) %>% 
  dplyr::filter(pricearea == "DK1")

# esp <- esp %>% 
#   ungroup() %>% 
#   dplyr::select(-DAP_DKK,-pricearea) %>%
#   dplyr::mutate(DAP_diff = c(NA,diff(DAP_EUR))) %>%
#   na.omit()


# Test with log-differenced prices
test <- esp %>%
  mutate(DAP_log_EUR = log(DAP_EUR + 3*abs(min(DAP_EUR)))) %>%
  mutate(DAP_log_diff = c(NA,diff(DAP_log_EUR))) %>%
  mutate(DAP_log_diff = c(NA,diff(DAP_log_diff, lag = 7)))
  na.omit()

  
test_ts <- ts(esp$DAP_EUR, start = 2019, frequency = 1)

test_ts2 <- log(test_ts+3*abs(min(test_ts))) %>% 
  diff() %>% 
  diff(lag = 7)

ggplot(test, aes(x= hourdk, y= DAP_log_EUR)) +
  geom_line() +
  labs(title = "DAILY Price (ALL TIME)", x = "Time", y = "Daily Price") +
  theme_minimal()


# Data analysis
summary(esp$DAP_EUR)
adf.test(esp$DAP_EUR)
acf(esp$DAP_EUR, lag.max = 50, main = "ACF of Electricity Price Differences in DE-LU BZN in October 2019- March 2022")
pacf(esp$DAP_EUR, lag.max = 50, main = "PACF of Electricity Price Differences in DE-LU BZN in October 2019- March 2022")
decomposition <- decompose(ts(esp$DAP_EUR, start = 2019, frequency = 365), type = "additive")
autoplot(decomposition)


# Splitting the data
size <- round(nrow(delu) * 0.9)
df <- delu[1:size, ]
df_test <- delu[(size + 1):nrow(delu), ]


# SARIMAX model construction
mod <- arima(esp$DAP_EUR, order = c(1, 1, 2), seasonal = list(order = c(3, 0, 0, 7)),
             xreg = df[, c('ttf', 'api2', 'CO2', 'Load', 'Coal', 'Gas', 'Oil', 'Hydro', 
                           'Pumped', 'Wind', 'Solar', 'Biofuels', 'Nuclear', 'AT_net', 
                           'BE_net', 'CH_net', 'CZ_net', 'DK1_net', 'DK2_net', 'FR_net', 
                           'NL_net', 'NO2_net', 'SE4_net')], 
             method = "ML")
summary(mod)

# Extracting residuals
sarimax_res <- residuals(mod)
sarimax_res_ret <- abs(diff(sarimax_res) / df$DAP[-1])

# Plotting ACF and PACF for residual returns
acf(sarimax_res_ret, lag.max = 50, main = "ACF of Residual Returns")
pacf(sarimax_res_ret, lag.max = 50, main = "PACF of Residual Returns")

# Fitting GARCH model
garch_mod <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                        mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                        distribution.model = "std")
garch_fit <- ugarchfit(spec = garch_mod, data = sarimax_res_ret)
summary(garch_fit)

# Forecasting residual returns by GARCH model
n_steps <- 30
forecast_garch <- ugarchforecast(garch_fit, n.ahead = n_steps)
pred_garch <- as.data.frame(forecast_garch@forecast$standardDeviation)
pred_garch$Date <- seq(Sys.Date() + 1, by = "day", length.out = n_steps)
colnames(pred_garch) <- c("Volatility", "Date")
