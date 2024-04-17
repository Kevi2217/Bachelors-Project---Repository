library(dplyr)
library(tidyr)
library(lubridate)

esp <- elspotprices_19_24 %>%
  rename_all(tolower)

esp_daily <- esp %>%
  dplyr::mutate(hourdk = as.Date(hourdk)) %>%
  dplyr::group_by(pricearea, hourdk) %>%
  dplyr::summarise(DAP_DKK = round(mean(spotpricedkk), 2),
                   DAP_EUR = round(mean(spotpriceeur), 2)) %>% 
  dplyr::filter(pricearea == "DK1")

# Converting data
delu <- esp_daily %>%
  mutate(ddmmyy = dmy(ddmmyy)) %>%
  mutate(DAP_return = c(NA, diff(DAP) / lag(DAP)) * 100) %>%
  mutate(DAP_delta = diff(DAP)) %>%
  na.omit() %>%
  select(-ddmmyy)

# Contains the prices of the exogenuous variables
exp <- raw_csv_prices %>%
  mutate(ddmmyy = dmy(ddmmyy)) %>%
  select(-ddmmyy)

# Data analysis
summary(esp_daily$DAP_EUR)
adf.test(esp_daily$DAP_EUR)
acf(esp_daily$DAP_EUR, lag.max = 50, main = "ACF of Electricity Price Differences in DE-LU BZN in October 2019- March 2022")
pacf(esp_daily$DAP_EUR, lag.max = 50, main = "PACF of Electricity Price Differences in DE-LU BZN in October 2019- March 2022")
decomposition <- decompose(ts(esp_daily$DAP_EUR, frequency = 30), type = "additive")
plot(decomposition)

# Splitting the data
size <- round(nrow(delu) * 0.9)
df <- delu[1:size, ]
df_test <- delu[(size + 1):nrow(delu), ]

# SARIMAX model construction
mod <- arima(df$DAP, order = c(1, 1, 2), seasonal = list(order = c(3, 0, 0, 7)),
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