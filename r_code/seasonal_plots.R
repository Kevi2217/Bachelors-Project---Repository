langsyn <- esp %>%
  dplyr::mutate(hourdk = as.Date(hourdk)) %>%
  dplyr::filter(pricearea == "DK1") %>%
  dplyr::group_by(hourdk) %>%
  dplyr::summarise(daily_mean_spotpriceeur = round(mean(spotpriceeur), 2))

#whole period plot
ggplot(langsyn, aes(x = hourdk, y = daily_mean_spotpriceeur)) +
  geom_line(linewidth = 0.2) +
  labs(title = "Price Comparison over Time", x = "Time", y = "Price") +
  theme_minimal()

# Til seasonal analysis
esp_hourly <- esp %>%
  dplyr::mutate(hour = hour(as.POSIXct(hourdk))) %>% 
  dplyr::filter(pricearea == "DK1") %>% 
  dplyr::group_by(hour) %>%
  dplyr::summarise(avg_hourly_price = mean(spotpriceeur)) %>% 
  ungroup()

# Hourly plot
ggplot(esp_hourly, aes(x = hour,
                       y = avg_hourly_price)) +
  geom_point() +
  geom_line() +
  labs(title = "Hourly Price (ALL TIME)", x = "Hour", y = "Average Hourly Price") +
  theme_minimal()

# selected hours per week
esp_hourly_week <- esp %>%
  dplyr::filter(pricearea == "DK1") %>% 
  dplyr::mutate(hour = hour(as.POSIXct(hourdk))) %>%
  dplyr::mutate(weekday = weekdays(hourdk)) %>%
  dplyr::filter(hour == 3 | hour == 12 | hour == 18) %>%
  dplyr::mutate(hour = as.character(hour)) %>% 
  dplyr::group_by(weekday, hour) %>%
  dplyr::summarise(avg_hourly_price = mean(spotpriceeur)) %>% 
  ungroup()
esp_hourly_week$weekday <- factor(esp_hourly_week$weekday,
                                  levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                                             "Friday", "Saturday", "Sunday"))

# Hourly_week plot
ggplot(esp_hourly_week, aes(x = weekday,
                       y = avg_hourly_price, group = hour, color = hour)) +
  geom_point() + geom_line() +
  labs(title = "Selected hour though the week (ALL TIME)", x = "week day", y = "Average Hourly Price") +
  theme_minimal()


# Weekday dataframe
esp_weekday <- esp %>%
  dplyr::mutate(hourdk = as.Date(hourdk)) %>%
  dplyr::mutate(weekday = weekdays(hourdk)) %>%
  dplyr::filter(pricearea == "DK1") %>%
  dplyr::group_by(weekday) %>%
  dplyr::summarise(avg_weekday_price = mean(spotpriceeur)) %>% 
  ungroup()

# Weekday plot
s_weekday <- ggplot(esp_weekday, aes(x = factor(weekday,
                                   levels = c("Monday", "Tuesday",
                                              "Wednesday", "Thursday",
                                              "Friday", "Saturday", "Sunday")),
                        y = avg_weekday_price)) +
  geom_line(aes(group = 1), size = 1) +
  labs(title = "Average daily prices grouped by weekday", x = "Weekday", y = "Average Weekday Price")


# daily prices on a monthly basis
esp_monthly <- esp %>%
  dplyr::mutate(hourdk = as.Date(hourdk)) %>%
  dplyr::mutate(month = month(hourdk)) %>%
  dplyr::filter(pricearea == "DK1") %>% 
  dplyr::group_by(month) %>%
  dplyr::summarise(avg_monthly_price = mean(spotpriceeur))

esp_monthly$month <- factor(month.abb[esp_monthly$month], levels = month.abb)


# Monthly plot
s_monthly <- ggplot(esp_monthly, aes(x = month,
                        y = avg_monthly_price)) +
  geom_line(aes(group = 1), size = 1) +
  labs(title = "Average daily prices grouped by month", x = "Month", y = "Average Monthly Price")
par(mfrow = c(1, 1))



grid.arrange(s_weekday, s_monthly, ncol = 2)

# corona tjek
# corona <- esp %>%
#   dplyr::filter(pricearea == "DK1") %>% 
#   dplyr::mutate(hourdk = as.Date(hourdk)) %>%
#   dplyr::filter(hourdk >= "2016-01-01") %>% 
#   dplyr::mutate(ba = ifelse(hourdk >= "2020-01-01", "2020-2023", "2016-2019")) %>% 
#   dplyr::mutate(month = month(hourdk)) %>% 
#   dplyr::group_by(month, ba) %>%
#   dplyr::summarise(monthly_mean_spotpricedkk = round(mean(spotpricedkk), 2),
#                    monthly_mean_spotpriceeur = round(mean(spotpriceeur), 2))
# corona$month <- factor(month.abb[corona$month], levels = month.abb)
# 
# # before/after 2020 (Notes: Big difference in price and price-volatility)
# ggplot(corona, aes(x = month, y = monthly_mean_spotpriceeur,
#                     group = ba, color = ba)) +
#   geom_line(linewidth = 0.2) +
#   labs(title = "Monthly price before and after  (DK1)", x = "Month", y = "Price") +
#   theme_minimal()