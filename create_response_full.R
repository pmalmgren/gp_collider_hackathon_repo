#### Set up libraries
library(remotes)
#install_github('GlobalParametrics/taRpan_readonly')
library(taRpan)
library(ggplot2)
library(dplyr)
library(fitdistrplus)
library(rgeos)
library(glue)
library(forcats)

## Other setup ----
cities <- c('Shanghai', 'Istanbul', 'Buenos Aires', 'Mumbai', 'Mexico City')
model <- 'cfs2_hindcast'

k_to_f <- function(k){
  (k-273.15)*9/5+32
  }

## Min temp ----
min_temp <- tarpan_model_data(dbname = 'collider_heat_stress',
                              geography = cities,
                              variable = 'min_temperature_gmtday',
                              model = model, bc_method = '')

min_temp <- min_temp %>%
  mutate(day_month = format(initial_time, "%m-%d"),
         valmin = k_to_f(val)) 

min_temp <- min_temp %>%
  group_by(gid) %>%
  mutate(cut_25_total = quantile(valmin, .25)) %>%
  group_by(gid, day_month) %>%
  mutate(avg_day_valmin = mean(valmin)) %>%
  mutate(below_25 = ifelse(avg_day_valmin <= cut_25_total, 1, 0)) %>%
  mutate(cut_10 = quantile(valmin, probs = .1)) %>%
  mutate(below_10 = ifelse(valmin < cut_10, 1, 0)) %>%
  ungroup()

min_temp <- min_temp %>%
  group_by(gid) %>%
  mutate(AvgRel4_check_10 = below_10 +
           lag(below_10, n = 1, order_by = initial_time) +
           lag(below_10, n = 2, order_by = initial_time) +
           lag(below_10, n = 3, order_by = initial_time)) %>%
  mutate(AvgRel4_10 = ifelse(AvgRel4_check_10 == 4 |
                               lead(AvgRel4_check_10, n = 1, order_by = initial_time) == 4 |
                               lead(AvgRel4_check_10, n = 2, order_by = initial_time) == 4 |
                               lead(AvgRel4_check_10, n = 3, order_by = initial_time) == 4, 1, 0))

min_temp <- min_temp %>%
  mutate(AvgRel4_10 = ifelse(is.na(AvgRel4_10), 0, AvgRel4_10),
         below_25 = ifelse(is.na(below_25), 0, below_25))

min_temp <- min_temp %>%
  mutate(AvgRel4_10 = ifelse(below_25 == 1, AvgRel4_10, 0))

#### Max Temp ----
max_temp <- tarpan_model_data(dbname = 'collider_heat_stress',
                              geography = cities,
                              variable = 'max_temperature_gmtday',
                              model = model, bc_method = bias_correction)

max_temp <- max_temp %>%
  mutate(day_month = format(initial_time, "%m-%d"),
         valmax = k_to_f(val)) 

max_temp <- max_temp %>%
  group_by(gid) %>%
  mutate(cut_75_total = quantile(valmax, .75)) %>%
  group_by(gid, day_month) %>%
  mutate(avg_day_valmax = mean(valmax)) %>%
  mutate(above_75 = ifelse(avg_day_valmax >= cut_75_total, 1, 0)) %>%
  mutate(cut_90 = quantile(valmax, probs = .9)) %>%
  mutate(above_90 = ifelse(valmax > cut_90, 1, 0)) %>%
  ungroup()

max_temp <- max_temp %>%
  group_by(gid) %>%
  mutate(AvgRel4_check_90 = above_90 +
           lag(above_90, n = 1, order_by = initial_time) +
           lag(above_90, n = 2, order_by = initial_time) +
           lag(above_90, n = 3, order_by = initial_time)) %>%
  mutate(AvgRel4_90 = ifelse(AvgRel4_check_90 == 4 |
                               lead(AvgRel4_check_90, n = 1, order_by = initial_time) == 4 |
                               lead(AvgRel4_check_90, n = 2, order_by = initial_time) == 4 |
                               lead(AvgRel4_check_90, n = 3, order_by = initial_time) == 4, 1, 0))

max_temp <- max_temp %>%
  mutate(above_90 = ifelse(is.na(above_90), 0, above_90),
         AvgRel4_90 = ifelse(is.na(AvgRel4_90), 0, AvgRel4_90))

max_temp <- max_temp %>%
  mutate(AvgRel4_90 = ifelse(above_75 == 1, AvgRel4_90, 0))

#### Combine ----
min_temp <- min_temp %>%
  filter(initial_time >= '2010-01-01' & initial_time <= '2015-12-31')

min_temp <- min_temp %>%
  mutate(this_color = ifelse(AvgRel4_10 == 1, "really cold",
                             ifelse(below_25 == 1, "cold", "neither")))

max_temp <- max_temp %>%
  filter(initial_time >= '2010-01-01' & initial_time <= '2015-12-31')

max_temp <- max_temp %>%
  mutate(this_color = ifelse(AvgRel4_90 == 1, "really hot",
                             ifelse(above_75 == 1, "hot", "neither")))

gid_names <- list(
  '1796236' = 'Shanghai',
  '745044'= 'Istanbul',
  '3435910' = 'Buenos Aires',
  '1275339' = 'Mumbai',
  '3530597' = 'Mexico City'
)

gid_names <- data.frame(
  gid = c(1796236, 745044, 3435910, 1275339, 3530597),
  city_name = c('Shanghai', 'Istanbul', 'Buenos Aires', 'Mumbai', 'Mexico City')
)

min_temp <- min_temp %>%
  inner_join(., gid_names)

max_temp <- max_temp %>%
  inner_join(., gid_names)

min_temp$this_color = factor(min_temp$this_color, levels = c("really cold",
                                                "cold",
                                                "neither",
                                                "hot",
                                                "really hot"), ordered = TRUE)
max_temp$this_color = factor(max_temp$this_color, levels = c("really cold",
                                                             "cold",
                                                             "neither",
                                                             "hot",
                                                             "really hot"), ordered = TRUE)

ggplot() +
  geom_line(data = min_temp, aes(initial_time, valmin, col = this_color, group = 1)) +
  geom_line(data = max_temp, aes(initial_time, valmax, col = this_color, group = 1)) +
  facet_wrap(city_name ~ .) +
  scale_x_date("Date") +
  scale_y_continuous("Temperature") +
  scale_color_manual("Extremes",
                     values = c("darkblue", "darkred",
                                "goldenrod",
                                "lightblue", "red"),
                     breaks = c("really cold",
                                "cold",
                                "neither",
                                "hot",
                                "really hot"))
ggsave(file = "temp_graph.png", width = 7)

#### Model the data
library('forecast')
library('tseries')

unique(min_temp$gid)

## Shanghai 1796236 ----
## Min
min_1275339 <- ts(min_temp[min_temp$gid == 1796236, ]$valmin,
                  frequency = 365)
decomp = stl(min_1275339, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)

fit_min_1275339 <- auto.arima(min_1275339, seasonal = FALSE)
fcast_min_1275339 <- forecast(fit_min_1275339, h=4)
plot(fcast_min_1275339)

min_1275339 <- min_temp %>%
  filter(gid == 1275339) %>%
  filter(day_month %in% c('03-15', '03-16', '03-17', '03-18'))

min_1275339$forecast <- fcast_min_1275339$mean
min_1275339 <- min_1275339 %>%
  mutate(predict_extreme = ifelse(below_25 == 1 &
                                    forecast <= cut_10, 1, 0))
sum(min_1275339$predict_extreme)

## max
max_1275339 <- ts(max_temp[max_temp$gid == 1796236, ]$valmax,
              frequency = 365)
decomp = stl(max_1275339, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)

fit_max_1275339 <- auto.arima(max_1275339, seasonal = FALSE)
fcast_max_1275339 <- forecast(fit_max_1275339, h=4)
plot(fcast_max_1275339)

max_1275339 <- max_temp %>%
  filter(gid == 1275339) %>%
  filter(day_month %in% c('03-15', '03-16', '03-17', '03-18'))

max_1275339$forecast <- fcast_max_1275339$mean
max_1275339 <- max_1275339 %>%
  mutate(predict_extreme = ifelse(above_75 == 1 &
                                    forecast >= cut_90, 1, 0))
sum(max_1275339$predict_extreme)
