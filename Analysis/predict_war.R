rm(list = ls())

setwd("C:/Users/maliq/Projects/GlobalAlliances/")

library(car)

# Sets constants
PERIOD_START = 1946
PERIOD_END = 2012
USA_CCODE = 2
UKG_CCODE = 200
FRN_CCODE = 220
CHN_CCODE = 710
RUS_CCODE = 365

#countries in secuirty council
SC_COUNTRY_CODES <- c(USA_CCODE, UKG_CCODE, FRN_CCODE, CHN_CCODE, RUS_CCODE)


raw_data <- read.csv("Data/ideal_points/ip_diff_change_alli_mid_past_fut.csv", sep = ",", header = TRUE)

raw_data[is.na(raw_data$new_def_alli_current_year), ]$new_def_alli_current_year <- 0
raw_data[is.na(raw_data$new_def_alli_next_year), ]$new_def_alli_next_year <- 0

raw_data[is.na(raw_data$hihost), ]$hihost <- 0
raw_data[is.na(raw_data$hihost_next_year), ]$hihost_next_year <- 0

raw_data[raw_data$hihost > 0, ]$hihost <- 1
raw_data[raw_data$hihost_next_year > 0, ]$hihost_next_year <- 1

to_swap <- raw_data$cinc_1 > raw_data$cinc_2
temp <- raw_data[to_swap, c("ccode_1", "cinc_1")]
raw_data[to_swap, c("ccode_1", "cinc_1")] <- raw_data[to_swap, c("ccode_2", "cinc_2")]
raw_data[to_swap, c("ccode_2", "cinc_2")] <- temp



raw_data$cinc_ratio <- raw_data$cinc_1 / raw_data$cinc_2
raw_data$cinc_mean <- (raw_data$cinc_1 + raw_data$cinc_2) / 2


# predict defense alliance next year
model <- glm(new_def_alli_next_year ~
               ip_diff +
               ip_diff_change_1_past + 
               ip_diff_change_5_past +
               ip_diff_change_10_past +
               cinc_mean +
               cinc_ratio +
               share_alli +
               already_allied +
               year,
             data = raw_data[raw_data$year <= 2011, ],
             family = "binomial")

summary(model)

# predict disputes next year
model <- glm(hihost_next_year ~
               ip_diff +
               ip_diff_change_1_past + 
               ip_diff_change_5_past +
               ip_diff_change_10_past +
               share_alli +
               already_allied +
               year +
               cinc_ratio +
               cinc_mean,
             data = raw_data[raw_data$year <= 2013, ],
             family = "binomial")

summary(model)

#predict future ip change

model <- lm(cbind(ip_diff_change_1_future, ip_diff_change_5_future, ip_diff_change_10_future) ~
              new_def_alli_current_year +
              hihost +
              ip_diff +
              ip_diff_change_1_past + 
              ip_diff_change_5_past +
              ip_diff_change_10_past +
              share_alli +
              already_allied +
              year +
              cinc_ratio +
              cinc_mean,
             data = raw_data[raw_data$year <= 2012, ])

summary(model)

vif(lm(ip_diff_change_5_future ~
         year +
         new_def_alli_current_year +
         hihost +
         ip_diff +
         ip_diff_change_1_past + 
         ip_diff_change_5_past +
         ip_diff_change_10_past +
         cinc_mean +
         cinc_ratio +
         share_alli +
         already_allied,
       data = raw_data[raw_data$year <= 2014, ]))


# AMERICA DATA

model <- lm(cbind(ip_diff_change_1_future, ip_diff_change_5_future, ip_diff_change_10_future) ~
              new_def_alli_current_year +
              hihost +
              ip_diff +
              ip_diff_change_1_past + 
              ip_diff_change_5_past +
              ip_diff_change_10_past +
              share_alli +
              already_allied +
              year +
              cinc_ratio +
              cinc_mean,
            data = raw_data[raw_data$cinc_ratio > 0.1, ])

summary(model)

