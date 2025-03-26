rm(list = ls())

setwd("C:/Users/maliq/Projects/GlobalAlliances/")

# Sets constants
PERIOD_START = 1946
PERIOD_END = 2012
#   correlates of war country codes
USA_CODE = 2
RUS_CODE = 365

# Loads ideal point measures based on UN voting data.
# Source: Bailey, Michael A., Anton Strezhnev, and Erik Voeten. "Estimating dynamic state preferences from United Nations voting data." Journal of Conflict Resolution 61.2 (2017): 430-456.

un_ideal_points <- read.csv("Data/ideal_points/IdealpointestimatesAll_Jun2024.csv", header = T, sep = ",")

# Selects subset of data
un_ideal_points <- un_ideal_points[, c("ccode", "Countryname", "session", "IdealPointAll")]
un_ideal_points$year_estimate <- un_ideal_points$session + 1945

# STEP 1
# Calculates the difference in ideal points for each pair of countries for each year

for (year in unique(un_ideal_points$year_estimate)) {
  print(year)
  
  ip_diff_dyadic <- data.frame()
  un_ip_yearly <- un_ideal_points[un_ideal_points$year_estimate == year, ]
  
  for (i in 1:nrow(un_ip_yearly)) {
    ccode_1 <- un_ip_yearly$ccode[i]
    
    for (j in i:nrow(un_ip_yearly)) {
      if (i == j) {
        next
      }
      
      ccode_2 <- un_ip_yearly$ccode[j]
      
      ip_diff <- abs(un_ip_yearly$IdealPointAll[i] - un_ip_yearly$IdealPointAll[j])
      
      ip_diff_dyadic <- rbind(ip_diff_dyadic,
                              cbind(ccode_1, ccode_2, year, ip_diff))
    }
  }
  
  #saves data so far
  file_path <- paste("Data/ideal_points/ip_diff_", year, ".csv", sep = "")
  write.csv(ip_diff_dyadic, file_path, row.names = FALSE)
}


# STEP 2
# Calculates the average change in difference in ideal points for the past year, 5 years, and 10 years
ip_diff_all <- data.frame()
for (year in unique(un_ideal_points$year_estimate)) {
  print(year)
  # loads necessary files
  file_path <- paste("Data/ideal_points/ip_diff_", year, ".csv", sep = "")
  ip_diff_year <- read.csv(file_path, header = T, sep = ",")
 
  ip_diff_all <- rbind(ip_diff_all, ip_diff_year)
}

ip_diff_change <- ip_diff_all

#calculates differences
for (i in c(1, 5, 10)) {
  print(i)
  temp <- ip_diff_all
  temp$year <- temp$year + i
  names(temp) <- c("ccode_1", "ccode_2", "year", 
                   paste("ip_diff_", i, sep = ""))
  
  ip_diff_change <- merge(ip_diff_change, temp, 
                           by = c("ccode_1", "ccode_2", "year"), 
                           all.x = TRUE)
}

#divides differences by time passed
ip_diff_change$ip_diff_1 <- (ip_diff_change$ip_diff - ip_diff_change$ip_diff_1) / 1
ip_diff_change$ip_diff_5 <- (ip_diff_change$ip_diff - ip_diff_change$ip_diff_5) / 5
ip_diff_change$ip_diff_10 <- (ip_diff_change$ip_diff - ip_diff_change$ip_diff_10) / 10

names(ip_diff_change) <- c("ccode_1", "ccode_2", "year", "ip_diff", "ip_diff_change_1_past", "ip_diff_change_5_past", "ip_diff_change_10_past")

#saves data so far
file_path <- paste("Data/ideal_points/ip_diff_change", ".csv", sep = "")
write.csv(ip_diff_change, file_path, row.names = FALSE)

#import military capability data
nmc_data <- read.table(file = "Data/nmc/NMC-60-abridged.csv", sep = ",", header = TRUE)
nmc_data <- nmc_data[, c("ccode", "year", "cinc")]

ip_diff_change <- merge(ip_diff_change, nmc_data, by.x = c("ccode_1", "year"), by.y = c("ccode", "year"))
colnames(ip_diff_change)[colnames(ip_diff_change) == "cinc"] <- "cinc_1"

ip_diff_change <- merge(ip_diff_change, nmc_data, by.x = c("ccode_2", "year"), by.y = c("ccode", "year"))
colnames(ip_diff_change)[colnames(ip_diff_change) == "cinc"] <- "cinc_2"

#saves data modified with military capabiltiies
file_path <- paste("Data/ideal_points/ip_diff_change_cinc.csv", sep = "")
write.csv(ip_diff_change, file_path, row.names = FALSE)


#imports alliance_data
alli_data <- read.csv(file = "Data/alliance_data/alliance_v4.1_by_dyad.csv", sep = ",", header = TRUE)
alli_data <- alli_data[alli_data$defense == 1, c("ccode1", "ccode2", "dyad_st_year", "dyad_end_year")]
alli_data$def_alli <- 1
names(alli_data) <-  c("ccode_1", "ccode_2", "year", "end_year", "new_def_alli_current_year")

alli_data[is.na(alli_data$end_year), ]$end_year <- Inf

#swaps columns so that ccode 1 is always a lower number than ccode 2
to_swap <- alli_data$ccode1 > alli_data$ccode2
temp <- alli_data$ccode1[to_swap]
alli_data$ccode1[to_swap] <- alli_data$ccode2[to_swap]
alli_data$ccode2[to_swap] <- temp

#gets future allaince data
alli_data_future <- alli_data[, c("ccode_1", "ccode_2", "year", "new_def_alli_current_year")]
alli_data_future$year <- alli_data_future$year - 1
names(alli_data_future) <- c("ccode_1", "ccode_2", "year", "new_def_alli_next_year")

#merge ideal points data with alliance data
ip_diff_change <- merge(ip_diff_change, alli_data, by = c("ccode_1", "ccode_2", "year"), all.x = TRUE)
ip_diff_change <- merge(ip_diff_change, alli_data_future, by = c("ccode_1", "ccode_2", "year"), all.x = TRUE)

ip_diff_change$share_alli <- 0
ip_diff_change$already_allied <- 0

#for each dyad, identify whether countries already had a ongoing alliance 
# or if they share an ongoing alliance with another state

for (i in 1:nrow(ip_diff_change)) {
  print(i)
  
  current_ip_row <- ip_diff_change[i,]
  
  #selects all alliances that existed before the time of this dyad
  previous_allies <- alli_data[(alli_data$year < current_ip_row$year) & 
                                 (alli_data$end_year > current_ip_row$year), ]
  
  ccode_1_allies <- previous_allies[previous_allies$ccode_1 == current_ip_row$ccode_1 | 
                                      previous_allies$ccode_2 == current_ip_row$ccode_1, c("ccode_1", "ccode_2")]
  ccode_1_allies[ccode_1_allies == current_ip_row$ccode_1] <- -1
  
  ccode_2_allies <- previous_allies[previous_allies$ccode_1 == current_ip_row$ccode_2 | 
                                      previous_allies$ccode_2 == current_ip_row$ccode_2, c("ccode_1", "ccode_2")]
  ccode_2_allies[ccode_2_allies == current_ip_row$ccode_2] <- -2
  
  #check if countries already allied
  if(current_ip_row$ccode_1 %in% ccode_2_allies$ccode_1 |
     current_ip_row$ccode_1 %in% ccode_2_allies$ccode_2 |
     current_ip_row$ccode_2 %in% ccode_1_allies$ccode_1 |
     current_ip_row$ccode_2 %in% ccode_1_allies$ccode_2) {
    ip_diff_change$already_allied[i] <- 1
  } 
  
  ccode_1_allies[ccode_1_allies == current_ip_row$ccode_2] <- -1
  ccode_2_allies[ccode_2_allies == current_ip_row$ccode_1] <- -2
  
  #check if countries share any alliaes
  if (any(ccode_1_allies$ccode_1 %in% ccode_2_allies$ccode_1) | 
      any(ccode_1_allies$ccode_1 %in% ccode_2_allies$ccode_2) |
      any(ccode_1_allies$ccode_2 %in% ccode_2_allies$ccode_2)) {
    ip_diff_change$share_alli[i] <- 1
  }
}


file_path <- paste("Data/ideal_points/ip_diff_change_alli.csv", sep = "")
write.csv(ip_diff_change, file_path, row.names = FALSE)

#add mids to data
mid_data <- read.csv("Data/mid/dyadic_mid_4.03.csv")
mid_data <- mid_data[mid_data$durindx == 1, c("statea", "stateb", "strtyr", "hihost")]

to_switch <- mid_data$statea > mid_data$stateb
temp <- mid_data[to_switch,]$statea
mid_data[to_switch,]$statea <- mid_data[to_switch,]$stateb
mid_data[to_switch,]$stateb <- temp

mid_data <- aggregate(mid_data$hihost, (mid_data[, c("statea", "stateb", "strtyr")]), max)
names(mid_data) <- c("statea", "stateb", "strtyr", "hihost")

ip_diff_change <- read.csv("Data/ideal_points/ip_diff_change_alli.csv")
ip_diff_change <- merge(ip_diff_change, mid_data, by.x = c("ccode_1", "ccode_2", "year"), by.y = c("statea", "stateb", "strtyr"), all.x = TRUE)

mid_data$strtyr <- mid_data$strtyr - 1
names(mid_data) <- c("statea", "stateb", "strtyr", "hihost_next_year")
ip_diff_change <- merge(ip_diff_change, mid_data, by.x = c("ccode_1", "ccode_2", "year"), by.y = c("statea", "stateb", "strtyr"), all.x = TRUE)

write.csv(ip_diff_change, "Data/ideal_points/ip_diff_change_alli_mid.csv", row.names = FALSE)


ip_diff_change_future <- ip_diff_change
#calculates differences
for (i in c(1, 5, 10)) {
  print(i)
  temp <- ip_diff_change
  temp$year <- temp$year - i
  names(temp) <- c("ccode_1", "ccode_2", "year", 
                   paste("ip_diff_", i, sep = ""))
  temp <- temp[, c("ccode_1", "ccode_2", "year", 
                   paste("ip_diff_", i, sep = ""))]
  
  ip_diff_change_future <- merge(ip_diff_change_future, temp, 
                          by = c("ccode_1", "ccode_2", "year"), 
                          all.x = TRUE)
}

ip_diff_change_future$ip_diff_1 <- (ip_diff_change_future$ip_diff_1 - ip_diff_change_future$ip_diff) / 1
ip_diff_change_future$ip_diff_5 <- (ip_diff_change_future$ip_diff_5 - ip_diff_change_future$ip_diff) / 5
ip_diff_change_future$ip_diff_10 <- (ip_diff_change_future$ip_diff_10 - ip_diff_change_future$ip_diff) / 10

names(ip_diff_change_future)[17:19] <- c("ip_diff_change_1_future", "ip_diff_change_5_future", "ip_diff_change_10_future")

write.csv(ip_diff_change_future, "Data/ideal_points/ip_diff_change_alli_mid_past_fut.csv", row.names = FALSE)
