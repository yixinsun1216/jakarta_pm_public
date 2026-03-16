# clean outdoor pm data for those in the fan control group

pacman::p_load(tidyverse, lubridate, data.table, readxl, sf)

rm(list=ls())
gc()
if(Sys.getenv("USER") == "yixinsun1"){
  ddir <- file.path("/Users/yixinsun1/Dropbox/Research/pollution_experience/fullscale")
  gdir <- file.path("/Users/yixinsun1/Documents/Github/jakarta_pm")
} else if(Sys.getenv("USER") == "jeannesorin"){
  gdir <- file.path("/Users/jeannesorin/github/jakarta_pm")
  ddir <- file.path("/Users/jeannesorin/Dropbox/pollution_experience/fullscale")
}

# load in baseline data to get respondent location
# also add column with distance to the 2 us embassy monitors
coords_south <- st_sfc(st_point(c(106.793244, -6.236704)), crs = 4326)
coords_central <- st_sfc(st_point(c(106.834236,-6.182536)), crs = 4326)


respondent <-
  readRDS(file.path(ddir, "generated_data/survey/baseline.RDS")) %>%
  filter(treatment_status == "Fan") %>%
  dplyr::select(respondent_id, longitude, latitude, round) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4326))%>%
  mutate(dist_central = as.numeric(st_distance(geometry, coords_central)),
        dist_south = as.numeric(st_distance(geometry, coords_south)))

# ===========================================================
# read in outdoor data at the sensor level
# ===========================================================
outdoor_files <- list.files(file.path(ddir, "data/outdoor"), pattern = "outdoor", full.names = TRUE)

outdoor <- data.table(time = POSIXct(), sensor_name = character(), measure = character(), value = numeric())

for(i in 1:length(outdoor_files)){
  print(outdoor_files[i])

  df <- read_excel(outdoor_files[i]) %>%
    filter(str_detect(measure, regex("PM2|Temp|humidity", ignore_case = TRUE))) %>%
    mutate(measure = case_when(str_detect(measure, "humidity") ~ "humidity",
                               str_detect(measure, "Temp") ~ "temp",
                               str_detect(measure, "PM2") ~ "pm25")) %>%
    as.data.table()
  df[, time := mdy_hm(time_raw)]

  # sometimes the dates are in dmy format instead of mdy
  alt_date <- c("20240721", "20240722", "20240728", "20240711-2", "20240711", 
                "20240710", "20240803", "20240908")

  if (str_replace_all(basename(outdoor_files[i]), "outdoor_|.xlsx", "") %in% alt_date) {
    df[, time := dmy_hm(time_raw)]
  }
  
  # Make sure data is on the same 10 minute intervals 
  df[, `:=`(minute = minute(time),
            hour = hour(time),
            date = date(time))]
  df[, minute := floor((minute - 5) / 10) * 10 + 5]
  df[minute < 0 & hour == 0, date := date - 1]
  df[minute < 0, `:=`(hour = 23, minute = 55)]
  df[, time := ymd_hm(paste(date, hour, minute))]
  
  df <- df[, .(value = mean(value, na.rm = TRUE)), 
           by = .(time, sensor_name, measure)]
  
  df <- df[!outdoor, on = .(time, sensor_name, measure)]
  
  print(unique(date(df$time)))
  
  outdoor <- rbind(outdoor, df)
}

tibble(time = seq(min(outdoor$time), max(outdoor$time), by = "10 min")) %>%
  mutate(date_hour = floor_date(time, "hour")) %>%
  dplyr::select(-time) %>%
  anti_join(mutate(outdoor, date_hour = floor_date(time, "hour"))) %>%
  count(date(date_hour))

outdoor %>% count(minute(time))

outdoor <- dplyr::select(outdoor, sensor_name, time, value, measure)
gc()

write_rds(outdoor, file.path(ddir, "generated_data/pm/df_outdoor_10min_bysensor.rds"))

# ===========================================================
# find distance between respondent long/lat and sensor long/lat
# ===========================================================
outdoor_location <-
  file.path(ddir, "data/tableau/Sensor Lon Lat_data.csv") %>%
  read_csv() %>%
  pivot_wider(id_cols = "name", names_from = "Measure Names", values_from = "Measure Values")  %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4326), agr = "constant") %>%
  # only keep sensors for which we have data
  inner_join(distinct(outdoor, name = sensor_name))
# 
# 
# stopifnot(outdoor_location %>% group_by(name) %>% filter(n() > 1) %>% nrow() == 0)
# 
# 
# stopifnot(respondent %>% group_by(respondent_id) %>% filter(n() > 1) %>% nrow() == 0)
# 
# # find pairwise distance between outdoor sensors and repsondents
# pairs <- expand_grid(name = unique(outdoor_location$name),
#                      respondent_id = unique(respondent$respondent_id)) %>%
#   left_join(rename(respondent, respondent_geometry = geometry)) %>%
#   left_join(outdoor_location)
# pairs$distance <- st_distance(pairs$respondent_geometry, pairs$geometry, by_element = TRUE)
# 
# 
# 
# pairs %>%
#   left_join(st_set_geometry(respondent, NULL)) %>%
#   dplyr::select(name, respondent_id, distance, dist_central, dist_south, round) %>%
#   write_csv(file.path(ddir, "generated_data/pm/hh_sensor_dist_control.csv"))

pairs <- read_csv(file.path(ddir, "generated_data/pm/hh_sensor_dist_control.csv"))

# restrict to just 10 closest sensors
pairs10 <-
  pairs %>%
  dplyr::select(name, respondent_id, distance, round) %>%
  mutate(distance = as.numeric(distance)) %>%
  group_by(respondent_id) %>%
  arrange(distance) %>%
  mutate(nclose = row_number()) %>%
  filter(nclose < 11) %>%
  ungroup %>%
  rename(sensor_name = name)

pairs10 <- as.data.table(pairs10)

# save sensor location information

sensor_locations <- 
  inner_join(outdoor_location, distinct(pairs10, name = sensor_name)) %>%
  left_join(count(filter(pairs10, nclose < 4), sensor_name), by = c(name = "sensor_name")) %>%
  mutate(n3 = !is.na(n), 
         dist_central = as.numeric(st_distance(geometry, coords_central)), 
         dist_south = as.numeric(st_distance(geometry, coords_south))) %>%
  dplyr::select(-n)

st_write(sensor_locations, file.path(ddir, "generated_data/pm/sensor_locations/sensor_locations.shp"), append = FALSE)

# ===========================================================
# then let's make 2 sets of outdoor data for each respondent
# 1. distance weighted by closest 3 sensors
# 2. distance weighted by closest 10 sensors 
# ===========================================================
outdoor_round1 <- filter(outdoor, date(time) <= ymd(20240908))
outdoor_round2 <- filter(outdoor, date(time) >= ymd(20240820))


# outdoor3
outdoor3 <- 
  merge(outdoor_round1, pairs10[nclose < 4 & round == 1], by = "sensor_name", all.y = TRUE, allow.cartesian = TRUE) %>%
  rbind(merge(outdoor_round2, pairs10[nclose < 4 & round == 2], by = "sensor_name", all.y = TRUE, allow.cartesian = TRUE))
outdoor3 <- outdoor3[!is.na(value), .(value = sum((1 / distance) * value) / sum(1 / distance)), 
                     by = .(respondent_id, time, measure)]

# Convert outdoor3 to wide format
outdoor3_wide <- dcast(outdoor3[value != 0], respondent_id + time ~ measure, 
                       value.var = "value")
setnames(outdoor3_wide, 
         old = c("pm25", "temp", "humidity"), 
         new = c("pm25_outdoor3", "temp_outdoor3", "humidity_outdoor3"))
rm(outdoor3)
gc()

# distance weighted by closest 10 sensors
outdoor10 <- 
  merge(outdoor_round1, pairs10[round == 1], by = "sensor_name", all.y = TRUE, allow.cartesian = TRUE) %>%
  rbind(merge(outdoor_round2, pairs10[round == 2], by = "sensor_name", all.y = TRUE, allow.cartesian = TRUE))
outdoor10 <- outdoor10[!is.na(value), .(value = sum((1 / distance) * value) / sum(1 / distance)), 
                       by = .(respondent_id, time, measure)]
outdoor10_wide <- dcast(outdoor10[value != 0], respondent_id + time ~ measure, 
                       value.var = "value")

setnames(outdoor10_wide, 
         old = c("pm25", "temp", "humidity"), 
         new = c("pm25_outdoor10", "temp_outdoor10", "humidity_outdoor10"))

rm(outdoor10)
gc()

# make sure outdoor data is balanced
outdoor_full <- 
  expand_grid(respondent_id = pull(filter(respondent, round == 1), respondent_id), 
              time = seq(min(outdoor_round1$time), max(outdoor_round1$time), by = "10 min")) %>%
  bind_rows(expand_grid(respondent_id = pull(filter(respondent, round == 2), respondent_id), 
                        time = seq(min(outdoor_round2$time), max(outdoor_round2$time), by = "10 min")))

#stopifnot(nrow(outdoor_full) == nrow(outdoor3_wide))

outdoor_data <- merge(outdoor3_wide, outdoor10_wide, by = c("respondent_id", "time"), all = TRUE)
outdoor_data <- merge(outdoor_data, as.data.table(outdoor_full), by = c("respondent_id", "time"), all = TRUE)
outdoor_data[, date := date(time)]

write_rds(outdoor_data, file.path(ddir, "generated_data/pm/df_outdoor_10min.rds"))


