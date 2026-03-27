# read in outdoor PM data
outdoor_raw <- 
  list.files(file.path(raw_dir, "pm_hourly"), pattern = "outdoor", full.names = TRUE) %>%
  map_df(function(x){
    date <- ymd(str_replace_all(basename(x), "outdoor_hourly|.xlsx", "")) 
    if(!is.na(date)) {
      read_excel(x) %>%
        mutate(date_pulled = ymd(str_extract(basename(x), "[[0-9]]+")))
    }
  }) %>%
  mutate(measure = str_to_lower(str_replace_all(measure, "Avg. |,| \\(.*\\)", ""))) 

# separately downloaded outdoor data for the week before monitors started going online,
# from april 24 - May 14th
prebaseline1 <-
  read_excel(file.path(raw_dir, "prebaseline1.xlsx"), skip = 1,
             col_names = c("sensor_name", "time_raw","humidity",  "pm1", "pm10", "pm25", "pressure", "temp"))
prebaseline2 <-
  read_excel(file.path(raw_dir, "prebaseline2.xlsx"), skip = 1,
             col_names = c("sensor_name", "time_raw","humidity",  "pm1", "pm10", "pm25", "pressure", "temp"))

outdoor <- 
  bind_rows(prebaseline1, prebaseline2) %>%
  fill(sensor_name, .direction = "down") %>%
  pivot_longer(cols = -c(sensor_name, time_raw), names_to = "measure") %>%
  mutate(time = dmy_hm(time_raw),
         date = date(time),
         date_hour = floor_date(time, "hour")) %>%
  dplyr::select(-time, -time_raw) %>%
  filter(date >= ymd(20240424), !is.na(value)) %>%
  bind_rows(outdoor_raw) %>%
  filter(sensor_name != "Muara Karang")  %>% # Muara Karang has abnormally low pm2.5 which is quite suspicious
  mutate(measure = if_else(str_detect(measure, "temp"), "temp", measure),
         value = if_else(measure == "pm25" & value > 500, 500, value))  %>%
  group_by(sensor_name, date_hour, measure) %>%
  arrange(desc(date_pulled)) %>%
  filter(row_number() == 1) %>%
  ungroup %>%
  dplyr::select(-date_pulled) %>%
  filter(!is.na(value))

stopifnot(group_by(outdoor, sensor_name, measure, date_hour) %>% filter(n() > 1) %>% nrow() == 0)

fwrite(as.data.table(outdoor), file.path(ddir, "pm_outdoor_bysensor.csv"))



# ===========================================================
# first we want to use sf to find distance between respondent long/lat and sensor long/lat
# ===========================================================
outdoor_location <-
  file.path(raw_dir, "sensor_locations.csv") %>%
  read_csv() %>%
  pivot_wider(id_cols = "name", names_from = "Measure Names", values_from = "Measure Values")  %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4326), agr = "constant") %>%
  # only keep sensors for which we have data
  inner_join(distinct(outdoor, name = sensor_name))


stopifnot(outdoor_location %>% group_by(name) %>% filter(n() > 1) %>% nrow() == 0)

respondent <-
  fread(file.path(ddir, "df_survey.csv")) %>%
  dplyr::select(respondent_id, longitude, latitude, round) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4326))

stopifnot(respondent %>% group_by(respondent_id) %>% filter(n() > 1) %>% nrow() == 0)

# find pairwise distance between outdoor sensors and repsondents
# pairs <- expand_grid(name = unique(outdoor_location$name),
#                      respondent_id = unique(respondent$respondent_id)) %>%
#   left_join(rename(respondent, respondent_geometry = geometry)) %>%
#   left_join(outdoor_location)
# pairs$distance <- st_distance(pairs$respondent_geometry, pairs$geometry, by_element = TRUE)

# pairs %>%
#   left_join(st_set_geometry(respondent, NULL)) %>%
#   dplyr::select(name, respondent_id, distance, dist_central, dist_south) %>%
#   write_csv(file.path(ddir, "generated_data/pm/hh_sensor_dist.csv"))

pairs <- read_csv(file.path(raw_dir, "hh_sensor_dist_control.csv"))

# restrict to just 10 closest sensors
pairs10 <- 
  pairs %>%
  dplyr::select(name, respondent_id, distance) %>%
  mutate(distance = as.numeric(distance)) %>%
  group_by(respondent_id) %>%
  arrange(distance) %>%
  mutate(nclose = row_number()) %>%
  filter(nclose < 11) %>%
  ungroup %>% 
  rename(sensor_name = name) 

rm(pairs)
gc()


# ===========================================================
# then let's make 2 sets of outdoor data for each respondent
# 1. distance weighted by closest 3 sensors
# 2. distance weighted by closest 10 sensors 
# ===========================================================
# dates for rounds
round1_dates <- c(ymd_h("20240424 01"), ymd_h("20240822 00"))
round2_dates <- c(ymd_h("20240822 01"), max(outdoor$date))

outdoor3_round1 <-
  filter(pairs10, nclose < 4) %>%
  inner_join(dplyr::select(filter(st_set_geometry(respondent, NULL), round == 1), respondent_id, round)) %>%
  left_join(filter(outdoor, date_hour < round2_dates[1]))

outdoor3_round2 <-
  filter(pairs10, nclose < 4) %>%
  inner_join(dplyr::select(filter(st_set_geometry(respondent, NULL), round == 2), respondent_id, round)) %>%
  left_join(filter(outdoor, date_hour >= round2_dates[1])) 


outdoor3_wide <- 
  bind_rows(outdoor3_round1, outdoor3_round2) %>%
  filter(!is.na(value)) %>% 
  mutate(weight = 1 / distance) %>% 
  group_by(respondent_id, date, date_hour, measure) %>%
  summarise(value = sum(weight * value) / sum(weight)) %>%
  ungroup %>%
  filter(value != 0) %>% 
  pivot_wider(id_cols = c(respondent_id, date, date_hour), 
              names_from = measure, values_from = value, 
              names_glue = "{measure}_outdoor3")

gc()

# outdoor10 <- 
#   outdoor %>%
#   right_join(pairs10) %>%
#   filter(!is.na(value)) %>% 
#   mutate(weight = 1 / distance) %>% 
#   group_by(respondent_id, date, date_hour, measure) %>%
#   summarise(value = sum(weight * value) / sum(weight)) %>%
#   ungroup
# 
# outdoor10 <- merge(as.data.table(outdoor), as.data.table(pairs10), by = "sensor_name", all.y = TRUE, allow.cartesian = TRUE)
# outdoor10[, weight := 1 / distance]
# outdoor10 <- outdoor10[, .(value = sum(weight * value, na.rm = TRUE) / sum(weight, na.rm = TRUE)), 
#                        by = .(respondent_id, date, date_hour, measure)]
# 
# 
# outdoor10_wide <- 
#   outdoor10 %>%
#   as_tibble() %>% 
#   filter(value != 0) %>% 
#   pivot_wider(id_cols = c(respondent_id, date, date_hour), 
#               names_from = measure, values_from = value, 
#               names_glue = "{measure}_outdoor10")
# 
# gc()

# ===========================================================
# make sure outdoor data is balanced
# ===========================================================
# do this separately for round 1 vs round 2 respondents
outdoor1_full <- 
  expand_grid(respondent_id = unique(filter(respondent,round == 1)$respondent_id), 
              date_hour = seq(ymd_h("20240424 01"), ymd_h("20240822 00"), by = "hour"))

outdoor2_full <- 
  expand_grid(respondent_id = unique(filter(respondent,round == 2)$respondent_id), 
              date_hour = seq(ymd_h("20240822 01"), max(outdoor$date), by = "hour"))


outdoor_data <- 
  outdoor3_wide %>%
  full_join(bind_rows(outdoor1_full, outdoor2_full)) %>%
  mutate(date = floor_date(date_hour, "day"))

outdoor_data %>% count(date, respondent_id) %>% count(date)

fwrite(as.data.table(outdoor_data), file.path(ddir, "pm_outdoor.csv"))
