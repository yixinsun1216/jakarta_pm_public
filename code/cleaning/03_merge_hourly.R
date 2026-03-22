# =========================================================================
# 06_merge_hourly.R
# Merge indoor + outdoor PM at hourly level with survey and auxiliary data
# Input: data/raw_data/pm_indoor.csv (already hourly),
#        data/df_outdoor_10min.rds, data/df_survey.RDS,
#        data/raw_data/survey_control_wroads.csv,
#        data/raw_data/timeuse_baseline.csv, data/raw_data/baseline_adult.csv
# Output: data/df_reg.rds, data/hh_locations_jittered.RDS
# =========================================================================
# read survey data
respondent <- read_rds(file.path(ddir, "df_survey.RDS"))

# =========================================================================
# Hourly pm data
# =========================================================================
pm_indoor_hourly <-
  fread(file.path(raw_dir, "pm_indoor.csv")) %>%
  mutate(date_hour = as.POSIXct(date_hour, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
         date = as.Date(date),
         hour = hour(date_hour)) %>%
  dplyr::select(respondent_id, date_hour, date, hour,
                pm25_indoor = pm25_purifier)

pm_outdoor_hourly <-  read_rds(file.path(ddir, "pm_outdoor.rds"))

# =========================================================================
# merge indoor + outdoor, filter to Fan treatment
# =========================================================================
pm_hourly <-
  full_join(pm_outdoor_hourly, pm_indoor_hourly,
            by = c("respondent_id", "date_hour", "date")) %>%
  filter(respondent_id %in% respondent$respondent_id) %>%
  arrange(respondent_id, date_hour) %>%
  # suspiciously high number of values where pm25 = 1 - drop these
  mutate(pm25_indoor = if_else(pm25_indoor == 1, NA_real_, pm25_indoor),
         hour = if_else(is.na(hour), hour(date_hour), hour))

# add lag of indoor PM
pm_hourly <-
  pm_hourly %>%
  group_by(respondent_id) %>%
  arrange(respondent_id, date_hour) %>%
  mutate(pm25_indoor_lag1 = lag(pm25_indoor, n = 1)) %>%
  ungroup()

# add outdoor PM lags
for(i in 1:23){
  lagno <- paste0("pm25_outdoor3_lag", i)
  pm_hourly <-
    pm_hourly %>%
    group_by(respondent_id) %>%
    mutate(!!lagno := dplyr::lag(pm25_outdoor3, n = i)) %>%
    ungroup()
}

# merge in survey data
pm_hourly <- left_join(pm_hourly, respondent, by = "respondent_id")


# =========================================================================
# time-use survey: indicator for whether hh is cooking
# =========================================================================
timeuse_cooking <-
  fread(file.path(raw_dir, "baseline_adult.csv")) %>%
  filter(treatment_status == "Fan") %>%
  dplyr::select(respondent_id, matches("time_use_activity_label_\\d+|time_use_end_\\d+|time_use_elapsed_\\d+")) %>%
  pivot_longer(
    cols = -respondent_id,
    names_to = c(".value", "time_use_group"),
    names_pattern = "(.*)_(\\d+)"
  ) %>%
  filter(time_use_activity_label == "Inside your own home - Cooking") %>%
  mutate(
    end = time_use_end,
    end_adj = if_else(end - time_use_elapsed < 0, end + 24, end),
    start_adj = end_adj - time_use_elapsed,
    hour_seq = map2(start_adj, end_adj, ~ {
      if (is.na(.x) | is.na(.y)) integer(0) else {
        hrs <- seq(floor(.x), ceiling(.y) - 1)
        hrs %% 24
      }
    })
  ) %>%
  unnest(hour_seq) %>%
  distinct(respondent_id, hour = hour_seq) %>%
  mutate(cooking = 1L)


# =========================================================================
# add roads and cooking data, create final variables
# =========================================================================
# restrict to global indoor PM date range
indoor_date_range <- pm_hourly %>%
  filter(!is.na(pm25_indoor)) %>%
  summarise(date_min = min(date) - weeks(2), date_max = max(date))
cat("Indoor PM date range:", as.character(indoor_date_range$date_min),
    "to", as.character(indoor_date_range$date_max), "\n")

roads <- fread(file.path(raw_dir, "survey_control_wroads.csv"))

pm <-
  pm_hourly %>%
  filter(date >= indoor_date_range$date_min,
         date <= indoor_date_range$date_max) %>%
  left_join(roads, by = "respondent_id") %>%
  left_join(timeuse_cooking, by = c("respondent_id", "hour")) %>%
  mutate(cooking = replace_na(cooking, 0L),
         dist_primary = if_else(is.na(dist_primary),
                                median(dist_primary, na.rm = TRUE),
                                dist_primary),
         pm25_change = pm25_indoor - pm25_indoor_lag1,
         spike = pm25_change > sd(pm_hourly$pm25_indoor, na.rm = TRUE),
         week = floor_date(date, "week")) %>%
  ungroup()

cat("Hourly PM panel:", nrow(pm), "rows,",
    length(unique(pm$respondent_id)), "respondents\n")

write_rds(pm, file.path(ddir, "df_reg.rds"))


# =========================================================================
# jittered household locations for mapping
# =========================================================================
hh_locations <-
  fread(file.path(raw_dir, "baseline_adult.csv")) %>%
  filter(treatment_status == "Fan") %>%
  dplyr::select(respondent_id, longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4326)) %>%
  st_jitter(factor = .01)

write_rds(hh_locations, file.path(ddir, "hh_locations_jittered.RDS"))

cat("=== Merge complete ===\n")
