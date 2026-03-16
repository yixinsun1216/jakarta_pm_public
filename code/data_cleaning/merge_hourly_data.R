pacman::p_load(tidyverse, lubridate, sf, ggplot2, mapview, fixest, broom,
               quantreg, data.table)

rm(list=ls())

if(Sys.getenv("USER") == "yixinsun1"){
  ddir <- file.path("/Users/yixinsun1/Dropbox/Research/pollution_experience/fullscale")
  gdir <- file.path("/Users/yixinsun1/Documents/Github/jakarta_pm")
} else if(Sys.getenv("USER") == "jeannesorin"){
  gdir <- file.path("/Users/jeannesorin/github/jakarta_pm")
  ddir <- file.path("/Users/jeannesorin/Dropbox/pollution_experience/fullscale")
} else if(Sys.getenv("USER") == "yixin.sun"){
  ddir <- file.path("/Users/yixin.sun/Documents/Educational/pollution_experience/fullscale")
  gdir <- file.path("/Users/yixin.sun/Documents/Educational/jakarta_pm")
}

# =========================================================================
# read in survey data
# =========================================================================
# load in baseline data
# merge in information on closest outdoor sensor. Only keep control HHs
# who have 
respondent <- 
  read_csv(file.path(ddir, "generated_data/pm/hh_sensor_dist.csv")) %>%
  group_by(respondent_id) %>%
  arrange(distance) %>%
  filter(row_number() == 1) %>%
  dplyr::select(respondent_id, sensor_mindist = distance, dist_south, dist_central) %>%
  full_join(readRDS(file.path(ddir, "generated_data/survey_all.RDS")), .) %>%
  filter(treatment_status == "Fan") %>% # add hardware_cancel later
  mutate(lpg = who_cookingfuel_label_baseline == "Gas/LPG",
         income = case_when(who_hhincome_baseline == 1 ~ 500000,
                            who_hhincome_baseline == 2 ~ 2000000,
                            who_hhincome_baseline == 3 ~ 4000000,
                            who_hhincome_baseline == 4 ~ 6500000,
                            who_hhincome_baseline == 5 ~ 9000000,
                            who_hhincome_baseline ==6 ~ 11500000,
                            who_hhincome_baseline == 7 ~ 15000000),
         foodexp = case_when(who_hhfoodexp_baseline == 1 ~ 100000,
                             who_hhfoodexp_baseline == 2 ~ 400000,
                             who_hhfoodexp_baseline == 3 ~ 800000,
                             who_hhfoodexp_baseline == 4 ~ 1500000,
                             who_hhfoodexp_baseline == 5 ~ 2500000,
                             who_hhfoodexp_baseline == 6 ~ 3500000,
                             who_hhfoodexp_baseline ==7 ~ 5000000),
         nonfoodexp = case_when(who_hhnonfoodexp_baseline == 1 ~ 100000,
                                who_hhnonfoodexp_baseline == 2 ~ 400000,
                                who_hhnonfoodexp_baseline == 3 ~ 800000,
                                who_hhnonfoodexp_baseline == 4 ~ 1500000,
                                who_hhnonfoodexp_baseline == 5 ~ 2500000,
                                who_hhnonfoodexp_baseline == 6 ~ 3500000,
                                who_hhnonfoodexp_baseline ==7 ~ 5000000),
         income_percapita = income / hh_membercount_baseline,
         foodexp_percapita = foodexp / hh_membercount_baseline,
         nonfoodexp_percapita = nonfoodexp / hh_membercount_baseline) %>%
  dplyr::select(respondent_id, sensor_mindist, starttime, starttime_baseline, 
         housing_ac_baseline, video_timing_baseline, age,
         contains("beliefs"), contains("aqi"), contains("pm25"), contains("close"),
         who_ind_employed_baseline, contains("itemcount"), contains("timeuse"),
         housing_bedroom_door_baseline, strata_baseline,
         child_count_baseline, lpg, 
         hh_foodexp = who_hhfoodexp_baseline, wealth_index_baseline,
         hh_nonfoodexp = who_hhnonfoodexp_baseline, hh_membercount_baseline,
         hh_membercount_baseline, hh_membercount_abv60_baseline, income, income_percapita,
         foodexp, foodexp_percapita, nonfoodexp, nonfoodexp_percapita,
         hh_income = who_hhincome_baseline, who_respondent = who_hoh_label_baseline,
         hoh_relation_label = who_hoh_relation_label_baseline,
         hoh_school = who_schooldetails_hoh_label_baseline,
         hoh_employed = who_ind_employed_hoh_baseline,
         resp_school = who_schooldetails_label_baseline,
         resp_employed = who_ind_employed_baseline,
         resp_gender = who_gender_label_baseline,
         treatment_status,  dist_central, dist_south, housing_room_number,
         who_wifi, housing_room_volume, hoh_schooling,
         hh_cookingfuel = who_cookingfuel_label_baseline,
         trash_burning_1week_endline = who_trash_label, 
         trash_burning_1week_baseline = who_trash_label_baseline,
         hoh_relation_label = who_hoh_relation_label_baseline,
         hoh_school = who_schooldetails_hoh_label_baseline,
         hoh_employed = who_ind_employed_hoh_baseline,
         resp_school = who_schooldetails_label_baseline,
         resp_employed = who_ind_employed_baseline,
         resp_gender = who_gender_label_baseline, 
         contains("demand"),follow_bicara, follow_nafas,
         room_ac = housing_purifier_room_ac_baseline,
         room_windows_night = close_window_label_baseline,
         room_windows = housing_purifier_room_window_baseline,
         room_windows_leak = housing_purifier_room_windows_leak_baseline,
         room_door = housing_bedroom_door_baseline,
         room_pmsource_gaps = housing_pm_source_1_baseline,
         room_pmsource_open = housing_pm_source_2_baseline,
         room_pmsource_kitchen = housing_pm_source_3_baseline,
         room_pmsource_smoke = housing_pm_source_4_baseline, 
         room_pmsource_burning = housing_pm_source_6_baseline,
         room_pmsource_candle = housing_pm_source_7_baseline,
         room_volume = housing_room_volume,
         room_kitchen = housing_purifier_room_type_3_baseline,
         housing_ac = housing_ac_baseline,
         treatment_status,  dist_central, dist_south,
         smoke24_endline = indoor_smoke,
         hh_smokers = who_smoke_num_baseline,
         matches("timeuse_home|timeuse_total")) %>%
  mutate(foodexp_quart = case_when(hh_foodexp <= 3 ~ "Food Exp 1", 
                                   hh_foodexp <= 4 ~ "Food Exp 2", 
                                   hh_foodexp <= 5 ~ "Food Exp 3", 
                                   hh_foodexp >= 6 ~ "Food Exp 4"), 
         income_quart = case_when(hh_income <= 2 ~ "Income Bin 1", 
                                  hh_income <= 3 ~ "Income Bin 2", 
                                  hh_income <= 4 ~ "Income Bin 3", 
                                  hh_income >= 5 ~ "Income Bin 4"), 
         nonfoodexp_quart = case_when(hh_foodexp <= 3 ~ "Non-Food Exp 1", 
                                      hh_foodexp <= 4 ~ "Non-Food Exp 2", 
                                      hh_foodexp <= 5 ~ "Non-Food Exp 3", 
                                      hh_foodexp >= 6 ~ "Non-Food Exp 4"), 
         hoh_school_primary = as.numeric(hoh_school) >= 2, 
         hoh_school_secondary = as.numeric(hoh_school) >= 3,
         hoh_school_tertiary = as.numeric(hoh_school) %in% c(9, 10, 11, 12),
         resp_school_primary = as.numeric(resp_school) >= 2, 
         resp_school_secondary = as.numeric(resp_school) >= 3,
         resp_school_tertiary = as.numeric(resp_school) %in% c(9, 10, 11, 12), 
         income_usd = income / 15961, 
         hhexp = foodexp + nonfoodexp)

# let's also add an index of housing attributes - useful for understanding things taht 
# affect infiltration rate
# start with an unweighted version following Kling - average of normalized z-scores
housing_vars <- 
  c("room_ac", "housing_ac", "room_door",
    "room_pmsource_gaps", "room_pmsource_open", "room_windows_leak", "room_volume", 
    "room_kitchen", "housing_room_number")
  

index <- 
  respondent %>%
  dplyr::select(respondent_id, all_of(housing_vars)) %>%
  # make sure all indicators are 1 for good quality, and 0 for worse quality
  mutate_at(vars(room_windows_leak,room_door, room_pmsource_gaps, room_pmsource_open, room_kitchen), ~ 1-.) %>% 
  mutate_at(vars(-respondent_id), as.numeric) %>%
  mutate_at(vars(-respondent_id), scale) %>%
  mutate_all(as.numeric) %>%
  rowwise() %>%
  mutate(housing_index = mean(c_across(-respondent_id), na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::select(respondent_id, housing_index)

respondent <- left_join(respondent, index)


write_rds(respondent, file.path(ddir, "generated_data/survey/df_survey.RDS"))

# =========================================================================
# read in pm data 
# =========================================================================
# suspiciously high number of values where pm25 = 1 - drop these
pm_hourly <- 
  read_rds(file.path(ddir, "generated_data/pm/pm_indoor.rds")) %>%
  filter(treatment_status == "Fan") %>%
  group_by(respondent_id) %>%
  arrange(respondent_id, date_hour) %>%
  rename(pm25_indoor = pm25_purifier) %>%
  mutate(pm25_indoor = if_else(pm25_indoor == 1, NA_real_, pm25_indoor),
         pm25_indoor_lag1 = lag(pm25_indoor, n = 1)) 

# add lags 
for(i in 1:23){
  lagno <- paste0("pm25_outdoor3_lag", i)
  pm_hourly <- mutate(pm_hourly, !!lagno := dplyr::lag(pm25_outdoor3, n = i))
}

pm_hourly <- ungroup(left_join(pm_hourly, respondent))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Time-use survey: indicator for whether hh is cooking --------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
timeuse_cooking <-
  read_csv(file.path(ddir, "generated_data/survey/timeuse_baseline.csv")) %>%
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
  distinct(respondent_id = respondent_id_manual, hour = hour_seq) %>%
  mutate(cooking = 1L)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# add in roads and time use for cooking
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pm_last_date <- 
  filter(pm_hourly, !is.na(pm25_indoor)) %>%
  group_by(respondent_id) %>%
  summarise(date_last = max(date)) %>%
  ungroup()

roads <- read_rds(file.path(ddir, "generated_data/survey_control_wroads.RDS")) 

pm <- 
  pm_hourly %>%
  left_join(pm_last_date, by = "respondent_id") %>%
  filter(date <= date_last, date >= date_first)  %>%
  left_join(roads, by = "respondent_id") %>%
  left_join(timeuse_cooking, by = c("respondent_id", "hour"))  %>%
  mutate(pm25_change = pm25_indoor - pm25_indoor_lag1, 
         spike = pm25_change > sd(pm_hourly$pm25_indoor, na.rm = TRUE)) %>%
  ungroup()

write_rds(pm, file.path(ddir, "generated_data/pm/df_reg.rds"))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# create a jittered version of the locations for mapping purposes
# this is just for visualization, not used in any analysis
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hh_locations <- 
  readRDS(file.path(ddir, "generated_data/survey/baseline.RDS")) %>%
  filter(treatment_status == "Fan") %>%
  dplyr::select(respondent_id, longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4326)) %>%
  st_jitter(factor =.01)
write_rds(hh_locations, file.path(ddir, "generated_data/survey/hh_locations_jittered.RDS"))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read in whatsapp data, and take out adult and child name
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
whatsapp <- 
  read_rds(file.path(ddir, "generated_data/whatsapp_survey.RDS")) %>%
  filter(treatment_status == "Fan") %>%
  dplyr::select( -adult, -child)
write_rds(whatsapp, file.path(ddir, "generated_data/survey/df_whatsapp.rds"))
