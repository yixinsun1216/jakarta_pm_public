# =========================================================================
# 00_prepare_raw_data.R
# Creates anonymized raw data in data/raw_data/ from private data sources
# This script is for provenance documentation - external users will receive
# the raw_data/ folder directly and do not need to run this script.
# All outputs are CSV (except shapefiles and Stata .dta)
# =========================================================================
pacman::p_load(tidyverse, lubridate, data.table, sf, readxl)

rm(list = ls())

if(Sys.getenv("USER") == "yixin.sun"){
  ddir_private <- file.path("/Users/yixin.sun/Documents/Educational/pollution_experience_data")
  gdir <- file.path("/Users/yixin.sun/Documents/Educational/jakarta_pm_public")
} else if(Sys.getenv("USER") == "yixinsun1"){
  ddir_private <- file.path("/Users/yixinsun1/Dropbox/Research/pollution_experience/fullscale")
  gdir <- file.path("/Users/yixinsun1/Documents/Github/jakarta_pm_public")
}

raw_dir <- file.path(gdir, "data/raw_data")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(42)


# =========================================================================
# 1. Indoor PM data (hourly, from pollution_experience pipeline)
#    Drop outdoor variables - outdoor cleaning is done separately
#    Filter to Fan treatment
# =========================================================================
cat("Processing indoor PM data...\n")

pm_indoor <- fread(file.path(ddir_private, "generated_data/pm/pm_indoor.csv"))

# filter to Fan treatment only
pm_indoor <- pm_indoor[treatment_status == "Fan"]

# drop outdoor variables
outdoor_cols <- grep("outdoor|pm1_|pm10_|pressure_", names(pm_indoor), value = TRUE)
pm_indoor[, (outdoor_cols) := NULL]

fwrite(pm_indoor, file.path(raw_dir, "pm_indoor.csv"))
cat("Indoor PM:", nrow(pm_indoor), "rows,",
    length(unique(pm_indoor$respondent_id)), "Fan respondents\n")
rm(pm_indoor); gc()

# =========================================================================
# 3. Sensor locations (already CSV)
# =========================================================================
file.copy(file.path(ddir_private, "data/tableau/Sensor Lon Lat_data.csv"),
          file.path(raw_dir, "sensor_locations.csv"), overwrite = TRUE)


# =========================================================================
# 4. HH-sensor distances (already CSV)
# =========================================================================
file.copy(file.path(ddir_private, "generated_data/pm/hh_sensor_dist_control.csv"),
          file.path(raw_dir, "hh_sensor_dist_control.csv"), overwrite = TRUE)


# =========================================================================
# 5. Survey data - baseline (control only, PII removed, coords jittered)
# =========================================================================

# flag round 1 respondents
raw_adult_round1 <-
  read_csv(file.path(ddir_private, "data/survey/round1/Full Scale Baseline_WIDE.csv")) %>%
  mutate(round = 1, 
         child_age_3 = as.numeric(child_age_3)) %>%
  mutate_at(vars(matches("knowledge_(.*)_label")), ~if_else(.x == "True", TRUE, FALSE))  %>%
  mutate_at(vars(matches("child_age_")),as.numeric)

# merge round 1 and round 2 together
baseline <-
  read_csv(file.path(ddir_private, "data/survey/round2/Full Scale Baseline_WIDE.csv")) %>%
  mutate(round = 2) %>%
  mutate_at(vars(matches("knowledge_(.*)_label")), ~if_else(.x == "True", TRUE, FALSE)) %>%
  mutate_at(vars(matches("child_age_")),as.numeric) %>%
  bind_rows(raw_adult_round1) %>%
  dplyr::select(-matches("\\[1\\]|\\[2\\]|time_use_child_intro")) %>%
  mutate(starttime = mdy_hms(starttime),
         age = interval(mdy(who_dob), ymd_hms(starttime)) / years(1)) %>%
  mutate_if(is.numeric, ~if_else(.x %in% c(-999, -888), NA_real_, .x)) %>%
  mutate_if(is.character, ~if_else(str_detect(.x, regex("Refuse|Does not know|doesn't know|Tidak tahu", ignore_case = TRUE)), NA_character_, .x)) %>%
  mutate(child_bedroom_label = if_else(child_bedroom_label == "(choice label unavailable)", NA_character_, child_bedroom_label)) %>%
  filter(!(respondent_id_manual == 3329 & child_name == "SHAHNAZ FARADIBA"), 
         respondent_id_manual != 1193, 
         respondent_id_manual != -9999, 
         name_respondent != "Test") %>%
  filter(treatment_status %in% c("Fan", "Control")) %>%
  mutate(longitude = `respondent_gps-Longitude` + runif(n(), -0.009, 0.009),
        latitude = `respondent_gps-Latitude` + runif(n(), -0.009, 0.009))  %>%
mutate(who_schooldetails_hoh_label = if_else(who_hoh_label == "Head of household", who_schooldetails_label, who_schooldetails_hoh_label), 
         who_ind_employed_hoh = if_else(who_hoh_label == "Head of household",who_ind_employed, who_ind_employed_hoh)) %>%
  dplyr::select(respondent_id = respondent_id_manual, starttime, beliefs_outdoor_scale,
    who_trash_label, who_cookingfuel_label, contains("foodexp"), who_hhincome, who_hhincome_label, 
    hh_membercount, who_gender_label, age, longitude, latitude, who_ind_employed_hoh,
    treatment_status, round, who_schooldetails_hoh_label, child_count, who_smoke_num, 
    contains("time"))

fwrite(baseline, file.path(raw_dir, "baseline_adult.csv"))


# endline -----------------------------
# flag round 1 respondents
end_adult_round1 <-
  read_csv(file.path(ddir_private, "data/survey/round1/Endline - Adult_WIDE.csv")) 

# read in round 2 data and combine with round 1
end_adult <-
  read_csv(file.path(ddir_private, "data/survey/round2/Endline - Adult_WIDE.csv")) %>%
  left_join(dplyr::select(end_adult_round1, respondent_id_manual)) %>%
  inner_join(distinct(baseline, respondent_id), by = c("respondent_id_manual" = "respondent_id"))  %>%
  dplyr::select(respondent_id = respondent_id_manual, starttime, housing_room_number, indoor_smoke, contains("close"))

fwrite(end_adult, file.path(raw_dir, "endline_adult.csv"))

# hardware installation survey
installations <- 
  read_csv(file.path(ddir_private, "data/survey/round2/Fullscale Hardware Installation_WIDE.csv")) %>%
  filter(respondent_id_manual %in% baseline$respondent_id) %>%
  group_by(respondent_id_manual) %>% 
  arrange(-duration) %>%
  filter(row_number() == 1) %>%
  dplyr::select(respondent_id_manual, starts_with("housing_"))
fwrite(installations, file.path(raw_dir, "installations.csv"))


# =========================================================================
# 8. Whatsapp survey (control only, PII removed)
# =========================================================================
whatsapp <- 
  read_rds(file.path(ddir_private, "generated_data/whatsapp_survey.rds")) %>%
  filter(respondent_id %in% baseline$respondent_id) %>%
  dplyr::select(respondent_id, date, date_hour, type, open_window, open_door) %>%
  filter(!is.na(open_window))

fwrite(whatsapp, file.path(raw_dir, "whatsapp_survey.csv"))


# =========================================================================
# 9. Road distances (already control only)
# =========================================================================
roads <- readRDS(file.path(ddir_private, "generated_data/survey_control_wroads.RDS"))
fwrite(roads, file.path(raw_dir, "survey_control_wroads.csv"))


# =========================================================================
# 10. Susenas population data
# =========================================================================
file.copy(file.path(ddir_private, "generated_data/Susenas23_DKI.dta"),
          file.path(raw_dir, "Susenas23_DKI.dta"), overwrite = TRUE)


# =========================================================================
# 11. Jakarta administrative boundary shapefile
# =========================================================================
shp_dir <- file.path(raw_dir, "Batas_adm_RW")
dir.create(shp_dir, showWarnings = FALSE)
shp_files <- list.files(file.path(ddir_private, "data/Batas Adm RW 01 Mei 2020"),
                         full.names = TRUE)
file.copy(shp_files, shp_dir, overwrite = TRUE)

