# =========================================================================
# clean_surveys.r
# Merge baseline, endline, and installation surveys into df_survey.RDS
# Input: data/raw_data/baseline_adult.csv, endline_adult.csv, installations.csv,
#        hh_sensor_dist_control.csv
# Output: data/df_survey.RDS
# =========================================================================

# =========================================================================
# 1. Baseline survey
# =========================================================================
baseline <- fread(file.path(raw_dir, "baseline_adult.csv"))


# =========================================================================
# 2. Aggregate time-use from baseline activity-level data
# =========================================================================
# adult time use
adult_timeuse <-
  baseline %>%
  dplyr::select(respondent_id, matches("time_use_activity_label_\\d+|time_use_elapsed_\\d+")) %>%
  pivot_longer(
    cols = -respondent_id,
    names_to = c(".value", "time_use_group"),
    names_pattern = "(.*)_(\\d+)"
  ) %>%
  filter(!is.na(time_use_activity_label)) %>%
  group_by(respondent_id) %>%
  summarise(
    adult_timeuse_home_baseline = sum(time_use_elapsed[str_detect(time_use_activity_label, "Inside your own home")], na.rm = TRUE),
    adult_timeuse_total_baseline = sum(time_use_elapsed, na.rm = TRUE),
    .groups = "drop"
  )

# child time use
child_timeuse <-
  baseline %>%
  dplyr::select(respondent_id, matches("time_use_activity_child_label_\\d+|time_use_child_elapsed_\\d+")) %>%
  pivot_longer(
    cols = -respondent_id,
    names_to = c(".value", "time_use_group"),
    names_pattern = "(.*)_(\\d+)"
  ) %>%
  filter(!is.na(time_use_activity_child_label)) %>%
  group_by(respondent_id) %>%
  summarise(
    child_timeuse_home_baseline = sum(time_use_child_elapsed[str_detect(time_use_activity_child_label, "Inside your own home")], na.rm = TRUE),
    child_timeuse_total_baseline = sum(time_use_child_elapsed, na.rm = TRUE),
    .groups = "drop"
  )


# =========================================================================
# 3. Endline survey
# =========================================================================
endline <- fread(file.path(raw_dir, "endline_adult.csv"))


# =========================================================================
# 4. Installation survey (housing characteristics)
# =========================================================================
installations <- fread(file.path(raw_dir, "installations.csv"))


# =========================================================================
# 5. Closest outdoor sensor distance
# =========================================================================
sensor_dist <-
  fread(file.path(raw_dir, "hh_sensor_dist_control.csv")) %>%
  group_by(respondent_id) %>%
  arrange(distance) %>%
  filter(row_number() == 1) %>%
  dplyr::select(respondent_id, sensor_mindist = distance) %>%
  ungroup()


# =========================================================================
# 6. Merge and create analysis variables
# =========================================================================
respondent <-
  baseline %>%
  dplyr::select(respondent_id, starttime_baseline = starttime,
                who_trash_label, who_cookingfuel_label,
                who_hhfoodexp, who_hhnonfoodexp, who_hhincome,
                hh_membercount_baseline = hh_membercount,
                resp_gender = who_gender_label, age,
                beliefs_outdoor_scale_label_baseline = beliefs_outdoor_scale,
                longitude, latitude, treatment_status, round,
                who_schooldetails_hoh_label, child_count_baseline = child_count,
                hh_smokers = who_smoke_num, hoh_employed = who_ind_employed_hoh) %>%
  # endline
  left_join(
    endline %>%
      mutate(starttime = mdy_hms(starttime)) %>%
      dplyr::select(respondent_id,
                    starttime,
                    housing_room_number,
                    smoke24_endline = indoor_smoke,
                    close_window_1hour, close_door_1hour),
    by = "respondent_id"
  ) %>%
  # installations
  left_join(
    installations %>%
      dplyr::select(respondent_id = respondent_id_manual,
                    room_ac = housing_purifier_room_ac,
                    housing_ac,
                    room_pmsource_kitchen = housing_pm_source_3),
    by = "respondent_id"
  ) %>%
  # sensor distance
  left_join(sensor_dist, by = "respondent_id") %>%
  # time use
  left_join(adult_timeuse, by = "respondent_id") %>%
  left_join(child_timeuse, by = "respondent_id") %>%
  # filter to Fan
  filter(treatment_status == "Fan") %>%
  # create derived variables
  mutate(
    # income / expenditure
    income = case_when(who_hhincome == 1 ~ 500000,
                       who_hhincome == 2 ~ 2000000,
                       who_hhincome == 3 ~ 4000000,
                       who_hhincome == 4 ~ 6500000,
                       who_hhincome == 5 ~ 9000000,
                       who_hhincome == 6 ~ 11500000,
                       who_hhincome == 7 ~ 15000000),
    foodexp = case_when(who_hhfoodexp == 1 ~ 100000,
                        who_hhfoodexp == 2 ~ 400000,
                        who_hhfoodexp == 3 ~ 800000,
                        who_hhfoodexp == 4 ~ 1500000,
                        who_hhfoodexp == 5 ~ 2500000,
                        who_hhfoodexp == 6 ~ 3500000,
                        who_hhfoodexp == 7 ~ 5000000),
    nonfoodexp = case_when(who_hhnonfoodexp == 1 ~ 100000,
                           who_hhnonfoodexp == 2 ~ 400000,
                           who_hhnonfoodexp == 3 ~ 800000,
                           who_hhnonfoodexp == 4 ~ 1500000,
                           who_hhnonfoodexp == 5 ~ 2500000,
                           who_hhnonfoodexp == 6 ~ 3500000,
                           who_hhnonfoodexp == 7 ~ 5000000),
    hhexp = foodexp + nonfoodexp,
    income_usd = income / 15961,
    # impute missing income with median
    hh_income = if_else(is.na(who_hhincome), median(who_hhincome, na.rm = TRUE), as.double(who_hhincome)),
    # income quartiles
    income_quart = case_when(hh_income <= 2 ~ "Income Bin 1",
                             hh_income <= 3 ~ "Income Bin 2",
                             hh_income <= 4 ~ "Income Bin 3",
                             hh_income >= 5 ~ "Income Bin 4"),
    # schooling (text labels from survey)
    hoh_school_secondary = who_schooldetails_hoh_label %in% c(
      "Junior high", "M. Tsanawiyah", "Senior high", "M. Aliyah", "Package C",
      "Vocational school", "Diploma 3", "Diploma 4/S1", "S2/S3"),
    hoh_school_tertiary = who_schooldetails_hoh_label %in% c(
      "Diploma 3", "Diploma 4/S1", "S2/S3"),
    hoh_schooling = case_when(hoh_school_tertiary ~ "Tertiary",
                              hoh_school_secondary ~ "Secondary",
                              TRUE ~ "Elementary"),
    hoh_schooling = factor(hoh_schooling, levels = c("Elementary", "Secondary", "Tertiary")),
    # cooking fuel
    hh_cookingfuel = who_cookingfuel_label,
    # trash burning
    trash_burning_1week_baseline = factor(who_trash_label),
    trash_burning_1week_baseline = relevel(trash_burning_1week_baseline, ref = "Never")
  )

respondent <-
  respondent %>%
  dplyr::select(-starts_with("who_"))

# impute median for missing covariates
respondent <-
  respondent %>%
  mutate(
    smoke24_endline = if_else(is.na(smoke24_endline),
                              median(smoke24_endline, na.rm = TRUE),
                              as.double(smoke24_endline)),
    room_pmsource_kitchen = if_else(is.na(room_pmsource_kitchen),
                                    median(room_pmsource_kitchen, na.rm = TRUE),
                                    as.double(room_pmsource_kitchen)),
    trash_burning_1week_baseline = if_else(is.na(trash_burning_1week_baseline),
                                           levels(trash_burning_1week_baseline)[
                                             which.max(table(trash_burning_1week_baseline))],
                                           as.character(trash_burning_1week_baseline)),
    trash_burning_1week_baseline = factor(trash_burning_1week_baseline),
    trash_burning_1week_baseline = relevel(trash_burning_1week_baseline, ref = "Never")
  )


cat("Survey:", nrow(respondent), "respondents\n")
write_rds(respondent, file.path(ddir, "df_survey.RDS"))
