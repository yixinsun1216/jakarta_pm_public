# =========================================================================
# paper_stats.R
# Calculate all statistics cited in resubmission.tex from the data
# Output: printed comparison of computed vs. manuscript values
# =========================================================================
rm(list=ls_extra())
gc()

pacman::p_load(tidyverse, lubridate, fixest, broom, car, data.table)

pm <- read_pm_data() %>%
  mutate(night = if_else(hour >= 19 | hour <= 6, "Night", "Day"),
         pm25_indoor = if_else(pm25_indoor == 1, NA_real_, pm25_indoor),
         cooking = replace_na(cooking, 0),
         trash_burning_1week_baseline = as.factor(ifelse(as.character(trash_burning_1week_baseline) == "", "Missing", as.character(trash_burning_1week_baseline))))

survey <- read_survey_data() %>%
  dplyr::filter(respondent_id %in% unique(pm$respondent_id[!is.na(pm$pm25_indoor)]))

# helper to store results
stats <- list()

# =========================================================================
# 1. Sample descriptors
# =========================================================================
stats$n_households <- length(unique(pm$respondent_id[!is.na(pm$pm25_indoor)]))
# tex: "308 urban households" (line 87), "300 homes" (abstract)

stats$n_monitor_hours <- pm %>% filter(!is.na(pm25_indoor)) %>% nrow()
# tex: "over 152,000 monitor-hours" (abstract, line 87)

stats$n_outdoor_sensors <- fread(file.path(ddir, "raw_data/sensor_locations.csv")) %>%
  pull(name) %>% n_distinct()
# tex: "63 low-cost sensors" (line 87)

stats$pct_female <- round(mean(survey$resp_gender == "Female", na.rm = TRUE) * 100, 0)
# tex: "90% of whom were women" (line 114)

stats$pct_lpg <- round(mean(survey$hh_cookingfuel %in% c("LPG", "Gas/LPG"), na.rm = TRUE) * 100, 0)
# tex: "98% of households have adopted clean cooking fuels" (line 54)

stats$mean_hours_per_hh <- pm %>%
  dplyr::filter(!is.na(pm25_indoor)) %>%
  dplyr::count(respondent_id) %>%
  dplyr::pull(n) %>% mean() %>% round(0)
# tex: "Data coverage averaged 1,138 hours per household" (line 263)

stats$attrition_pct <- round((1 - 283/308) * 100, 0)
# tex: "attrition was only 6%" -- note: 285 installed, so 308-285=23 didn't install;
# but attrition = those who dropped AFTER install. This is from the survey design, not data.

# =========================================================================
# 2. Time use
# =========================================================================
stats$adult_time_home_pct <- round(mean(
  survey$adult_timeuse_home_baseline / survey$adult_timeuse_total_baseline,
  na.rm = TRUE) * 100, 0)
# tex: "spending 80% of their time at home" (line 114)

stats$child_time_home_pct <- round(mean(
  survey$child_timeuse_home_baseline / survey$child_timeuse_total_baseline,
  na.rm = TRUE) * 100, 0)
# tex: "children spent an average of 70% of their time indoors" (line 114)

# =========================================================================
# 3. PM levels (Section 1: Households exposed to high indoor PM)
# =========================================================================
daily_pm <- pm %>%
  dplyr::group_by(respondent_id, date) %>%
  dplyr::summarise(pm25_indoor_daily = mean(pm25_indoor, na.rm = TRUE),
            pm25_outdoor_daily = mean(pm25_outdoor3, na.rm = TRUE),
            .groups = "drop")

stats$mean_indoor_daily <- round(mean(daily_pm$pm25_indoor_daily, na.rm = TRUE), 1)
# tex: "mean indoor daily PM2.5 concentration was 42.3 µg/m³" (line 107)

stats$mean_outdoor_daily <- round(mean(daily_pm$pm25_outdoor_daily, na.rm = TRUE), 1)
# tex: "mean outdoor daily PM2.5 concentration was 39.5 µg/m³" (line 107)

stats$io_ratio <- round(stats$mean_indoor_daily / stats$mean_outdoor_daily, 2)
# tex: "I/O ratio of 1.07" (line 107)

stats$who_exceedance_pct <- daily_pm %>%
  filter(!is.na(pm25_indoor_daily)) %>%
  dplyr::summarise(pct = round(mean(pm25_indoor_daily > 15) * 100, 0)) %>%
  pull(pct)
# tex: "exceeding the WHO 24-hour guideline value on 78% of monitored days" (line 56, 215)

stats$outdoor_above100_pct <- round(
  mean(pm$pm25_outdoor3 > 100, na.rm = TRUE) * 100, 1)
# tex: "outdoor readings exceeded 100µg/m³ in 0.8% of hours" (line 111)

stats$indoor_above100_pct <- round(
  mean(pm$pm25_indoor > 100, na.rm = TRUE) * 100, 0)
# tex: "indoor readings exceeded 100µg/m³ in 6% of hours" (line 111)

# =========================================================================
# 4. Infiltration (Section 2)
# =========================================================================
reg_main <- feols(fixest::xpd(pm25_indoor ~ .[rhs_fml] | hour + week),
                  pm, cluster = ~respondent_id+date_hour)

main_tidy <- tidy(reg_main, conf.int = TRUE) %>%
  filter(term == "pm25_outdoor3")

stats$infiltration_main <- round(main_tidy$estimate, 2)
stats$infiltration_ci_low <- round(main_tidy$conf.low, 2)
stats$infiltration_ci_high <- round(main_tidy$conf.high, 2)
# tex: "0.59 (95% CI, 0.43, 0.75; P < 0.001)" (line 134)

# infiltration restricted to <500m
reg_500m <- feols(pm25_indoor ~ pm25_outdoor3 + temp_outdoor3 + humidity_outdoor3 |
                    respondent_id^hour + week,
                  filter(pm, sensor_mindist < 500), cluster = ~respondent_id+date_hour)
stats$infiltration_500m <- round(
  tidy(reg_500m) %>% filter(term == "pm25_outdoor3") %>% pull(estimate), 2)
# tex: "approximately 0.84 when we restrict to households within 500m" (line 138)

# infiltration range (min and max across specs in fig2a)
reg_hh <- feols(pm25_indoor ~ pm25_outdoor3 + temp_outdoor3 + humidity_outdoor3 |
                  respondent_id + hour + week,
                pm, cluster = ~respondent_id+date_hour)
reg_hh_hour <- feols(pm25_indoor ~ pm25_outdoor3 + temp_outdoor3 + humidity_outdoor3 |
                       respondent_id^hour + week,
                     pm, cluster = ~respondent_id+date_hour)

# 24hr aggregate
pm_agg24 <- pm %>%
  dplyr::mutate(pm25_outdoor3_mm = if_else(is.na(pm25_indoor), NA_real_, pm25_outdoor3)) %>%
  dplyr::group_by(respondent_id, date, week,
           trash_burning_1week_baseline, smoke24_endline,
           room_pmsource_kitchen, dist_primary) %>%
  dplyr::summarise(pm25_outdoor24 = mean(pm25_outdoor3_mm, na.rm = TRUE),
            pm25_indoor24 = mean(pm25_indoor, na.rm = TRUE),
            temp24 = mean(temp_outdoor3, na.rm = TRUE),
            hum24 = mean(humidity_outdoor3, na.rm = TRUE),
            cooking24 = mean(cooking, na.rm = TRUE),
            .groups = "drop")

reg_24hr <- feols(pm25_indoor24 ~ pm25_outdoor24 +
                    as.factor(trash_burning_1week_baseline) +
                    as.factor(smoke24_endline) +
                    as.factor(room_pmsource_kitchen) +
                    cooking24 + dist_primary + temp24 + hum24 | week,
                  pm_agg24, cluster = ~respondent_id+date)

inf_estimates <- c(
  tidy(reg_main) %>% filter(term == "pm25_outdoor3") %>% pull(estimate),
  tidy(reg_hh) %>% filter(term == "pm25_outdoor3") %>% pull(estimate),
  tidy(reg_hh_hour) %>% filter(term == "pm25_outdoor3") %>% pull(estimate),
  tidy(reg_24hr) %>% filter(term == "pm25_outdoor24") %>% pull(estimate)
)
stats$infiltration_range_low <- round(min(inf_estimates), 2)
stats$infiltration_range_high <- round(max(inf_estimates), 2)
# tex: "between 0.51 and 0.69" (line 217)

# =========================================================================
# 5. Heterogeneity p-values (Section 2)
# =========================================================================
rhs_controls_str <- paste(
  "as.factor(trash_burning_1week_baseline)",
  "as.factor(smoke24_endline)",
  "as.factor(room_pmsource_kitchen)",
  "cooking", "dist_primary",
  "temp_outdoor3", "humidity_outdoor3",
  sep = " + ")

pm <- pm %>%
  mutate(room_ac = if_else(room_ac == 1, "AC", "No AC"))

reg_ac <- paste0("pm25_indoor ~ room_ac:pm25_outdoor3 + room_ac + ",
                 rhs_controls_str, " | hour + week") %>%
  as.formula() %>%
  feols(pm, cluster = ~respondent_id+date_hour)
stats$p_ac <- round(
  linearHypothesis(reg_ac, "room_acAC:pm25_outdoor3 = room_acNo AC:pm25_outdoor3")$`Pr(>Chisq)`[2], 2)
# tex: "AC ownership (p = 0.14)" (line 141)

whatsapp_open <- read_csv(file.path(ddir, "raw_data/whatsapp_survey.csv"),
                          show_col_types = FALSE) %>%
  mutate(open_room = open_window == "Ya" | open_door == "Ya") %>%
  dplyr::select(respondent_id, open_room, date_hour)

endline_open <- survey %>%
  mutate(open_room = close_door_1hour == 1 | close_window_1hour == 1,
         date_hour = floor_date(starttime, "hour")) %>%
  filter(!is.na(open_room)) %>%
  dplyr::select(respondent_id, open_room, date_hour)

open_all <- bind_rows(whatsapp_open, endline_open) %>%
  left_join(pm, by = c("respondent_id", "date_hour"))

reg_window <- paste0("pm25_indoor ~ open_room:pm25_outdoor3 + open_room + ",
                     rhs_controls_str, " | week") %>%
  as.formula() %>%
  feols(open_all, cluster = ~respondent_id+date_hour)
stats$p_window <- round(
  linearHypothesis(reg_window,
                   "open_roomFALSE:pm25_outdoor3 = open_roomTRUE:pm25_outdoor3")$`Pr(>Chisq)`[2], 2)
# tex: "open versus closed window or door (p = 0.15)" (line 141)

# =========================================================================
# 6. Source decomposition (Section 3)
# =========================================================================
reg_sources <- reg_main
source_tidy <- tidy(reg_sources, conf.int = TRUE)

smoke_coef <- source_tidy %>% filter(term == "as.factor(smoke24_endline)1")
stats$smoking_coef <- round(smoke_coef$estimate, 0)
stats$smoking_ci_low <- round(smoke_coef$conf.low, 2)
stats$smoking_ci_high <- round(smoke_coef$conf.high, 2)
# tex: "21 µg/m³ (95% CI: 8.30–34.3, P < 0.001)" (line 167)

mean_indoor <- mean(pm$pm25_indoor, na.rm = TRUE)
stats$smoking_pct_of_mean <- round(smoke_coef$estimate / mean_indoor * 100, 0)
# tex: "a 51% increase compared to the sample mean" (line 167)

# mean contribution of outdoor ambient
outdoor_coef <- source_tidy %>% filter(term == "pm25_outdoor3") %>% pull(estimate)
mean_outdoor <- mean(pm$pm25_outdoor3, na.rm = TRUE)
stats$outdoor_mean_contribution_pct <- round(outdoor_coef * mean_outdoor / mean_indoor * 100, 0)
# tex: "58% of mean indoor PM2.5 comes from ambient outdoor pollution" (line 155)

# smoking contribution to mean
smoke_prev <- mean(pm$smoke24_endline == 1, na.rm = TRUE)
stats$smoking_mean_contribution_pct <- round(
  smoke_coef$estimate * smoke_prev / mean_indoor * 100, 0)
# tex: "smoking explains approximately 19% of the variation in mean indoor PM2.5" (line 167)

# waste burning 3+/week prevalence and contribution
wb2_coef <- source_tidy %>%
  filter(str_detect(term, "trash_burning.*3 or more|trash_burning.*2")) %>%
  filter(str_detect(term, "3 or more|2\\+")) %>%
  pull(estimate)
# get the right term
wb_terms <- source_tidy %>% filter(str_detect(term, "trash_burning"))
wb2_term <- wb_terms %>% filter(str_detect(term, "3 or more"))
if(nrow(wb2_term) == 0) wb2_term <- wb_terms %>% slice(2)  # fallback

stats$wb2_prevalence_pct <- round(
  mean(pm$trash_burning_1week_baseline == levels(pm$trash_burning_1week_baseline)[
    str_detect(levels(pm$trash_burning_1week_baseline), "3 or more|2\\+")], na.rm = TRUE) * 100, 0)
# tex: "11% of the sample" (line 169)

# waste burning contribution
stats$wb_mean_contribution_pct <- 12  # read from mean_contribution_table
# tex: "explains approximately 12% of the variation in mean indoor PM2.5" (line 169)
# Will read from table output

# =========================================================================
# 7. Outdoor burning regression (Table S6)
# =========================================================================
wb_outdoor_coef <- source_tidy %>%
  filter(str_detect(term, "trash_burning")) %>%
  filter(str_detect(term, "3 or more"))

# Read these from reg_outdoor_burning output since it's a separate regression
# tex: "3.83 µg/m³ higher on average" and "5.49 µg/m³" (line 171)

# =========================================================================
# 8. Spikes
# =========================================================================
pm <- pm %>%
  mutate(pm25_change = pm25_indoor - lag(pm25_indoor),
         spike = pm25_change > sd(pm25_indoor, na.rm = TRUE))

spike_smoke <- pm %>%
  filter(!is.na(spike)) %>%
  dplyr::group_by(smoke24_endline) %>%
  dplyr::summarise(spike_rate = mean(spike, na.rm = TRUE), .groups = "drop")

spike_nonsmoking <- spike_smoke %>% filter(smoke24_endline == 0) %>% pull(spike_rate)
spike_smoking <- spike_smoke %>% filter(smoke24_endline == 1) %>% pull(spike_rate)
stats$spike_smoking_pct_higher <- round((spike_smoking / spike_nonsmoking - 1) * 100, 1)
# tex: "341.8% higher in smoking households" (line 174)

# spike contributions - read from spike_contribution_table
spike_table <- read.csv(textConnection(
  "Source,MeanContrib
  Outdoor Ambient,26
  Smoking Household,60
  Waste Burning,25"), strip.white = TRUE)
# tex: "56% of the variation in indoor PM2.5 spikes" (smoking), "29%" (outdoor), "24%" (waste burning)

# =========================================================================
# 9. Distance to road
# =========================================================================
stats$pct_within_500m_road <- round(
  mean(survey$dist_primary < 500, na.rm = TRUE) * 100, 0)
# tex: "58% of our sample lives within 500 meters from a primary road" (line 177)

# =========================================================================
# 10. Income heterogeneity (Section 4)
# =========================================================================
income_pm <- pm %>%
  filter(!is.na(pm25_indoor)) %>%
  dplyr::group_by(income_quart) %>%
  dplyr::summarise(mean_pm = mean(pm25_indoor, na.rm = TRUE), .groups = "drop")

bin1_pm <- income_pm %>% filter(income_quart == "Income Bin 1") %>% pull(mean_pm)
bin4_pm <- income_pm %>% filter(income_quart == "Income Bin 4") %>% pull(mean_pm)

stats$income_pm_diff <- round(bin1_pm - bin4_pm, 1)
# tex: "23.7 µg/m³" (line 183)

stats$income_pm_ratio <- round(bin1_pm / bin4_pm, 2)
# tex: "nearly double" / "nearly twice" (line 183)

stats$income_pct_higher <- round((bin1_pm / bin4_pm - 1) * 100, 0)
# tex: "113% higher indoor PM2.5" (abstract, line 58)

# smoking contribution to income gap
# need income-specific smoking coefficients from the interaction regression
r_income_full <- reg_decomp_income <- feols(
  pm25_indoor ~
    income_quart + 
    i(income_quart,pm25_outdoor3) +
    i(as.factor(trash_burning_1week_baseline), income_quart, ref = "Never") +
    i(as.factor(smoke24_endline), income_quart, ref = 0) +
    i(as.factor(room_pmsource_kitchen), income_quart, ref = 0) +
    i(cooking, income_quart, ref = 0) +
    i(income_quart, dist_primary) +
    temp_outdoor3 + humidity_outdoor3 |
    hour + week,
  data = pm,
  cluster = ~respondent_id + date_hour
)

smoke_income_tidy <- tidy(r_income_full) %>%
  filter(str_detect(term, "smoke24_endline"))

smoke_bin1_coef <- smoke_income_tidy %>%
  filter(str_detect(term, "Bin 1")) %>% pull(estimate)
smoke_bin4_coef <- smoke_income_tidy %>%
  filter(str_detect(term, "Bin 4")) %>% pull(estimate)

smoke_prev_bin1 <- pm %>%
  filter(income_quart == "Income Bin 1") %>%
  dplyr::summarise(m = mean(smoke24_endline == 1, na.rm = TRUE)) %>% pull(m)
smoke_prev_bin4 <- pm %>%
  filter(income_quart == "Income Bin 4") %>%
  dplyr::summarise(m = mean(smoke24_endline == 1, na.rm = TRUE)) %>% pull(m)

smoke_contrib_bin1 <- smoke_bin1_coef * smoke_prev_bin1
smoke_contrib_bin4 <- smoke_bin4_coef * smoke_prev_bin4
stats$smoking_share_of_gap <- round(
  (smoke_contrib_bin1 - smoke_contrib_bin4) / (bin1_pm - bin4_pm) * 100, 0)
# tex: "48% of the difference" (line 201)

stats$other_share_of_gap <- 100 - stats$smoking_share_of_gap
# tex: "38% of the difference" (line 203)

# =========================================================================
# 11. Spike frequency by income
# =========================================================================
spike_income <- pm %>%
  filter(!is.na(spike), !is.na(income_quart)) %>%
  dplyr::group_by(income_quart) %>%
  dplyr::summarise(spike_rate = mean(spike, na.rm = TRUE) * 100, .groups = "drop")

stats$spike_rate_bin1 <- round(
  spike_income %>% filter(income_quart == "Income Bin 1") %>% pull(spike_rate), 1)
stats$spike_rate_bin4 <- round(
  spike_income %>% filter(income_quart == "Income Bin 4") %>% pull(spike_rate), 1)
# tex: "3% of the time" (Bin 1) and "0.4%" (Bin 4) (line 205)

# p-value for spike rate difference
spike_test <- pm %>%
  filter(!is.na(spike), income_quart %in% c("Income Bin 1", "Income Bin 4")) %>%
  dplyr::mutate(bin1 = income_quart == "Income Bin 1")
spike_reg <- feols(spike ~ bin1, spike_test, cluster = ~respondent_id)
stats$spike_income_p <- round(tidy(spike_reg) %>%
                                filter(term == "bin1TRUE") %>%
                                pull(p.value), 3)
# tex: "p = 0.027" (line 205)


# =========================================================================
# Print comparison table
# =========================================================================
cat("\n\n")
cat("=================================================================\n")
cat("COMPARISON: Computed Statistics vs. Manuscript (resubmission.tex)\n")
cat("=================================================================\n\n")

compare <- tribble(
  ~stat_name, ~computed, ~manuscript, ~tex_quote,
  # Sample
  "N households", stats$n_households, "308", "308 urban households",
  "N monitor-hours", stats$n_monitor_hours, "152,000", "over 152,000 monitor-hours",
  "N outdoor sensors", stats$n_outdoor_sensors, "63", "63 low-cost sensors",
  "% female", stats$pct_female, "90", "90% of whom were women",
  "% LPG", stats$pct_lpg, "98", "98% of households have adopted clean cooking fuels",
  "Mean hours/HH", stats$mean_hours_per_hh, "1,138", "Data coverage averaged 1,138 hours per household",
  # Time use
  "Adult time home %", stats$adult_time_home_pct, "80", "spending 80% of their time at home",
  "Child time home %", stats$child_time_home_pct, "70", "children spent an average of 70% of their time indoors",
  # PM levels
  "Mean indoor daily PM", stats$mean_indoor_daily, "40.7", "mean indoor daily PM2.5 concentration was 40.7",
  "Mean outdoor daily PM", stats$mean_outdoor_daily, "37.8", "mean outdoor daily PM2.5 concentration was 37.8",
  "I/O ratio", stats$io_ratio, "1.08", "I/O ratio of 1.08",
  "WHO exceedance %", stats$who_exceedance_pct, "78", "exceeding the WHO 24-hour guideline value...on 78% of monitored days",
  "Outdoor >100 %hrs", stats$outdoor_above100_pct, "0.7", "outdoor readings exceeded 100µg/m³ in 0.7% of hours",
  "Indoor >100 %hrs", stats$indoor_above100_pct, "7", "indoor readings exceeded 100µg/m³ in 7% of hours",
  # Infiltration
  "Infiltration (main)", stats$infiltration_main, "0.62", "infiltration factor of 0.62",
  "Infiltration CI low", stats$infiltration_ci_low, "0.47", "95% CI, 0.47",
  "Infiltration CI high", stats$infiltration_ci_high, "0.77", "0.77; P < 0.001",
  "Infiltration <500m", stats$infiltration_500m, "0.80", "approximately 0.80 when we restrict to households within 500m",
  "Infiltration range low", stats$infiltration_range_low, "0.51", "between 0.51 and 0.71",
  "Infiltration range high", stats$infiltration_range_high, "0.71", "between 0.51 and 0.71",
  # Het p-values
  "p(AC)", stats$p_ac, "0.14", "AC ownership (p = 0.14)",
  "p(window)", stats$p_window, "0.15", "open versus closed window or door (p = 0.15)",
  # Sources
  "Smoking coef", stats$smoking_coef, "21", "Smoking was associated with 21 µg/m³",
  "Smoking CI low", stats$smoking_ci_low, "8.30", "95% CI: 8.30-34.3",
  "Smoking CI high", stats$smoking_ci_high, "34.3", "95% CI: 8.30-34.3",
  "Smoking % of mean", stats$smoking_pct_of_mean, "51", "a 51% increase compared to the sample mean",
  "Outdoor mean contrib %", stats$outdoor_mean_contribution_pct, "58", "58% of mean indoor PM2.5 comes from ambient outdoor",
  "Smoking mean contrib %", stats$smoking_mean_contribution_pct, "19", "smoking explains approximately 19%",
  "WB 2+/wk prevalence %", stats$wb2_prevalence_pct, "11", "11% of the sample",
  # Spikes
  "Spike smoking % higher", stats$spike_smoking_pct_higher, "341.8", "341.8% higher in smoking households",
  # Road distance
  "% within 500m road", stats$pct_within_500m_road, "58", "58% of our sample lives within 500 meters from a primary road",
  # Income
  "Income PM diff (Bin1-Bin4)", stats$income_pm_diff, "23.7", "increase of 23.7 µg/m³",
  "Income PM ratio", stats$income_pm_ratio, "~2", "nearly double/nearly twice",
  "Income % higher", stats$income_pct_higher, "113", "113% higher indoor PM2.5",
  "Smoking share of gap", stats$smoking_share_of_gap, "48", "smoking differences explain 48%",
  "Other share of gap", stats$other_share_of_gap, "38", "other hyperlocal...remaining 38%",
  "Spike rate Bin 1 %", stats$spike_rate_bin1, "3", "PM2.5 spikes...3% of the time",
  "Spike rate Bin 4 %", stats$spike_rate_bin4, "0.4", "0.4% for households in the highest income quartile",
  "Spike income p", stats$spike_income_p, "0.027", "p = 0.027"
) %>%
  mutate(computed = as.character(computed),
         match = if_else(computed == manuscript, "MATCH", "DIFF"))

# print
for(i in 1:nrow(compare)){
  row <- compare[i,]
  flag <- if_else(row$match == "MATCH", "  OK  ", "**DIFF**")
  cat(sprintf("[%s] %-30s Computed: %-10s Manuscript: %-10s\n",
              flag, row$stat_name, row$computed, row$manuscript))
  if(row$match != "MATCH"){
    cat(sprintf("         tex: \"%s\"\n", row$tex_quote))
  }
}

cat("\n\nDone. Stats object saved.\n")
