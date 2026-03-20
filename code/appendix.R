
rm(list=ls_extra())
gc()

# read in survey data
survey <- 
  read_rds(file.path(ddir, "df_survey.rds"))  %>%
  mutate(income_high = hh_income >= 4, 
         house_size = case_when(housing_room_number < 5 ~ "Small",
                                housing_room_number >= 5 ~ "Big"),
         house_size = factor(house_size, levels = c("Small", "Big")))

# read in pm data
pm <- read_rds(file.path(ddir, "df_reg.rds")) %>%
  mutate(cooking = replace_na(cooking, 0))


rp_to_usd <- 0.000062

# =========================================================================
# show table of covariates 
# =========================================================================
vars <- c("Female", 
          "age",
          "hh_membercount_baseline", 
          "child_count_baseline",
          "hhexp", 
          "income_usd",
          "hoh_school_secondary",
          "hoh_school_tertiary", 
          "housing_ac", 
          "lpg",
          "hh_smokers")

# mean and std dev of sample  -----------------------------------------
survey <- 
  survey %>% 
  mutate(Female = resp_gender == "Female", 
         lpg = hh_cookingfuel == "Gas/LPG",
         hhexp = hhexp*rp_to_usd) 

mean_sd <- 
  survey %>%
  summarise_at(vars(vars), .funs = list(mean = mean, sd = sd), na.rm = TRUE) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("variable", "statistic"),
    names_pattern = "(.+)_(mean|sd)",
    values_to = "value"
  ) %>%
  mutate(metric = if_else(statistic == "mean", "estimate", "std.error")) 

n_all <- 
  survey %>%
  summarise_at(vars(vars), ~sum(!is.na(.x))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "N",
  ) %>%
  mutate(metric = "estimate")

# add in comparison to jakarta-wide population-----------------------------------
susenas <- 
  file.path(ddir, "Susenas23_DKI.dta") %>%
  haven::read_dta() 

susenas_hoh <- 
  susenas %>% 
  filter(R403 == 1) %>%
  mutate(hoh_secondary = R614 >= 8, 
         hoh_tertiary = R614 >= 14) %>%
  summarise(hoh_school_secondary_mean = wtd.mean(hoh_secondary, wert, na.rm = TRUE), 
            hoh_school_tertiary_mean = wtd.mean(hoh_tertiary, wert, na.rm = TRUE))

susenas_hh <- 
  susenas %>%
  group_by(urut, wert, R301, R302, R1817, R2001C) %>%
  summarise(count_smokers = sum(R1206 %in% c(1, 2)) + sum(R1207 %in% c(1, 2)), 
            hh_exp = sum(exp_pc)) %>%
  ungroup %>%
  summarise(hh_smokers_mean = wtd.mean(count_smokers, wert), 
            hh_smokers_sd = wtd.var(count_smokers, wert), 
            hhexp_mean = wtd.mean(hh_exp*rp_to_usd, wert), 
            hhexp_sd = sqrt(wtd.var(hh_exp*rp_to_usd, wert)), 
            hh_membercount_baseline_mean = wtd.mean(R301, wert), 
            hh_membercount_baseline_sd = sqrt(wtd.var(R301, wert)), 
            lpg_mean = wtd.mean(R1817 %in% c(2, 3, 4), wert), 
            housing_ac_mean = wtd.mean(R2001C == 1, wert))


susenas_mean <-
  bind_cols(susenas_hoh, susenas_hh) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("variable", "statistic"),
    names_pattern = "(.+)_(mean|sd)",
    values_to = "value"
  ) %>%
  mutate(metric = if_else(statistic == "mean", "estimate", "std.error")) %>%
  mutate(value = if_else(statistic == "sd", paste0("(", round(value,digits = 2), ")"), as.character(round(value, digits = 2)))) %>%
  arrange(variable, statistic) %>%
  rename(pop = value) %>%
  dplyr::select(-metric)


# bind together -----------------------------------

table_out <- 
  mean_sd %>%
  left_join(n_all) %>% 
  left_join(susenas_mean) %>%
  mutate(variable = factor(variable, levels = vars), 
         value = if_else(statistic == "sd", paste0("(", round(value,digits = 2), ")"), as.character(round(value, digits = 2))), 
         value = if_else(variable %in% c("Female", "hoh_school_secondary", "hoh_school_tertiary", "lpg", 
                                         "housing_ac_baseline", "who_ind_employed_hoh_baseline") & metric == "std.error", 
                         "", value)) %>%
  arrange(variable, statistic) %>% 
  mutate(variable = case_when(variable == "age" ~ "Age", 
                              variable == "who_ind_employed_baseline" ~ "Employed", 
                              variable == "hh_membercount_baseline" ~ "HH Size",
                              variable == "child_count_baseline" ~ "Number of Children",
                              variable == "income_usd" ~ "HH Monthly Income (USD)",
                              variable == "hhexp" ~ "HH Monthly Exp. (USD)",
                              variable == "hoh_school_secondary" ~ "HH Head Attended Secondary",
                              variable == "hoh_school_tertiary" ~ "HH Head Attended Tertiary",
                              variable == "housing_ac" ~ "Owns AC",
                              variable == "lpg" ~ "LPG Stove",
                              variable == "hh_smokers" ~ "Number of Smokers", 
                              TRUE ~ variable)) %>%
  mutate(variable = if_else(statistic == "sd", "", variable), 
         N = if_else(statistic == "sd", "", format(N, digits = 2))) %>%
  dplyr::select(Variable = variable, `Control Mean` = value, N, `Population Mean` = pop) %>%
  mutate_all(~if_else(is.na(.x), "", .x)) 

table_out %>%
  kable(format = "latex", booktabs = TRUE, linesep = "", align = c("l", rep("c", 5))) %>%
  # add_header_above(c("", paste0("(", 1:(ncol(table_out) -1 ), ")")), 
  #                 line = FALSE) %>%
  row_spec(nrow(table_out), hline_after = TRUE) %>%  # Adds a line above the last row
  writeLines(file.path(gdir, "output/tables/covariates.tex"))


# =========================================================================
# no evidence of selection in outdoor sensor locations
# =========================================================================
hh_pm_indoor <- 
  pm %>% 
  group_by(respondent_id, hour) %>% 
  summarise(pm_indoor = mean(pm25_indoor, na.rm = TRUE)) %>% 
  group_by(respondent_id) %>% 
  summarise(pm25_indoor = mean(pm_indoor, na.rm = TRUE))

hh_mindist <- 
  read_csv(file.path(ddir, "hh_sensor_dist.csv")) %>%
  group_by(respondent_id) %>%
  arrange(distance) %>%
  filter(row_number() == 1) %>%
  dplyr::select(respondent_id, sensor_mindist = distance, dist_central, dist_south) %>%
  full_join(survey) %>%
  left_join(hh_pm_indoor) %>% 
  ungroup() %>% 

  # fill in missing pm25_indoor with average 
  mutate(pm25_indoor = if_else(is.na(pm25_indoor), mean(hh_pm_indoor$pm25_indoor, na.rm = TRUE), pm25_indoor), 
        housing_room_number = if_else(is.na(housing_room_number), mean(housing_room_number, na.rm = TRUE), housing_room_number)) %>%
  filter(treatment_status == "Fan") # add in !hardware cancel later

xvars <- "pm25_indoor + pm25_outdoor24_baseline +income_usd  + housing_room_number + housing_ac +  
        hoh_employed + hoh_school_secondary + hoh_school_tertiary"

# ADD AVERAGE INDOOR PM, NUMBER OF ROOMS AND AC ONCE THE INSTALLATION DATA COMES IN, ALSO WEALTH INDEX

feols(as.formula(paste("sensor_mindist ~", xvars)), data = hh_mindist)  %>%
  etable(dict = c(pm25_indoor = "Indoor PM2.5", 
                  pm25_outdoor24_baseline = "Outdoor PM2.5", 
                  income_usd = "Income (USD)", 
                  housing_room_number = "Housing Size", 
                  housing_ac = "AC", 
                  hoh_employed = "HOH Employed", 
                  hoh_school_secondaryTRUE = "HOH Attended Secondary School", 
                  hoh_school_tertiaryTRUE = "HOH Attended Tertiary School", 
                  sensor_mindist = "Distance to Outdoor Sensor (meters)"), 
         drop = "Constant", fitstat = c("my", "n"), 
         digits = "r2", replace = TRUE,
         tex = TRUE, file = file.path(gdir, "output/tables/sensor_selection.tex"))



# =========================================================================
# other aggregation techniques for income-quartile specific pm2.5
# =========================================================================
tidy_up <- function(r){
  tidy(r, conf.int = TRUE) %>% 
    dplyr::select(term, conf.low95 = conf.low, conf.high95 = conf.high) %>%
    left_join((tidy(r, conf.int = TRUE, conf.level = .9))) %>%
    mutate(type = if_else(str_detect(all.vars(r$call)[1], "outdoor"), 
                          "Ambient Outdoor", "Indoor"))
}

pm %>%
  group_by(respondent_id, income_quart) %>%
  summarise(pm25_indoor = mean(pm25_indoor, na.rm = TRUE), 
            pm25_outdoor = mean(pm25_outdoor3, na.rm = TRUE))

pm_hh_hour <- 
  pm %>%
  group_by(respondent_id, hour, income_quart) %>%
  summarise(pm25_indoor = mean(pm25_indoor, na.rm = TRUE), 
            pm25_outdoor = mean(pm25_outdoor3, na.rm = TRUE))

r_outdoor1 <- feols(pm25_outdoor3 ~ income_quart + 0, data = pm, cluster = ~respondent_id + date_hour)
r_indoor1 <- feols(pm25_indoor ~ income_quart + 0, data = pm, cluster = ~respondent_id + date_hour)
r_outdoor2 <- feols(pm25_outdoor ~ income_quart + 0, data = pm_hh_hour, cluster = ~respondent_id)
r_indoor2 <- feols(pm25_indoor ~ income_quart + 0, data = pm_hh_hour, cluster = ~respondent_id) 
r_outdoor3 <- feols(pm25outdoor_mean ~ income_quart + 0, data = filter(survey, sensor_mindist < 2000), cluster = ~respondent_id)
r_indoor3 <- feols(pm25_mean ~ income_quart + 0, data = filter(survey, sensor_mindist < 2000), cluster = ~respondent_id)

income_mean <- 
  list(r_outdoor1, r_indoor1, r_outdoor2, r_indoor2, r_outdoor3, r_indoor3) %>%
  map2_df(c(rep("HH-Hour-Day", 2), rep("HH-Hour-of-Day", 2), 
            rep("HH, <2km from Outdoor Sensor", 2)), 
       ~mutate(tidy_up(.x), model = !!.y)) %>%
  mutate(term = str_replace(term, "income_quart", ""), 
         term = if_else(term == "Income Bin 1", "Income Bin 1 (lowest)", term), 
         term = factor(term, levels = c("Income Bin 4", "Income Bin 3", 
                                        "Income Bin 2", "Income Bin 1 (lowest)")))  

p_income <- 
  income_mean %>%
  ggplot(aes(y = term, x = estimate, color = type, shape = model)) + 
  geom_point(size = 1, position=position_dodge(width=0.5)) + 
  facet_wrap(~type, scales = "free_x") +
  geom_errorbar(aes(xmin = conf.low95, xmax = conf.high95), width = 0, alpha = .4, 
                position=position_dodge(width=0.5)) + 
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0, 
                position=position_dodge(width=0.5)) + 
  theme_classic() + 
  scale_color_brewer(palette = "Dark2") + 
  xlim(c(0, 90)) + 
  # ggtitle("a.") + 
  guides(colour = "none", 
         shape = guide_legend(reverse=T)) + 
  theme(title = element_text(face = "bold",size = 6),
        axis.text =element_text(size = 6), 
        axis.line = element_line(size = .1),
        axis.ticks = element_line(size = .1),
        axis.title = element_text(size = 6),
        axis.title.y = element_blank(), 
        strip.text= element_text(size = 6),
        strip.background = element_blank(),
        panel.grid= element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA), 
        legend.position = "bottom", 
        legend.title = element_blank(), 
        legend.text = element_text(size = 5), 
        legend.box.margin=margin(-10,-10,0, 0),        
        legend.key.size = unit(0.05, "cm")) + 
  xlab(expression(PM[2.5] ~ (mu * g/m^3))); p_income

ggsave(file.path(gdir, "output/figures/pm_income_robustness.png"), 
       width = 14, height= 6, bg = "transparent", units = "cm")

# ==================================================
# infiltration rate - lags
# ==================================================
x_pm_lags <- paste0(c("pm25_outdoor3", paste0("pm25_outdoor3_lag", 1:11)), collapse = "+ ")
fes <- c("hour + week", "respondent_id + hour + week", "respondent_id^hour + week")

reg_lags <-
  paste0("pm25_indoor", "~", x_pm_lags, "+temp_outdoor3 + humidity_outdoor3 + as.factor(trash_burning_1week_baseline) + as.factor(smoke24_endline) + as.factor(room_pmsource_kitchen) + cooking + dist_primary |") %>%
  paste0(fes) %>%
  map(~feols(as.formula(.x), data = pm, cluster= ~respondent_id+date_hour))

coef_lags <-
  reg_lags %>%
  map2_df(fes, function(x, y){
    tidy(x, conf.int = TRUE) %>%
      dplyr::select(term, conf.low95 = conf.low, conf.high95 = conf.high) %>%
      left_join((tidy(x, conf.int = TRUE, conf.level = .9))) %>%
      mutate(spec = !!y) 
  }) %>%
  mutate(lag = str_replace_all(term, "log\\(|pm25_outdoor3|_|\\)|lag", ""), 
         lag = if_else(lag == "", 0, as.numeric(lag)))  %>%
  filter(!is.na(lag)) %>%
  group_by(spec) %>%
  mutate(sum_beta = round(sum(estimate), digits = 2))

p_lags <- 
  coef_lags %>%
  mutate(spec = str_replace(spec, "hour", "Hour"), 
         spec = str_replace(spec, "week", "Week"), 
         spec = str_replace(spec, "respondent_id", "HH"), 
         spec = factor(spec, levels = c("Hour + Week", "HH + Hour + Week", "HH^Hour + Week"))) %>%
  ggplot(aes(x = lag, y = estimate)) + 
  geom_errorbar(aes(ymin = conf.low95, ymax = conf.high95), width = 0, alpha = .4, size = .2) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0,size = .2) + 
  geom_point(size = .2) + 
  theme_classic() + 
  facet_wrap(~spec, scales = "free_y") +
  ylim(c(-.2, 1)) + 
  geom_hline(aes(yintercept = 0), size = .1) + 
  xlab("Time lag k (hours)") + 
  ylab(expression(beta[k]))+
  theme(axis.title = element_text(size = 5),
        axis.line.x = element_blank(),
        axis.line.y = element_line(size = .1),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(size = .1),
        axis.title.y = element_text(angle = 0, vjust = .5),
        axis.text = element_text(size = 4),
        strip.text = element_text(size = 6),
        strip.background =element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  geom_text(aes(x = 8, y = .5, label = paste0("Inf. Rate = ", sum_beta)), size =2) ; p_lags

ggsave(file.path(gdir, "output/figures/inf_lags.png"), width = 13, height= 6, bg = "transparent", units = "cm")


# =========================================================================
# Infiltration at different levels of aggregation 
# =========================================================================
# show main results
reg_main <-
  paste0("pm25_indoor~pm25_outdoor3+temp_outdoor3 + humidity_outdoor3 |", fes) %>%
  map(~feols(as.formula(.x), data = pm, cluster= ~respondent_id+date_hour)) %>%
  append(list(feols(pm25_indoor ~ pm25_outdoor3 + temp_outdoor3 + humidity_outdoor3 | respondent_id^hour + week,
                    data = filter(pm, sensor_mindist < 1000), cluster = ~respondent_id+date_hour)))

tidy_main <- 
  map2_df(reg_main,  c("Hour +\nWeek FE", "HH + Hour\n+ Week FE", "HH^Hour\n+ Week FE", "<1km to\nOutdoor\nSensor"), 
    ~tidy_up(.x) %>% mutate(reg = .y, type = "Main, 1 hour"))

# aggregate to 8 hour chunks
pm_agg8 <-
  pm %>%
  mutate(pm25_outdoor3_matchmissing = if_else(is.na(pm25_indoor), NA_real_, pm25_outdoor3),
    # split day up into 8 hour chunks
    period = case_when(hour(date_hour) %in% c(22, 23, 0:5) ~ "Night",
                       hour(date_hour) %in% 6:14 ~ "Daytime",
                       TRUE ~ "Afternoon"),
    date = date_hour) %>%
  group_by(respondent_id, date, period, week, sensor_mindist) %>%
  summarise(pm25_outdoor8hr = mean(pm25_outdoor3, na.rm = TRUE),
            pm25_outdoor_matchmissing8hr = mean(pm25_outdoor3_matchmissing, na.rm = TRUE),
         pm25_indoor8hr = mean(pm25_indoor, na.rm = TRUE),
         temp_outdoor8hr = mean(temp_outdoor3, na.rm = TRUE),
         humidity_outdoor8hr = mean(humidity_outdoor3, na.rm = TRUE)) %>%
 ungroup()

reg_8hours <-
  list(feols(pm25_indoor8hr ~ pm25_outdoor_matchmissing8hr + temp_outdoor8hr + humidity_outdoor8hr | period + week,
            data = pm_agg8, cluster = ~respondent_id+date8hr),
       feols(pm25_indoor8hr ~ pm25_outdoor_matchmissing8hr + temp_outdoor8hr + humidity_outdoor8hr | period + respondent_id+ week,
            data = pm_agg8, cluster = ~respondent_id+date8hr),
       feols(pm25_indoor8hr ~ pm25_outdoor_matchmissing8hr + temp_outdoor8hr + humidity_outdoor8hr | respondent_id^period + week,
            data = pm_agg8, cluster = ~respondent_id+date8hr),
       feols(pm25_indoor8hr ~ pm25_outdoor_matchmissing8hr + temp_outdoor8hr + humidity_outdoor8hr | respondent_id^period + week,
            data = filter(pm_agg8, sensor_mindist < 1000), cluster = ~respondent_id+date8hr))  

tidy_8hours <- 
  map2_df(reg_8hours, c("Period +\nWeek FE", "HH + Period\n+ Week FE", "HH^Period\n+ Week FE", "<1km to\nOutdoor\nSensor"), 
    ~tidy_up(.x) %>% mutate(reg = .y, type = "8 hours"))


# 24 hour averages ----------------------------
pm_agg24 <-
  pm %>%
  mutate(pm25_outdoor3_matchmissing = if_else(is.na(pm25_indoor), NA_real_, pm25_outdoor3),
    date = as.Date(date_hour)) %>%
  arrange(date_hour) %>%
  group_by(respondent_id, date, week, sensor_mindist) %>%
  summarise(pm25_outdoor24hr = mean(pm25_outdoor3, na.rm = TRUE),
            pm25_outdoor_matchmissing24hr = mean(pm25_outdoor3_matchmissing, na.rm = TRUE),
         pm25_indoor24hr = mean(pm25_indoor, na.rm = TRUE),
         temp_outdoor24hr = mean(temp_outdoor3, na.rm = TRUE),
         humidity_outdoor24hr = mean(humidity_outdoor3, na.rm = TRUE)) %>%
 ungroup()

reg_24hours <-
  list(feols(pm25_indoor24hr ~ pm25_outdoor_matchmissing24hr + temp_outdoor24hr + humidity_outdoor24hr | week,
            data = pm_agg24, cluster = ~respondent_id+date24hr),
       feols(pm25_indoor24hr ~ pm25_outdoor_matchmissing24hr + temp_outdoor24hr + humidity_outdoor24hr | respondent_id + week,
            data = pm_agg24, cluster = ~respondent_id+date24hr),
       feols(pm25_indoor24hr ~ pm25_outdoor_matchmissing24hr + temp_outdoor24hr + humidity_outdoor24hr | respondent_id + week,
            data = filter(pm_agg24, sensor_mindist < 1000), cluster = ~respondent_id+date24hr))

tidy_24hours <- 
  map2_df(reg_24hours, c("Week FE", "HH + Week FE", "<1km to\nOutdoor\nSensor"), 
    ~tidy_up(.x) %>% mutate(reg = .y, type = "24 hours"))


bind_rows(tidy_main, tidy_8hours, tidy_24hours) %>%
  filter(str_detect(term, "pm25")) %>%
   mutate(reg = factor(reg, levels = c("Hour +\nWeek FE", "HH + Hour\n+ Week FE", "HH^Hour\n+ Week FE", 
                                       "Period +\nWeek FE", "HH + Period\n+ Week FE", "HH^Period\n+ Week FE", 
                                       "Week FE", "HH + Week FE", "<1km to\nOutdoor\nSensor")), 
          type = factor(type, levels = c("Main, 1 hour", "8 hours", "24 hours")))  %>%
  ggplot(aes(x = reg, y = estimate, fill = type, color = type)) + 
  facet_wrap(~type, scales = "free_x") + 
  geom_errorbar(aes(ymin = conf.low95, ymax = conf.high95), width = 0, alpha = .4, size = .2) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0,size = .2) + 
  geom_point(size = .5) + 
  theme_classic() +
  ylab("Infiltration Factor") + 
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(limits = c(0, 1.2), expand = c(0, 0)) +
  theme(axis.line = element_line(size = .1), 
        axis.ticks = element_line(size = .1),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA), 
        panel.grid.major.y = element_line(size = .1, color = "gray80"),

        text = element_text(size = 6),
        legend.position = "none",
        strip.text = element_text(size = 7), 
        strip.background = element_blank(), 
        )
ggsave(file.path(gdir, "output/figures/inf_aggregation.png"), width = 14, height= 6, bg = "transparent", units = "cm")
# now just with customers for whom we have a lot of data for
# actually we can't do this - because missingness is completely correlated with income, and
# high income households have low infiltration

# ===================================================================
# is reported waste burning correlated with higher outdoor pollution
# ===================================================================
reg_outdoor_burning <-
  feols(pm25_outdoor3 ~ trash_burning_1week_baseline + temp_outdoor3 + humidity_outdoor3| week + hour, data = pm,
      cluster = ~respondent_id+date_hour)

reg_outdoor_burning_2k <-
  feols(pm25_outdoor3 ~ trash_burning_1week_baseline + temp_outdoor3 + humidity_outdoor3| week + hour, data = filter(pm, sensor_mindist < 2000),
      cluster = ~respondent_id+date_hour)

etable(reg_outdoor_burning, reg_outdoor_burning_2k, 
        dict =c("trash_burning_1week_baseline1or2times" = "Waste Burning (1-2/week)", 
                "trash_burning_1week_baseline3ormoretimes" = "Waste Burning (2+/week)", 
                "temp_outdoor3" = "Outdoor Temperature",
                "humidity_outdoor3" = "Outdoor Humidity",
                pm25_outdoor3 = "Outdoor PM2.5", 
                week = "Week FE",
                hour = "Hour FE"), 
      headers = c("Closest 3 Sensors", "HH Within 2km of Outdoor Sensor"),
      digits = 3, tex = TRUE, replace = TRUE,
      file = file.path(gdir, "output/tables/reg_outdoor_burning.tex"))



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ------- Density of distance to roads --------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pm %>%
  dplyr::distinct(respondent_id, dist_motorway, dist_primary, dist_secondary, 
        dist_tertiary) %>%
  pivot_longer(cols = starts_with("dist"), names_to = "road_type", values_to = "distance")  %>%
  mutate(Type = str_to_title(str_remove(road_type, "dist_"))) %>%
  ggplot(aes(x = distance, color = Type)) +
  stat_ecdf() + 
  scale_color_brewer(palette = "Dark2") + 
  theme_classic() + 
  theme(legend.position = "bottom",
        text = element_text(size = 15)) +
  xlab("Distance (meters)")
ggsave(file.path(gdir, "output/figures/distance_to_road_ecdf.png"),
    width = 15, height= 10, bg = "transparent", units = "cm")



# =========================================================================
# find pairwise correlations between sensors
# =========================================================================
df_sensors <- 
  st_read(file.path(ddir, "sensor_locations/sensor_locations.shp")) %>%
  filter(n3 == 1, name != "Muara Karang")

# find parwise combination of names of sensors
pairs <- 
  combn(df_sensors$name,2) %>% 
  t() %>%
  as_tibble() %>%
  setNames(c("sensor1", "sensor2"))

# find distance between pairs
pairs_dist <- 
  pairs %>%
  left_join(dplyr::select(df_sensors, sensor1 = name, geometry1 = geometry)) %>%
  left_join(dplyr::select(df_sensors, sensor2 = name, geometry2 = geometry)) %>%
  mutate(dist = as.numeric(st_distance(geometry1, geometry2, by_element = TRUE)))

# -----------------------------------------------------
# for each pair, calculate R2 between the sensor data
# -----------------------------------------------------
pm_sensors <- 
  read_rds(file.path(ddir, "pm_outdoor_bysensor.rds")) %>%
  filter(measure == "pm25") %>%
  dplyr::select(-measure) %>% 
  mutate(value = if_else(value <= 1, NA_real_, value)) %>%
  dplyr::select(sensor_name, date_hour, value)  %>%
  distinct(sensor_name, date_hour, .keep_all = TRUE)

sensors_r2 <- 
  pmap(pairs, list) %>%
  map_dbl(function(x){
    df_r2 <- 
      filter(pm_sensors, sensor_name %in% c(x$sensor1, x$sensor2)) %>%
      mutate(value = log(value)) %>%
      pivot_wider(id_cols = "date_hour", names_from = "sensor_name", values_from = "value") %>%
      janitor::clean_names()
    f <- paste0(colnames(df_r2)[2], "~", colnames(df_r2)[3])
    
    r2 <- summary(lm(as.formula(f), df_r2))$r.squared
  })

pairs_dist <- 
  pairs_dist %>%
  mutate(r2 = as.numeric(sensors_r2))

# correlation of log transformations
# plot correlation between two sensors as a function of the distance between the sensors 
ggplot(pairs_dist, aes(x = dist / 1000, y = r2)) +
  #geom_bin2d(bins = 20) +
  #scale_fill_continuous(type = "viridis") +
  xlab("pairwise distance (km)") + 
  ylab(expression(R^2)) + 
  geom_point(color = "gray40", alpha = .5) + 
  # geom_smooth(color="black", se = FALSE)  +
  stat_summary_bin(geom="line", size = 1)+ 
  theme_linedraw() + 
  scale_fill_gradient2(high = "darkgreen")  +
  theme(axis.title.y=element_text(angle=0, vjust = .5), text = element_text(size = 24)) + 
  ylim(0, 1)
ggsave(file.path(gdir, "output/figures/sensor_correlations.png"), width = 13, height= 8)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Spike decomposition (moved from fig3_source_decomposition)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rhs_fml_sources <- ~ as.factor(trash_burning_1week_baseline) +
  as.factor(smoke24_endline) +
  as.factor(room_pmsource_kitchen) +
  pm25_outdoor3 +
  cooking +
  dist_primary +
  temp_outdoor3 +
  humidity_outdoor3

dict_sources <- c(
  "pm25_outdoor3" = "Outdoor Ambient",
  "cooking" = "Cooking",
  "dist_primary" = "Distance to Main Road",
  "as.factor(smoke24_endline)1" = "Smoking Household",
  "as.factor(smoke24_endline)" = "Smoking Household",
  "as.factor(room_pmsource_kitchen)1" = "Kitchen source",
  "as.factor(room_pmsource_kitchen)" = "Kitchen source",
  "as.factor(trash_burning_1week_baseline)1 or 2 times" = "Waste Burning (1-2/week)",
  "as.factor(trash_burning_1week_baseline)3 or more times" = "Waste Burning (2+/week)",
  "as.factor(trash_burning_1week_baseline)" = "Waste Burning"
)

compute_mean_contrib_app <- function(model, rhs_fml, data, outcome_var) {
  X <- stats::model.matrix(rhs_fml, data = data)
  if ("(Intercept)" %in% colnames(X)) {
    X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  }
  coef_est <- coef(model)
  coeftable <- summary(model)$coeftable
  p_col <- grep("^Pr\\(", colnames(coeftable), value = TRUE)[1]
  pvals <- rep(NA_real_, length(coef_est))
  names(pvals) <- names(coef_est)
  if (!is.na(p_col)) {
    shared_terms <- intersect(names(pvals), rownames(coeftable))
    pvals[shared_terms] <- coeftable[shared_terms, p_col]
  }
  ci_low <- rep(NA_real_, length(coef_est))
  ci_high <- rep(NA_real_, length(coef_est))
  names(ci_low) <- names(coef_est)
  names(ci_high) <- names(coef_est)
  ci <- tryCatch(confint(model, level = 0.95), error = function(e) NULL)
  if (!is.null(ci)) {
    ci_terms <- intersect(names(coef_est), rownames(ci))
    ci_low[ci_terms] <- ci[ci_terms, 1]
    ci_high[ci_terms] <- ci[ci_terms, 2]
  }
  coef_tbl <- tibble(term = names(coef_est), estimate = unname(coef_est),
                     pvalue = unname(pvals), conf.low = unname(ci_low), conf.high = unname(ci_high))
  mean_tbl <- tibble(term = colnames(X), mean_x = colMeans(X, na.rm = TRUE))
  mean_outcome <- mean(data[[outcome_var]], na.rm = TRUE)
  coef_tbl %>%
    left_join(mean_tbl, by = "term") %>%
    mutate(contribution = estimate * mean_x, frac = contribution / mean_outcome,
           frac.low = mean_x * conf.low / mean_outcome, frac.high = mean_x * conf.high / mean_outcome)
}

reg_spikes <- feols(
  fixest::xpd(spike ~ .[rhs_fml] | hour + week),
  data = pm,
  cluster = ~ respondent_id + date_hour
)

output_spike_contrib <- compute_mean_contrib_app(
  model = reg_spikes,
  rhs_fml = rhs_fml_sources,
  data = pm,
  outcome_var = "spike"
) %>%
  filter(!term %in% c("temp_outdoor3", "humidity_outdoor3"))

# ---- Variance Decomposition (LMG) for spikes ----
reg_spike_lm <- lm(
  spike ~ pm25_outdoor3 +
    as.factor(smoke24_endline) +
    as.factor(trash_burning_1week_baseline) +
    as.factor(room_pmsource_kitchen) +
    cooking +
    dist_primary +
    as.factor(hour) +
    as.factor(week),
  data = pm
)

compute_lmg_tbl_app <- function(lm_model) {
  relimp <- relaimpo::calc.relimp(lm_model, type = "lmg", rela = TRUE)
  tibble(term = names(relimp$lmg), lmg = unname(relimp$lmg)) %>%
    arrange(desc(lmg)) %>%
    filter(!str_detect(term, "as.factor\\(week|hour"))
}

output_lmg_spike_tbl <- compute_lmg_tbl_app(reg_spike_lm)

output_spike <-
  output_spike_contrib %>%
  mutate(term = dplyr::recode(term, !!!dict_sources)) %>%
  full_join(mutate(output_lmg_spike_tbl, term = dplyr::recode(term, !!!dict_sources)), by = "term") %>%
  mutate(lmg = scales::percent(lmg, accuracy = 1),
         term = factor(term, levels = c("Outdoor Ambient", "Smoking Household", "Waste Burning (1-2/week)",
                                        "Waste Burning (2+/week)", "Waste Burning", "Kitchen source", "Cooking",
                                        "Distance to Main Road"))) %>%
  arrange(term)

options(knitr.kable.NA = '')
output_spike %>%
  mutate(frac = scales::percent(frac, accuracy = 1)) %>%
  dplyr::select("Source" = term, "Reg. Estimate" = estimate, "p.value" = pvalue,
      "Mean Value of Source" = mean_x, "Mean Contribution" = frac,
      "R2 Contribution" = lmg) %>%
  knitr::kable(format.args = list(big.mark = ","),
              digits = c(NA, 3, 3, 2, 1, 1), format = "latex",
              booktabs = TRUE, align = "c") %>%
 # kableExtra::kable_styling(full_width = FALSE, latex_options = c("HOLD_position", "scale_down")) %>%
  writeLines(file.path(gdir, "output/tables/spike_contribution_table.tex"))

# Left panel: coefficient plot (mirroring fig3 panel a style)
spike_source_terms <- c(
  "as.factor(trash_burning_1week_baseline)1 or 2 times",
  "as.factor(trash_burning_1week_baseline)3 or more times",
  "as.factor(smoke24_endline)1",
  "as.factor(room_pmsource_kitchen)1",
  "cooking",
  "dist_primary"
)
mean_spike <- mean(pm$spike, na.rm = TRUE)

coef_spike <-
  tidy(reg_spikes, conf.int = TRUE) %>%
  dplyr::select(term, estimate, conf.low95 = conf.low, conf.high95 = conf.high) %>%
  left_join(tidy(reg_spikes, conf.int = TRUE, conf.level = 0.9) %>%
              dplyr::select(term, conf.low, conf.high)) %>%
  filter(term %in% spike_source_terms) %>%
  mutate(term = dplyr::recode(term, !!!dict_sources),
         term = factor(term, levels = rev(c("Smoking Household",
                                            "Waste Burning (2+/week)",
                                            "Waste Burning (1-2/week)",
                                            "Kitchen source",
                                            "Cooking",
                                            "Distance to Main Road"))))

p_spike_coef <-
  ggplot(coef_spike, aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, color = "gray50", linetype = "dashed", linewidth = .1) +
  geom_vline(xintercept = mean_spike, linetype = "dashed", color = "#1b9e77", size = .2) +
  geom_errorbarh(aes(xmin = conf.low95, xmax = conf.high95), height = 0, alpha = .4, size = .2, color = "#1b9e77") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, size = .2, color = "#1b9e77") +
  geom_point(size = .8, color = "#1b9e77") +
  xlab(expression(Pr(PM[2.5]~Spike))) +
  ylab("Effect of\nHyperlocal\nSource") +
  annotate("text", x = mean_spike, y = 6.5, label = "mean", size = 1.5, color = "#1b9e77") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.line.y = element_blank(),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 5, angle = 90),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        text = element_text(size = 6),
        axis.line = element_line(size = .1),
        axis.ticks = element_line(size = .1),
        legend.position = "none")

# Right panel: contribution bars (horizontal, mirroring fig3 panel b)
p_spike <-
  output_spike %>%
  filter(!is.na(frac)) %>%
  mutate(outdoor = str_detect(term, "Outdoor"),
         term = str_wrap(term, 12),
         term = factor(term, levels = c("Distance to\nMain Road", "Cooking", "Kitchen\nsource",
                                        "Waste\nBurning\n(1-2/week)", "Waste\nBurning\n(2+/week)",
                                        "Smoking\nHousehold", "Outdoor\nAmbient"))) %>%
  ggplot(aes(y = term, x = frac, fill = outdoor)) +
  geom_col(width = .75) +
  geom_errorbarh(aes(xmin = frac.low, xmax = frac.high),
        height = 0, size = .1, color = "gray60") +
  geom_vline(xintercept = 0, size = .1) +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-.382, .81)) +
  ylab("Source") +
  xlab("Estimated Contribution to PM2.5 Spikes (%)") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 6, angle = 90),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        text = element_text(size = 6),
        axis.line = element_line(size = .1),
        axis.ticks = element_line(size = .1),
        legend.position = "none")

p_spike_coef + p_spike +
  plot_annotation(tag_levels = "a", tag_suffix = ".")
ggsave(file.path(gdir, "output/figures/fig_appendix_spike_sources.png"),
    width = 14, height = 8, bg = "transparent", units = "cm")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Missing data diagnostics
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1) CDF of fraction missing by household
hh_missing <-
  pm %>%
  group_by(respondent_id) %>%
  summarise(n_total = n(),
            n_missing = sum(is.na(pm25_indoor)),
            frac_missing = n_missing / n_total,
            .groups = "drop")

p_missing_cdf <-
  ggplot(hh_missing, aes(x = frac_missing)) +
  stat_ecdf(color = "#1b9e77", linewidth = .4) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  xlab("Fraction of Hours with Missing Indoor PM2.5") +
  ylab("Cumulative Share\nof Households") +
  theme_classic() +
  theme(axis.title = element_text(size = 6),
        axis.text = element_text(size = 5),
        axis.line = element_line(size = .1),
        axis.ticks = element_line(size = .1),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA)); p_missing_cdf

ggsave(file.path(gdir, "output/figures/fig_appendix_missing_data.png"),
       width = 8, height = 6, bg = "transparent", units = "cm")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Robustness: infiltration under alternative weighting schemes
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Weight 2: inverse frequency by household (upweight HHs with more missing data)
hh_weights <- pm %>%
  group_by(respondent_id) %>%
  summarise(prop_observed_hh = mean(!is.na(pm25_indoor)), .groups = "drop") %>%
  mutate(weight_hh = 1 / prop_observed_hh)

# Weight 3: inverse frequency by household x hour
hh_hour_weights <- pm %>%
  group_by(respondent_id, hour) %>%
  summarise(prop_observed_hh_hour = mean(!is.na(pm25_indoor)), .groups = "drop") %>%
  mutate(weight_hh_hour = 1 / prop_observed_hh_hour)

pm_weights <- pm %>%
  left_join(hh_weights, by = "respondent_id") %>%
  left_join(hh_hour_weights, by = c("respondent_id", "hour"))

# Col 1: main spec (hour weights)
reg_w1 <- feols(fixest::xpd(pm25_indoor ~ .[rhs_fml] | hour + week),
                data = pm_weights, cluster = ~respondent_id + date_hour)

# Col 2: household weights
reg_w2 <- feols(fixest::xpd(pm25_indoor ~ .[rhs_fml] | hour + week),
                data = pm_weights, cluster = ~respondent_id + date_hour,
                weights = ~weight_hh)

# Col 3: household-hour weights
reg_w3 <- feols(fixest::xpd(pm25_indoor ~ .[rhs_fml] | hour + week),
                data = pm_weights, cluster = ~respondent_id + date_hour,
                weights = ~weight_hh_hour)

etable(reg_w1, reg_w2, reg_w3,
       headers = c("Hour Weights", "HH Weights", "HH-Hour Weights"),
       keep = "Outdoor",
       dict = c("pm25_outdoor3" = "Outdoor PM2.5 (Infiltration Rate)"),
       fitstat = c("n", "r2"),
       tex = TRUE, replace = TRUE,
       title = "Infiltration Rate Under Alternative Weighting Schemes",
       file = file.path(gdir, "output/tables/infiltration_weights_robustness.tex"))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Robustness: infiltration by distance to outdoor sensor
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dist_cuts <- c(500, 1000, 2000, Inf)
dist_labels <- c("<500m", "<1km", "<2km", "All")

regs_dist <- map2(dist_cuts, dist_labels, function(d, lab) {
  dat <- if (is.infinite(d)) pm else filter(pm, sensor_mindist < d)
  feols(fixest::xpd(pm25_indoor ~ .[rhs_fml] | hour + week),
        data = dat, cluster = ~respondent_id + date_hour)
})

p_dist_robust <-
  map2_df(regs_dist, dist_labels, function(x, y) {
    tidy(x, conf.int = TRUE) %>%
      dplyr::select(term, conf.low95 = conf.low, conf.high95 = conf.high) %>%
      mutate(model = y) %>%
      left_join(tidy(x, conf.int = TRUE, conf.level = .9))
  }) %>%
  filter(str_detect(term, "pm25_outdoor3")) %>%
  mutate(model = factor(model, levels = dist_labels)) %>%
  ggplot(aes(x = model, y = estimate)) +
  geom_errorbar(aes(ymin = conf.low95, ymax = conf.high95), width = 0, alpha = .4, size = .2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, size = .2) +
  geom_point(size = .5) +
  ylab("Infiltration Factor") +
  xlab("Maximum Distance to Outdoor Sensor") +
  scale_y_continuous(limits = c(0, 1.1), breaks = c(0.25, 0.5, 0.75, 1)) +
  theme_classic() +
  theme(axis.title = element_text(size = 6),
        axis.text = element_text(size = 5),
        axis.line = element_line(size = .1),
        axis.ticks = element_line(size = .1),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA)); p_dist_robust

ggsave(file.path(gdir, "output/figures/fig_appendix_inf_by_distance.png"),
       width = 8, height = 6, bg = "transparent", units = "cm")
