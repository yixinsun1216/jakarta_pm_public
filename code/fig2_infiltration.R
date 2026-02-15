
rm(list=ls_extra())
gc()



theme_inf <- 
  theme_classic() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA), 
        plot.background = element_rect(fill = "transparent", colour = NA), 
        axis.line = element_line(size = .1), 
        axis.ticks = element_line(size = .1), 
        axis.text = element_text(size = 5), 
        axis.title = element_text(size = 6), 
        plot.tag = element_text(size = 6, face = "bold"), 
        panel.spacing = unit(0, "lines"), # Remove spacing between panels
        strip.background = element_blank(), # Optional: Customize facet strip background
        strip.text = element_text(size = 6), # Customize facet strip text
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 6),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 6))
theme_set(theme_inf)


# read in survey data
survey <- 
  read_rds(file.path(ddir, "df_survey.RDS"))  %>%
  mutate(income_high = hh_income >= 4, 
         house_size = case_when(housing_room_number < 3 ~ "Small", 
                                housing_room_number < 6 ~ "Medium", 
                                housing_room_number >= 6 ~"Big"), 
         house_size = factor(house_size, levels = c("Small", "Medium", "Big")))

# read in pm data
pm <- 
  read_rds(file.path(ddir, "df_reg.rds"))  %>%
  mutate(night = if_else(hour >= 19 | hour <= 6, "Night", "Day"), 
         pm25_indoor = if_else(pm25_indoor == 1, NA_real_, pm25_indoor), 
         splines = Hmisc::cut2(pm25_outdoor3, cuts = c(30, 40, 50)), 
         income_high = hh_income >= 4, 
         spike = pm25_indoor > 100, 
         room_ac = if_else(room_ac == 1, "AC", "No AC"),
         house_size = case_when(housing_room_number < 5 ~ "Small", 
                                housing_room_number >= 5 ~"Big"), 
         house_size = factor(house_size, levels = c("Small", "Big"))) 


# ==================================================
# Panel 3a. main infiltration graph
# ==================================================
# robustness -----------------------------------------------
reg1 <- feols(pm25_indoor ~ pm25_outdoor3 + temp_outdoor3 + humidity_outdoor3 | hour + week, 
              pm, cluster = ~respondent_id+date_hour) 

reg2 <- feols(pm25_indoor ~ pm25_outdoor3 + temp_outdoor3 + humidity_outdoor3 | respondent_id + hour + week, 
              pm, cluster = ~respondent_id+date_hour) 

reg3 <- feols(pm25_indoor ~ pm25_outdoor3 + temp_outdoor3 + humidity_outdoor3 | respondent_id^hour + week, 
              pm, cluster = ~respondent_id+date_hour) 

reg4 <- feols(pm25_indoor ~ pm25_outdoor3 + temp_outdoor3 + humidity_outdoor3 | respondent_id^hour + week, 
              filter(pm, sensor_mindist < 1000), cluster = ~respondent_id+date_hour) 

us_inf <- .29

p_inf <- 
  map2_df(list(reg1, reg2, reg3, reg4), c("Main, Hour + Week FE", "HH + Hour + Week FE", "HH-Hour + Week FE",  "<1km to Outdoor Sensor"), 
          function(x, y){
            tidy(x, conf.int = TRUE) %>% 
              dplyr::select(term, conf.low95 = conf.low, conf.high95 = conf.high) %>%
              mutate(model = y) %>%
              left_join((tidy(x, conf.int = TRUE, conf.level = .9))) 
          } ) %>%
  filter(str_detect(term, "pm25")) %>%
  mutate(main = if_else(str_detect(model, "Main"), TRUE, FALSE), 
         model = str_wrap(model, width = 6),
         model = factor(model, levels = c("Main,\nHour\n+ Week\nFE", "HH +\nHour\n+ Week\nFE", "HH-Hour\n+ Week\nFE", "<1km\nto\nOutdoor\nSensor"))) %>% 
  ggplot(aes(x = model, y = estimate)) + 
  geom_errorbar(aes(ymin = conf.low95, ymax = conf.high95), width = 0, alpha = .39, size = .2) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0,size = .2) + 
  geom_point(size = .5) + 
  ylab("Infiltration Rate") + 
  facet_wrap(~"Overall") + 
  ylim(c(0, 1.5)) +
  # geom_hline(aes(yintercept = reg1$coefficients[1]), color = "gray50", linetype = "dashed") + 
  # geom_text(aes(x = 4, y = .75, label = "Main Specification"), size = 4, color = "gray50") +
  geom_hline(aes(yintercept = .29), color = "#d95f02", linetype = "dashed", size = .1) + 
  geom_text(aes(x = 3.5, y = .4, label = "US Inf. Rate\n(Lunderberg et. al 2023)"), size = 1.4, color = "#d95f02") + 
  theme(axis.text.x = element_text(size = 4.5)) ; p_inf


# ==================================================
# 3c. heterogeneity of infiltration rates
# ==================================================
vars <- c("income_quart", "night", "house_size", "room_ac")
titles <- c("Income", "Day/Night", "House Size", "AC")
tests <- c("income_quartIncome Bin 1:pm25_outdoor3 = income_quartIncome Bin 4:pm25_outdoor3", 
           "nightDay:pm25_outdoor3 = nightNight:pm25_outdoor3", 
           "house_sizeSmall:pm25_outdoor3 = house_sizeBig:pm25_outdoor3",
           "room_acAC:pm25_outdoor3 = room_acNo AC:pm25_outdoor3")

# run all heterogeneity regressions and make into tidy dataframe
# add ftest 
regs_het <- 
  pmap_df(list(vars, titles, tests), function(var, title, test){
    r <-
      paste0("pm25_indoor ~", var, ":pm25_outdoor3+", var, "+ temp_outdoor3 +humidity_outdoor3 | hour + week") %>%
      as.formula() %>%
      feols(pm, cluster = ~respondent_id) 
    
    f_stat <- str_replace_all(test, "house_size|:pm25_outdoor3|income_quartIncome |room_ac|night", "")
    f_stat <- paste0(f_stat, ": ", round(linearHypothesis(r, c(test))$`Pr(>Chisq)`[2], digits = 2))
    
    tidy(r, conf.int = TRUE) %>%
      dplyr::select(term, conf.low95 = conf.low, conf.high95 = conf.high) %>%
      left_join((tidy(r, conf.int = TRUE, conf.level = .9))) %>%
      filter(str_detect(term, "pm25")) %>%
      mutate(fstat = f_stat, 
             title = !!title)
  }) %>%
  mutate(type = str_replace_all(term, "house_size|:pm25_outdoor3|income_quartIncome |room_ac|night", ""))


# combine with open window/door -------------------------------
# use whatsapp survey on opening of windows
# read in whatsapp data and merge in with pm_data
whatsapp_open <- 
  read_rds(file.path(ddir, "df_whatsapp.rds")) %>%
  filter(!is.na(open_window)) %>%
  dplyr::select(respondent_id, date_hour, type, contains("open")) %>%
  mutate(open_room_behavior = open_room == "Ya" | open_door == "Ya", 
         open_room_behavior = if_else(str_detect(open_window, "Tak ada") & str_detect(open_door, "Tak ada|Tiada pintu"), NA_real_, open_room_behavior))

# combine whatsapp data with endline survey data
endline_open <- 
  mutate(survey, open_room = close_door_1hour == 1 | close_window_1hour == 1, 
         date_hour = floor_date(starttime, "hour")) %>%
  filter(!is.na(open_room)) %>%
  dplyr::select(respondent_id, open_room, date_hour)

open_all <- 
  bind_rows(whatsapp_open, endline_open) %>%
  left_join(pm) %>%
  filter(treatment_status == "Fan")


# big relationship between opening windows and infiltration rate
reg_window <- feols(pm25_indoor ~ open_room:pm25_outdoor3+  open_room + temp_outdoor3 +humidity_outdoor3 , open_all, cluster = ~respondent_id)  
f_window <- linearHypothesis(reg_window, c("open_roomFALSE:pm25_outdoor3 = open_roomTRUE:pm25_outdoor3 "))$`Pr(>Chisq)`[2]

r_window <- 
  tidy(reg_window, conf.int = TRUE) %>%
  dplyr::select(term, conf.low95 = conf.low, conf.high95 = conf.high) %>%
  left_join((tidy(reg_window, conf.int = TRUE, conf.level = .9))) %>%
  filter(str_detect(term, "pm25")) %>%
  mutate(fstat = paste0("Open = Closed: ", round(f_window, digits = 2)), 
         type = str_replace_all(term, "open_room:|:pm25_outdoor3", ""), 
         type = if_else(str_detect(type, "TRUE"), "Open", "Closed"),
         title = "Open Window/Door")  

# combine all het regs together -------------------------------
p_het <- 
  bind_rows(regs_het, r_window) %>%
  mutate(title = str_wrap(title, 12), 
         title = factor(title, levels = c("Income",  "House Size", "AC", "Open\nWindow/Door", 
                                          "Day/Night")), 
         type = if_else(type == "Bin 1", "Bin 1\n(lowest)", type),
         type = factor(type, levels = c("Bin 1\n(lowest)", "Bin 2", "Bin 3", "Bin 4", 
                                        "Small", "Big", "No AC", "AC", "Open", "Closed", 
                                        "Day", "Night"))) %>%
  ggplot(aes(x = type, y = estimate, color = title)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0,size = .2) + 
  geom_errorbar(aes(ymin = conf.low95, ymax = conf.high95), width = 0, alpha = .4, size = .2) + 
  geom_point(size = .5)+
  facet_grid(~title, scales = "free_x", space = "free_x") + 
  scale_color_brewer(palette = "Dark2") + 
  ylab("Infiltration Rate") +
  geom_text(aes(x = 1.75, y = -.1, label = fstat), size = 1.5) + 
  theme(legend.position = "none") +
  ylim(c(0, 1.5)) +
  annotate(# Add vertical lines between graphs
    "segment", x = 0,  xend = 0, y = -Inf, yend = Inf, color = "gray80",
    linetype = "solid") ; p_het


# ==========================================================================
# 3d. check whether short-term adaptive behaviors are correlated with income
# ==========================================================================
tidy_het <- function(r){
  tidy(r, conf.int = TRUE) %>%
    dplyr::select(term, conf.low95 = conf.low, conf.high95 = conf.high) %>%
    left_join((tidy(r, conf.int = TRUE, conf.level = .9))) %>%
    filter(str_detect(term, "pm25"))
}

# are households responding to outdoor pollution?
r_spline <- feols(pm25_indoor ~ splines:pm25_outdoor3 + temp_outdoor3 + humidity_outdoor3 | hour + week, 
                  pm, cluster = ~respondent_id) %>%
  tidy_het() %>%
  mutate(type = case_when(str_detect(term, ", 30.00") ~ "<30", 
                          str_detect(term, "\\[ 30.00") ~ "30-40", 
                          str_detect(term, "\\[ 40.00") ~ "40-50", 
                          str_detect(term, "\\[ 50.00") ~ ">50"), 
         type = factor(type, levels = c("<30", "30-40", "40-50", ">50")))

p_spline <- 
  ggplot(r_spline, aes(x = type, y = estimate)) + 
  geom_errorbar(aes(ymin = conf.low95, ymax = conf.high95), width = 0, alpha = .4, size = .2, color = "#d95f02") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0,size = .2, color = "#d95f02") + 
  geom_point(size = .5, color = "#d95f02") + 
  ylab("Infiltration Rate") + 
  ylim(c(0, 1.06)) + 
  xlab(expression(Outdoor ~PM[2.5] ~ Range~ (mu * g~m^-3)))   ; p_spline

# are households opening their windows in response to outdoor pollution?
r_open_pm <- 
  feols(open_room ~ splines +0 +  as.factor(week) , open_all, cluster = ~respondent_id) 

p_open_pm <- 
  tidy(r_open_pm, conf.int = TRUE) %>%
  filter(str_detect(term, "spline")) %>% 
  dplyr::select(term, conf.low95 = conf.low, conf.high95 = conf.high) %>%
  left_join((tidy(r_open_pm, conf.int = TRUE, conf.level = .9))) %>%
  mutate(type = case_when(str_detect(term, ", 30.00") ~ "<30", 
                          str_detect(term, "\\[ 30.00") ~ "30-40", 
                          str_detect(term, "\\[ 40.00") ~ "40-50", 
                          str_detect(term, "\\[ 50.00") ~ ">50"), 
         type = factor(type, levels = c("<30", "30-40", "40-50", ">50"))) %>%
  filter(!is.na(type)) %>%
  ggplot(aes(x = type, y = estimate)) + 
  geom_errorbar(aes(ymin = conf.low95, ymax = conf.high95), width = 0, alpha = .4, size = .2, color = "#d95f02") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0,size = .2, color = "#d95f02") + 
  geom_point(size = .5, color = "#d95f02") + 
  ylab("Prob(Window is Open)") + 
  ylim(c(0, 1.06)) + 
  xlab(expression(Outdoor ~PM[2.5] ~ Range~ (mu * g~m^-3)))  ; p_open_pm

# is there heterogeneity in infiltration based on beliefs about outdoor pollution?
pm <- 
  pm %>%
  mutate(beliefs_unhealthy = as.numeric(beliefs_outdoor_scale_label_baseline) > 3)

p_beliefs <- 
  feols(pm25_indoor ~ beliefs_unhealthy:pm25_outdoor3 + temp_outdoor3 + humidity_outdoor3 | hour + week, 
        pm, cluster = ~respondent_id) %>%
  tidy_het() %>%
  mutate(type =if_else(str_detect(term, "FALSE"), "Low AQI", "High AQI"), 
         type = factor(type, levels = c("Low AQI", "High AQI"))) %>%
  ggplot(aes(x = type, y = estimate)) + 
  geom_errorbar(aes(ymin = conf.low95, ymax = conf.high95), width = 0, alpha = .4, size = .2, color = "#d95f02") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0,size = .2, color = "#d95f02") + 
  geom_point(size = .5, color = "#d95f02") + 
  ylab("Infiltration Rate") + 
  xlab("Beliefs About Outdoor AQI") + 
  ylim(c(0, 1.06)) ; p_beliefs

p_behaviors <- 
  wrap_elements((p_spline + p_beliefs + p_open_pm) + plot_layout(width = c(2, 1, 2))) ; p_behaviors
 # plot_annotation(tag_levels = "a", tag_suffix = ".") &


# Combine the top row with the bottom plot
p_top <- (p_inf  + p_het) + plot_layout(width = c(0.7, 2))

(p_top/ p_behaviors) + 
  plot_annotation(tag_levels = "a", tag_suffix = ".") & 
  theme(axis.ticks = element_line(size = .1), 
        plot.tag = element_text(size = 6, face = "bold"))

ggsave(file.path(gdir, "output/figures/fig2_infiltation.png"), width = 15, height= 10, bg = "transparent", units = "cm")

