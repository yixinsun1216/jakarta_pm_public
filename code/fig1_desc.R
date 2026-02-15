
pacman::p_load(tidyverse, lubridate, sf, ggplot2, mapview, patchwork, 
               ggthemes, ggspatial, ggmap, fixest, broom, quantreg, knitr, 
               extrafont)

rm(list=ls())
gc()

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

source(file.path(gdir, "code/analysis/utils.R"))

# ===========================================================
# read in data
# ===========================================================
pm <- 
  read_rds(file.path(ddir, "generated_data/pm/df_reg.rds")) %>%
  mutate(pm25_indoor = if_else(date <= ymd(20240908) & date >= ymd(20240826), NA_real_, pm25_indoor))
# mutate(house_size = case_when(housing_room_number < 4 ~ "Small", 
#                                housing_room_number < 6 ~ "Medium", 
#                                TRUE ~"Big"))
# wealth_quart = Hmisc::cut2(wealth_index, g = 4)) 


# read in survey data
survey <- 
  file.path(ddir, "generated_data/survey/survey_control.RDS") %>%
  read_rds() %>%
  mutate(adult_timeuse_home_frac = adult_timeuse_home_baseline / adult_timeuse_total_baseline,
         child_timeuse_home_frac = child_timeuse_home_baseline / child_timeuse_total_baseline, 
         day_of_week = lubridate::wday(starttime_baseline, label = TRUE)) 



# ===========================================================
# Panel 1a. map of respondent and sensor locations
# ===========================================================
# sensor and hh location information 
sensor_locations <- st_read(file.path(ddir, "generated_data/pm/sensor_locations/sensor_locations.shp")) 

hh_locations <- 
  readRDS(file.path(ddir, "generated_data/survey/hh_locations_jittered.RDS")) 

jakarta_shp <- 
  file.path(ddir, "data/Batas Adm RW 01 Mei 2020/Batas_adm_RW_010520.shp") %>%
  st_read() %>%
  #group_by(WILAYAH) %>%
  summarise(type = "Jakarta")

data_locations <- 
  bind_rows(mutate(filter(sensor_locations, n3 == 1), type = "Outdoor\nSensor"), 
            mutate(hh_locations, type = "Indoor HH\nMonitor")) %>%
  mutate(type = factor(type, levels = c("Outdoor\nSensor", "Indoor HH\nMonitor")))

# get a nice background for the maps - need to register stadia API
register_stadiamaps("44c45f7e-fc84-46ec-9f3e-60444545e954", write = TRUE)
sensor_bbox <- st_bbox(st_buffer(data_locations, 100))
names(sensor_bbox) <- c("left", "bottom", "right", "top")
background <- get_stadiamap(sensor_bbox, zoom = 12, maptype = "alidade_smooth") 

p_map <- 
  ggmap(background) + 
  coord_sf(crs = st_crs(3857)) +
  geom_sf(data = data_locations, aes(color = type, shape = type), 
          inherit.aes = F, size = .5) + 
  geom_sf(data = jakarta_shp,fill = "transparent", 
          inherit.aes = F, linewidth = .1) +
  theme_map() +
  scale_shape_manual(values = c(17, 16)) + 
  scale_color_brewer(palette = "Dark2") + 
  theme(legend.text=element_text(size=5), 
        legend.key.size = unit(0.4, "cm"),
        legend.title = element_blank(), 
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", colour = NA)); p_map

# =========================================================================
# Panel 1b: daily pm levels, outdoor + indoor 
# =========================================================================
# average daily
df_date_hour <- 
  pm %>% 
  group_by(date) %>%
  summarise(Indoor = mean(pm25_indoor, na.rm = TRUE), 
            "Ambient Outdoor" = mean(pm25_outdoor3, na.rm = TRUE)) %>%
  pivot_longer(cols = c("Indoor", "Ambient Outdoor")) %>%
  filter(date >= ymd(20240601)) %>%
  ungroup

# average daily for duration of data collection
p_pm_daily <- 
  df_date_hour %>%
  ggplot(aes(x = as.Date(date), y = value, color = name)) + 
  geom_line(alpha = .75, size = .2) + 
  theme_classic() + 
  ylab(expression(paste("PM"[2.5], " (", mu, "g/m"^3, ")"))) + 
  scale_color_brewer(palette = "Dark2") + 
  geom_hline(aes(yintercept = 5), linetype = "dashed", size = .2) + 
  annotate("text", x = ymd("20240801"), y =7, label = "WHO Annual Std.", size = 1.5) + 
  annotate("segment", x = ymd("2024-06-01"), xend = ymd("2024-08-26"),
           y = 80, yend = 80, size = 0.2) +
  annotate("segment", x = ymd("2024-06-01"), xend = ymd("2024-06-01"),
           y = 79, yend = 80, size = 0.2) +
  annotate("segment", x = ymd("2024-08-26"), xend = ymd("2024-08-26"),
           y = 79, yend = 80, size = 0.2) +
  annotate("text", x = ymd("2024-07-14"), y = 83, label = "Data Collection Round 1", size = 1.5) +

  # Round 2 bracket
  annotate("segment", x = ymd("2024-09-10"), xend = ymd("2024-12-12"),
           y = 80, yend = 80, size = 0.2) +
  annotate("segment", x = ymd("2024-09-10"), xend = ymd("2024-09-10"),
           y = 79, yend = 80, size = 0.2) +
  annotate("segment", x = ymd("2024-12-12"), xend = ymd("2024-12-12"),
           y = 79, yend = 80, size = 0.2) +
  annotate("text", x = ymd("2024-10-24"), y = 83, label = "Round 2", size = 1.5) +
  scale_x_date(date_breaks = "1 month",date_labels = "%b-%d",) +
  theme(legend.position = c(.85,.75), 
        #legend.direction = "horizontal",
        legend.key.size = unit(0.2, "cm"),
        legend.title = element_blank(), 
        legend.text = element_text(size = 4.5),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 5), 
        axis.title = element_text(size = 5),
        axis.line = element_line(size = .1, color = "gray20")); p_pm_daily


# =========================================================================
# Panel 1c: hourly indoor data, hh level, 8/1-8/14
# =========================================================================
p_indoor_week <- 
  pm %>%
  filter(date >= ymd(20240801), 
         date <= ymd(20240814)) %>% 
  dplyr::select(date_hour, value = pm25_indoor, respondent_id, income_quart) %>%
  # pivot_longer(cols = c(pm25_indoor, pm25_outdoor3)) %>%
  ggplot(aes(x = date_hour, y = value, group = respondent_id)) + 
  geom_line(color = "gray60", size = .1) +
  stat_summary(fun = "mean", color = "darkred", geom ="line", aes(group = 1), size = .4) +
  #annotate("text", x = ymd_h("20240714 16"), y =90, label = "Mean", size = 5, color = "darkred") + 
  theme_classic() + 
  scale_y_continuous(breaks = seq(0, 500, by=100), limits=c(1,500), expand = c(0, 0)) + 
  ylab(expression(paste("PM"[2.5], " (", mu, "g/m"^3, ")"))) +
  theme(#panel.spacing.x = unit(1, "cm"), 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()) + 
  facet_wrap(~"Indoor") + 
  # geom_hline(aes(yintercept = 5), linetype = "dashed", color= "gray30", size = .1) +
  geom_hline(aes(yintercept = 250), linetype = "dashed", color= "tomato4", size = .1) +
  geom_hline(aes(yintercept = 125.5), linetype = "dashed", color= "deeppink4", size = .1) +
  annotate("text", x = ymd_h("20240731 01"), y =50, label = "Mean", size = 1.5, color = "darkred") 


p_indoor_week_income <- 
  pm %>%
  filter(date >= ymd(20240801), 
         date <= ymd(20240814)) %>% 
  dplyr::select(date_hour, value = pm25_indoor, respondent_id, income_quart) %>%
  # pivot_longer(cols = c(pm25_indoor, pm25_outdoor3)) %>%
  ggplot(aes(x = date_hour, y = value, group = respondent_id)) + 
  geom_line(color = "gray50", size = .1) +
  stat_summary(fun = "mean", color = "darkred", geom ="line", aes(group = 1), size = .2) +
  #annotate("text", x = ymd_h("20240714 16"), y =90, label = "Mean", size = 5, color = "darkred") + 
  theme_classic() + 
  scale_y_continuous(breaks = seq(0, 500, by=100), limits=c(1,500), expand = c(0, 0)) + 
  ylab(expression(paste("PM"[2.5], " (", mu, "g/m"^3, ")"))) +
  theme(#panel.spacing.x = unit(1, "cm"), 
    axis.title.y = element_blank(), 
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank()) + 
  facet_wrap(~"Indoor") + 
  # geom_hline(aes(yintercept = 5), linetype = "dashed", color= "gray30", size = .1) +
  annotate("text", x = ymd_h("20240731 01"), y =50, label = "Mean", size = 1.5, color = "darkred") +
  facet_wrap(~income_quart) ; p_indoor_week_income

# =========================================================================
# Panel 1d: hourly OUTDOOR data, hh level, 8/1-8/14
# =========================================================================
p_outdoor_week <- 
  pm %>%
  filter(date >= ymd(20240801), 
         date <= ymd(20240814)) %>% 
  dplyr::select(date_hour, value = pm25_outdoor3, respondent_id) %>%
  # mutate(who = "WHO Annual Std.") %>%
  # pivot_longer(cols = c(pm25_indoor, pm25_outdoor3)) %>%
  ggplot(aes(x = date_hour, y = value, group = respondent_id)) + 
  geom_line(color = "gray70", size = .1) +
  stat_summary(fun = "mean", color = "darkred", geom ="line", aes(group = 1), size = .4) +
  annotate("text", x = ymd_h("20240807 04"), y =190, label = "Individual Household Data", color = "gray30", size = 1.5) +
  geom_segment(aes(xend = ymd_h("20240804 01"), x = ymd_h("20240804 14"), 
                   yend = 140, y = 180), colour = "gray", size = .1,
               arrow = arrow(length=unit(0.1,"cm"))) +
  theme_classic() + 
  scale_y_continuous(breaks = seq(0, 500, by=100), limits=c(0,500), expand = c(0, 0)) + 
  ylab(expression(paste("PM"[2.5], " (", mu, "g/m"^3, ")"))) +
 #  geom_text(aes(x = ymd_h("20240730 01"), label = who, y = 5), size = 1.5, color= "gray30") + 
 # geom_hline(aes(yintercept = 5), linetype = "dashed", color= "gray30", size = .1) +
  coord_cartesian(xlim = c(ymd_h("20240801 01"), ymd_h("20240814 23")), clip = 'off')  + 
  theme(text = element_text(size = 24, family = "Arial")) + 
        # panel.spacing.x = unit(1, "cm")) + 
        # strip.background =element_blank(), 
       # plot.margin = margin(1, 1, 1, 1.01, "cm")) + 
  geom_hline(aes(yintercept = 250), linetype = "dashed", color= "tomato4", size = .1) +
  annotate("text", x = ymd_h("20240812 10"), y =270, label = "US EPA - Hazardous", color = "tomato4", 
           size = 1.5) +
  geom_hline(aes(yintercept = 125.5), linetype = "dashed", color= "deeppink4", size = .1) +
  annotate("text", x = ymd_h("20240812 10"), y =140, label = "US EPA - Very Unhealthy", color = "deeppink4", 
           size = 1.5) +
  coord_cartesian(xlim = c(ymd_h("20240801 01"), ymd_h("20240814 23")), clip = 'off') +
  facet_wrap(~"Ambient Outdoor")


# =========================================================================
# put everything together
# =========================================================================
# put top panel together first
p_top <- 
  (p_map + plot_spacer() + p_pm_daily) + 
  plot_annotation(tag_level = "a", tag_suffix = ".") + 
  plot_layout(widths = c(.7, .03, 1)) &
  theme(text = element_text(size = 5), 
        axis.ticks = element_line(size = .1),
        plot.tag = element_text(size = 8, face = "bold"))


# put bottom panel together
p_2weeks <- 
  (p_outdoor_week + p_indoor_week) &
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA), 
        plot.title = element_text(hjust = 0.5), 
        strip.text = element_text(size = 8),
        axis.line = element_line(size = .1),
        axis.text = element_text(size = 8),
        axis.ticks = element_line(size = .1),
        axis.title = element_text(size = 8),
        strip.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor= element_blank(), 
        axis.title.x = element_blank(), 
        plot.tag = element_text(size = 8, face = "bold")) ; p_2weeks
ggsave(file.path(gdir, "output/desc/fig1_2weeks.png"), p_2weeks,
       width =16, height= 7, bg = "transparent", units = "cm")


# combine top and bottom panels and save
p_top / p_2weeks + plot_annotation(tag_levels = "a")

ggsave(file.path(gdir, "output/desc/fig1_desc.png"), 
       width =16, height= 11, bg = "transparent", units = "cm")
