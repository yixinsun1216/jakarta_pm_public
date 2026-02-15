rm(list=ls_extra())
gc()

# ===========================================================
# read in data
# ===========================================================
pm <- 
  read_rds(file.path(ddir, "df_reg.rds"))  %>%
  mutate(night = if_else(hour >= 19 | hour <= 6, "Night", "Day"), 
         pm25_indoor = if_else(pm25_indoor == 1, NA_real_, pm25_indoor), 
         spike = pm25_indoor > 125.5, 
         trash = as.numeric(trash_burning_1week_baseline) > 1, 
         income_high = hh_income >= 4) 

# read in survey data
survey <- 
  file.path(ddir, "df_survey.rds") %>%
  read_rds() %>%
  mutate(adult_timeuse_home_frac = adult_timeuse_home_baseline / adult_timeuse_total_baseline,
         child_timeuse_home_frac = child_timeuse_home_baseline / child_timeuse_total_baseline, 
         day_of_week = lubridate::wday(starttime_baseline, label = TRUE), 
         trash = as.numeric(trash_burning_1week_baseline) > 1, 
         open_room = close_door_1hour == 1 | close_window_1hour == 1, 
         date_hour = floor_date(starttime, "hour")) 


# Decomposition of indoor pm for each income quartile
# created in fig3_decomposition
p_decomp <- 
  read_rds(file.path(ddir, "plot_decomp_by_income.rds")) +
  geom_text(aes(x = 1, y = 10, label = "Smoked\n(24 Hours)"), size = 1.3, color = "white") +
  geom_text(aes(x = 1, y = 24, label = "Other\n(hyperlocal)"), size = 1.3, color = "white") +
  geom_text(aes(x = 1, y = 42, label = "Outdoor\nAmbient" ), size = 1.3, color = "white") + 
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.ticks = element_line(size = .1), 
        text = element_text(size = 6), 
        strip.text= element_text(size = 5.5),
        title = element_text(face = "bold",size = 5)) ; p_decomp

# =========================================================================
# correlation between income infiltration variables
# =========================================================================
# infiltration variables
tidy_income <- function(r){
  tidy(r, conf.int = TRUE) %>%
    dplyr::select(term, conf.low95 = conf.low, conf.high95 = conf.high) %>%
    left_join((tidy(r, conf.int = TRUE, conf.level = .9))) %>%
    mutate(income = str_replace(term, "income_quart", ""), 
           income = if_else(income == "Income Bin 1", "Income Bin 1\n(lowest)", income), 
           income = str_wrap(income, width = 5)) %>%
    filter(str_detect(term, "Income"))
}

vars <-   c("housing_room_number", "room_ac", "open_room") 
var_yaxis <- c("HH Number of Rooms", "Prob(Has AC)", "Prob(Window/Door Open)")

p_char_income <- 
  map2_df(vars, var_yaxis, function(x, y){
    print(x)
    out <- feols(as.formula(paste0(x, "~income_quart + 0")), data = survey, cluster = ~respondent_id)

    f_stat <- paste0("Bin 1 = Bin 4: ", round(linearHypothesis(out, c("income_quartIncome Bin 1 - income_quartIncome Bin 4 = 0"))$`Pr(>Chisq)`[2], digits = 3))
    
    out %>%
      tidy_income() %>% 
      mutate(var = y, 
             fstat = f_stat)
  }) %>%
  mutate(var = factor(var, levels = c("Prob(Has AC)", "Prob(Window/Door Open)", "HH Number of Rooms"))) %>%
  ggplot(aes(x = income, y = estimate)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), color = "#1b9e77", width = 0, size = .1) + 
  geom_errorbar(aes(ymin = conf.low95, ymax = conf.high95), color = "#1b9e77", width = 0, alpha = .4, size = .1) + 
  geom_point(size = .5, color = "#1b9e77")+
  theme_classic() + 
  expand_limits(y = 0) + 
  facet_wrap(~var, scales = "free_y", nrow = 1) + 
  ggtitle("c.") + 
  geom_text(aes(x = 3.5, y = -.1, label = fstat), size = 1.5, color = "#1b9e77") + 
  theme(title = element_text(face = "bold",size = 5),
        axis.text =element_text(size = 5), 
        axis.line = element_line(size = .1),
        axis.ticks = element_line(size = .1),
        axis.title = element_blank(),
        axis.title.y = element_blank(), 
        strip.text= element_text(size = 5),
        strip.background = element_blank(),
        panel.grid= element_blank(),
        panel.spacing = unit(.5, "cm"),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA), 
        legend.position = "none") ; p_char_income


# indoor sources -----------
regs_indoor <- 
  list(feols(spike ~ income_quart  + 0 + as.factor(hour), data = pm, cluster = ~respondent_id) , 
       feols(trash ~ income_quart + 0, data = survey, cluster = ~respondent_id), 
       feols(smoke24_endline ~ income_quart + 0, data = survey, cluster = ~respondent_id)) 
tidy_trash <- 
  tidy_income(regs_indoor[[2]])  %>%
  mutate(title = "Prob(Neighborhood~Waste~Burning)", 
         order = 3, 
         fstat = paste0("Bin 1 = Bin 4: ", round(linearHypothesis(regs_indoor[[2]], c("income_quartIncome Bin 1 = income_quartIncome Bin 4"))$`Pr(>Chisq)`[2], digits = 3)))

tidy_smoke <- 
  tidy_income(regs_indoor[[3]]) %>%
  mutate(title = "Prob(Smoked,~Last~24~Hours)", 
         order = 2,
         fstat = paste0("Bin 1 = Bin 4: ", round(linearHypothesis(regs_indoor[[3]], c("income_quartIncome Bin 1 = income_quartIncome Bin 4"))$`Pr(>Chisq)`[2], digits = 3)))

p_hyperlocal_income <- 
  list(tidy_trash, tidy_smoke) %>%
  bind_rows() %>%
  mutate(title = fct_reorder(factor(title), order)) %>%
  ggplot(aes(x = income, y = estimate)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), color = "#d95f02", width = 0, size = .1) + 
  geom_errorbar(aes(ymin = conf.low95, ymax = conf.high95), color = "#d95f02", width = 0, alpha = .4, size = .1) + 
  geom_point(size = .5, color = "#d95f02")+
  theme_classic() +
  ggtitle("b.") + 
  geom_text(aes(x = 1.5, y = 0, label = fstat), size = 1.5, color = "#d95f02") + 
  facet_wrap(~title, labeller = label_parsed, scales = "free_y") + 
  theme(title = element_text(face = "bold",size = 5),
        axis.text =element_text(size = 5), 
        axis.line = element_line(size = .1),
        axis.ticks = element_line(size = .1),
        axis.title = element_blank(), 
        strip.text= element_text(size = 5),
        strip.background = element_blank(),
        panel.grid= element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA), 
        legend.position = "none") ; p_hyperlocal_income


p_top <- (p_decomp + ggtitle("a.")) + p_hyperlocal_income + 
  plot_layout(widths = c(.75, 1))

p_top / p_char_income 
ggsave(file.path(gdir, "output/figures/fig4_pm_income.png"), 
       width = 15, height= 10, bg = "transparent", units = "cm")

