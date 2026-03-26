rm(list=ls_extra())
gc()

# ===========================================================
# read in data
# ===========================================================
pm <-
  read_rds(file.path(ddir, "df_reg.rds")) %>%
  mutate(night = if_else(hour >= 19 | hour <= 6, "Night", "Day"),
         trash = trash_burning_1week_baseline != "Never",
         income_high = hh_income >= 4,
         cooking = replace_na(cooking, 0),
         trash = ifelse(trash_burning_1week_baseline == "", NA, trash))

# read in survey data
survey <-
  file.path(ddir, "df_survey.rds") %>%
  read_rds() %>%
  mutate(adult_timeuse_home_frac = adult_timeuse_home_baseline / adult_timeuse_total_baseline,
         child_timeuse_home_frac = child_timeuse_home_baseline / child_timeuse_total_baseline,
         day_of_week = lubridate::wday(starttime_baseline, label = TRUE),
         trash = trash_burning_1week_baseline != "Never",
         trash = ifelse(trash_burning_1week_baseline == "", NA, trash),
         open_room = close_door_1hour == 1 | close_window_1hour == 1,
         date_hour = floor_date(starttime, "hour"))


# =========================================================================
# correlation between income infiltration variables
# =========================================================================
# infiltration variables
tidy_income <- function(r){
  tidy(r, conf.int = TRUE) %>%
    dplyr::select(term, conf.low95 = conf.low, conf.high95 = conf.high) %>%
    left_join((tidy(r, conf.int = TRUE, conf.level = .9))) %>%
    mutate(income = str_replace(term, "income_quart", ""),
           income = case_when(
             income == "Income Bin 1" ~ "Income Bin 1\n(lowest)",
             income == "Income Bin 2" ~ "Bin 2",
             income == "Income Bin 3" ~ "Bin 3",
             income == "Income Bin 4" ~ "Bin 4",
             TRUE ~ income
           ),
           income = factor(income, levels = c("Income Bin 1\n(lowest)", "Bin 2", "Bin 3", "Bin 4"))) %>%
    filter(str_detect(term, "Income"))
}

vars <-   c("housing_room_number", "room_ac", "open_room")
var_yaxis <- c("HH Number of Rooms", "Pr(Has AC)", "Pr(Window/Door Open)")

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
  mutate(var = factor(var, levels = c("Pr(Has AC)", "Pr(Window/Door Open)", "HH Number of Rooms"))) %>%
  ggplot(aes(x = income, y = estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), color = "#A6761D", width = 0, size = .1) +
  geom_errorbar(aes(ymin = conf.low95, ymax = conf.high95), color = "#A6761D", width = 0, alpha = .4, size = .1) +
  geom_point(size = .5, color = "#A6761D")+
  theme_classic() +
  expand_limits(y = 0) +
  facet_wrap(~var, scales = "free_y", nrow = 1) +
  geom_text(aes(x = 2, y = -.1, label = fstat), size = 2.2, color = "#A6761D") +
  theme(title = element_text(face = "bold",size = 6),
        axis.text =element_text(size = 6),
        axis.text.x = element_text(size = 6, angle = 45, hjust = 1),
        axis.line = element_line(size = .1),
        axis.ticks = element_line(size = .1),
        axis.title = element_blank(),
        axis.title.y = element_blank(),
        strip.text= element_text(size = 6, vjust = 5, hjust = 0, margin = margin(l = -10)),
        strip.clip = "off",
        strip.background = element_blank(),
        panel.grid= element_blank(),
        panel.spacing = unit(.5, "cm"),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.position = "none") ; p_char_income


# indoor sources -----------
regs_indoor <-
  list(feols(spike ~ income_quart  + 0 + as.factor(hour), data = pm,
             cluster = ~respondent_id + date_hour) ,
       feols(trash ~ income_quart + 0, data = survey, cluster = ~respondent_id),
       feols(smoke24_endline ~ income_quart + 0, data = survey, cluster = ~respondent_id))
tidy_trash <-
  tidy_income(regs_indoor[[2]])  %>%
  mutate(title = "Pr(Waste~Burning)",
         order = 3,
         fstat = paste0("Bin 1 = Bin 4: ", round(linearHypothesis(regs_indoor[[2]], c("income_quartIncome Bin 1 = income_quartIncome Bin 4"))$`Pr(>Chisq)`[2], digits = 3)))

tidy_smoke <-
  tidy_income(regs_indoor[[3]]) %>%
  mutate(title = "Pr(Smoking~HH)",
         order = 2,
         fstat = paste0("Bin 1 = Bin 4: ", round(linearHypothesis(regs_indoor[[3]], c("income_quartIncome Bin 1 = income_quartIncome Bin 4"))$`Pr(>Chisq)`[2], digits = 3)))

p_hyperlocal_income <-
  list(
    tidy_trash %>% mutate(source = "Trash"),
    tidy_smoke %>% mutate(source = "Smoking")
  ) %>%
  bind_rows() %>%  
  mutate(title = fct_reorder(factor(title), order)) %>%
  ggplot(aes(x = income, y = estimate, color = source, group = source)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, size = .1) +
  geom_errorbar(aes(ymin = conf.low95, ymax = conf.high95), width = 0, alpha = .4, size = .1) +
  geom_point(size = .5)+
  theme_classic() +
  geom_text(aes(x = 2.5, y = 0, label = fstat), size = 2.2) +
  facet_wrap(~title, labeller = label_parsed, scales = "free_y") +
  scale_color_manual(values=c("#7570B3", "#66A61E")) + 
  theme(title = element_text(face = "bold",size = 6),
        axis.text =element_text(size = 6),
        axis.text.x = element_text(size = 6, angle = 45, hjust = 1),
        axis.line = element_line(size = .1),
        axis.ticks = element_line(size = .1),
        axis.title = element_blank(),
        strip.text= element_text(size = 6, vjust = 5, hjust = 0, margin = margin(l = -10)),
        strip.clip = "off", 
        strip.background = element_blank(),
        panel.grid= element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.position = "none") ; p_hyperlocal_income

  

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pane a: income indoor vs outdoor
# Single regression with income_quart interacted with all source variables
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tidy_up <- function(r){
  tidy(r, conf.int = TRUE) %>%
    dplyr::select(term, conf.low95 = conf.low, conf.high95 = conf.high) %>%
    left_join((tidy(r, conf.int = TRUE, conf.level = .9))) %>%
    mutate(type = if_else(str_detect(all.vars(r$call)[1], "outdoor"),
                          "Ambient Outdoor", "Indoor"))
}
pm_hh_hour <-
  pm %>%
  dplyr::group_by(respondent_id, hour, income_quart) %>%
  dplyr::summarise(pm25_indoor = mean(pm25_indoor, na.rm = TRUE),
                   pm25_outdoor = mean(pm25_outdoor3, na.rm = TRUE))

r_outdoor1 <- feols(pm25_outdoor3 ~ income_quart + 0, data = pm, cluster = ~respondent_id + date_hour)
r_indoor1 <- feols(pm25_indoor ~ income_quart + 0, data = pm, cluster = ~respondent_id + date_hour)

income_mean <-
  list(r_outdoor1, r_indoor1) %>%
  map2_df(c(rep("HH-Hour-Day", 2)),
          ~mutate(tidy_up(.x), model = !!.y)) %>%
  mutate(term = str_replace(term, "income_quart", ""),
         term = if_else(term == "Income Bin 1", "Income Bin 1 (lowest)", term),
         term = factor(term, levels = c("Income Bin 4", "Income Bin 3",
                                        "Income Bin 2", "Income Bin 1 (lowest)")))

pd <- position_dodge(width = 0.55)

p_income <-
  income_mean %>%
  mutate(
    term = str_replace(term, "income_quart", ""),
    term = if_else(term == "Income Bin 1 (lowest)", "Income Bin 1", term),
    term = if_else(term == "Income Bin 2", "Bin 2", term),
    term = if_else(term == "Income Bin 3", "Bin 3", term),
    term = if_else(term == "Income Bin 4", "Bin 4", term),
    term = factor(term, levels = c(
      "Income Bin 1",
      "Bin 2",
      "Bin 3",
      "Bin 4"
    ))
  ) %>%
  ggplot(aes(x = term, y = estimate, color = type, group = type)) +
  geom_point(size = 1, position = pd) +
  geom_errorbar(
    aes(ymin = conf.low95, ymax = conf.high95),
    width = 0,
    alpha = 0.33,
    position = pd
  ) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0,
    position = pd
  ) +
  theme_classic() +
  scale_color_manual(values = c("#D95F02", "#1B9E77")) +
  coord_cartesian(ylim = c(0, 90)) +
  ylab(expression(PM[2.5] ~ (mu * g/m^3))) +
  xlab("Income quartile") +
  theme(
    title = element_text(face = "bold", size = 6),
    axis.text = element_text(size = 6),
    axis.text.x = element_text(size = 6, angle = 45, hjust = 1),
    axis.line = element_line(linewidth = 0.1),
    axis.ticks = element_line(linewidth = 0.1),
    axis.title = element_text(size = 6),
    axis.title.x = element_text(size = 0),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    
    legend.position = c(0.5, 0.02),   # 👈 key line
    legend.justification = c(0.5, 0), # anchor bottom-center
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 6),
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill = "transparent"),
    legend.key.size = unit(0.4, "cm"),
    strip.text = element_text(size = 6, hjust=0),
    strip.background = element_blank(),
    legend.spacing.y = unit(-0.3, "cm"),
    legend.key.height = unit(0.05, "cm")) +
    guides(color = guide_legend(ncol = 1)) +
  facet_wrap(
    ~"PM[2.5]~(mu*g/m^3)",
    labeller = label_parsed
  ) +
  labs(y=NULL); p_income



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pane d: Income decomposition via interaction approach
# Single regression with income_quart interacted with all source variables
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
reg_decomp_income <- feols(
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

# Extract income-quartile-specific contributions
income_quart_levels <- pm %>%
  filter(!is.na(income_quart)) %>%
  distinct(income_quart) %>%
  pull(income_quart) %>%
  as.character()

coefs_decomp <- coef(reg_decomp_income)

decomp_by_income <-
  map_df(income_quart_levels, function(iq) {
    data_iq <- filter(pm, income_quart == iq)

    # outdoor ambient: i(income_quart, pm25_outdoor3) -> "income_quart::<iq>:pm25_outdoor3"
    outdoor_coef_name <- paste0("income_quart::", iq, ":pm25_outdoor3")
    beta_outdoor <- unname(coefs_decomp[outdoor_coef_name])
    mean_outdoor <- mean(data_iq$pm25_outdoor3, na.rm = TRUE)

    # smoking: i(as.factor(smoke24_endline), income_quart, ref = 0) -> "as.factor(smoke24_endline)::1:income_quart::<iq>"
    smoke_coef_name <- paste0("as.factor(smoke24_endline)::1:income_quart::", iq)
    beta_smoke <- unname(coefs_decomp[smoke_coef_name])
    mean_smoke <- mean(as.numeric(as.character(data_iq$smoke24_endline)), na.rm = TRUE)

    tibble(
      income_quart = iq,
      term         = c("Outdoor\nAmbient", "Smoking\nHousehold"),
      contribution = c(beta_outdoor * mean_outdoor, beta_smoke * mean_smoke)
    )
  })

decomp_by_income_tokeep <-
  decomp_by_income %>%
  dplyr::select(income_quart, term, contribution)

r_indoor <-
  feols(pm25_indoor ~ income_quart + 0, data = pm,
        cluster = ~respondent_id+date_hour) %>%
  tidy(conf.int = TRUE) %>%
  dplyr::rename(income_quart = term) %>%
  mutate(income_quart = str_remove(income_quart, "income_quart"))

decomp_remainder <-
  decomp_by_income_tokeep %>%
  dplyr::group_by(income_quart) %>%
  dplyr::summarise(contribution = sum(contribution, na.rm = TRUE), .groups = "drop") %>%
  left_join(r_indoor, by = "income_quart") %>%
  mutate(unexplained = estimate - contribution) %>%
  dplyr::select(income_quart, contribution = unexplained) %>%
  mutate(term = "Other\n(Hyperlocal)")

decomp_all <-
  bind_rows(decomp_by_income_tokeep, decomp_remainder) %>%
  mutate(term = factor(term, levels = c("Smoking\nHousehold", "Other\n(Hyperlocal)", "Outdoor\nAmbient")),
         income_quart = if_else(income_quart == "Income Bin 1", "Income Bin 1 (lowest)", income_quart),
         income_quart = str_wrap(income_quart, width = 5),
         income_quart = factor(income_quart, levels = c("Income\nBin 1\n(lowest)", "Income\nBin 2", "Income\nBin 3", "Income\nBin 4")))

# compute label positions (cumulative midpoints) for Bin 1 only
decomp_labels <- decomp_all %>%
  filter(income_quart == "Income\nBin 1\n(lowest)") %>%
  arrange(term) %>%
  mutate(y_mid = c(50, 33, 12),
         term_label = as.character(term))


p_decomp_income <-
  decomp_all %>%
  ggplot(aes(x = income_quart, y = contribution, fill = term)) +
  geom_col(width = .75) +
  geom_text(data = decomp_labels,
            aes(y = y_mid, label = term_label),
            size = 1.3, color = "white", lineheight = 0.85) +
  geom_hline(yintercept = 0, linewidth = .2) +
  # scale_fill_brewer(palette = "Dark2") +
  scale_fill_manual(values=c("#7570B3", "#66A61E", "#D95F02")) +
  ylab(expression(PM[2.5] ~ (mu * g/m^3))) +
  xlab("Income quartile") +
  facet_wrap(~"Indoor PM2.5 Decomposition") +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.title.y = element_text(angle = 90),
        strip.background = element_blank(), 
        axis.ticks = element_line(size = .1),
        legend.position = "none",
        text = element_text(size = 6),
        strip.text= element_text(size = 6.5),
        title = element_text(face = "bold",size = 6)); p_decomp_income


# save this as a plot for figure 4, when we discuss income heterogeneity
# write_rds(p_decomp_income, file.path(ddir, "plot_decomp_by_income.rds"))


# =========================================================================
# Income heterogeneity in infiltration rate 
# =========================================================================
income_inf_test <- "income_quart::Income Bin 1:pm25_outdoor3 = income_quart::Income Bin 4:pm25_outdoor3"
income_inf_fstat <- paste0("Bin 1 = Bin 4: ",
                           round(linearHypothesis(reg_decomp_income, c(income_inf_test))$`Pr(>Chisq)`[2], digits = 2))

income_inf_tidy <-
  tidy(reg_decomp_income, conf.int = TRUE) %>%
  dplyr::select(term, conf.low95 = conf.low, conf.high95 = conf.high) %>%
  left_join((tidy(reg_decomp_income, conf.int = TRUE, conf.level = .9))) %>%
  filter(str_detect(term, "pm25")) %>%
  mutate(fstat = income_inf_fstat,
         type = str_replace_all(term, "income_quart::Income |:pm25_outdoor3", ""),
         title = "Income")

p_income_inf <-
  income_inf_tidy %>%
  mutate(income = if_else(type == "Bin 1", "Income Bin 1\n(lowest)", type),
         income = factor(income, levels = c("Income Bin 1\n(lowest)", "Bin 2", "Bin 3", "Bin 4"))) %>%
  ggplot(aes(x = income, y = estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), color = "#D95F02", width = 0, size = .1) +
  geom_errorbar(aes(ymin = conf.low95, ymax = conf.high95), color = "#D95F02", width = 0, alpha = .4, size = .1) +
  geom_point(size = .5, color = "#D95F02") +
  theme_classic() +
  expand_limits(y = 0) +
  facet_wrap(~"Infiltration Factor") +
  geom_text(aes(x = 3.5, y = -.05, label = fstat), size = 2.2, color = "#D95F02") +
  ylab("Infiltration Factor") +
  theme(title = element_text(face = "bold", size = 6),
        axis.text = element_text(size = 6),
        axis.line = element_line(size = .1),
        axis.ticks = element_line(size = .1),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 6),
        strip.text = element_text(size = 6),
        strip.background = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.position = "none") ; p_income_inf


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Source decomposition by smoking status (analogous to p_decomp_income)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
reg_decomp_smoke <- feols(
  pm25_indoor ~
    as.factor(smoke24_endline) +
    i(as.factor(smoke24_endline), pm25_outdoor3) +
    i(as.factor(trash_burning_1week_baseline), as.factor(smoke24_endline), ref = "Never") +
    i(as.factor(room_pmsource_kitchen), as.factor(smoke24_endline), ref = 0) +
    i(cooking, as.factor(smoke24_endline), ref = 0) +
    i(as.factor(smoke24_endline), dist_primary) |
    hour + week,
  data = pm,
  cluster = ~respondent_id + date_hour
)

coefs_decomp_smoke <- coef(reg_decomp_smoke)
smoke_groups <- c("0", "1")

decomp_by_smoke <- map_df(smoke_groups, function(sg) {
  data_sg <- filter(pm, smoke24_endline == as.numeric(sg))

  # outdoor ambient: group-specific infiltration × group mean outdoor PM
  outdoor_coef_name <- paste0("as.factor(smoke24_endline)::", sg, ":pm25_outdoor3")
  beta_outdoor <- unname(coefs_decomp_smoke[outdoor_coef_name])
  mean_outdoor <- mean(data_sg$pm25_outdoor3, na.rm = TRUE)

  # smoking effect: main effect of as.factor(smoke24_endline)1
  # only applies to smoking group; 0 for non-smokers (reference)
  smoke_contrib <- if (sg == "1") {
    unname(coefs_decomp_smoke["as.factor(smoke24_endline)1"])
  } else {
    0
  }

  tibble(
    smoke_group = sg,
    term         = c("Outdoor\nAmbient", "Smoking\nHousehold"),
    contribution = c(beta_outdoor * mean_outdoor, smoke_contrib)
  )
})

# observed mean indoor PM by smoking group
r_indoor_smoke <-
  feols(pm25_indoor ~ as.factor(smoke24_endline) + 0, data = pm,
        cluster = ~respondent_id + date_hour) %>%
  tidy(conf.int = TRUE) %>%
  dplyr::rename(smoke_group = term) %>%
  mutate(smoke_group = str_extract(smoke_group, "[01]$"))

# residual = observed mean - (outdoor + waste burning)
decomp_remainder_smoke <-
  decomp_by_smoke %>%
  group_by(smoke_group) %>%
  summarise(contribution = sum(contribution, na.rm = TRUE), .groups = "drop") %>%
  left_join(r_indoor_smoke, by = "smoke_group") %>%
  mutate(contribution = estimate - contribution,
         term = "Other\n(Hyperlocal)") %>%
  dplyr::select(smoke_group, term, contribution)

# label positions for the taller bar (Smoking)
decomp_labels_smoke <- decomp_all_smoke %>%
  filter(smoke_label == "Smoking") %>%
  arrange(term) %>%
  mutate(cumtop = cumsum(contribution),
         y_mid  = cumtop - contribution / 2,
         term_label = as.character(term))

decomp_labels_smoke <- tribble(~smoke_label, ~y_mid, ~term,
                    "Smoking", 39, "Smoking\nHousehold",
                    "Non-smoking", 27, "Other\n(Hyperlocal)",
                    "Smoking", 13, "Outdoor\nAmbient") 


p_decomp_smoke <-
  decomp_all_smoke %>%
  mutate(smoke_label = factor(smoke_label, levels = c("Smoking", "Non-smoking"))) %>%
  ggplot(aes(x = smoke_label, y = contribution, fill = term)) +
  geom_col(width = .75) +
  geom_text(data = decomp_labels_smoke,
            aes(y = y_mid, label = term),
            size = 1.3, color = "white", lineheight = 0.85) +
  geom_hline(yintercept = 0, linewidth = .2) +
  scale_fill_brewer(palette = "Dark2") +
  ylab(expression(PM[2.5] ~ (mu * g/m^3))) +
  facet_wrap(~"Indoor PM2.5") +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.title.y = element_text(angle = 90),
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        axis.ticks = element_line(size = .1),
        legend.position = "none",
        text = element_text(size = 6),
        strip.text = element_text(size = 5.5),
        title = element_text(face = "bold", size = 5)) ; p_decomp_smoke


# =========================================================================
# Assemble figure 4:
# a = p_decomp_income, b = p_hyperlocal_income,
# c = p_income_inf, d = p_char_income
# =========================================================================
(p_income + p_hyperlocal_income  + p_decomp_income + plot_layout(widths = c(1, 2.2, 1.9))) /
  ((p_income_inf + p_char_income) + plot_layout(widths = c(1, 2))) +
  plot_layout(heights = c(1, 0.75)) +
  plot_annotation(tag_levels = "a", tag_suffix = ".") &
  theme(axis.ticks = element_line(size = .1),
        plot.tag = element_text(size = 6, face = "bold"))

ggsave(file.path(gdir, "output/figures/fig4_pm_income.png"),
       width = 16, height= 12, bg = "transparent", units = "cm", dpi = 300)

ggsave(file.path(gdir, "output/figures/fig4_pm_income.tiff"),
       width = 16, height= 12, bg = "transparent", units = "cm", dpi = 300, compression = "lzw")
