rm(list = ls_extra())
gc()

my_theme <- theme_classic() + 
  theme(panel.grid= element_blank(),
        legend.key.size = unit(0.2, "cm"),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(angle = 0),
        panel.background = element_rect(fill = "transparent", colour = NA), 
        plot.background = element_rect(fill = "transparent", colour = NA), 
        legend.position = "none", 
        text = element_text(size = 6), 
        axis.line = element_line(size = .1), 
        axis.ticks = element_line(size = .1)) 
theme_set(my_theme)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# -------------------- Read in PM data -------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pm <-
  read_rds(file.path(ddir, "df_reg.rds")) %>%
  mutate(
    traffic_hours = hour(date_hour) %in% c(7, 8, 9, 16, 17, 18, 19), 
    cooking_night = hour(date_hour) %in% c(17, 18, 19, 20),
  ) %>%
  mutate(cooking = replace_na(cooking, 0))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# -------------- Helper functions ----------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
compute_mean_contrib <- function(model, rhs_fml, data, outcome_var) {
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

  coef_tbl <- tibble(
    term = names(coef_est),
    estimate = unname(coef_est),
    pvalue = unname(pvals),
    conf.low = unname(ci_low),
    conf.high = unname(ci_high)
  )

  mean_tbl <- tibble(term = colnames(X), mean_x = colMeans(X, na.rm = TRUE))
  mean_outcome <- mean(data[[outcome_var]], na.rm = TRUE)

  mean_contrib <- coef_tbl %>%
    left_join(mean_tbl, by = "term") %>%
    mutate(contribution = estimate * mean_x,
           frac = contribution / mean_outcome, 
           frac.low = mean_x*conf.low / mean_outcome, 
           frac.high = mean_x*conf.high / mean_outcome)

  return(mean_contrib)
}

compute_lmg_tbl <- function(lm_model) {
  relimp <- relaimpo::calc.relimp(lm_model, type = "lmg", rela = TRUE)
  lmg_tbl <- tibble(
    term = names(relimp$lmg),
    lmg = unname(relimp$lmg)
  ) %>%
    arrange(desc(lmg)) %>%
    filter(!str_detect(term, "as.factor\\(week|hour"))

  return(lmg_tbl)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- 1) Mean Contribution Decomposition ----------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rhs_fml <- ~ as.factor(trash_burning_1week_baseline) +
  as.factor(smoke24_endline) +
  as.factor(room_pmsource_kitchen) +
  pm25_outdoor3 +
  cooking +
  dist_primary

reg_fe <- feols(
  fixest::xpd(pm25_indoor ~ .[rhs_fml] | hour + week) ,
  data = pm,
  cluster = ~ respondent_id + date_hour
)

etable(reg_fe)

output_mean_contrib <- compute_mean_contrib(
  model = reg_fe,
  rhs_fml = rhs_fml,
  data = pm,
  outcome_var = "pm25_indoor"
)


# ----------- 2) Variance Decomposition (LMG) --------------------
reg_lm <- lm(
  pm25_indoor ~ pm25_outdoor3 +
    as.factor(smoke24_endline) +
    as.factor(trash_burning_1week_baseline) +
    as.factor(room_pmsource_kitchen) +
    cooking + 
    dist_primary + 
   as.factor(hour) + as.factor(week),
  data = pm
)

output_lmg_tbl <- compute_lmg_tbl(reg_lm)

# combine mean contribution with R2 analysis to output 1 table

dict <- c(
  "pm25_outdoor3" = "Outdoor Ambient",
  "cooking" = "Cooking",
  "dist_primary" = "Distance to Main Road",
  "as.factor(smoke24_endline)1" = "Smoked (24 Hours)",
  "as.factor(smoke24_endline)" = "Smoked (24 Hours)",
  "as.factor(room_pmsource_kitchen)1" = "Kitchen source", 
  "as.factor(room_pmsource_kitchen)" = "Kitchen source",
  "as.factor(trash_burning_1week_baseline)1 or 2 times" = "Waste Burning (1-2/week)", 
  "as.factor(trash_burning_1week_baseline)3 or more times" = "Waste Burning (2+/week)", 
  "as.factor(trash_burning_1week_baseline)" = "Waste Burning"
)

output <- 
  output_mean_contrib %>%
  mutate(term = dplyr::recode(term, !!!dict)) %>%
  full_join(mutate(output_lmg_tbl, term = dplyr::recode(term, !!!dict)), by = "term") %>% 
  mutate(
    term = factor(term, levels = c("Outdoor Ambient", "Smoked (24 Hours)", "Waste Burning (1-2/week)", 
                                  "Waste Burning (2+/week)", "Waste Burning", "Kitchen source", "Cooking", 
                                  "Distance to Main Road"))) %>%
  arrange(term)  ; output

options(knitr.kable.NA = '')
output %>%
  mutate(frac = scales::percent(frac, accuracy = 1), 
      lmg = scales::percent(lmg, accuracy = 1)) %>%
   dplyr::select("Source" = term, "Reg. Estimate" = estimate, "p.value" = pvalue, 
      "Mean Value of Source" = mean_x, "Mean Contribution" = frac, 
      "R2 Contribution" = lmg) %>%
  knitr::kable(format.args = list(big.mark = ","), 
              digits = c(NA, 2, 3, 2, 1, 1), format = "latex", 
              booktabs = TRUE, align = "c") %>%
  kableExtra::kable_styling(full_width = FALSE) %>%
  writeLines(file.path(gdir, "output/tables/mean_contribution_table.tex"))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# repeat exercise, but with spikes as the outcome
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
reg_spikes <- feols(
  fixest::xpd(spike ~ .[rhs_fml] | hour + week) ,
  data = pm,
  cluster = ~ respondent_id + date_hour
)

output_spike_contrib <- compute_mean_contrib(
  model = reg_spikes,
  rhs_fml = rhs_fml,
  data = pm,
  outcome_var = "spike"
)


# probit and logit give someone similar effects - let's proceed with linear probability model


# ---- 2) Variance Decomposition (LMG) ----

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

output_lmg_spike_tbl <- compute_lmg_tbl(reg_spike_lm)

# combine mean contribution with R2 analysis to output 1 table
 output_spike <- 
   output_spike_contrib %>%
   mutate(term = dplyr::recode(term, !!!dict)) %>%
   full_join(mutate(output_lmg_spike_tbl, term = dplyr::recode(term, !!!dict)), by = "term") %>% 
   mutate(lmg = scales::percent(lmg, accuracy = 1), 
      term = factor(term, levels = c("Outdoor Ambient", "Smoked (24 Hours)", "Waste Burning (1-2/week)", 
                                    "Waste Burning (2+/week)", "Waste Burning", "Kitchen source", "Cooking", 
                                    "Distance to Main Road"))) %>%
   arrange(term)  ; output_spike

options(knitr.kable.NA = '')
output_spike %>%
  mutate(frac = scales::percent(frac, accuracy = 1)) %>%
  dplyr::select("Source" = term, "Reg. Estimate" = estimate, "p.value" = pvalue, 
      "Mean Value of Source" = mean_x, "Mean Contribution" = frac, 
      "R2 Contribution" = lmg) %>%
  knitr::kable(format.args = list(big.mark = ","), 
              digits = c(NA, 3, 3, 2, 1, 1), format = "latex", 
              booktabs = TRUE, 
              align = "c") %>%
  kableExtra::kable_styling(full_width = FALSE) %>%
  writeLines(file.path(gdir, "output/tables/spike_contribution_table.tex"))



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot of contributions to pm and spikes
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot breakdown of indoor vs outdoor sources as pyramid chart
p_contributions <- 
  output %>%
  filter(!is.na(frac)) %>%
  mutate(outdoor = str_detect(term, "Outdoor"), 
         term = str_wrap(term, 12), 
         term = factor(term, levels = c("Outdoor\nAmbient", "Smoked (24\nHours)", "Waste\nBurning\n(1-2/week)", 
                                        "Waste\nBurning\n(2+/week)", "Kitchen\nsource", "Cooking", 
                                        "Distance to\nMain Road"))) %>%
  ggplot(aes(x = term, y = frac, fill = outdoor)) + 
  geom_col(width = .75) + 
  geom_errorbar(aes(ymin = frac.low, ymax = frac.high), 
        width = 0, size = .1, color = "gray60") +
  geom_hline(aes(yintercept = 0), size = .1) + 
  scale_fill_brewer(palette = "Dark2") + 
 # scale_color_manual(values = c("#99d8c9", "#fee391")) + 
  geom_vline(aes(xintercept = 0)) + 
  ylab("Source of\nIndoor PM2.5") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-.382, .81)) 


p_spike <- 
  output_spike %>%
  filter(!is.na(frac)) %>%
  mutate(outdoor = str_detect(term, "Outdoor"), 
         term = str_wrap(term, 12), 
         term = factor(term, levels = c("Outdoor\nAmbient", "Smoked (24\nHours)", "Waste\nBurning\n(1-2/week)", 
                                        "Waste\nBurning\n(2+/week)", "Kitchen\nsource", "Cooking", 
                                        "Distance to\nMain Road"))) %>%
  ggplot(aes(x = term, y = frac, fill = outdoor)) + 
  geom_col(width = .75) + 
  geom_errorbar(aes(ymin = frac.low, ymax = frac.high), 
        width = 0, size = .1, color = "gray60") +
  geom_hline(aes(yintercept = 0), size = .1) + 
  scale_fill_brewer(palette = "Dark2") + 
 # scale_color_manual(values = c("#99d8c9", "#fee391")) + 
  geom_hline(aes(yintercept = 0), size = .1) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-.382, .81)) + 
   ylab("Source of\n PM2.5 Spikes") 

p_contributions / p_spike +
  plot_annotation(tag_levels = "a", tag_suffix = ".") 
ggsave(file.path(gdir, "output/figures/fig3_sources.png"), 
    width = 10, height= 10, bg = "transparent", units = "cm")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Repeat decomposition for each income quartile
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
run_mean_contrib_by_group <- function(data_group, group_value, rhs_fml, dict, outcome_var) {
  reg_group <- feols(
    fixest::xpd(as.formula(paste0(outcome_var, " ~ .[rhs_fml] | hour + week"))),
    data = data_group,
    cluster = ~ respondent_id + date_hour
  )

  compute_mean_contrib(
    model = reg_group,
    rhs_fml = rhs_fml,
    data = data_group,
    outcome_var = outcome_var
  ) %>%
    mutate(term = dplyr::recode(term, !!!dict)) %>%
    mutate(income_quart = group_value)
}

income_quart_levels <- pm %>%
  filter(!is.na(income_quart)) %>%
  distinct(income_quart) %>%
  pull(income_quart) %>%
  as.character()

decomp_by_income <- map_df(
  income_quart_levels,
  ~ run_mean_contrib_by_group(
    data_group = filter(pm, income_quart == .x),
    group_value = .x,
    rhs_fml = rhs_fml,
    dict = dict,
    outcome_var = "pm25_indoor"
  ) %>%
  mutate(income_quart = .x)
)

# only keep pm2.5 and smoking since they're ethe only precisely estimated ones
decomp_by_income_tokeep <- 
  decomp_by_income %>%
  filter(str_detect(term, "Outdoor|Smoked")) %>%
  dplyr::select(income_quart, term, contribution)  

# Plot: mean indoor PM2.5 by income quartile decomposed by term
r_indoor <- 
  feols(pm25_mean ~ income_quart + 0, data = pm, cluster = ~respondent_id+date_hour) %>%
  tidy(conf.int = TRUE) %>%
  dplyr::rename(income_quart = term) %>%
  mutate(income_quart = str_remove(income_quart, "income_quart")) 

decomp_remainder <- 
  decomp_by_income_tokeep %>%
  group_by(income_quart) %>%
  summarise(contribution = sum(contribution, na.rm = TRUE), .groups = "drop") %>%
  left_join(r_indoor, by = "income_quart") %>%  
  mutate(unexplained = estimate - contribution) %>%
  dplyr::select(income_quart, contribution = unexplained) %>%
  mutate(term = "Other")

p_decomp_income <- 
  bind_rows(decomp_by_income_tokeep, decomp_remainder) %>%
  mutate(term = factor(term, levels = c("Outdoor Ambient", "Other", "Smoked (24 Hours)"))) %>%
   mutate(income_quart = if_else(income_quart == "Income Bin 1", "Income Bin 1 (lowest)", income_quart), 
          income_quart = str_wrap(income_quart, width = 5),
          income_quart = factor(income_quart, levels = c("Income\nBin 1\n(lowest)", "Income\nBin 2", "Income\nBin 3", "Income\nBin 4"))) %>%
  ggplot(aes(x = income_quart, y = contribution, fill = term)) +
  geom_col(width = .75) +
  geom_hline(yintercept = 0, linewidth = .2) +
  scale_fill_brewer(palette = "Dark2") + 
  ylab(expression(PM[2.5] ~ (mu * g/m^3))) +
  xlab("Income quartile") +
  facet_wrap(~"Indoor PM2.5") + 
  theme(axis.title.y = element_text(angle = 90), 
        strip.background = element_blank()); p_decomp_income

# save this as a plot for figure 4, when we discuss income heterogeneity
write_rds(p_decomp_income, file.path(ddir, "plot_decomp_by_income.rds"))

