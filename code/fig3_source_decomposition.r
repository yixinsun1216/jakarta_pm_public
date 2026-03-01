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

# ------------------------------------------------------------------
# Inverse-frequency weights: upweight hours with more missing data
# ------------------------------------------------------------------
hour_weights <- pm %>%
  group_by(hour) %>%
  summarise(prop_observed = mean(!is.na(pm25_indoor)), .groups = "drop") %>%
  mutate(weight_hour = 1 / prop_observed)
pm <- pm %>% left_join(hour_weights, by = "hour")


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
rhs_fml_sources <- ~ as.factor(trash_burning_1week_baseline) +
  as.factor(smoke24_endline) +
  as.factor(room_pmsource_kitchen) +
  pm25_outdoor3 +
  cooking +
  dist_primary +
  temp_outdoor3 +
  humidity_outdoor3

reg_fe <- feols(
  fixest::xpd(pm25_indoor ~ .[rhs_fml] | hour + week) ,
  data = pm,
  cluster = ~ respondent_id + date_hour,
  weights = ~weight_hour
)

etable(reg_fe)

output_mean_contrib <- compute_mean_contrib(
  model = reg_fe,
  rhs_fml = rhs_fml_sources,
  data = pm,
  outcome_var = "pm25_indoor"
) %>%
  filter(!term %in% c("temp_outdoor3", "humidity_outdoor3"))


# ----------- 2) Variance Decomposition (LMG) --------------------
reg_lm <- lm(
  pm25_indoor ~ pm25_outdoor3 +
    as.factor(smoke24_endline) +
    as.factor(trash_burning_1week_baseline) +
    as.factor(room_pmsource_kitchen) +
    cooking +
    dist_primary +
   as.factor(hour) + as.factor(week),
  data = pm,
  weights = weight_hour
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
# plot of contributions to pm (left pane: coeff plot; right pane: contribution bars)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Left pane: coefficient plot (analogous to old Figure 3b)
# Extract coefficients from reg_fe for source variables at 90% and 95% CIs
mean_pm_indoor <- mean(pm$pm25_indoor, na.rm = TRUE)

source_terms <- c(
  "as.factor(trash_burning_1week_baseline)1 or 2 times",
  "as.factor(trash_burning_1week_baseline)3 or more times",
  "as.factor(smoke24_endline)1",
  "as.factor(room_pmsource_kitchen)1",
  "cooking",
  "dist_primary"
)

coef_pm <-
  tidy(reg_fe, conf.int = TRUE) %>%
  dplyr::select(term, estimate, conf.low95 = conf.low, conf.high95 = conf.high) %>%
  left_join(tidy(reg_fe, conf.int = TRUE, conf.level = 0.9) %>%
              dplyr::select(term, conf.low, conf.high)) %>%
  filter(term %in% source_terms) %>%
  mutate(term = dplyr::recode(term, !!!dict),
         term = factor(term, levels = rev(c("Smoked (24 Hours)",
                                            "Waste Burning (2+/week)",
                                            "Waste Burning (1-2/week)",
                                            "Kitchen source",
                                            "Cooking",
                                            "Distance to Main Road"))))

p_sources_coef <-
  ggplot(coef_pm, aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, color = "gray50", linetype = "dashed", linewidth = .1) +
  geom_vline(xintercept = mean_pm_indoor, linetype = "dashed", color = "#1b9e77", size = .2) +
  geom_errorbarh(aes(xmin = conf.low95, xmax = conf.high95), height = 0, alpha = .4, size = .2, color = "#1b9e77") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, size = .2, color = "#1b9e77") +
  geom_point(size = .8, color = "#1b9e77") +
  xlab(expression(PM[2.5] ~ (mu * g~m^-3))) +
  ylab("Effect of\nHyperlocal\nSource") +
  annotate("text", x = mean_pm_indoor + 0.3, y = 6.5, label = "mean", size = 1.5, color = "#1b9e77", hjust = 0) +
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

# Right pane: mean contribution bars (unchanged)
p_contributions <-
  output %>%
  filter(!is.na(frac)) %>%
  mutate(outdoor = str_detect(term, "Outdoor"),
         term = str_wrap(term, 12),
         term = factor(term, levels = c("Distance to\nMain Road", "Cooking", "Kitchen\nsource",
                                        "Waste\nBurning\n(1-2/week)", "Waste\nBurning\n(2+/week)",
                                        "Smoked (24\nHours)", "Outdoor\nAmbient"))) %>%
  ggplot(aes(y = term, x = frac, fill = outdoor)) +
  geom_col(width = .75) +
  geom_errorbarh(aes(xmin = frac.low, xmax = frac.high),
        height = 0, size = .1, color = "gray60") +
  geom_vline(xintercept = 0, size = .1) +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-.382, .81)) +
  ylab("Source") +
  xlab("Estimated Contribution to Indoor PM2.5 (%)") +
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

p_sources_coef + p_contributions +
  plot_annotation(tag_levels = "a", tag_suffix = ".")
ggsave(file.path(gdir, "output/figures/fig3_sources.png"),
    width = 14, height= 8, bg = "transparent", units = "cm")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Income decomposition via interaction approach
# Single regression with income_quart interacted with all source variables
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
reg_decomp_income <- feols(
  pm25_indoor ~
    income_quart + 
    i(income_quart, pm25_outdoor3) +
    i(as.factor(trash_burning_1week_baseline), income_quart, ref = "Never") +
    i(as.factor(smoke24_endline), income_quart, ref = 0) +
    i(as.factor(room_pmsource_kitchen), income_quart, ref = 0) +
    i(cooking, income_quart, ref = 0) +
    i(income_quart, dist_primary) +
    temp_outdoor3 + humidity_outdoor3 |
    hour + week,
  data = pm,
  cluster = ~respondent_id + date_hour,
  weights = ~weight_hour
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
        cluster = ~respondent_id+date_hour, weights = ~weight_hour) %>%
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
  scale_fill_brewer(palette = "Dark2") +
  ylab(expression(PM[2.5] ~ (mu * g/m^3))) +
  xlab("Income quartile") +
  facet_wrap(~"Indoor PM2.5") +
  theme(axis.title.y = element_text(angle = 90),
        strip.background = element_blank()); p_decomp_income

# save this as a plot for figure 4, when we discuss income heterogeneity
write_rds(p_decomp_income, file.path(ddir, "plot_decomp_by_income.rds"))
