#!/usr/bin/env Rscript

# ============================================================
# Calculate key statistics for manuscript text and export TeX
# ============================================================
suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(broom)
})

gdir <- getwd()
ddir <- file.path(gdir, "data")
out_tex <- file.path(gdir, "output", "tables", "paper_stats.tex")

pm <- read_rds(file.path(ddir, "df_reg.rds")) %>%
  mutate(cooking = replace_na(cooking, 0))
survey <- read_rds(file.path(ddir, "df_survey.rds"))

# Inverse-frequency weights: upweight hours with more missing indoor PM2.5
hour_weights <- pm %>%
  group_by(hour) %>%
  summarise(prop_observed = mean(!is.na(pm25_indoor)), .groups = "drop") %>%
  mutate(weight_hour = 1 / prop_observed)

pm <- pm %>% left_join(hour_weights, by = "hour")

fmt_pct <- function(x, digits = 1) sprintf(paste0("%.", digits, "f"), x)

# 1) Share of monitored hours with indoor PM2.5 above 100
spike_pct <- mean(pm$pm25_indoor > 100, na.rm = TRUE) * 100

# 2) Bin 1 vs Bin 4 indoor PM2.5 gap from weighted regression used in figures
r_indoor <- feols(
  pm25_indoor ~ income_quart + 0,
  data = pm,
  cluster = ~respondent_id + date_hour,
  weights = ~weight_hour
)

income_est <- tidy(r_indoor) %>%
  mutate(income_quart = str_remove(term, "income_quart")) %>%
  select(income_quart, estimate)

bin1 <- income_est %>% filter(income_quart == "Income Bin 1") %>% pull(estimate)
bin4 <- income_est %>% filter(income_quart == "Income Bin 4") %>% pull(estimate)
income_gap_pct <- (bin1 - bin4) / bin4 * 100

# 3) Share of sample with >2 neighborhood waste-burning events/week
survey_unique <- survey %>% distinct(respondent_id, .keep_all = TRUE)
trash_gt2 <- survey_unique %>%
  mutate(
    trash_num = suppressWarnings(as.numeric(as.character(trash_burning_1week_baseline))),
    trash_chr = as.character(trash_burning_1week_baseline)
  ) %>%
  transmute(
    trash_gt2 = case_when(
      !is.na(trash_num) ~ trash_num > 2,
      TRUE ~ str_detect(trash_chr, "3|more")
    )
  ) %>%
  pull(trash_gt2)
trash_gt2_pct <- mean(trash_gt2, na.rm = TRUE) * 100

# 4) Share within 500m of a primary road
within500_pct <- pm %>%
  distinct(respondent_id, dist_primary) %>%
  summarise(pct = mean(dist_primary <= 500, na.rm = TRUE) * 100) %>%
  pull(pct)

# 4b) Spike probability difference: smoking vs non-smoking households
spike_smoke <- pm %>%
  filter(!is.na(spike), !is.na(smoke24_endline), !is.na(weight_hour)) %>%
  mutate(
    smoke_hh = smoke24_endline == 1,
    spike_num = as.numeric(spike)
  )

p_smoke <- spike_smoke %>%
  filter(smoke_hh) %>%
  summarise(p = weighted.mean(spike_num, weight_hour, na.rm = TRUE)) %>%
  pull(p)

p_nonsmoke <- spike_smoke %>%
  filter(!smoke_hh) %>%
  summarise(p = weighted.mean(spike_num, weight_hour, na.rm = TRUE)) %>%
  pull(p)

smoke_spike_line <- if (!is.finite(p_nonsmoke) || p_nonsmoke <= 0) {
  sprintf(
    "\\item The probability of measuring spikes in indoor pollution levels was also \\textcolor{magenta}{undefined} higher in smoking households than in non-smoking households (smoking: %s\\%%, non-smoking: %s\\%%).",
    fmt_pct(p_smoke * 100, 1),
    fmt_pct(p_nonsmoke * 100, 1)
  )
} else {
  smoke_spike_lift_pct <- (p_smoke - p_nonsmoke) / p_nonsmoke * 100
  sprintf(
    "\\item The probability of measuring spikes in indoor pollution levels was also \\textcolor{magenta}{%s\\%%} higher in smoking households than in non-smoking households.",
    fmt_pct(smoke_spike_lift_pct, 1)
  )
}

# 5-7) Decomposition of Bin1-Bin4 PM2.5 gap into infiltration, smoking, other
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

coef_decomp <- coef(reg_decomp_income)
income_levels <- sort(unique(as.character(pm$income_quart[!is.na(pm$income_quart)])))

decomp_by_income <- map_dfr(income_levels, function(iq) {
  data_iq <- filter(pm, income_quart == iq)

  beta_outdoor <- unname(coef_decomp[paste0("income_quart::", iq, ":pm25_outdoor3")])
  mean_outdoor <- mean(data_iq$pm25_outdoor3, na.rm = TRUE)

  beta_smoke <- unname(coef_decomp[paste0("as.factor(smoke24_endline)::1:income_quart::", iq)])
  mean_smoke <- mean(as.numeric(as.character(data_iq$smoke24_endline)), na.rm = TRUE)

  tibble(
    income_quart = iq,
    term = c("Outdoor Ambient", "Smoking Household"),
    contribution = c(beta_outdoor * mean_outdoor, beta_smoke * mean_smoke)
  )
})

decomp_remainder <- decomp_by_income %>%
  group_by(income_quart) %>%
  summarise(contribution = sum(contribution, na.rm = TRUE), .groups = "drop") %>%
  left_join(income_est, by = "income_quart") %>%
  mutate(unexplained = estimate - contribution) %>%
  transmute(income_quart, term = "Other", contribution = unexplained)

all_decomp <- bind_rows(decomp_by_income, decomp_remainder)

gap_by_term <- all_decomp %>%
  pivot_wider(names_from = income_quart, values_from = contribution) %>%
  mutate(diff = `Income Bin 1` - `Income Bin 4`) %>%
  select(term, diff)

total_gap <- bin1 - bin4
infiltration_pct <- (gap_by_term %>% filter(term == "Outdoor Ambient") %>% pull(diff)) / total_gap * 100
smoking_pct <- (gap_by_term %>% filter(term == "Smoking Household") %>% pull(diff)) / total_gap * 100
other_pct <- (gap_by_term %>% filter(term == "Other") %>% pull(diff)) / total_gap * 100

lines_out <- c(
  "\\begin{itemize}",
  sprintf("\\item Hourly concentrations spiked above 100 \\(\\mu\\)g/m\\(^3\\) during %s\\%% of monitored hours.", fmt_pct(spike_pct, 1)),
  sprintf("\\item Low-income (income\\_quart = bin 1) households experience %s\\%% higher indoor PM2.5 than high-income households (income\\_quart = bin 4).", fmt_pct(income_gap_pct, 1)),
  sprintf("\\item We estimate that households in areas with more than two waste-burning events per week (%s\\%% of the sample) experienced both higher average indoor PM2.5 concentrations and a higher probability of spikes in indoor PM2.5.", fmt_pct(trash_gt2_pct, 1)),
  smoke_spike_line,
  sprintf("\\item %s\\%% of our sample lives within 500 meters from a primary road (dist\\_primary).", fmt_pct(within500_pct, 1)),
  sprintf("\\item Overall, we estimate that differences in infiltration explain %s\\%% of the difference in average indoor PM2.5 between the lowest and highest income quartile.", fmt_pct(infiltration_pct, 1)),
  sprintf("\\item Overall, we estimate that smoking differences explain %s\\%% of the difference in average indoor PM2.5 between the lowest and highest income quartile.", fmt_pct(smoking_pct, 1)),
  sprintf("\\item We estimate that differences in other hyperlocal pollution sources explain the remaining %s\\%% of the difference in average indoor PM2.5 between the lowest and highest income quartile.", fmt_pct(other_pct, 1)),
  "\\end{itemize}"
)

writeLines(lines_out, out_tex)

cat("Wrote: ", out_tex, "\n", sep = "")
cat(paste(lines_out, collapse = "\n"), "\n")
