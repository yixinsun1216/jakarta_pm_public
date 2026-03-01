# ============================================================
# Calculate key statistics for paper text
# ============================================================
library(tidyverse)
library(fixest)

ddir <- "data"

pm <- read_rds(file.path(ddir, "df_reg.rds")) %>%
  mutate(cooking = replace_na(cooking, 0))
survey <- read_rds(file.path(ddir, "df_survey.rds"))

# Inverse-frequency weights
hour_weights <- pm %>%
  group_by(hour) %>%
  summarise(prop_observed = mean(!is.na(pm25_indoor)), .groups = "drop") %>%
  mutate(weight_hour = 1 / prop_observed)
pm <- pm %>% left_join(hour_weights, by = "hour")

cat("============================================================\n")
cat("KEY STATISTICS FOR PAPER\n")
cat("============================================================\n\n")

# 1. Spike percentage
spike_pct <- mean(pm$pm25_indoor > 100, na.rm = TRUE) * 100
cat(sprintf("Hourly concentrations spiked above 100 ug/m3 during %.1f%% of monitored hours.\n\n", spike_pct))

# 2. Low vs high income
mean_by_income <- pm %>%
  filter(!is.na(income_quart)) %>%
  group_by(income_quart) %>%
  summarise(mean_pm = mean(pm25_indoor, na.rm = TRUE))
bin1 <- mean_by_income %>% filter(income_quart == "Income Bin 1") %>% pull(mean_pm)
bin4 <- mean_by_income %>% filter(income_quart == "Income Bin 4") %>% pull(mean_pm)
pct_higher <- (bin1 - bin4) / bin4 * 100
cat(sprintf("Low-income (income_quart = bin 1) households experience %.0f%% higher indoor PM2.5 than high-income households (income_quart = bin 4).\n\n", pct_higher))

# 3. Waste burning >2/week
trash_pct <- mean(as.numeric(survey$trash_burning_1week_baseline) > 2, na.rm = TRUE) * 100
cat(sprintf("We estimate that households in areas with more than two waste-burning events per week (%.0f%% of the sample) experienced both higher average indoor PM2.5 concentrations and a higher probability of spikes in indoor PM2.5.\n\n", trash_pct))

# 4. Within 500m of primary road (household level)
within500 <- pm %>%
  distinct(respondent_id, dist_primary) %>%
  summarise(pct = mean(dist_primary <= 500, na.rm = TRUE) * 100) %>%
  pull(pct)
cat(sprintf("%.0f%% of our sample lives within 500 meters from a primary road.\n\n", within500))

# 5-7. Income decomposition via interaction model
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

coefs_decomp <- coef(reg_decomp_income)

income_quart_levels <- pm %>%
  filter(!is.na(income_quart)) %>%
  distinct(income_quart) %>%
  pull(income_quart) %>%
  as.character()

decomp_by_income <-
  map_df(income_quart_levels, function(iq) {
    data_iq <- filter(pm, income_quart == iq)

    beta_outdoor <- unname(coefs_decomp[paste0("income_quart::", iq, ":pm25_outdoor3")])
    mean_outdoor <- mean(data_iq$pm25_outdoor3, na.rm = TRUE)

    beta_smoke <- unname(coefs_decomp[paste0("as.factor(smoke24_endline)::1:income_quart::", iq)])
    mean_smoke <- mean(as.numeric(as.character(data_iq$smoke24_endline)), na.rm = TRUE)

    tibble(
      income_quart = iq,
      term = c("Outdoor Ambient", "Smoking Household"),
      contribution = c(beta_outdoor * mean_outdoor, beta_smoke * mean_smoke)
    )
  })

r_indoor <-
  feols(pm25_indoor ~ income_quart + 0, data = pm,
        cluster = ~respondent_id + date_hour, weights = ~weight_hour) %>%
  broom::tidy(conf.int = TRUE) %>%
  dplyr::rename(income_quart = term) %>%
  mutate(income_quart = str_remove(income_quart, "income_quart"))

decomp_remainder <-
  decomp_by_income %>%
  group_by(income_quart) %>%
  summarise(contribution = sum(contribution, na.rm = TRUE), .groups = "drop") %>%
  left_join(r_indoor, by = "income_quart") %>%
  mutate(unexplained = estimate - contribution) %>%
  dplyr::select(income_quart, contribution = unexplained) %>%
  mutate(term = "Other")

all_decomp <- bind_rows(decomp_by_income, decomp_remainder)

diff_tbl <- all_decomp %>%
  pivot_wider(names_from = income_quart, values_from = contribution) %>%
  mutate(diff = `Income Bin 1` - `Income Bin 4`)

total_diff <- bin1 - bin4

outdoor_pct <- diff_tbl %>% filter(term == "Outdoor Ambient") %>% pull(diff) / total_diff * 100
smoke_pct <- diff_tbl %>% filter(term == "Smoking Household") %>% pull(diff) / total_diff * 100
other_pct <- diff_tbl %>% filter(term == "Other") %>% pull(diff) / total_diff * 100

cat(sprintf("Overall, we estimate that differences in infiltration explain %.0f%% of the difference in average indoor PM2.5 between the lowest and highest income quartile.\n\n", outdoor_pct))
cat(sprintf("Overall, we estimate that smoking differences explain %.0f%% of the difference in average indoor PM2.5 between the lowest and highest income quartile.\n\n", smoke_pct))
cat(sprintf("We estimate that differences in other hyperlocal pollution sources explain the remaining %.0f%% of the difference in average indoor PM2.5 between the lowest and highest income quartile.\n", other_pct))
