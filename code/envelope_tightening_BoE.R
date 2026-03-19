rm(list = ls_extra())
gc()

# ===========================================================
# read in data
# ===========================================================

# read in survey data
survey <- 
  file.path(ddir, "df_survey.rds") %>%
  read_rds() %>%
  mutate(adult_timeuse_home_frac = adult_timeuse_home_baseline / adult_timeuse_total_baseline,
         child_timeuse_home_frac = child_timeuse_home_baseline / child_timeuse_total_baseline, 
         day_of_week = lubridate::wday(starttime_baseline, label = TRUE), 
         trash = as.numeric(trash_burning_1week_baseline) > 1, 
         open_room = close_door_1hour == 1 | close_window_1hour == 1, 
         date_hour = floor_date(starttime, "hour"))  %>%
  dplyr::select(respondent_id, income_quart, smoke24_endline, pm25_mean, pm25outdoor_mean)


# ===========================================================
# Compute objects of interest from the data
# ===========================================================

# average indoor / outdoor PM
data_summ = survey %>%
  dplyr::group_by(income_quart, smoke24_endline) %>%
  dplyr::summarize(pm25_mean = mean(pm25_mean, na.rm=T),
                   pm25outdoor_mean = mean(pm25outdoor_mean, na.rm=T)) %>% ungroup() %>%
  dplyr::filter(income_quart %in% c("Income Bin 1", "Income Bin 4") & smoke24_endline %in% c(0, 1)) %>%
  mutate(
    row_label = paste0(income_quart, ", ", ifelse(smoke24_endline == 1, "Smoking", "Non-smoking"))
  ) %>%
  dplyr::select(row_label, pm25_mean, pm25outdoor_mean) 

# infiltration rates (from fig2_infiltration)
data_summ = data_summ %>%
  dplyr::mutate(Finf =ifelse(grepl("Income Bin 1", row_label), 0.7775, ifelse(grepl("Income Bin 4", row_label), 0.5141, NA)))
setnames(data_summ, old=c("pm25_mean", "pm25outdoor_mean"), new=c("Cin", "Cout"))


# ===========================================================
# External parameters
# ===========================================================

data_summ$k = 0.5
data_summ$P = 1.0

data_summ$k_min = 0.3
data_summ$k_max = 5.0
data_summ$P_min = 0.7
data_summ$P_max = 1.0

# ===========================================================
# Recover other objects given data and external parameters
# ===========================================================



data_summ = data_summ %>%
  dplyr::mutate(# Residual indoor pollution not explained by outdoor infiltration
                res = Cin - Finf * Cout,
                # Air exchange rate a
                a = k * Finf / (P - Finf),
                # S 
                S = res * (a + k),
                
                # a post weatherization
                a_weather20 = a * 0.8,
                # Cin post weatherization
                Cin_weather20 = (a_weather20 * P * Cout + S) / (a_weather20 + k),

                # P with better ventilation (lower flow of air coming in)
                P_vent = P * 0.5,
                # Cin post ventilation
                Cin_vent = (a * P_vent * Cout + S) / (a + k),
                
                
                # Robustness - P_min-P_max ; k_min-k_max
                a_Pmin_kmin = k_min * Finf / (P_min - Finf),
                a_Pmin_kmax = k_max * Finf / (P_min - Finf),
                a_Pmax_kmin = k_min * Finf / (P_max - Finf),
                a_Pmax_kmax = k_max * Finf / (P_max - Finf),
                
                # S 
                S_Pmin_kmin = res * (a_Pmin_kmin + k_min),
                S_Pmin_kmax = res * (a_Pmin_kmax + k_max),
                S_Pmax_kmin = res * (a_Pmax_kmin + k_min),
                S_Pmax_kmax = res * (a_Pmax_kmax + k_max),
                
                # a post weatherization
                a_weather20_Pmin_kmin = a_Pmin_kmin * 0.8,
                a_weather20_Pmin_kmax = a_Pmin_kmax * 0.8,
                a_weather20_Pmax_kmin = a_Pmax_kmin * 0.8,
                a_weather20_Pmax_kmax = a_Pmax_kmax * 0.8,
                # Cin post weatherization
                Cin_weather20_Pmin_kmin = (a_weather20_Pmin_kmin * P_min * Cout + S) / (a_weather20_Pmin_kmin + k_min),
                Cin_weather20_Pmin_kmax = (a_weather20_Pmin_kmax * P_min * Cout + S) / (a_weather20_Pmin_kmax + k_max),
                Cin_weather20_Pmax_kmin = (a_weather20_Pmax_kmin * P_max * Cout + S) / (a_weather20_Pmax_kmin + k_min),
                Cin_weather20_Pmax_kmax = (a_weather20_Pmax_kmax * P_max * Cout + S) / (a_weather20_Pmax_kmax + k_max),
                
                Cin_diff_weather20_Pmin_kmin = Cin_weather20_Pmin_kmin - Cin,
                Cin_diff_weather20_Pmin_kmax = Cin_weather20_Pmin_kmin - Cin,
                Cin_diff_weather20_Pmax_kmin = Cin_weather20_Pmax_kmin - Cin,
                Cin_diff_weather20_Pmax_kmax = Cin_weather20_Pmax_kmax - Cin)



# MAKE TABLE WITH PARAMETERS ---------------------------------------------------
# =============================================================================
# Generate back-of-envelope PM2.5 LaTeX table body from data_summ
# =============================================================================
# Outputs ONLY the \begin{tabular}...\end{tabular} block plus the notes
# minipage. The \begin{table}, \caption, \label, \resizebox etc. are all
# handled in Overleaf (see table_boe_wrapper.tex).
# =============================================================================

library(dplyr)

# ---- helpers ----------------------------------------------------------------
fmt1  <- function(x) formatC(round(x, 1), format = "f", digits = 1)
fmt3  <- function(x) formatC(round(x, 3), format = "f", digits = 3)

fmt1s <- function(x) {
  ifelse(x >= 0,
         paste0("$+", fmt1(x), "$"),
         paste0("$",  fmt1(x), "$"))
}

robustness_range <- function(row) {
  pmin_invalid <- row$P_min < row$Finf
  if (pmin_invalid) {
    valid_diffs <- c(row$Cin_diff_weather20_Pmax_kmin,
                     row$Cin_diff_weather20_Pmax_kmax)
    dagger <- "\\textsuperscript{\\dag}"
  } else {
    valid_diffs <- c(row$Cin_diff_weather20_Pmin_kmin,
                     row$Cin_diff_weather20_Pmin_kmax,
                     row$Cin_diff_weather20_Pmax_kmin,
                     row$Cin_diff_weather20_Pmax_kmax)
    dagger <- ""
  }
  lo <- min(valid_diffs)
  hi <- max(valid_diffs)
  paste0("[", fmt1s(lo), ",\\;", fmt1s(hi), "]", dagger)
}

# ---- row builders -----------------------------------------------------------
panel_a_row <- function(row) {
  paste(
    row$row_label,
    fmt1(row$Cin),
    fmt1(row$Cout),
    fmt3(row$Finf),
    fmt1(row$a),
    fmt1(row$S),
    sep = " & "
  ) |> paste0(" \\\\")
}

panel_b_row <- function(row) {
  delta_w <- row$Cin_weather20 - row$Cin
  delta_v <- row$Cin_vent      - row$Cin
  paste(
    row$row_label,
    fmt1s(delta_w),
    robustness_range(row),
    fmt1s(delta_v),
    "Always $\\downarrow$",
    sep = " & "
  ) |> paste0(" \\\\")
}

panel_a_body <- sapply(seq_len(nrow(data_summ)),
                       function(i) panel_a_row(as.list(data_summ[i, ]))) |>
  paste(collapse = "\n")

panel_b_body <- sapply(seq_len(nrow(data_summ)),
                       function(i) panel_b_row(as.list(data_summ[i, ]))) |>
  paste(collapse = "\n")

# ---- parameter values for notes --------------------------------------------
k_central <- data_summ$k[1]
P_central <- data_summ$P[1]
k_min_val <- data_summ$k_min[1]
k_max_val <- data_summ$k_max[1]
P_min_val <- data_summ$P_min[1]
P_max_val <- data_summ$P_max[1]

needs_dagger     <- any(data_summ$P_min < data_summ$Finf)
dagger_bin_label <- data_summ |>
  filter(P_min < Finf) |>
  pull(row_label) |>
  gsub(",.*", "", x = _) |>
  unique() |>
  paste(collapse = " and ")

dagger_note <- if (needs_dagger) {
  paste0(
    "\n\\smallskip\n",
    "\\textsuperscript{\\dag}~For ", dagger_bin_label,
    " ($\\hat{F}_{inf} > ", fmt1(P_min_val), "$), ",
    "$P_{\\min} = ", fmt1(P_min_val), " < \\hat{F}_{inf}$, ",
    "which would imply a non-positive air exchange rate and is therefore excluded; ",
    "the robustness range for these groups varies only $k$, holding $P = ",
    fmt1(P_max_val), "$."
  )
} else {
  ""
}

# =============================================================================
# Assemble: tabular body + notes only (no \begin{table} wrapper)
# =============================================================================

lines <- c(
  "% -----------------------------------------------------------------------",
  "% Auto-generated by table_boe.R -- do not edit by hand",
  "% -----------------------------------------------------------------------",
  "",
  "\\begin{tabular}{lcccccc}",
  "\\toprule",
  "%---- Panel A -------------------------------------------------------",
  "\\multicolumn{7}{l}{\\textbf{Panel A. Data inputs and derived parameters}} \\\\[2pt]",
  "  & $C_{in}$",
  "  & $C_{out}$",
  "  & $\\hat{F}_{inf}$",
  "  & $a$",
  "  & $S$",
  "  \\\\",
  "  & ($\\mu$g\\,m$^{-3}$)",
  "  & ($\\mu$g\\,m$^{-3}$)",
  "  &",
  "  & (h$^{-1}$)",
  "  & ($\\mu$g\\,m$^{-3}$\\,h$^{-1}$)",
  "  \\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-4}\\cmidrule(lr){5-6}",
  panel_a_body,
  "\\midrule",
  "%---- Panel B -------------------------------------------------------",
  "\\multicolumn{7}{l}{\\textbf{Panel B. Counterfactual $\\Delta C_{in}$ ($\\mu$g\\,m$^{-3}$)}} \\\\[2pt]",
  "  &",
  "  \\multicolumn{2}{c}{Weatherization ($a \\times 0.8$)}",
  "  &",
  "  \\multicolumn{2}{c}{Ventilation ($P \\times 0.5$)}",
  "  & \\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
  "  & Central",
  "  & Robust.~range\\textsuperscript{(a)}",
  "  & Central",
  "  & Direction",
  "  & \\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
  panel_b_body,
  "\\bottomrule",
  "\\end{tabular}",
  "",
  "%---- Notes (minipage so footnotes stay attached to table) ----------",
  "\\begin{minipage}{\\linewidth}",
  "\\footnotesize",
  "\\textit{Notes.}",
  "$C_{in}$ and $C_{out}$ are mean observed indoor and outdoor PM$_{2.5}$ concentrations",
  "($\\mu$g\\,m$^{-3}$).",
  "$\\hat{F}_{inf}$ is the estimated infiltration factor from regression (see main text).",
  "The air exchange rate $a$ and indoor source strength $S$ are derived from $\\hat{F}_{inf}$,",
  "$C_{in}$, $C_{out}$ using Equations~(2)--(4), at the central parameter values",
  paste0("$k = ", fmt1(k_central), "$ h$^{-1}$ and $P = ", fmt1(P_central), "$."),
  "For the weatherization scenario, $a$ is reduced by 20\\% and",
  "$C_{in}$ is recalculated holding $S$, $k$, and $P$ fixed.",
  "For the ventilation scenario, $P$ is halved and $C_{in}$ is recalculated",
  "holding $S$, $k$, and $a$ fixed; this intervention mechanically reduces",
  "$C_{in}$ regardless of the level of indoor sources.",
  "",
  "\\smallskip",
  paste0(
    "\\textsuperscript{(a)}~Robustness range for the weatherization scenario spans ",
    "$k \\in \\{", fmt1(k_min_val), ",\\,", fmt1(k_max_val), "\\}$\\,h$^{-1}$ and ",
    "$P \\in \\{", fmt1(P_min_val), ",\\,", fmt1(P_max_val), "\\}$ where feasible.",
    dagger_note
  ),
  "\\end{minipage}"
)

# =============================================================================
# Write output
# =============================================================================

output_path <- file.path(gdir, "output/tables/si_table_boe.tex")
writeLines(lines, output_path)
message("Written: ", output_path)
