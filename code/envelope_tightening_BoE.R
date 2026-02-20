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
top <- c("\\centering", 
         "\\newcolumntype{c}{>{\\centering\\arraybackslash}X}",
         "\\footnotesize",
         "\\begin{tabularx}{\\linewidth}{lcc}",
         "\\toprule",
         "\\tabularnewline",
         "Variable & \\multicolumn{2}{Bottom Income Quartile} & \\multicolumn{2}{Top Income Quartile} \\\\",
         " & Smokers & Non-Smokers & Smokers & Non-Smokers \\tabularnewline",
         "\\midrule \\addlinespace[\\belowrulesep]")


table_tex <- top
table_tex <- append(table_tex, "\\textit{\\textbf{Panel A: Estimated Elasticities and Quantities from Our Data}} & \\\\")
table_tex <- append(table_tex,  paste(" Average  C$_{in}$ & ", paste(round(data_summ$Cin), collapse=" & "), "\\\\", sep=""))
table_tex <- append(table_tex,  paste(" Average C$_{out}$ & ", paste(round(data_summ$Cout), collapse=" & "), "\\\\", sep=""))
table_tex <- append(table_tex,  paste(" Estimated F$_{inf}$ & ", paste(round(data_summ$Finf, 3), collapse=" & "), "\\\\", sep=""))
table_tex <- append(table_tex,  "\\hline")
table_tex <- append(table_tex, "\\textit{\\textbf{Panel B: External Parameters}} & \\\\")
table_tex <- append(table_tex,  paste(" k ", paste(round(data_summ$k, 2), collapse=" & "), "\\\\", sep=""))
table_tex <- append(table_tex,  paste(" P (baseline) ", paste(round(data_summ$P, 2), collapse=" & "), "\\\\", sep=""))
table_tex <- append(table_tex,  paste(" P (ventilation) ", paste(round(data_summ$P_vent, 2), collapse=" & "), "\\\\", sep=""))
table_tex <- append(table_tex,  paste(" a (baseline) ", paste(round(data_summ$a, 2), collapse=" & "), "\\\\", sep=""))
table_tex <- append(table_tex,  paste(" a (weatherization) ", paste(round(data_summ$a_weather20, 2), collapse=" & "), "\\\\", sep=""))
table_tex <- append(table_tex,  "\\hline")
table_tex <- append(table_tex, "\\textit{\\textbf{Panel C: Results}} & \\\\")
table_tex <- append(table_tex,  paste(" C$_{in}$ (weatherization) ", paste(round(data_summ$Cin_weather20), collapse=" & "), "\\\\", sep=""))
table_tex <- append(table_tex,  paste(" C$_{in}$ (ventilation) ", paste(round(data_summ$Cin_vent), collapse=" & "), "\\\\", sep=""))

bottom <- c("\\bottomrule \\end{tabularx}")



table_tex <- append(table_tex, bottom)
write(table_tex,file.path(gdir, "output/tables/BoE_envelopetightening.tex"))
