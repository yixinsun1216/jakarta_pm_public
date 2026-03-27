pacman::p_load(readxl,
  tidyverse, lubridate, sf, patchwork, ggthemes, ggmap, scales,
  fixest, broom, knitr, kableExtra, car, relaimpo, latex2exp, Hmisc, data.table)


if(!exists("gdir")){
  if(Sys.getenv("USER") == "jeannesorin"){
    gdir <- file.path("~/Library/CloudStorage/Dropbox/github/jakarta_pm_public")
  } else if(Sys.getenv("USER") == "yixin.sun"){
    gdir <- file.path("/Users/yixin.sun/Documents/Educational/jakarta_pm_public")
  }
}
ddir <- file.path(gdir, "data")
raw_dir <- file.path(gdir, "data/raw_data")

ls_extra <- function(){
  keep <- c(
    "ddir", "gdir", "ls_extra", "rhs_fml",
    "read_pm_data", "read_survey_data", "read_outdoor_data", "read_hh_locations"
  )
  ls(envir = parent.frame())[!ls(envir = parent.frame()) %in% keep]
}
rhs_fml <- ~ as.factor(trash_burning_1week_baseline) + as.factor(smoke24_endline) + as.factor(room_pmsource_kitchen) + pm25_outdoor3 + cooking +  dist_primary + temp_outdoor3 + humidity_outdoor3

read_survey_data <- function() {
  fread(file.path(ddir, "df_survey.csv")) %>%
    mutate(
      respondent_id = as.integer(respondent_id),
      starttime_baseline = as.POSIXct(starttime_baseline, tz = "UTC"),
      starttime = as.POSIXct(starttime, tz = "UTC")
    )
}

read_pm_data <- function() {
  fread(file.path(ddir, "df_reg.csv")) %>%
    mutate(
      respondent_id = as.integer(respondent_id),
      date_hour = as.POSIXct(date_hour, tz = "UTC"),
      date = as.Date(date),
      week = as.Date(week)
    )
}

read_outdoor_data <- function() {
  fread(file.path(ddir, "pm_outdoor.csv")) %>%
    mutate(
      respondent_id = as.integer(respondent_id),
      date_hour = as.POSIXct(date_hour, tz = "UTC"),
      date = as.Date(date)
    )
}

read_hh_locations <- function() {
  st_read(
    file.path(ddir, "hh_locations_jittered", "hh_locations_jittered.shp"),
    quiet = TRUE
  ) %>%
    rename(respondent_id = resp_id)
}


# run cleaning pipeline
source(file.path(gdir, "code/cleaning/run_cleaning.r"))
print("Cleaning finished")

source(file.path(gdir, "code/analysis/fig1_desc.r"))
print("Figure 1 finished")

source(file.path(gdir, "code/analysis/fig2_infiltration.r"))
print("Figure 2 finished")

source(file.path(gdir, "code/analysis/fig3_source_decomposition.r"))
print("Figure 3 finished")

source(file.path(gdir, "code/analysis/fig4_income.r"))
print("Figure 4 finished")

source(file.path(gdir, "code/analysis/appendix.r"))
print("Appendix finished")
