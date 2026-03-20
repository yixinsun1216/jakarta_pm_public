# read in US embassy data to use as robustness check

pacman::p_load(tidyverse, lubridate, sf, ggplot2, mapview, airnow)

rm(list=ls())
if(Sys.getenv("USER") == "yixinsun1"){
  ddir <- file.path("/Users/yixinsun1/Dropbox/Research/pollution_experience/fullscale")
  gdir <- file.path("/Users/yixinsun1/Documents/Github/jakarta_pm")
} else if(Sys.getenv("USER") == "jeannesorin"){
  gdir <- file.path("/Users/jeannesorin/github/jakarta_pm")
  ddir <- file.path("/Users/jeannesorin/Dropbox/pollution_experience/fullscale")
}

# read in raw datasets from Central and South
south_ytd <- read_csv("https://dosairnowdata.org/dos/historical/JakartaSouth/2024/JakartaSouth_PM2.5_2024_YTD.csv")
south_mtd <- read_csv("https://dosairnowdata.org/dos/historical/JakartaSouth/2024/JakartaSouth_PM2.5_2024_08_MTD.csv")

central_ytd <- read_csv("https://dosairnowdata.org/dos/historical/JakartaCentral/2024/JakartaCentral_PM2.5_2024_YTD.csv")
central_mtd <- read_csv("https://dosairnowdata.org/dos/historical/JakartaCentral/2024/JakartaCentral_PM2.5_2024_08_MTD.csv")

set_airnow_token("E23EA323-5E56-4C59-86F8-202568A4CFE1")

coords_south <- c(-6.236704, 106.793244)
coords_south <- c(-6.182536, 106.834236)

pm_embassy_raw <- 
  list(south_ytd, south_mtd, central_ytd, central_mtd) %>%
  reduce(bind_rows) 

stopifnot(count(pm_embassy_raw, Parameter) %>% nrow() == 1)
stopifnot(filter(pm_embassy_raw, Duration != "1 Hr") %>% nrow() == 0)

pm_embassy <- 
  pm_embassy_raw %>%
  mutate(date_hour = ymd_hm(`Date (LT)`)) %>%
  dplyr::select(Site, date_hour, pm25_embassy = `Raw Conc.`) %>%
  mutate(pm25_embassy = if_else(pm25_embassy <= 0 | pm25_embassy == 985, NA_real_, pm25_embassy))

stopifnot(filter(pm_embassy, is.na(date_hour)) %>% nrow() == 0)

write_rds(pm_embassy, file.path(ddir, "generated_data/pm/pm_embassy.rds"))
