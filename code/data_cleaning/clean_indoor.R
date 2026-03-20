# clean indoor pm data for those in the fan control group
# resulting data is at 10 minute intervals

pacman::p_load(tidyverse, lubridate, data.table, writexl, googlesheets4)

rm(list=ls())
if(Sys.getenv("USER") == "yixinsun1"){
  ddir <- file.path("/Users/yixinsun1/Dropbox/Research/pollution_experience/fullscale")
  gdir <- file.path("/Users/yixinsun1/Documents/Github/jakarta_pm")
} else if(Sys.getenv("USER") == "jeannesorin"){
  gdir <- file.path("/Users/jeannesorin/github/jakarta_pm")
  ddir <- file.path("/Users/jeannesorin/Dropbox/pollution_experience/fullscale")
}


# id mapping device id to respondent
# we only want data for fan group
ids <- 
  readRDS(file.path(ddir, "generated_data/pm/match_devices_respondents.RDS")) %>%
  mutate(respondent_id = as.numeric(respondent_id)) %>%
  bind_rows(read_sheet("https://docs.google.com/spreadsheets/d/1Bc_rKKUFktsr9PIzutXk_6RJEvNjK99tv669cn8UaQY/edit?gid=988032625#gid=988032625", sheet = "Device Names - Server")) %>%
  filter(treatment_status %in% c("Fan", "Control")) %>%
  dplyr::select(id, respondent_id, hardware_cancel)

# there's a LOT of data, let's read data in one by one and make sure there
# are no repeats
indoor_files <- list.files(file.path(ddir, "data/indoor"), pattern = "indoor|error", full.names = TRUE) 

# first file is a bonus file of device id's that wasn't properly in the server
indoor <- 
  read_csv( file.path(ddir, "data/indoor/errors.csv")) %>%
  inner_join(ids) %>%
  mutate(time = time_raw, 
         minute = minute(time), 
         hour = hour(time),
         date = date(time),
         minute = floor((minute - 5) / 10) * 10 + 5,
         date = if_else(minute < 0 & hour == 0, date - 1, date),
         hour = if_else(minute < 0, 23, hour),
         minute = if_else(minute < 0, 55, minute), 
         time = ymd_hm(paste(date, hour, minute))) %>%
  group_by(time, respondent_id, hardware_cancel, date, hour, id) %>%
  summarise(value = mean(value, na.rm = TRUE))  %>%
  as.data.table()

for (i in 2:length(indoor_files)) {
  print(basename(indoor_files[i]))
  
  # Read the CSV file and perform an inner join
  df <- fread(indoor_files[i])
  df <- merge(df, ids, by = "id", all = FALSE)
  
  # first couple of files is all airtest, rather than from the fan control group
  if (nrow(df) > 0) {
    # Convert time_raw to POSIXct
    df[, time := mdy_hm(time_raw)]
    
    # sometimes the dates are in dmy format instead of mdy
    alt_date <- c("20240721", "20240722", "20240728","20240711-2", "20240711", 
                  "20240710", "20240803", "20240828", "20240901", "20240904", "20240908")

    if (str_replace_all(basename(indoor_files[i]), "indoor_|.csv", "") %in% alt_date) {
      df[, time := dmy_hm(time_raw)]
    }
    
    print(unique(date(df$time)))
    # print(count(df, minute(time)))
    
    # Filter, arrange, and collapse data down to 10-minute intervals
    df <- df[measure %in% c("Avg. Pm25")]
    setorder(df, time)
    
    df[, `:=`(minute = minute(time),
              hour = hour(time),
              date = date(time))]
    df[, minute := floor((minute - 5) / 10) * 10 + 5]
    df[minute < 0 & hour == 0, date := date - 1]
    df[minute < 0, `:=`(hour = 23, minute = 55)]
    df[, time := ymd_hm(paste(date, hour, minute))]
    
    df <- df[, .(value = mean(value, na.rm = TRUE)), 
             by = .(time, respondent_id, hardware_cancel, date, hour, id)]
    
    df <- df[!indoor, on = .(time, respondent_id)]
    
    
    # Bind rows to the main dataset
    indoor <- rbind(indoor, df, fill = TRUE)
    indoor <- distinct(indoor, time, respondent_id, .keep_all = TRUE)
  }

  gc()
}

indoor <- indoor[!is.na(respondent_id),]

tibble(time = seq(min(indoor$time), max(indoor$time), by = "10 min")) %>%
  mutate(date = date(time), 
         hour = hour(time)) %>% 
  dplyr::select(-time) %>% 
  anti_join(indoor) %>%
  count(date)

write_rds(indoor, file.path(ddir, "generated_data/pm/df_indoor_10min.rds"))
