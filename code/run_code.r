pacman::p_load(readxl,
  tidyverse, lubridate, sf, patchwork, ggthemes, ggmap, scales,
  fixest, broom, knitr, kableExtra, car, relaimpo, latex2exp, Hmisc, data.table)


if(!exists("gdir")){
  if(Sys.getenv("USER") == "yixinsun1"){
    gdir <- file.path("/Users/yixinsun1/Documents/Github/jakarta_pm_public")
  } else if(Sys.getenv("USER") == "jeannesorin"){
    gdir <- file.path("~/Library/CloudStorage/Dropbox/github/jakarta_pm_public")
  } else if(Sys.getenv("USER") == "yixin.sun"){
    gdir <- file.path("/Users/yixin.sun/Documents/Educational/jakarta_pm_public")
  }
}
ddir <- file.path(gdir, "data")
raw_dir <- file.path(gdir, "data/raw_data")

ls_extra <- function(){
  ls(envir = parent.frame())[!ls(envir = parent.frame()) %in% c("ddir", "gdir", "ls_extra", "rhs_fml")]
}
rhs_fml <- ~ as.factor(trash_burning_1week_baseline) + as.factor(smoke24_endline) + as.factor(room_pmsource_kitchen) + pm25_outdoor3 + cooking +  dist_primary + temp_outdoor3 + humidity_outdoor3


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

