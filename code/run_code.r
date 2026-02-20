pacman::p_load(
  tidyverse, lubridate, sf, patchwork, ggthemes, ggmap, scales,
  fixest, broom, knitr, kableExtra, car, relaimpo, latex2exp, Hmisc)


if(Sys.getenv("USER") == "yixinsun1"){
  gdir <- file.path("/Users/yixinsun1/Documents/Github/jakarta_pm_public")
} else if(Sys.getenv("USER") == "jeannesorin"){
  gdir <- file.path("/Users/jeannesorin/Dropbox/github/jakarta_pm_public")
} else if(Sys.getenv("USER") == "yixin.sun"){
  gdir <- file.path("/Users/yixin.sun/Documents/Educational/jakarta_pm_public")
}

ddir <- file.path(gdir, "data")

# ls() function except don't list ddir and gdir
ls_extra <- function(){
  ls()[!ls() %in% c("ddir", "gdir")]
}

source(file.path(gdir, "code/fig1_desc.R"))
print("Figure 1 finished")

source(file.path(gdir, "code/fig2_infiltration.R"))
print("Figure 2 finished")

source(file.path(gdir, "code/fig3_source_decomposition.R"))
print("Figure 3 finished")

source(file.path(gdir, "code/fig4_income.R"))
print("Figure 4 finished")

source(file.path(gdir, "code/appendix.R"))
print("Appendix finished")