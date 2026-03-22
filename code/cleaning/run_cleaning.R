# =========================================================================
# run_cleaning.R
# Orchestrates all cleaning scripts in order
# Input: data/raw_data/
# Output: data/ (cleaned datasets ready for analysis)
# =========================================================================

rm(list = ls())

if(Sys.getenv("USER") == "yixin.sun"){
  gdir <- file.path("/Users/yixin.sun/Documents/Educational/jakarta_pm_public")
} else if(Sys.getenv("USER") == "yixinsun1"){
  gdir <- file.path("/Users/yixinsun1/Documents/Github/jakarta_pm_public")
} else if(Sys.getenv("USER") == "jeannesorin"){
  gdir <- file.path("~/Library/CloudStorage/Dropbox/github/jakarta_pm_public")
}

cat("=== Running cleaning pipeline ===\n")
cat("Project directory:", gdir, "\n\n")

source(file.path(gdir, "code/cleaning/01_clean_survey.R"))
cat("\n")

source(file.path(gdir, "code/cleaning/02_clean_outdoor.R"))
cat("\n")

source(file.path(gdir, "code/cleaning/03_merge_hourly.R"))
cat("\n")

cat("=== Cleaning pipeline complete ===\n")
