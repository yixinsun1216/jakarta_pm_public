# =========================================================================
# run_cleaning.R
# Orchestrates all cleaning scripts in order
# Input: data/raw_data/
# Output: data/ (cleaned datasets ready for analysis)
# =========================================================================

if(!exists("gdir")){
  if(Sys.getenv("USER") == "yixin.sun"){
    gdir <- file.path("/Users/yixin.sun/Documents/Educational/jakarta_pm_public")
  } else if(Sys.getenv("USER") == "yixinsun1"){
    gdir <- file.path("/Users/yixinsun1/Documents/Github/jakarta_pm_public")
  } else if(Sys.getenv("USER") == "jeannesorin"){
    gdir <- file.path("~/Library/CloudStorage/Dropbox/github/jakarta_pm_public")
  }
}
ddir <- file.path(gdir, "data")
raw_dir <- file.path(gdir, "data/raw_data")

cat("=== Running cleaning pipeline ===\n")
cat("Project directory:", gdir, "\n\n")

source(file.path(gdir, "code/cleaning/01_clean_survey.r"))
cat("\n")

source(file.path(gdir, "code/cleaning/02_clean_outdoor.r"))
cat("\n")

source(file.path(gdir, "code/cleaning/03_merge_hourly.r"))
cat("\n")

cat("=== Cleaning pipeline complete ===\n")
