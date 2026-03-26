# =========================================================================
# run_cleaning.R
# Orchestrates all cleaning scripts in order
# Input: data/raw_data/
# Output: data/ (cleaned datasets ready for analysis)
# =========================================================================
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
