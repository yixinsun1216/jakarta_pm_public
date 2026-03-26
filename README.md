# Read Me

This repository contains the public analysis pipeline for a Jakarta household air pollution project. At a high level, the project combines hourly indoor PM2.5 measurements from in-home monitors, outdoor PM2.5 measurements from a distributed ambient sensor network, and household survey data to study three main questions:

1. How high indoor PM2.5 exposures are in the study sample.
2. How much outdoor pollution infiltrates indoors.
3. How indoor sources and household resources shape exposure, including heterogeneity by income.

The repository is organized to mirror that workflow. `code/cleaning/` turns anonymized raw inputs into analysis-ready datasets in `data/`. `code/analysis/` uses those cleaned files to generate the paper figures, appendix figures, and tables in `output/`. `notes/` contains manuscript-related files.

## Project flow

The intended data flow is:

`private original files -> code/cleaning/00_prepare_raw_data.R -> data/raw_data/ -> code/cleaning/*.R -> data/*.RDS -> code/analysis/*.R -> output/`

In other words:

- `00_prepare_raw_data.R` documents how the public-facing raw inputs were created from private source files.
- The remaining cleaning scripts convert those anonymized raw inputs into the cleaned datasets used in the paper.
- The analysis scripts produce the figures and tables used in the manuscript and supplement.

## Why `00_prepare_raw_data.R` exists

`code/cleaning/00_prepare_raw_data.R` is included so that the public repository shows exactly how the raw inputs in `data/raw_data/` were constructed from the original private project archive, even though outside users are not expected to run it.

That script does two important things before data enter the public workflow:

1. It removes or obscures identifying information. The script keeps only analysis-relevant variables, drops direct identifiers, and jitters household coordinates before writing public-facing files. Its purpose is to make the downstream repository reproducible without exposing PII.
   
2. It restricts the sample to the households relevant for this paper. The broader fieldwork generated data for more than one research project. This repository is only for the subset of households used in the Jakarta PM paper. The script therefore filters the original archive down to the relevant households rather than carrying forward the full cross-project sample. That separation is intentional: another project based on a different subset of the same broader data collection effort is still ongoing, so those households are not included here.

Because `00_prepare_raw_data.R` reads from private directories that are not part of this repository, most users should treat `data/raw_data/` as the starting point for replication.

## Directory guide

### `code/`

Contains all scripts used to clean data and produce paper outputs.

#### `code/cleaning/`

Builds the cleaned analysis datasets.

- `00_prepare_raw_data.R`: Creates the anonymized, paper-specific `data/raw_data/` inputs from the original private archive. This is for documentation and provenance, not for routine public replication.
- `01_clean_survey.R`: Merges baseline, endline, installation, and distance data into `data/df_survey.RDS`, and constructs household covariates used in the analysis.
- `02_clean_outdoor.R`: Cleans outdoor sensor data, merges in pre-baseline outdoor measurements, matches households to nearby sensors, and creates respondent-level outdoor pollution series in `data/pm_outdoor.rds`.
- `03_merge_hourly.r`: Merges indoor hourly PM, household-specific outdoor PM, survey covariates, road-distance measures, and cooking indicators into the main panel `data/df_reg.rds`. It also creates jittered household locations for mapping.
- `run_cleaning.R`: Runs the cleaning scripts in sequence.

#### `code/analysis/`

Produces the main figures, appendix figures, and paper tables.

- `fig1_desc.R`: Descriptive figure with study geography, sensor and household locations, and basic time-series patterns in indoor and outdoor PM.
- `fig2_infiltration.R`: Main infiltration figure and related heterogeneity/behavior panels.
- `fig3_source_decomposition.r`: Source decomposition figure and supporting contribution tables.
- `fig4_income.R`: Income heterogeneity analysis and the corresponding figure.
- `appendix.R`: Appendix figures, robustness checks, and supplementary tables.
- `paper_stats.R`: Recomputes manuscript statistics cited in the draft and checks them against the data.

#### `code/run_code.r`

Top-level script that loads packages, defines project paths, runs the cleaning pipeline, and then runs the analysis scripts in order.

### `data/`

Contains the inputs and intermediate datasets used by the public pipeline.

- `raw_data/`: The anonymized raw inputs that the public cleaning scripts start from. These are already filtered to the households relevant for this paper.
- `df_survey.RDS`: Cleaned household-level survey dataset.
- `pm_outdoor.rds`: Household-matched outdoor pollution panel.
- `df_reg.rds`: Main hourly analysis panel used by most regressions and figures.
- `hh_locations_jittered.RDS`: Jittered household geometry used for mapping.
- `income_infiltration.rds`, `plot_decomp_by_income.rds`: Saved intermediate objects used by parts of the analysis workflow.
- `sensor_locations/`: Shapefile version of outdoor sensor locations used in maps.
- `Batas Adm RW 01 Mei 2020/`: Jakarta administrative boundary shapefile used for mapping context.

### `output/`

Stores files created by the analysis scripts.

- `output/figures/`: Final figure files produced by `code/analysis/`.
- `output/tables/`: LaTeX-ready tables and supporting tabular outputs for the manuscript and supplement.

### `notes/`

Manuscript materials and related writing files.

- `resubmission.tex`: Main paper draft.
- `supplementary_info.tex`: Supplement draft.
- `JakartaPM.bib`: Bibliography.
- `paper_stats.md`: Notes tied to manuscript statistics.
- PDF files in this folder are compiled manuscript artifacts.

## Replication notes

For most users, replication should start from the anonymized files already placed in `data/raw_data/`.

Typical order:

1. Ensure the project path `gdir` in [code/run_code.r](/Users/yixin.sun/Documents/Educational/jakarta_pm_public/code/run_code.r) points to your local clone.
2. Make sure the anonymized raw inputs exist under `data/raw_data/`.
3. Run [code/run_code.r](/Users/yixin.sun/Documents/Educational/jakarta_pm_public/code/run_code.r).

That script will:

1. Run the cleaning pipeline in [code/cleaning/run_cleaning.R](/Users/yixin.sun/Documents/Educational/jakarta_pm_public/code/cleaning/run_cleaning.R).
2. Build cleaned datasets in `data/`.
3. Generate figures and tables in `output/`.

## R Session Info

The package versions below were captured by running `sessionInfo()` after loading the packages listed in [code/run_code.r](/Users/yixin.sun/Documents/Educational/jakarta_pm_public/code/run_code.r).

```r
R version 4.5.2 (2025-10-31)
Platform: aarch64-apple-darwin20
Running under: macOS Sequoia 15.6.1

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
LAPACK: /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.1

locale:
[1] C.UTF-8/C.UTF-8/C.UTF-8/C/C.UTF-8/C.UTF-8

time zone: Europe/London
tzcode source: internal

attached base packages:
[1] grid      stats     graphics  grDevices utils     datasets  methods  
[8] base     

other attached packages:
 [1] data.table_1.18.0 Hmisc_5.2-3       latex2exp_0.9.8   relaimpo_2.2-7   
 [5] mitools_2.4       survey_4.4-8      survival_3.8-3    Matrix_1.7-4     
 [9] boot_1.3-32       MASS_7.3-65       car_3.1-3         carData_3.0-5    
[13] kableExtra_1.4.0  knitr_1.50        broom_1.0.8       fixest_0.13.3    
[17] scales_1.4.0      ggmap_4.0.2       ggthemes_5.2.0    patchwork_1.3.1  
[21] sf_1.0-21         lubridate_1.9.4   forcats_1.0.0     stringr_1.5.1    
[25] dplyr_1.1.4       purrr_1.0.4       readr_2.1.5       tidyr_1.3.1      
[29] tibble_3.3.0      ggplot2_3.5.2     tidyverse_2.0.0   readxl_1.4.5     

loaded via a namespace (and not attached):
 [1] tidyselect_1.2.1    viridisLite_0.4.2   farver_2.1.2       
 [4] bitops_1.0-9        fastmap_1.2.0       pacman_0.5.1       
 [7] rpart_4.1.24        digest_0.6.37       timechange_0.3.0   
[10] lifecycle_1.0.4     cluster_2.1.8.1     dreamerr_1.5.0     
[13] magrittr_2.0.3      compiler_4.5.2      rlang_1.1.6        
[16] tools_4.5.2         htmlwidgets_1.6.4   classInt_0.4-11    
[19] plyr_1.8.9          xml2_1.3.8          RColorBrewer_1.1-3 
[22] abind_1.4-8         KernSmooth_2.23-26  foreign_0.8-90     
[25] withr_3.0.2         numDeriv_2016.8-1.1 nnet_7.3-20        
[28] colorspace_2.1-1    e1071_1.7-16        cli_3.6.5          
[31] rmarkdown_2.29      generics_0.1.4      rstudioapi_0.17.1  
[34] httr_1.4.7          tzdb_0.5.0          DBI_1.2.3          
[37] proxy_0.4-27        splines_4.5.2       cellranger_1.1.0   
[40] base64enc_0.1-3     stringmagic_1.2.0   vctrs_0.6.5        
[43] sandwich_3.1-1      hms_1.1.3           htmlTable_2.4.3    
[46] Formula_1.2-5       systemfonts_1.3.1   jpeg_0.1-11        
[49] units_0.8-7         glue_1.8.0          stringi_1.8.7      
[52] gtable_0.3.6        pillar_1.10.2       htmltools_0.5.8.1  
[55] R6_2.6.1            textshaping_1.0.1   evaluate_1.0.4     
[58] lattice_0.22-7      png_0.1-8           backports_1.5.0    
[61] corpcor_1.6.10      class_7.3-23        Rcpp_1.1.1         
[64] checkmate_2.3.2     gridExtra_2.3       svglite_2.2.1      
[67] nlme_3.1-168        xfun_0.52           zoo_1.8-14         
[70] pkgconfig_2.0.3    
```
