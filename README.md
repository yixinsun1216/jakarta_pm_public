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

