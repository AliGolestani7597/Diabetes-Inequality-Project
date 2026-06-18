# Socioeconomic inequalities in diabetes care: analysis code

This repository contains the R code used to prepare two rounds of WHO STEPS
survey data and analyse socioeconomic inequalities in the diabetes care
cascade. Diabetes mellitus (DM) is the primary article analysis. The closely
related hypertension (HTN) workflow is included as a companion analysis.

The repository contains code only. Individual-level survey data are not
included and must not be committed to GitHub. Access to the source datasets is
subject to the data custodian's conditions.

## Repository contents

- `00_STEPS_helper_functions.R`: general functions developed for STEPS papers.
- `01_SII_RII_functions.R`: modified SII and RII functions used in this study.
- `02_clean_data_DM_HTN.R`: harmonisation and derivation of analysis variables.
- `03_DM_analysis.R`: primary diabetes analysis and result tables.
- `04_HTN_analysis.R`: companion hypertension analysis and result tables.
- `run_analysis.R`: convenience script that runs the files in the correct order.
- `data/synthetic/`: a 10,000-row synthetic dataset for demonstrating the code.
- `generate_synthetic_data.py`: reproducible synthetic-data generator.

The functions and analytical model code are retained from the original research
workflow. Publishing changes are limited to documentation, execution order, and
portable file paths.

## Data setup

Create a local `data/raw` directory and place these source files inside it:

- `steps_data_main_20171217.dta`
- `steps_2020_7.11.2021_01_p3_final_4.1.2.dta`

If the files are stored elsewhere, set `STEPS_DATA_DIR` to that directory. For
example, in R:

```r
Sys.setenv(STEPS_DATA_DIR = "path/to/the/raw/data")
```

The raw and derived data directories are excluded by `.gitignore`.

### Synthetic demonstration data

The repository includes synthetic `.dta` and `.csv` versions of the combined
analysis dataset. They contain no actual participant records and are intended
only for code demonstration, teaching, and workflow checks. Estimates calculated
from synthetic data must not be presented as study findings.

To use the synthetic file without running the confidential-data cleaning step:

```r
library(haven)
steps_1395_1400_dm_htn_inequality <- read_dta(
  file.path("data", "synthetic", "steps_inequality_synthetic.dta")
)
source("00_STEPS_helper_functions.R")
source("01_SII_RII_functions.R")
source("03_DM_analysis.R")
```

## Software requirements

The scripts require R and the following packages:

```r
install.packages(c(
  "car", "dplyr", "emmeans", "haven", "jtools", "nrba", "openxlsx",
  "patchwork", "readxl", "reshape2", "survey", "tableone", "tidyr"
))
```

Some general helper functions additionally load packages on demand through
`pacman`; those packages are only needed if the corresponding helpers are used.

## Running the analysis

From the repository root, run the primary DM workflow:

```sh
Rscript run_analysis.R DM
```

The optional HTN workflow can be run with `HTN`, or both workflows with `both`.
The cleaning script writes harmonised data to `data/derived`. Analysis tables
are written to `results/DM` and `results/HTN`. These generated files are ignored
by Git.

## Reproducibility notes

- Survey years are retained using the values encoded in the original workflow
  (`1395` and `1400`).
- No absolute computer-specific paths, credentials, or source data are included.
- Package versions and R session information were not available in the original
  scripts. For a fixed computational environment, consider adding `renv` before
  archival publication.

## Citation and licence

When the article citation is final, add it here and create a `CITATION.cff` file.
Before making the repository public, select a software licence that is compatible
with your institution's policy and any code on which the modified SII/RII
functions are based.
