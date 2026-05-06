# Open Play: A longitudinal dataset of multi-platform video game digital trace data and psychological measures

🟢️ Stable releases of the data and code found in this repository are available on Zenodo ([https://doi.org/10.5281/zenodo.17536656](https://doi.org/10.5281/zenodo.17536656)). 🟢️

🔗 The output from this repo can be viewed at [https://digital-wellbeing.github.io/open-play/](https://digital-wellbeing.github.io/open-play/). 🔗

This repo hosts the data and descriptive analysis code for our project on video game play and wellbeing. 

# Getting the Data

Data files are located in `data/raw` (minimally processed) and `data/clean` (analysis-ready). There are three ways to access them:

1. **Download from Zenodo** (recommended): Download the latest stable archive from [https://doi.org/10.5281/zenodo.17536656](https://doi.org/10.5281/zenodo.17536656). This includes all data files without needing git.

2. **Clone this repository**: `git clone https://github.com/digital-wellbeing/open-play.git`, then find data files under `data/`.

3. **Download a ZIP**: On the GitHub page, click **Code → Download ZIP**, then extract and navigate to `data/`.

4. **Download individual files**: Navigate to a file on GitHub and click **Download raw file**, or use the command line with `wget https://github.com/digital-wellbeing/open-play/raw/main/data/clean/survey_daily.csv.gz` (replacing the filename as needed).

All data files are `.csv.gz` (gzip-compressed CSVs). They can be unzipped and opened in a spreadsheet program, or read directly into R with `readr::read_csv("file.csv.gz")` or Python with `pandas.read_csv("file.csv.gz")`.

# Getting Started

The best starting point for understanding the data presented here is to look at [codebook.xlsx](https://github.com/digital-wellbeing/open-play/raw/refs/heads/main/codebook.xlsx), which describes the variables in both the cleaned and raw dataset.

Other key elements of the repo are:

- `codebook.xlsx`, which describes the variables in both the cleaned and raw dataset
- `data-process.qmd`, which processes the raw data into cleaned data
- `index.qmd`, which generates manuscript documenting the dataset
- `data/raw`, which contains the minimally processed, pseudonymized raw data
- `data/clean`, which contains cleaned data ready for analysis

# Reproducing

To reproduce the primary data manuscript, run `quarto render`. To reproduce the data manuscript and the simulations, run `quarto render --profile sim`.

# Contributing

Contributors should ensure they are up-to-date on the `main` branch, and that they have installed all the necessary packages from `renv` by running `renv::restore()` in R. 

Contributors should then:

- create a new branch for their work
- make any desired changes (for readability, we recommend making frequent small commits focused on changing an individual feature, but these can be pushed less frequently)
- run `quarto render` locally to ensure their changes do not break the build
- submit a pull request when they are ready to merge their changes back into `main`. 

Rendering locally will update the _freeze folder, which in turn will update the GitHub Pages site when the PR is merged; make sure PRs commit changes to _freeze or they will fail the build check.

## R code

In `R/`, we have a number of helper functions used in the analysis scripts, as well as unit_tests (currently implemented for the key function used to clean Xbox data, `canonicalize_xbox_sessions()`. Supplementary features can be added by adding new scripts to this folder.

## Simulation 

For the Stage 1 Registered Report associated with this project, we did extensive simulations to validate our data collection and analyses. These remain present in the current repo, and can be run by using the `--profile sim` flag when rendering the quarto documents. This will then generate a book instead of a website. 

Data for the simulations is found in `data/synthetic`. 

The simulation pipeline consists of:

- [not run] `0_generateSyntheticData.qmd`, documents the internal process we used to generate synthetic data
- `1_preprocess.qmd` cleans the data and calculates relevant derived variables (e.g., mean scores, play behavior metrics, and so on).
- `2_basicNeeds.qmd` presents the analysis code for Study 1: the relationship between basic needs and video game play.
- `3_sleep.qmd` presents the analysis code for Study 2: the relationship between sleep and video game play.
- `4_genres.qmd` presents the analysis code for Study 3: the relationship between video game genres and video game play.

