# Relationships Between Health and Logged Video Game Play Across Platforms

🟢️ The output from this repo can be viewed at [https://digital-wellbeing.github.io/platform-study-rr/](https://digital-wellbeing.github.io/platform-study-rr/). 🟢️

This repo hosts the data for our project on video game play and wellbeing. Its key elements are:
- `index.qmd`, which cleans the data generates a manuscript documenting the dataset
- `data/raw-minimal-pseudo`, which contains the minimally processed, pseudonymized raw data
- `data/clean`, which contains cleaned data ready for analysis

In addition, the repo contains simulated data and code, used in our Stage 1 Registered Report and associated power analysis. The simulation code:
- generates simulated data to illustrate our preregistered analyses
- preprocesses the data for analysis 
- analyses that data for three outputs, structured to match our programmatic registered report. 

# Reproducing

To reproduce the primary data manuscript, run `quarto render`. To reproduce the data manuscript and the simulations, run `quarto render --profile sim`. 

Data files are saved as .csv.gz for size. These can either be unzipped and opened in a spreadsheet program, or read directly into R using `readr::read_csv()` or Python using `pandas.read_csv()`.

## Simulation 

### Scripts

The first simulation script generates a series of 8 simulated data tables, overviewed in `codebook.xlsx`. Generating the simulated data is only possible by **internal** users, but the code is available in `0_generateSyntheticData.qmd`. The remaining scripts can be run by **external** users.

These data tables are generated in the following scripts:

- In `0_generateSyntheticData.qmd`, we simulate a total of 8 data tables that will mimic the structure of the eventual
- In `1_preprocess.qmd`, we clean the data and calculate relevant derived variables (e.g., mean scores, play behavior metrics, and so on).

We then analyze these data in the following scripts:

- In `2_basicNeeds.qmd`, we present the analysis code for Study 1: the relationship between basic needs and video game play.
- In `3_sleep.qmd`, we present the analysis code for Study 2: the relationship between sleep and video game play.
- In `4_genres.qmd`, we present the analysis code for Study 3: the relationship between video game genres and video game play.
- In `9_screenshots.qmd`, we present work-in-progress optical character recognition code for extracting screen use data from iOS screenshots. 

## Hygiene files

- `.Renviron` defines the path to key internal data files and API credentials.
- `index.qmd` is the header file that stitches the other Quarto files together into book form. 
- `_quarto.yml` defines the order in which files are run and project-level variables for **internal** use 
- `_quarto-external.yml` defines the order in which files are run and project-level variables for **external** use (same as `_quarto-internal.yml` with the exception of not running `0_generateSyntheticData.qmd`)
