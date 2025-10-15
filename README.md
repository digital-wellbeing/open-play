# Relationships Between Health and Logged Video Game Play Across Platforms

🟢️ The output from this repo can be viewed at [https://digital-wellbeing.github.io/platform-study-rr/](https://digital-wellbeing.github.io/platform-study-rr/). 🟢️

This repo hosts the data for our project on video game play and wellbeing. Its key elements are:
- `data-process.qmd`, which processes the raw data into cleaned data
- `index.qmd`, which generates manuscript documenting the dataset
- `data/raw`, which contains the minimally processed, pseudonymized raw data
- `data/clean`, which contains cleaned data ready for analysis

# Reproducing

To reproduce the primary data manuscript, run `quarto render`. To reproduce the data manuscript and the simulations, run `quarto render --profile sim`. 

Data files are saved as .csv.gz for size. These can either be unzipped and opened in a spreadsheet program, or read directly into R using `readr::read_csv()` or Python using `pandas.read_csv()`.

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

🔴️ The simulatio analysis pipeline is not currently integrated into the rendering pipeline - Nick will fix. 🔴

- In `1_preprocess.qmd`, we clean the data and calculate relevant derived variables (e.g., mean scores, play behavior metrics, and so on).

We then analyze these data in the following scripts:

- In `2_basicNeeds.qmd`, we present the analysis code for Study 1: the relationship between basic needs and video game play.
- In `3_sleep.qmd`, we present the analysis code for Study 2: the relationship between sleep and video game play.
- In `4_genres.qmd`, we present the analysis code for Study 3: the relationship between video game genres and video game play.

