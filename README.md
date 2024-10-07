# Relationships Between Health and Logged Video Game Play Across Platforms

🟢️ The output from this repo can be viewed at [https://digital-wellbeing.github.io/platform-study-rr/](https://digital-wellbeing.github.io/platform-study-rr/). 🟢️

This repo documents the data and analysis code for our project on the relationship between video game play and wellbeing. It has four main components:
- generating simulated data to illustrate our preregistered analyses
- documenting the data and creating a codebook
- preprocessing the data for analysis 
- data analysis for three outputs, structured to match our programmatic registered report. 

To reproduce the project in its entirety, run `quarto render` (for non-lab members). More details are provided below. 

Data files are saved as .csv.gz for space efficiency. These can either be unzipped and opened in a spreadsheet program, or read directly into R using `readr::read_csv()` or Python using `pandas.read_csv()`.

## Scripts

The first script generates a series of 8 simulated data tables, overviewed in `codebook.xlsx`. Generating the simulated data is only possible by **internal** users, but the code is available in `0_generateSyntheticData.qmd`. The remaining scripts can be run by **external** users.

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

## Running

- Run `quarto render --profile external` in the Rstudio terminal to render all of the quarto files except 0_generateSyntheticData.qmd, which requires internal credentials. This command uses the specifications in `_quarto.yml` to render the files in the correct order (indicated by their number), and output them to `outputs/`.

- For **internal use**, run `quarto render` in the Rstudio terminal to render *all* quarto files. This command uses the specifications in `_quarto-internal.yml` to render the files in the correct order (indicated by their number), and output them to `docs/`. The files in `docs/` are hosted on GitHub pages here: [https://digital-wellbeing.github.io/platform-study-rr/](https://digital-wellbeing.github.io/platform-study-rr/).

