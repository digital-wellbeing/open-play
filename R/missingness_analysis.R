# =============================================================================
# Data Missingness Analysis
# =============================================================================
# This script generates a hierarchical table describing missingness patterns
# across survey, telemetry, and cognitive task datasets, and exports to
# multiple formats (HTML, LaTeX, PDF, Markdown, DOCX)
# =============================================================================

# Load libraries
library(tidyverse)
library(tinytable)

# Load Data ===================================================================

intake <- read_csv("data/clean/intake.csv.gz", show_col_types = FALSE)
diary <- read_csv("data/clean/diary.csv.gz", show_col_types = FALSE)
panel <- read_csv("data/clean/panel.csv.gz", show_col_types = FALSE)
nintendo <- read_csv("data/clean/nintendo.csv.gz", show_col_types = FALSE)
steam <- read_csv("data/clean/steam.csv.gz", show_col_types = FALSE)
xbox <- read_csv("data/clean/xbox.csv.gz", show_col_types = FALSE)
simon <- read_csv("data/clean/simon.csv.gz", show_col_types = FALSE)

# Calculate Missingness Statistics ============================================

# Helper function to calculate study days from enrollment date
get_study_days <- function(dates, pid, intake_data) {
  enrollment_date <- intake_data %>%
    filter(pid == !!pid) %>%
    pull(enrollment_date) %>%
    first()

  if (is.na(enrollment_date)) {
    return(rep(NA_real_, length(dates)))
  }

  enrollment_datetime <- as.POSIXct(enrollment_date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  date_datetimes <- as.POSIXct(dates, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  study_days <- as.numeric(difftime(date_datetimes, enrollment_datetime, units = "days"))
  return(ceiling(study_days) + 1)  # Day 1 is enrollment day
}

# Identify participants based on actual survey participation
# UK: unique PIDs in panel
uk_pids <- panel %>%
  filter(pid %in% (intake %>% filter(country == "UK") %>% pull(pid))) %>%
  pull(pid) %>%
  unique()

# US: unique PIDs in either diary OR panel
us_pids_panel <- panel %>%
  filter(pid %in% (intake %>% filter(country == "US") %>% pull(pid))) %>%
  pull(pid) %>%
  unique()

us_pids_diary <- diary %>%
  pull(pid) %>%
  unique()

us_pids <- union(us_pids_panel, us_pids_diary)

# SURVEY DATA -----------------------------------------------------------------

# Helper function to calculate survey missingness
calc_survey_missingness <- function(data, pids, region, measure_name, expected_n) {
  by_participant <- data %>%
    filter(pid %in% pids) %>%
    count(pid, name = "observed") %>%
    complete(pid = pids, fill = list(observed = 0)) %>%
    mutate(
      expected = expected_n,
      missing = expected - observed
    )

  actual_observed <- data %>%
    filter(pid %in% pids) %>%
    nrow()

  # Calculate retention over time based on measure type
  if (measure_name == "Daily Diary") {
    # For daily diary, use study days 1-30
    max_days <- 30
  } else {
    # For panel surveys, use study days 1-84
    max_days <- 84
  }

  # Get enrollment dates for all participants
  enrollment_lookup <- intake %>%
    filter(pid %in% pids) %>%
    select(pid, enrollment_date) %>%
    distinct()

  # Calculate last response day for each participant
  last_response_by_participant <- data %>%
    filter(pid %in% pids) %>%
    left_join(enrollment_lookup, by = "pid") %>%
    mutate(
      enrollment_datetime = as.POSIXct(enrollment_date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      response_datetime = as.POSIXct(date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      study_day = ceiling(as.numeric(difftime(response_datetime, enrollment_datetime, units = "days"))) + 1
    ) %>%
    filter(!is.na(study_day) & study_day >= 1 & study_day <= max_days) %>%
    group_by(pid) %>%
    summarise(last_day = max(study_day, na.rm = TRUE), .groups = "drop")

  # Calculate cumulative retention: for each day, count participants still active
  retention_data <- sapply(1:max_days, function(day) {
    # Participants are "retained" at day X if their last response is >= day X
    n_retained <- sum(last_response_by_participant$last_day >= day)
    n_retained / length(pids)
  })

  tibble(
    region = region,
    data_type = "Survey",
    measure = measure_name,
    n_participants = length(pids),
    total_expected = length(pids) * expected_n,
    total_observed = actual_observed,
    total_missing = length(pids) * expected_n - actual_observed,
    median_missing_per_participant = median(by_participant$missing, na.rm = TRUE),
    obs_per_participant = list(by_participant$observed),
    retention_over_time = list(retention_data),
    complete_missingness = FALSE  # Surveys don't have complete missingness
  )
}

# DIARY - US only (30 expected)
diary_stats_us <- calc_survey_missingness(diary, us_pids, "US", "Daily Diary", 30)

# PANEL - US and UK separately (6 expected each)
panel_stats_us <- calc_survey_missingness(panel, us_pids, "US", "Biweekly Panel", 6)
panel_stats_uk <- calc_survey_missingness(panel, uk_pids, "UK", "Biweekly Panel", 6)

# TELEMETRY DATA --------------------------------------------------------------

# Helper function to check if participant plays a platform
get_platform_players <- function(platform_name, pids = NULL) {
  result <- intake %>%
    filter(
      str_detect(linked_platforms %||% "", platform_name) |
      case_when(
        platform_name == "Xbox" ~ coalesce(prop_xbox, 0) > 0,
        platform_name == "Nintendo" ~ coalesce(prop_nintendo, 0) > 0,
        platform_name == "Steam" ~ coalesce(prop_steam, 0) > 0,
        TRUE ~ FALSE
      )
    ) %>%
    pull(pid) %>%
    unique()
  
  if (!is.null(pids)) {
    result <- intersect(result, pids)
  }
  
  return(result)
}

# Helper function to calculate telemetry missingness
calc_telemetry_missingness <- function(telemetry_data, platform_name, pids, region) {
  telemetry_pids <- unique(telemetry_data$pid)
  players <- get_platform_players(platform_name, pids)
  no_telemetry <- setdiff(players, telemetry_pids)

  # For telemetry, count sessions per participant
  by_participant <- telemetry_data %>%
    filter(pid %in% players) %>%
    count(pid, name = "observed") %>%
    complete(pid = players, fill = list(observed = 0))

  # Filter outliers for density plot using IQR method
  # Only apply if there are observations > 0
  obs_values <- by_participant$observed

  # Check if we have complete missingness (no telemetry data at all)
  has_complete_missingness <- length(intersect(players, telemetry_pids)) == 0

  if (has_complete_missingness) {
    # Complete missingness - return two zeros (minimum for density plot)
    obs_for_plot <- c(0, 0)
  } else if (any(obs_values > 0)) {
    q1 <- quantile(obs_values, 0.25, na.rm = TRUE)
    q3 <- quantile(obs_values, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower_bound <- max(0, q1 - 1.5 * iqr)  # Keep at least 0
    upper_bound <- q3 + 1.5 * iqr
    obs_for_plot <- obs_values[obs_values >= lower_bound & obs_values <= upper_bound]
    # Ensure we have at least 2 points for density plot
    if (length(obs_for_plot) < 2) {
      obs_for_plot <- obs_values
    }
  } else {
    obs_for_plot <- obs_values
  }

  # Final safety check: ensure at least 2 points for density plot
  if (length(obs_for_plot) < 2) {
    # Duplicate the single value or add a zero
    if (length(obs_for_plot) == 1) {
      obs_for_plot <- c(obs_for_plot, obs_for_plot)
    } else {
      obs_for_plot <- c(0, 0)
    }
  }

  # Calculate retention over time (84 days)
  # For each day, calculate proportion of participants still active
  if (has_complete_missingness || nrow(telemetry_data %>% filter(pid %in% players)) == 0) {
    # No data - return all zeros
    retention_data <- rep(0, 84)
  } else {
    # Get enrollment dates for all players at once
    enrollment_lookup <- intake %>%
      filter(pid %in% players) %>%
      select(pid, enrollment_date) %>%
      distinct()

    # Calculate last activity day for each participant
    last_activity_by_participant <- telemetry_data %>%
      filter(pid %in% players) %>%
      left_join(enrollment_lookup, by = "pid") %>%
      mutate(
        enrollment_datetime = as.POSIXct(enrollment_date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        session_datetime = as.POSIXct(session_start, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        study_day = ceiling(as.numeric(difftime(session_datetime, enrollment_datetime, units = "days"))) + 1
      ) %>%
      filter(!is.na(study_day) & study_day >= 1 & study_day <= 84) %>%
      group_by(pid) %>%
      summarise(last_day = max(study_day, na.rm = TRUE), .groups = "drop")

    # Calculate cumulative retention: for each day, count participants still active
    retention_data <- sapply(1:84, function(day) {
      # Participants are "retained" at day X if their last activity is >= day X
      n_retained <- sum(last_activity_by_participant$last_day >= day)
      n_retained / length(players)
    })
  }

  tibble(
    region = region,
    data_type = "Telemetry",
    measure = platform_name,
    n_participants = length(players),
    total_expected = length(players),
    total_observed = length(intersect(players, telemetry_pids)),
    total_missing = length(no_telemetry),
    median_missing_per_participant = NA_real_,
    obs_per_participant = list(obs_for_plot),  # Use filtered data for plot
    retention_over_time = list(retention_data),
    complete_missingness = has_complete_missingness  # Flag for empty distribution
  )
}

# Xbox - US and UK
xbox_stats_us <- calc_telemetry_missingness(xbox, "Xbox", us_pids, "US")
xbox_stats_uk <- calc_telemetry_missingness(xbox, "Xbox", uk_pids, "UK")

# Nintendo - US and UK
nintendo_stats_us <- calc_telemetry_missingness(nintendo, "Nintendo", us_pids, "US")
nintendo_stats_uk <- calc_telemetry_missingness(nintendo, "Nintendo", uk_pids, "UK")

# Steam - US and UK
steam_stats_us <- calc_telemetry_missingness(steam, "Steam", us_pids, "US")
steam_stats_uk <- calc_telemetry_missingness(steam, "Steam", uk_pids, "UK")

# COGNITIVE TASK DATA ---------------------------------------------------------

# Helper function to calculate cognitive task missingness
calc_task_missingness <- function(data, pids, region, measure_name, expected_n) {
  by_participant <- data %>%
    filter(pid %in% pids) %>%
    count(pid, name = "observed") %>%
    complete(pid = pids, fill = list(observed = 0)) %>%
    mutate(
      expected = expected_n,
      missing = expected - observed
    )

  actual_observed <- data %>%
    filter(pid %in% pids) %>%
    nrow()

  # Calculate retention over time at expected waves (1, 3, 5)
  # For each wave, calculate proportion of participants still active
  expected_waves <- c(1, 3, 5)

  # Find last wave completed by each participant
  last_wave_by_participant <- data %>%
    filter(pid %in% pids) %>%
    group_by(pid) %>%
    summarise(last_wave = max(wave, na.rm = TRUE), .groups = "drop")

  # Calculate cumulative retention: for each wave, count participants still active
  retention_data <- sapply(expected_waves, function(w) {
    # Participants are "retained" at wave X if their last response is >= wave X
    n_retained <- sum(last_wave_by_participant$last_wave >= w)
    n_retained / length(pids)
  })

  tibble(
    region = region,
    data_type = "Task",
    measure = measure_name,
    n_participants = length(pids),
    total_expected = length(pids) * expected_n,
    total_observed = actual_observed,
    median_missing_per_participant = median(by_participant$missing, na.rm = TRUE),
    total_missing = length(pids) * expected_n - actual_observed,
    obs_per_participant = list(by_participant$observed),
    retention_over_time = list(retention_data),
    complete_missingness = FALSE  # Tasks don't have complete missingness
  )
}

# SIMON TASK - US and UK separately (3 expected per participant at waves 1, 3, 5)
simon_stats_us <- calc_task_missingness(simon, us_pids, "US", "Simon Task", 3)
simon_stats_uk <- calc_task_missingness(simon, uk_pids, "UK", "Simon Task", 3)

# Combine and Format Table ====================================================

# Combine all statistics (ordered by region: US first, then UK)
missingness_table <- bind_rows(
  # US statistics
  diary_stats_us,
  panel_stats_us,
  xbox_stats_us,
  nintendo_stats_us,
  steam_stats_us,
  simon_stats_us,
  # UK statistics
  panel_stats_uk,
  xbox_stats_uk,
  nintendo_stats_uk,
  steam_stats_uk,
  simon_stats_uk
)

# Extract density plot data and retention data before formatting
density_plot_data <- missingness_table$obs_per_participant

# Convert retention data to data frames with x and y columns for plot_tt
retention_plot_data <- lapply(1:nrow(missingness_table), function(i) {
  ret_vec <- missingness_table$retention_over_time[[i]]

  # Determine x values based on measure type
  if (missingness_table$measure[i] == "Daily Diary") {
    # Daily diary: days 1-30
    x_vals <- 1:30
  } else if (missingness_table$data_type[i] == "Task") {
    # Tasks: waves 1, 3, 5 (mapped to x positions)
    x_vals <- c(1, 3, 5)
  } else {
    # Telemetry and Panel: days 1-84
    x_vals <- 1:84
  }

  data.frame(x = x_vals, y = ret_vec)
})

# Prepare data for table with region and data type columns
table_data <- missingness_table %>%
  mutate(
    pct_complete = (total_observed / total_expected) * 100,
    `N` = format(n_participants, big.mark = ","),
    Expected = format(total_expected, big.mark = ","),
    Observed = format(total_observed, big.mark = ","),
    Missing = format(total_missing, big.mark = ","),
    `% Complete` = sprintf("%.1f%%", pct_complete),
    `Median Missing` = ifelse(
      is.na(median_missing_per_participant),
      "—",
      sprintf("%.1f", median_missing_per_participant)
    ),
    Retention = "",  # Placeholder for retention plots
    Density = ""  # Placeholder for density plots
  ) %>%
  # Only show region and data type on first occurrence within each group
  group_by(region, data_type) %>%
  mutate(
    show_type = row_number() == 1,
    show_region = row_number() == 1
  ) %>%
  ungroup() %>%
  group_by(region) %>%
  mutate(
    show_region = ifelse(row_number() == 1, TRUE, FALSE)
  ) %>%
  ungroup() %>%
  select(
    Region = region,
    `Data Type` = data_type,
    Measure = measure,
    `N`,
    Expected,
    Observed,
    Missing,
    `Median Missing`,
    `% Complete`,
    Retention,
    Density,
    show_type,
    show_region
  ) %>%
  mutate(
    Region = ifelse(show_region, Region, ""),
    `Data Type` = ifelse(show_type, `Data Type`, "")
  ) %>%
  select(-show_type, -show_region)

# Identify row positions for styling (first row of each region/data type group)
us_start <- 1
uk_start <- which(missingness_table$region == "UK")[1]
data_type_starts <- table_data %>%
  mutate(row_num = row_number()) %>%
  filter(`Data Type` != "") %>%
  pull(row_num)

# Create output directories if they don't exist (needed for plot_tt)
if (!dir.exists("outputs")) {
  dir.create("outputs")
}
if (!dir.exists("outputs/tinytable_assets")) {
  dir.create("outputs/tinytable_assets", recursive = TRUE)
}

# Create base table with retention and density plots
missingness_table_base <- tt(table_data) %>%
  plot_tt(
    j = 10,  # Retention column
    fun = "line",
    data = retention_plot_data,
    color = "steelblue"
  ) %>%
  plot_tt(
    j = 11,  # Density column
    fun = "density",
    data = density_plot_data,
    color = "steelblue"
  ) %>%
  style_tt(
    i = 0,
    bold = TRUE
  ) %>%
  style_tt(
    i = c(us_start, uk_start),  # First row of each region
    j = 1,
    bold = TRUE,
    line = "t",
    line_width = 0.15
  ) %>%
  style_tt(
    i = data_type_starts,  # First row of each data type
    j = 2,
    bold = TRUE,
    line = "t",
    line_width = 0.1
  ) %>%
  style_tt(
    j = c(9, 10, 11),  # Center align all plot columns
    align = "c"
  )

# Print table to console (will show without plots in terminal)
print(missingness_table_base)

# Create version with bar plots for HTML export
pct_complete_values <- as.list(
  (missingness_table$total_observed / missingness_table$total_expected)
)

missingness_table_output <- missingness_table_base %>%
  plot_tt(
    j = 9,  # % Complete column
    fun = "bar",
    data = pct_complete_values,
    color = c("steelblue", "lightgrey"),
    xlim = c(0, 1)
  )

# Create version without plots for text-based formats
table_data_no_plots <- table_data %>%
  select(-Retention, -Density)  # Remove plot columns for plain text formats

missingness_table_no_plots <- tt(table_data_no_plots) %>%
  style_tt(
    i = 0,
    bold = TRUE
  ) %>%
  style_tt(
    i = c(us_start, uk_start),  # First row of each region
    j = 1,
    bold = TRUE,
    line = "t",
    line_width = 0.15
  ) %>%
  style_tt(
    i = data_type_starts,  # First row of each data type
    j = 2,
    bold = TRUE,
    line = "t",
    line_width = 0.1
  )

# Export to Multiple Formats ==================================================

cat("\nExporting table to multiple formats...\n")

# Clean up old tinytable assets to avoid accumulation of files with random IDs
if (dir.exists("outputs/tinytable_assets")) {
  unlink("outputs/tinytable_assets", recursive = TRUE)
}
dir.create("outputs/tinytable_assets", recursive = TRUE)

# 1. HTML
save_tt(missingness_table_output, "outputs/missingness_table.html", overwrite = TRUE)
# Fix image paths in HTML (remove "outputs/outputs/" to make paths relative)
html_content <- readLines("outputs/missingness_table.html")
html_content <- gsub('src="outputs/outputs/', 'src="', html_content)

# Replace retention and density plots with "—" for rows with complete missingness
complete_missingness_rows <- which(missingness_table$complete_missingness)
for (row_idx in complete_missingness_rows) {
  # HTML row indices are 1-based in tinytable output
  # Find and replace the img tag in the Retention column (column 10) for this row
  pattern <- sprintf('data-row="%d" data-col="10">.*?</td>', row_idx)
  replacement <- sprintf('data-row="%d" data-col="10">—</td>', row_idx)
  html_content <- gsub(pattern, replacement, html_content)

  # Find and replace the img tag in the Density column (column 11) for this row
  pattern <- sprintf('data-row="%d" data-col="11">.*?</td>', row_idx)
  replacement <- sprintf('data-row="%d" data-col="11">—</td>', row_idx)
  html_content <- gsub(pattern, replacement, html_content)
}

writeLines(html_content, "outputs/missingness_table.html")
cat("✓ HTML: outputs/missingness_table.html\n")

# 2. LaTeX (without plots)
save_tt(missingness_table_no_plots, "outputs/missingness_table.tex", overwrite = TRUE)
cat("✓ LaTeX: outputs/missingness_table.tex\n")

# 3. Markdown (without plots)
save_tt(missingness_table_no_plots, "outputs/missingness_table.md", overwrite = TRUE)
cat("✓ Markdown: outputs/missingness_table.md\n")

# 4. DOCX
tryCatch({
  save_tt(missingness_table_output, "outputs/missingness_table.docx", overwrite = TRUE)
  cat("✓ DOCX: outputs/missingness_table.docx\n")
}, error = function(e) {
  cat("⚠ DOCX export failed (may require pandoc R package)\n")
})

# 5. PNG (let chromote take as long as it needs)
png_success <- FALSE

# First check if chromote is available
if (!requireNamespace("chromote", quietly = TRUE)) {
  cat("⚠ PNG export skipped (install chromote: install.packages('chromote'))\n")
} else {
  tryCatch({
    # Use output table with both density and bar plots for PNG
    cat("  Rendering PNG (please wait, this may take a while)...\n")

    save_tt(missingness_table_output, "outputs/missingness_table.png", overwrite = TRUE)
    
    # Check if file was created
    if (file.exists("outputs/missingness_table.png")) {
      cat("✓ PNG: outputs/missingness_table.png\n")
      png_success <- TRUE
    } else {
      cat("⚠ PNG file not created\n")
    }
  }, error = function(e) {
    msg <- conditionMessage(e)
    cat("⚠ PNG export failed:", msg, "\n")
    if (grepl("Chrome|browser|chromote", msg, ignore.case = TRUE)) {
      cat("  Note: Chromote requires headless Chrome to be working properly\n")
    }
  })
}

# Try alternative PNG generation via PDF if chromote failed
if (!png_success && file.exists("outputs/missingness_table.tex")) {
  cat("\n  Trying alternative: LaTeX → PDF → PNG...\n")
  tryCatch({
    # Create standalone LaTeX document wrapper
    standalone_tex <- c(
      "\\documentclass[border=2mm]{standalone}",
      "\\usepackage{tabularray}",
      "\\begin{document}",
      "\\input{missingness_table.tex}",
      "\\end{document}"
    )
    writeLines(standalone_tex, "outputs/missingness_table_standalone.tex")

    # Compile LaTeX to PDF (run from outputs directory for relative paths)
    old_wd <- getwd()
    setwd("outputs")
    pdf_result <- system2("pdflatex",
                         args = c("-interaction=nonstopmode",
                                 "missingness_table_standalone.tex"),
                         stdout = FALSE, stderr = FALSE)
    setwd(old_wd)
    
    # Check if PDF was created (ignore exit code as LaTeX may have non-fatal warnings)
    if (file.exists("outputs/missingness_table_standalone.pdf")) {
      cat("  LaTeX PDF created, converting to PNG with ImageMagick...\n")
      
      # Convert PDF to PNG using ImageMagick (don't check return code - just check if files were created)
      system2("convert",
             args = c("-density", "300",
                     "-quality", "100",
                     "outputs/missingness_table_standalone.pdf",
                     "outputs/missingness_table_temp.png"),
             stdout = FALSE, stderr = FALSE)
      
      # Wait a moment for file system to sync
      Sys.sleep(0.5)
      
      # ImageMagick creates numbered files for multi-page PDFs
      if (file.exists("outputs/missingness_table_temp-0.png")) {
        file.rename("outputs/missingness_table_temp-0.png", "outputs/missingness_table.png")
        # Remove other pages if they exist
        unlink("outputs/missingness_table_temp-1.png", force = TRUE)
        cat("✓ PNG created via PDF conversion: outputs/missingness_table.png\n")
        png_success <- TRUE
      } else if (file.exists("outputs/missingness_table_temp.png")) {
        file.rename("outputs/missingness_table_temp.png", "outputs/missingness_table.png")
        cat("✓ PNG created via PDF conversion: outputs/missingness_table.png\n")
        png_success <- TRUE
      } else {
        cat("  Note: PNG files not found after convert. Checking directory...\n")
        temp_files <- list.files("outputs", pattern = "missingness_table_temp.*\\.png$", full.names = TRUE)
        if (length(temp_files) > 0) {
          cat("  Found:", paste(temp_files, collapse = ", "), "\n")
        }
      }
      
      # Clean up auxiliary files
      if (png_success) {
        unlink(c("outputs/missingness_table_standalone.aux",
                "outputs/missingness_table_standalone.log",
                "outputs/missingness_table_standalone.tex",
                "outputs/missingness_table_standalone.pdf"))
      }
    } else {
      cat("  PDF compilation failed or PDF not found\n")
    }
  }, error = function(e) {
    cat("  LaTeX→PDF→PNG conversion error:", conditionMessage(e), "\n")
  })
}

# Clean up temporary HTML files created by chromote/tinytable
# Check both current directory and outputs directory
temp_html_files <- c(
  list.files(".", pattern = "^id[a-z0-9]+\\.html$", full.names = TRUE),
  list.files("outputs", pattern = "^id[a-z0-9]+\\.html$", full.names = TRUE)
)
if (length(temp_html_files) > 0) {
  unlink(temp_html_files)
}

# Final message if all PNG methods failed
if (!png_success) {
  cat("  → Recommended: Open outputs/missingness_table.html in browser and screenshot\n")
  cat("  → Or install ImageMagick: sudo apt install imagemagick\n")
}

# 6. Typst (without plots)
tryCatch({
  save_tt(missingness_table_no_plots, "outputs/missingness_table.typ", overwrite = TRUE)
  # Fix image paths in Typst (remove "outputs/outputs/" to make paths relative)
  typ_content <- readLines("outputs/missingness_table.typ")
  typ_content <- gsub('"outputs/outputs/', '"', typ_content)
  writeLines(typ_content, "outputs/missingness_table.typ")
  cat("✓ Typst: outputs/missingness_table.typ\n")
}, error = function(e) {
  cat("⚠ Typst export skipped\n")
})

# Summary Statistics ==========================================================

cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("MISSINGNESS SUMMARY BY REGION\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n", sep = "")

# Helper function to print stats
print_stats <- function(stats, label, include_median = TRUE) {
  cat(sprintf("  %s:\n", label))
  cat(sprintf("    - N Participants: %s\n", format(stats$n_participants, big.mark = ",")))
  cat(sprintf("    - Expected: %s\n", format(stats$total_expected, big.mark = ",")))
  cat(sprintf("    - Observed: %s\n", format(stats$total_observed, big.mark = ",")))
  cat(sprintf("    - Missing: %s (%.1f%%)\n", 
              format(stats$total_missing, big.mark = ","),
              (stats$total_missing / stats$total_expected) * 100))
  if (include_median && !is.na(stats$median_missing_per_participant)) {
    cat(sprintf("    - Median missing/participant: %.1f\n", 
                stats$median_missing_per_participant))
  }
  cat("\n")
}

cat("=== US REGION ===\n\n")
cat("Survey Data:\n")
print_stats(diary_stats_us, "Daily Diary")
print_stats(panel_stats_us, "Biweekly Panel")

cat("Telemetry Data:\n")
print_stats(xbox_stats_us, "Xbox", include_median = FALSE)
print_stats(nintendo_stats_us, "Nintendo", include_median = FALSE)
print_stats(steam_stats_us, "Steam", include_median = FALSE)

cat("Cognitive Task:\n")
print_stats(simon_stats_us, "Simon Task")

cat("=== UK REGION ===\n\n")
cat("Survey Data:\n")
print_stats(panel_stats_uk, "Biweekly Panel")

cat("Telemetry Data:\n")
print_stats(xbox_stats_uk, "Xbox", include_median = FALSE)
print_stats(nintendo_stats_uk, "Nintendo", include_median = FALSE)
print_stats(steam_stats_uk, "Steam", include_median = FALSE)

cat("Cognitive Task:\n")
print_stats(simon_stats_uk, "Simon Task")

cat(paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("All outputs saved to: outputs/\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n", sep = "")

# Note about PNG export
if (!file.exists("outputs/missingness_table.png")) {
  cat("\nNOTE: PNG export failed due to chromote timeout.\n")
  cat("To create a PNG version:\n")
  cat("  1. Open outputs/missingness_table.html in a browser and screenshot\n")
  cat("  2. Use wkhtmltoimage: wkhtmltoimage outputs/missingness_table.html outputs/missingness_table.png\n")
  cat("  3. Compile LaTeX to PDF then convert to PNG\n\n")
}

