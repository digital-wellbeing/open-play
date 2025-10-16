# =============================================================================
# Participant Retention Analysis
# =============================================================================
# This script calculates participant retention rates at specific study dates
# based on the last time participants completed either a diary or panel survey
# =============================================================================

# Load libraries
library(tidyverse)

# Load Data ===================================================================

intake <- read_csv("data/clean/intake.csv.gz", show_col_types = FALSE)
diary <- read_csv("data/clean/diary.csv.gz", show_col_types = FALSE)
panel <- read_csv("data/clean/panel.csv.gz", show_col_types = FALSE)

# Define study dates for retention calculation
study_dates <- c(1, 8, 15, 22, 29, 36, 43, 50, 57, 64, 71, 78)

# Calculate Valid Participants and Retention =================================

cat("Calculating participant retention...\n")

# Get enrollment dates for all participants
enrollment_lookup <- intake %>%
  select(pid, enrollment_date) %>%
  distinct()

# Calculate study day for each diary response
diary_study_days <- diary %>%
  left_join(enrollment_lookup, by = "pid") %>%
  mutate(
    enrollment_datetime = as.POSIXct(
      enrollment_date,
      format = "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ),
    response_datetime = as.POSIXct(
      date,
      format = "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ),
    study_day = ceiling(as.numeric(
      difftime(response_datetime, enrollment_datetime, units = "days")
    )) + 1
  ) %>%
  filter(!is.na(study_day), study_day >= 1) %>%
  select(pid, study_day)

# Calculate study day for each panel response
panel_study_days <- panel %>%
  left_join(enrollment_lookup, by = "pid") %>%
  mutate(
    enrollment_datetime = as.POSIXct(
      enrollment_date,
      format = "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ),
    response_datetime = as.POSIXct(
      date,
      format = "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ),
    study_day = ceiling(as.numeric(
      difftime(response_datetime, enrollment_datetime, units = "days")
    )) + 1
  ) %>%
  filter(!is.na(study_day), study_day >= 1) %>%
  select(pid, study_day)

# Combine diary and panel responses to find last response day per participant
all_responses <- bind_rows(diary_study_days, panel_study_days)

# Calculate last response day for each participant
# The reference set is participants with at least one valid response
last_response_by_participant <- all_responses %>%
  group_by(pid) %>%
  summarise(last_day = max(study_day, na.rm = TRUE), .groups = "drop")

# Reference set: unique PIDs with valid responses (Day 1 = 100% by definition)
reference_pids <- last_response_by_participant$pid
n_total <- length(reference_pids)

# For each target study day, calculate retention rate
retention_results <- map_dfr(study_dates, function(day) {
  # Participants are "retained" at day X if their last response is >= day X
  n_retained <- sum(last_response_by_participant$last_day >= day)
  retention_rate <- n_retained / n_total

  tibble(
    study_day = day,
    n_retained = n_retained,
    n_total = n_total,
    retention_rate = retention_rate,
    retention_pct = retention_rate * 100
  )
})

# Display Results =============================================================

cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("PARTICIPANT RETENTION BY STUDY DAY\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n", sep = "")

retention_results %>%
  mutate(
    study_day = sprintf("Day %2d", study_day),
    retention = sprintf(
      "%4d / %4d (%5.1f%%)",
      n_retained,
      n_total,
      retention_pct
    )
  ) %>%
  select(study_day, retention) %>%
  print(n = Inf, width = Inf)

cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")

# Export Results ==============================================================

# Create outputs directory if it doesn't exist
if (!dir.exists("outputs")) {
  dir.create("outputs")
}

# Save as CSV
write_csv(retention_results, "outputs/retention_by_study_day.csv")
cat("\nResults saved to: outputs/retention_by_study_day.csv\n")

# Save as RDS for use in other R scripts
saveRDS(retention_results, "outputs/retention_by_study_day.rds")
cat("Results saved to: outputs/retention_by_study_day.rds\n\n")

cat(paste(rep("=", 70), collapse = ""), "\n\n", sep = "")
