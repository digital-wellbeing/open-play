# Steam Profile Visibility Analysis
#
# This script analyzes Steam profile visibility (public vs. private) over the
# course of the study period for each participant. It answers: "When we don't see
# gaming data, is it because the profile was private, or because the participant
# didn't play?"
#
# Key insight: The presence of gaming data in data/clean/steam.csv.gz proves the
# profile was public at that time. Visibility events tell us about state changes.
#
# Input files:
# - data/clean/intake.csv.gz: Contains enrollment_date for each participant
# - data/raw/telemetry_steam_account_linking_raw.csv: Public/private visibility events
# - data/clean/steam.csv.gz: Actual gaming data (proves profile was public)

library(tidyverse)
library(lubridate)

# Function to calculate visibility coverage for a single participant ------

calculate_steam_visibility_coverage <- function(
  participant_id,
  visibility_data,
  gaming_data,
  enrollment_data
) {
  #' Calculate Steam Profile Visibility Coverage
  #'
  #' For a given participant, calculates how many hours during their 84-day
  #' study period their Steam profile was visible (public) to researchers.
  #'
  #' @param participant_id Character. The participant ID (pid)
  #' @param visibility_data Data frame. Steam visibility events with columns:
  #'   pid, timestamp, event, is_public
  #' @param gaming_data Data frame. Steam gaming data with columns:
  #'   pid, hour_start
  #' @param enrollment_data Data frame. Enrollment info with columns:
  #'   pid, enrollment_datetime, study_end_datetime
  #'
  #' @return Named list with:
  #'   - hours_visible: Total hours profile was visible/public
  #'   - hours_hidden: Total hours profile was confirmed private
  #'   - hours_unknown: Total hours with unknown status
  #'   - hours_total: Total hours in study period (typically 2016)
  #'   - proportion_visible: Proportion of study period visible (0-1)
  #'   - has_gaming_data: Boolean, whether participant has any gaming data
  #'   - has_visibility_events: Boolean, whether participant has visibility events

  # Get enrollment info
  participant_info <- enrollment_data |> filter(pid == participant_id)

  if (nrow(participant_info) == 0) {
    warning(paste("No enrollment data found for participant:", participant_id))
    return(list(
      hours_visible = NA,
      hours_hidden = NA,
      hours_unknown = NA,
      hours_total = NA,
      proportion_visible = NA,
      has_gaming_data = FALSE,
      has_visibility_events = FALSE
    ))
  }

  enroll_dt <- participant_info$enrollment_datetime
  end_dt <- participant_info$study_end_datetime
  expected_hours <- as.numeric(difftime(end_dt, enroll_dt, units = "hours"))

  # Check for gaming data during study period
  gaming_hours <- gaming_data |>
    filter(
      pid == participant_id,
      hour_start >= enroll_dt,
      hour_start < end_dt
    ) |>
    distinct(hour_start) |>
    nrow()

  has_gaming <- gaming_hours > 0

  # Get visibility events during study period
  events_during <- visibility_data |>
    filter(
      pid == participant_id,
      timestamp >= enroll_dt,
      timestamp <= end_dt
    ) |>
    arrange(timestamp)

  has_events <- nrow(events_during) > 0

  # Calculate visibility hours based on events
  if (has_events) {
    # Get gaming hours for this participant during study period
    gaming_times <- gaming_data |>
      filter(
        pid == participant_id,
        hour_start >= enroll_dt,
        hour_start < end_dt
      ) |>
      pull(hour_start) |>
      sort()

    # Calculate duration of each visibility state, accounting for gaming data
    # that signals transition from private to public
    events_with_duration <- events_during |>
      mutate(
        next_timestamp = lead(timestamp, default = end_dt),
        next_event_is_public = lead(is_public, default = NA)
      ) |>
      rowwise() |>
      mutate(
        # For private periods, check if gaming data occurs before next event
        # Gaming data signals profile became public again
        gaming_during_period = {
          if (!is_public && length(gaming_times) > 0) {
            first_gaming <- gaming_times[
              gaming_times >= timestamp & gaming_times < next_timestamp
            ]
            if (length(first_gaming) > 0) {
              as.numeric(min(first_gaming))
            } else {
              NA_real_
            }
          } else {
            NA_real_
          }
        },
        # If we're in a private period and there's gaming data,
        # split it: private until gaming, then public
        actual_end = if (!is.na(gaming_during_period)) {
          as.POSIXct(gaming_during_period, origin = "1970-01-01", tz = "UTC")
        } else {
          next_timestamp
        },
        duration_hours = as.numeric(difftime(
          actual_end,
          timestamp,
          units = "hours"
        ))
      ) |>
      ungroup()

    # Calculate private hours (before gaming data interrupts)
    hours_private_from_events <- events_with_duration |>
      filter(!is_public) |>
      pull(duration_hours) |>
      sum(na.rm = TRUE)

    # Calculate public hours from explicit public events
    hours_public_from_events <- events_with_duration |>
      filter(is_public) |>
      mutate(
        duration_hours = as.numeric(difftime(
          next_timestamp,
          timestamp,
          units = "hours"
        ))
      ) |>
      pull(duration_hours) |>
      sum(na.rm = TRUE)

    # Add public hours from gaming data that occurred during private periods
    # (these are periods that became public due to gaming activity)
    hours_public_from_gaming_during_private <- events_with_duration |>
      filter(!is_public, !is.na(gaming_during_period)) |>
      mutate(
        duration_after_gaming = as.numeric(difftime(
          next_timestamp,
          as.POSIXct(gaming_during_period, origin = "1970-01-01", tz = "UTC"),
          units = "hours"
        ))
      ) |>
      pull(duration_after_gaming) |>
      sum(na.rm = TRUE)

    # Total public hours = explicit public periods + gaming-triggered public periods
    hours_public_total <- hours_public_from_events +
      hours_public_from_gaming_during_private

    # Check for any gap BEFORE the first event
    first_event_time <- min(events_during$timestamp)
    hours_before_first_event <- as.numeric(difftime(
      first_event_time,
      enroll_dt,
      units = "hours"
    ))

    if (hours_before_first_event > 0) {
      # Check if there's gaming data before the first event
      gaming_before_first <- any(gaming_times < first_event_time)
      if (gaming_before_first) {
        # Has gaming before first event, assume public for that period
        hours_public_total <- hours_public_total + hours_before_first_event
        hours_unknown <- 0
      } else {
        # No gaming before first event, status unknown for that period
        hours_unknown <- hours_before_first_event
      }
    } else {
      hours_unknown <- 0
    }

    hours_visible <- hours_public_total
    hours_hidden <- hours_private_from_events
  } else {
    # No visibility events
    if (has_gaming) {
      # Gaming data proves visibility
      # If we successfully polled them and got gaming data at various times,
      # we can assume their profile was public the entire study period
      # (they just didn't play during the other hours)
      hours_visible <- expected_hours
      hours_hidden <- 0
      hours_unknown <- 0
    } else {
      # No visibility events and no gaming data
      # We don't know if they were public or private - completely unknown
      hours_visible <- 0
      hours_hidden <- 0
      hours_unknown <- expected_hours
    }
  }

  list(
    hours_visible = hours_visible,
    hours_hidden = hours_hidden,
    hours_unknown = hours_unknown,
    hours_total = expected_hours,
    proportion_visible = hours_visible / expected_hours,
    has_gaming_data = has_gaming,
    has_visibility_events = has_events
  )
}

# Load data ---------------------------------------------------------------

cat("Loading data...\n")

# Load intake data to get enrollment dates
intake <- read_csv("data/clean/intake.csv.gz", show_col_types = FALSE) |>
  select(pid, enrollment_date) |>
  mutate(
    enrollment_datetime = as.POSIXct(
      enrollment_date,
      format = "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ),
    study_end_datetime = enrollment_datetime + days(84)
  )

# Load Steam visibility events (only available for subset of participants)
steam_visibility <- read_csv(
  "data/raw/telemetry_steam_account_linking_raw.csv",
  show_col_types = FALSE
) |>
  filter(platform == "steam") |>
  mutate(
    timestamp = as.POSIXct(
      timestamp,
      format = "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ),
    is_public = event == "steamDataPublic"
  ) |>
  select(pid, timestamp, event, is_public) |>
  arrange(pid, timestamp)

# Load actual gaming data - this PROVES profile was public when we have data
steam_gaming <- read_csv("data/clean/steam.csv.gz", show_col_types = FALSE) |>
  mutate(
    timestamp = as.POSIXct(
      timestamp,
      format = "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ),
    hour_start = floor_date(timestamp, "hour")
  ) |>
  select(pid, hour_start) |>
  distinct()

# Load survey data to identify participants with diary/panel responses
diary <- read_csv("data/clean/diary.csv.gz", show_col_types = FALSE) |>
  select(pid) |>
  distinct()

panel <- read_csv("data/clean/panel.csv.gz", show_col_types = FALSE) |>
  select(pid) |>
  distinct()

cat("Data loaded successfully.\n")
cat("Participants with gaming data:", n_distinct(steam_gaming$pid), "\n")
cat(
  "Participants with visibility events:",
  n_distinct(steam_visibility$pid),
  "\n"
)
cat("Participants with diary responses:", n_distinct(diary$pid), "\n")
cat("Participants with panel responses:", n_distinct(panel$pid), "\n\n")

# Calculate visibility for all participants with gaming data --------------

cat("Calculating visibility metrics...\n")

# Get all participants with gaming data
all_gaming_participants <- steam_gaming |>
  left_join(intake, by = "pid") |>
  filter(!is.na(enrollment_datetime)) |>
  # Filter to study period only
  filter(
    hour_start >= enrollment_datetime,
    hour_start < study_end_datetime
  ) |>
  group_by(pid, enrollment_datetime, study_end_datetime) |>
  summarize(
    hours_with_gaming_data = n(),
    .groups = "drop"
  )

# For participants with visibility events, calculate public/private periods
participants_with_events <- steam_visibility |>
  pull(pid) |>
  unique()

visibility_from_events <- map_dfr(
  participants_with_events,
  function(participant_id) {
    # Get enrollment info
    participant_info <- intake |> filter(pid == participant_id)
    if (nrow(participant_info) == 0) {
      return(NULL)
    }

    enroll_dt <- participant_info$enrollment_datetime
    end_dt <- participant_info$study_end_datetime

    # Get visibility events for this participant
    events <- steam_visibility |>
      filter(
        pid == participant_id,
        timestamp >= enroll_dt,
        timestamp <= end_dt
      ) |>
      arrange(timestamp)

    if (nrow(events) == 0) {
      return(NULL)
    }

    # Get gaming hours for this participant during study period
    gaming_times <- steam_gaming |>
      filter(
        pid == participant_id,
        hour_start >= enroll_dt,
        hour_start < end_dt
      ) |>
      pull(hour_start) |>
      sort()

    # Calculate duration of each state, accounting for gaming data
    # that signals transition from private to public
    events_with_duration <- events |>
      mutate(
        next_timestamp = lead(timestamp, default = end_dt),
        next_event_is_public = lead(is_public, default = NA)
      ) |>
      rowwise() |>
      mutate(
        # For private periods, check if gaming data occurs before next event
        gaming_during_period = {
          if (!is_public && length(gaming_times) > 0) {
            first_gaming <- gaming_times[
              gaming_times >= timestamp & gaming_times < next_timestamp
            ]
            if (length(first_gaming) > 0) {
              as.numeric(min(first_gaming))
            } else {
              NA_real_
            }
          } else {
            NA_real_
          }
        },
        # If we're in a private period and there's gaming data, split it
        actual_end = if (!is.na(gaming_during_period)) {
          as.POSIXct(gaming_during_period, origin = "1970-01-01", tz = "UTC")
        } else {
          next_timestamp
        },
        duration_hours = as.numeric(difftime(
          actual_end,
          timestamp,
          units = "hours"
        ))
      ) |>
      ungroup()

    # Calculate private hours (before gaming data interrupts)
    total_hours_private <- events_with_duration |>
      filter(!is_public) |>
      pull(duration_hours) |>
      sum(na.rm = TRUE)

    # Calculate public hours from explicit public events
    total_hours_public <- events_with_duration |>
      filter(is_public) |>
      mutate(
        duration_hours = as.numeric(difftime(
          next_timestamp,
          timestamp,
          units = "hours"
        ))
      ) |>
      pull(duration_hours) |>
      sum(na.rm = TRUE)

    # Add public hours from gaming data that occurred during private periods
    hours_public_from_gaming_during_private <- events_with_duration |>
      filter(!is_public, !is.na(gaming_during_period)) |>
      mutate(
        duration_after_gaming = as.numeric(difftime(
          next_timestamp,
          as.POSIXct(gaming_during_period, origin = "1970-01-01", tz = "UTC"),
          units = "hours"
        ))
      ) |>
      pull(duration_after_gaming) |>
      sum(na.rm = TRUE)

    total_hours_public <- total_hours_public +
      hours_public_from_gaming_during_private

    tibble(
      pid = participant_id,
      hours_public_from_events = total_hours_public,
      hours_private_from_events = total_hours_private,
      n_visibility_changes = nrow(events)
    )
  }
)

# Combine gaming data with visibility events
steam_visibility_summary <- all_gaming_participants |>
  left_join(visibility_from_events, by = "pid") |>
  mutate(
    # If we have gaming data, those hours were definitely public
    hours_public_with_gaming = hours_with_gaming_data,

    # If we have visibility events, use them; otherwise we only know about gaming hours
    has_visibility_events = !is.na(hours_public_from_events),

    # Expected total
    expected_total_hours = as.numeric(difftime(
      study_end_datetime,
      enrollment_datetime,
      units = "hours"
    )),

    # Total public hours = max of gaming hours or event-based public hours
    # (gaming data proves it was public)
    # If no visibility events but has gaming data, assume public entire period
    total_hours_public = case_when(
      has_visibility_events ~
        pmax(hours_public_from_events, hours_with_gaming_data),
      hours_with_gaming_data > 0 ~ expected_total_hours, # Has gaming, assume public entire period
      TRUE ~ 0 # No gaming, no events - unknown
    ),

    # Public hours without gaming = public hours - gaming hours
    hours_public_no_gaming = case_when(
      has_visibility_events ~
        pmax(0, hours_public_from_events - hours_with_gaming_data),
      hours_with_gaming_data > 0 ~
        expected_total_hours - hours_with_gaming_data, # Assume public entire period
      TRUE ~ 0
    ),

    # Private hours only known if we have visibility events
    total_hours_private = coalesce(hours_private_from_events, 0),

    # Total tracked hours
    total_hours_tracked = total_hours_public + total_hours_private,

    # Hours with unknown status (no visibility events, no gaming data)
    hours_unknown = case_when(
      has_visibility_events ~ expected_total_hours - total_hours_tracked,
      hours_with_gaming_data > 0 ~ 0, # Has gaming, assume all known (public entire period)
      TRUE ~ expected_total_hours # No gaming, no events - all unknown
    ),

    # Visibility changes
    n_visibility_changes = coalesce(n_visibility_changes, 0L),

    # Proportions
    proportion_hours_public = total_hours_public / expected_total_hours,
    proportion_hours_private = total_hours_private / expected_total_hours,
    proportion_public_with_gaming = hours_public_with_gaming /
      expected_total_hours,
    proportion_public_no_gaming = hours_public_no_gaming / expected_total_hours,
    proportion_unknown = hours_unknown / expected_total_hours
  ) |>
  select(
    pid,
    enrollment_datetime,
    study_end_datetime,
    hours_public_with_gaming,
    hours_public_no_gaming,
    total_hours_public,
    total_hours_private,
    hours_unknown,
    expected_total_hours,
    has_visibility_events,
    n_visibility_changes,
    proportion_public_with_gaming,
    proportion_public_no_gaming,
    proportion_hours_public,
    proportion_hours_private,
    proportion_unknown
  ) |>
  rename(
    enrollment_date = enrollment_datetime,
    study_end_date = study_end_datetime
  )

cat("Visibility calculations complete.\n\n")

# Summary statistics ------------------------------------------------------

cat("\n=== Steam Profile Visibility Summary ===\n\n")
cat("Total participants analyzed:", nrow(steam_visibility_summary), "\n")
cat(
  "Participants with visibility events:",
  sum(steam_visibility_summary$has_visibility_events),
  "\n"
)
cat(
  "Participants without visibility events:",
  sum(!steam_visibility_summary$has_visibility_events),
  "\n\n"
)

cat("--- Time-based metrics (hours, all participants) ---\n")
cat(
  "Average hours with gaming data:",
  round(
    mean(steam_visibility_summary$hours_public_with_gaming, na.rm = TRUE),
    1
  ),
  "\n"
)
cat(
  "Average hours public without gaming (from visibility events):",
  round(mean(steam_visibility_summary$hours_public_no_gaming, na.rm = TRUE), 1),
  "\n"
)
cat(
  "Average hours known private:",
  round(mean(steam_visibility_summary$total_hours_private, na.rm = TRUE), 1),
  "\n"
)
cat(
  "Average hours unknown status:",
  round(mean(steam_visibility_summary$hours_unknown, na.rm = TRUE), 1),
  "\n\n"
)

cat("--- For participants WITH visibility events only ---\n")
with_events <- steam_visibility_summary |> filter(has_visibility_events)
cat("N =", nrow(with_events), "\n")
cat(
  "Average hours with gaming data:",
  round(mean(with_events$hours_public_with_gaming, na.rm = TRUE), 1),
  "\n"
)
cat(
  "Average hours public without gaming:",
  round(mean(with_events$hours_public_no_gaming, na.rm = TRUE), 1),
  "\n"
)
cat(
  "Average hours private:",
  round(mean(with_events$total_hours_private, na.rm = TRUE), 1),
  "\n"
)
cat(
  "Average number of visibility changes:",
  round(mean(with_events$n_visibility_changes, na.rm = TRUE), 1),
  "\n\n"
)

cat("--- Proportions of study period (84 days = 2016 hours) ---\n")
cat(
  "Average proportion time with gaming:",
  round(
    mean(steam_visibility_summary$proportion_public_with_gaming, na.rm = TRUE),
    3
  ),
  "\n"
)
cat(
  "Median proportion time with gaming:",
  round(
    median(
      steam_visibility_summary$proportion_public_with_gaming,
      na.rm = TRUE
    ),
    3
  ),
  "\n"
)
cat(
  "Average proportion public (no gaming):",
  round(
    mean(steam_visibility_summary$proportion_public_no_gaming, na.rm = TRUE),
    3
  ),
  "\n"
)
cat(
  "Average proportion private:",
  round(
    mean(steam_visibility_summary$proportion_hours_private, na.rm = TRUE),
    3
  ),
  "\n"
)
cat(
  "Average proportion unknown:",
  round(mean(steam_visibility_summary$proportion_unknown, na.rm = TRUE), 3),
  "\n\n"
)

# Survey-active participant analysis --------------------------------------

cat("=== SURVEY-ACTIVE STEAM PARTICIPANTS ===\n\n")

# Identify Steam participants (those with gaming data)
steam_participants <- steam_gaming |>
  pull(pid) |>
  unique()

# Identify participants with at least one diary or panel response
participants_with_diary <- diary |> pull(pid) |> unique()
participants_with_panel <- panel |> pull(pid) |> unique()
participants_with_survey <- unique(c(
  participants_with_diary,
  participants_with_panel
))

# Survey-active Steam participants (completed at least 1 diary OR panel)
survey_active_steam <- intersect(steam_participants, participants_with_survey)

cat("Survey-active Steam participants:", length(survey_active_steam), "\n")
cat(
  "  - With at least one diary:",
  length(intersect(steam_participants, participants_with_diary)),
  "\n"
)
cat(
  "  - With at least one panel:",
  length(intersect(steam_participants, participants_with_panel)),
  "\n\n"
)

# For each survey-active participant, check visibility events in three periods
visibility_timing <- map_dfr(
  survey_active_steam,
  function(participant_id) {
    # Get enrollment info
    participant_info <- intake |> filter(pid == participant_id)
    if (nrow(participant_info) == 0) {
      return(NULL)
    }

    enroll_dt <- participant_info$enrollment_datetime
    end_dt <- participant_info$study_end_datetime

    # Get all visibility events for this participant
    events <- steam_visibility |>
      filter(pid == participant_id)

    if (nrow(events) == 0) {
      return(tibble(
        pid = participant_id,
        has_events_before = FALSE,
        has_events_during = FALSE,
        has_events_after = FALSE,
        n_events_before = 0,
        n_events_during = 0,
        n_events_after = 0
      ))
    }

    # Categorize events by timing
    events_before <- events |> filter(timestamp < enroll_dt)
    events_during <- events |>
      filter(timestamp >= enroll_dt, timestamp <= end_dt)
    events_after <- events |> filter(timestamp > end_dt)

    tibble(
      pid = participant_id,
      has_events_before = nrow(events_before) > 0,
      has_events_during = nrow(events_during) > 0,
      has_events_after = nrow(events_after) > 0,
      n_events_before = nrow(events_before),
      n_events_during = nrow(events_during),
      n_events_after = nrow(events_after)
    )
  }
)

# Summary statistics
cat("--- Visibility Event Timing (relative to 84-day study period) ---\n\n")

n_total <- nrow(visibility_timing)
n_before <- sum(visibility_timing$has_events_before)
n_during <- sum(visibility_timing$has_events_during)
n_after <- sum(visibility_timing$has_events_after)
n_any <- sum(
  visibility_timing$has_events_before |
    visibility_timing$has_events_during |
    visibility_timing$has_events_after
)
n_none <- n_total - n_any

cat("Total survey-active Steam participants:", n_total, "\n\n")

cat("Participants with visibility events:\n")
cat(
  "  - BEFORE study period:",
  n_before,
  sprintf("(%.1f%%)", 100 * n_before / n_total),
  "\n"
)
cat(
  "  - DURING study period:",
  n_during,
  sprintf("(%.1f%%)", 100 * n_during / n_total),
  "\n"
)
cat(
  "  - AFTER study period:",
  n_after,
  sprintf("(%.1f%%)", 100 * n_after / n_total),
  "\n"
)
cat(
  "  - ANY time (before/during/after):",
  n_any,
  sprintf("(%.1f%%)", 100 * n_any / n_total),
  "\n"
)
cat(
  "  - NO visibility events:",
  n_none,
  sprintf("(%.1f%%)", 100 * n_none / n_total),
  "\n\n"
)

# Breakdown by combinations
visibility_patterns <- visibility_timing |>
  mutate(
    pattern = case_when(
      !has_events_before & !has_events_during & !has_events_after ~ "None",
      has_events_before & !has_events_during & !has_events_after ~
        "Before only",
      !has_events_before & has_events_during & !has_events_after ~
        "During only",
      !has_events_before & !has_events_during & has_events_after ~ "After only",
      has_events_before & has_events_during & !has_events_after ~
        "Before + During",
      has_events_before & !has_events_during & has_events_after ~
        "Before + After",
      !has_events_before & has_events_during & has_events_after ~
        "During + After",
      has_events_before & has_events_during & has_events_after ~
        "All three periods",
      TRUE ~ "Other"
    )
  ) |>
  count(pattern) |>
  arrange(desc(n))

cat("Visibility event patterns:\n")
for (i in 1:nrow(visibility_patterns)) {
  cat(sprintf(
    "  %-20s: %4d (%.1f%%)\n",
    visibility_patterns$pattern[i],
    visibility_patterns$n[i],
    100 * visibility_patterns$n[i] / n_total
  ))
}
cat("\n")

# Average number of events by period
cat("Average number of visibility events per participant:\n")
cat(
  "  - Before study period:",
  round(mean(visibility_timing$n_events_before), 1),
  "\n"
)
cat(
  "  - During study period:",
  round(mean(visibility_timing$n_events_during), 1),
  "\n"
)
cat(
  "  - After study period:",
  round(mean(visibility_timing$n_events_after), 1),
  "\n\n"
)

# For participants with events during study period only
with_during_events <- visibility_timing |> filter(has_events_during)
cat(
  "For participants with visibility events DURING study period (n=",
  nrow(with_during_events),
  "):\n",
  sep = ""
)
cat(
  "  - Average events during:",
  round(mean(with_during_events$n_events_during), 1),
  "\n"
)
cat(
  "  - Median events during:",
  round(median(with_during_events$n_events_during), 1),
  "\n"
)
cat(
  "  - Range:",
  min(with_during_events$n_events_during),
  "to",
  max(with_during_events$n_events_during),
  "\n\n"
)

# Hidden profile analysis -------------------------------------------------

cat("--- Hidden Steam Profiles During Study Period ---\n\n")
cat("Note: Gaming data presence PROVES profile was public at those times.\n")
cat(
  "We only analyze visibility events that occurred DURING the 84-day study period.\n\n"
)

# For each survey-active participant, check visibility status DURING study only
# Also check if they have gaming data (which proves public status)
hidden_status <- map_dfr(
  survey_active_steam,
  function(participant_id) {
    # Get enrollment info
    participant_info <- intake |> filter(pid == participant_id)
    if (nrow(participant_info) == 0) {
      return(NULL)
    }

    enroll_dt <- participant_info$enrollment_datetime
    end_dt <- participant_info$study_end_datetime

    # Check if participant has gaming data during study
    has_gaming_data <- steam_gaming |>
      filter(
        pid == participant_id,
        hour_start >= enroll_dt,
        hour_start < end_dt
      ) |>
      nrow() >
      0

    # ONLY look at visibility events DURING the study period
    events_during <- steam_visibility |>
      filter(
        pid == participant_id,
        timestamp >= enroll_dt,
        timestamp <= end_dt
      ) |>
      arrange(timestamp)

    n_private <- sum(!events_during$is_public)
    n_public <- sum(events_during$is_public)
    has_visibility_events <- nrow(events_during) > 0
    has_private_events <- n_private > 0
    has_public_events <- n_public > 0

    # Determine status based on visibility events AND gaming data
    status <- case_when(
      # Confirmed hidden: has private events AND no gaming data
      has_private_events & !has_gaming_data ~
        "Confirmed hidden (private events, no gaming data)",

      # Public with private periods: has gaming data (proves public) AND has private events
      has_gaming_data & has_private_events ~
        "Public with private periods (gaming data + private events)",

      # Confirmed public only: has gaming data OR only public events, no private events
      (has_gaming_data | has_public_events) & !has_private_events ~
        "Confirmed public only (gaming data or public events only)",

      # Unknown: no visibility events and no gaming data
      !has_visibility_events & !has_gaming_data ~
        "Unknown (no visibility tracking, no gaming data)",

      TRUE ~ "Other"
    )

    tibble(
      pid = participant_id,
      has_gaming_data = has_gaming_data,
      has_visibility_events_during = has_visibility_events,
      has_private_events_during = has_private_events,
      has_public_events_during = has_public_events,
      n_private_events = n_private,
      n_public_events = n_public,
      status = status
    )
  }
)

# Summary statistics
cat("Total survey-active Steam participants:", nrow(hidden_status), "\n\n")

n_with_gaming <- sum(hidden_status$has_gaming_data)
n_no_gaming <- sum(!hidden_status$has_gaming_data)
n_with_visibility <- sum(hidden_status$has_visibility_events_during)
n_no_visibility <- sum(!hidden_status$has_visibility_events_during)

cat("Gaming data and visibility tracking:\n")
cat(
  "  - With gaming data during study:",
  n_with_gaming,
  sprintf("(%.1f%%)", 100 * n_with_gaming / n_total),
  "\n"
)
cat(
  "  - No gaming data during study:",
  n_no_gaming,
  sprintf("(%.1f%%)", 100 * n_no_gaming / n_total),
  "\n"
)
cat(
  "  - With visibility events during study:",
  n_with_visibility,
  sprintf("(%.1f%%)", 100 * n_with_visibility / n_total),
  "\n"
)
cat(
  "  - No visibility events during study:",
  n_no_visibility,
  sprintf("(%.1f%%)", 100 * n_no_visibility / n_total),
  "\n\n"
)

# Breakdown by status
status_summary <- hidden_status |>
  count(status) |>
  arrange(desc(n))

cat("Classification (combining gaming data + visibility events):\n\n")
for (i in 1:nrow(status_summary)) {
  cat(sprintf(
    "  %-60s: %4d (%.1f%%)\n",
    status_summary$status[i],
    status_summary$n[i],
    100 * status_summary$n[i] / n_total
  ))
}
cat("\n")

# Key insights
confirmed_hidden <- hidden_status |>
  filter(grepl("Confirmed hidden", status))

public_with_private <- hidden_status |>
  filter(grepl("Public with private periods", status))

confirmed_public <- hidden_status |>
  filter(grepl("Confirmed public only", status))

cat("Key insights:\n")
cat(
  "  - Confirmed hidden (private events, no gaming):",
  nrow(confirmed_hidden),
  "\n"
)
cat("    → These participants had private profiles and we captured no gaming\n")
cat("  - Public with private periods:", nrow(public_with_private), "\n")
cat(
  "    → These participants had gaming data (proves public) but also had private periods\n"
)
cat("  - Confirmed public only:", nrow(confirmed_public), "\n")
cat(
  "    → These participants had gaming data and/or only public visibility events\n\n"
)

# Gaming visibility analysis ----------------------------------------------

cat("--- Gaming Visibility Analysis ---\n\n")
cat(
  "This section estimates how much gaming activity was visible vs invisible to researchers.\n\n"
)

# Get visibility summary for survey-active participants
survey_active_visibility <- steam_visibility_summary |>
  filter(pid %in% survey_active_steam)

cat(
  "Survey-active Steam participants in visibility summary:",
  nrow(survey_active_visibility),
  "\n\n"
)

# Calculate hours breakdown
survey_active_visibility <- survey_active_visibility |>
  mutate(
    # Hours we captured gaming data (profile was public)
    hours_with_gaming = hours_public_with_gaming,

    # Hours profile was confirmed private (from visibility events)
    hours_confirmed_private = total_hours_private,

    # Hours with unknown status (no visibility tracking)
    hours_unknown_status = hours_unknown,

    # Hours without gaming data = private + unknown + (public but not playing)
    hours_no_gaming = expected_total_hours - hours_with_gaming
  )

# Summary statistics
cat("Hours during 84-day study period (per participant, avg):\n")
cat(
  "  Total hours in study:",
  round(mean(survey_active_visibility$expected_total_hours), 1),
  "\n\n"
)

cat(
  "  Hours WITH gaming data:",
  round(mean(survey_active_visibility$hours_with_gaming), 1),
  sprintf(
    "(%.1f%%)",
    100 *
      mean(
        survey_active_visibility$hours_with_gaming /
          survey_active_visibility$expected_total_hours
      )
  ),
  "\n"
)
cat("    → Profile was definitely public during these hours\n\n")

cat(
  "  Hours WITHOUT gaming data:",
  round(mean(survey_active_visibility$hours_no_gaming), 1),
  sprintf(
    "(%.1f%%)",
    100 *
      mean(
        survey_active_visibility$hours_no_gaming /
          survey_active_visibility$expected_total_hours
      )
  ),
  "\n"
)
cat(
  "    → Could be: (a) private/hidden, (b) public but not playing, or (c) unknown\n\n"
)

cat("    Breakdown of hours without gaming:\n")
cat(
  "      - Confirmed private (from visibility events):",
  round(mean(survey_active_visibility$hours_confirmed_private), 1),
  "\n"
)
cat(
  "      - Unknown status (no visibility tracking):",
  round(mean(survey_active_visibility$hours_unknown_status), 1),
  "\n\n"
)

# Total across all survey-active participants
total_expected <- sum(survey_active_visibility$expected_total_hours)
total_with_gaming <- sum(survey_active_visibility$hours_with_gaming)
total_no_gaming <- sum(survey_active_visibility$hours_no_gaming)
total_confirmed_private <- sum(survey_active_visibility$hours_confirmed_private)
total_unknown <- sum(survey_active_visibility$hours_unknown_status)

cat(
  "Totals across all",
  nrow(survey_active_visibility),
  "survey-active participants:\n"
)
cat(
  "  Total hours in study period:",
  format(total_expected, big.mark = ","),
  "\n"
)
cat(
  "  Hours with gaming data:",
  format(total_with_gaming, big.mark = ","),
  sprintf("(%.1f%%)", 100 * total_with_gaming / total_expected),
  "\n"
)
cat(
  "  Hours without gaming data:",
  format(total_no_gaming, big.mark = ","),
  sprintf("(%.1f%%)", 100 * total_no_gaming / total_expected),
  "\n"
)
cat(
  "    - Confirmed private:",
  format(total_confirmed_private, big.mark = ","),
  sprintf("(%.1f%%)", 100 * total_confirmed_private / total_expected),
  "\n"
)
cat(
  "    - Unknown status:",
  format(total_unknown, big.mark = ","),
  sprintf("(%.1f%%)", 100 * total_unknown / total_expected),
  "\n\n"
)

# Breakdown by participants with/without visibility events during study
with_events_during <- survey_active_visibility |> filter(has_visibility_events)
without_events_during <- survey_active_visibility |>
  filter(!has_visibility_events)

cat("Comparison by visibility event availability:\n\n")

cat(
  "Participants WITH visibility events during study (n=",
  nrow(with_events_during),
  "):\n",
  sep = ""
)
cat(
  "  Hours with gaming:",
  round(mean(with_events_during$hours_with_gaming), 1),
  "\n"
)
cat(
  "  Hours confirmed private:",
  round(mean(with_events_during$hours_confirmed_private), 1),
  "\n"
)
cat(
  "  Hours unknown:",
  round(mean(with_events_during$hours_unknown_status), 1),
  "\n\n"
)

cat(
  "Participants WITHOUT visibility events during study (n=",
  nrow(without_events_during),
  "):\n",
  sep = ""
)
cat(
  "  Hours with gaming:",
  round(mean(without_events_during$hours_with_gaming), 1),
  "\n"
)
cat(
  "  Hours confirmed private:",
  round(mean(without_events_during$hours_confirmed_private), 1),
  "\n"
)
cat(
  "  Hours unknown:",
  round(mean(without_events_during$hours_unknown_status), 1),
  "\n"
)
cat(
  "  Note: Most hours are 'unknown' - we only know they were public when we have gaming data\n\n"
)

# Specific group analysis -------------------------------------------------

cat("--- Hours Unknown/Hidden by Specific Groups ---\n\n")

# Get visibility data for specific participant groups from hidden_status
public_with_private_visibility <- survey_active_visibility |>
  filter(pid %in% public_with_private$pid)

unknown_pids <- hidden_status$pid[
  hidden_status$status == "Unknown (no visibility tracking, no gaming data)"
]
unknown_visibility <- survey_active_visibility |>
  filter(pid %in% unknown_pids)

# Public with private periods (n=58)
if (nrow(public_with_private_visibility) > 0) {
  cat(
    "Public with private periods (n=",
    nrow(public_with_private),
    "):\n",
    sep = ""
  )
  cat(
    "  Total hours in study period:",
    format(
      sum(public_with_private_visibility$expected_total_hours),
      big.mark = ","
    ),
    "\n"
  )
  cat(
    "  Hours with gaming data:",
    format(
      sum(public_with_private_visibility$hours_with_gaming),
      big.mark = ","
    ),
    "\n"
  )
  cat(
    "  Hours confirmed private (hidden):",
    format(
      sum(public_with_private_visibility$hours_confirmed_private),
      big.mark = ","
    ),
    "\n"
  )
  cat(
    "  Hours unknown:",
    format(
      sum(public_with_private_visibility$hours_unknown_status),
      big.mark = ","
    ),
    "\n"
  )
  cat(
    "  → Total hours hidden or unknown:",
    format(
      sum(
        public_with_private_visibility$hours_confirmed_private +
          public_with_private_visibility$hours_unknown_status
      ),
      big.mark = ","
    ),
    sprintf(
      "(%.1f%% of their study time)\n\n",
      100 *
        sum(
          public_with_private_visibility$hours_confirmed_private +
            public_with_private_visibility$hours_unknown_status
        ) /
        sum(public_with_private_visibility$expected_total_hours)
    )
  )
}

# Unknown group (n=14)
# These participants have NO gaming data, so they're not in survey_active_visibility
# We need to calculate their hours manually
n_unknown <- length(unknown_pids)
if (n_unknown > 0) {
  # For participants with no gaming data and no visibility events,
  # all 84 days = 2016 hours are unknown
  total_hours_unknown_group <- n_unknown * 2016

  cat(
    "Unknown (no visibility tracking, no gaming data) (n=",
    n_unknown,
    "):\n",
    sep = ""
  )
  cat(
    "  Total hours in study period:",
    format(n_unknown * 2016, big.mark = ","),
    "\n"
  )
  cat("  Hours with gaming data: 0\n")
  cat("  Hours confirmed private (hidden): 0\n")
  cat(
    "  Hours unknown:",
    format(total_hours_unknown_group, big.mark = ","),
    "\n"
  )
  cat(
    "  → Total hours hidden or unknown:",
    format(total_hours_unknown_group, big.mark = ","),
    "(100.0% of their study time)\n\n"
  )
}

# Combined total for both groups
combined_pids <- c(public_with_private$pid, unknown_pids)
n_combined <- length(combined_pids)

# Calculate totals
# Public with private: sum from visibility data
total_hours_combined <- sum(
  public_with_private_visibility$expected_total_hours
) +
  (n_unknown * 2016)
total_hidden_unknown_combined <- sum(
  public_with_private_visibility$hours_confirmed_private +
    public_with_private_visibility$hours_unknown_status
) +
  (n_unknown * 2016)

cat("Combined total for both groups (n=", n_combined, "):\n", sep = "")
cat(
  "  Total hours in study period:",
  format(total_hours_combined, big.mark = ","),
  "\n"
)
cat(
  "  Total hours hidden or unknown:",
  format(total_hidden_unknown_combined, big.mark = ","),
  "\n"
)
cat(
  "  Proportion:",
  sprintf(
    "%.1f%%\n\n",
    100 * total_hidden_unknown_combined / total_hours_combined
  )
)

# Save results ------------------------------------------------------------

write_csv(
  steam_visibility_summary,
  "R/steam_visibility_summary.csv.gz"
)

cat("Results saved to: R/steam_visibility_summary.csv.gz\n\n")

# Create visualizations ---------------------------------------------------

if (!dir.exists("figures")) {
  dir.create("figures", recursive = TRUE)
}

# 1. Distribution of gaming activity
p1 <- ggplot(steam_visibility_summary, aes(x = proportion_public_with_gaming)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Distribution of Steam Gaming Activity",
    subtitle = sprintf(
      "Proportion of 84-day study period with gaming data (N=%d participants)",
      nrow(steam_visibility_summary)
    ),
    x = "Proportion of time with gaming data",
    y = "Number of participants"
  ) +
  theme_minimal()

ggsave(
  "figures/steam_visibility_distribution.png",
  p1,
  width = 10,
  height = 6,
  dpi = 300
)

# 2. Stacked bar comparing participants with/without visibility events
summary_by_event_status <- steam_visibility_summary |>
  mutate(
    event_group = ifelse(
      has_visibility_events,
      "With visibility events",
      "Without visibility events"
    )
  ) |>
  group_by(event_group) |>
  summarize(
    n = n(),
    `Gaming` = mean(proportion_public_with_gaming, na.rm = TRUE),
    `Public (no gaming)` = mean(proportion_public_no_gaming, na.rm = TRUE),
    `Private` = mean(proportion_hours_private, na.rm = TRUE),
    `Unknown` = mean(proportion_unknown, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = c(Gaming, `Public (no gaming)`, Private, Unknown),
    names_to = "Status",
    values_to = "Proportion"
  ) |>
  mutate(
    Status = factor(
      Status,
      levels = c("Gaming", "Public (no gaming)", "Private", "Unknown")
    ),
    label = sprintf("%s\n(n=%d)", event_group, n)
  )

p2 <- ggplot(
  summary_by_event_status,
  aes(x = label, y = Proportion, fill = Status)
) +
  geom_col(position = "stack") +
  scale_fill_manual(
    values = c(
      "Gaming" = "#2ecc71",
      "Public (no gaming)" = "#3498db",
      "Private" = "#e74c3c",
      "Unknown" = "#95a5a6"
    )
  ) +
  labs(
    title = "Average Time Allocation During Study Period",
    subtitle = "Comparison by availability of visibility event data",
    x = NULL,
    y = "Proportion of 84-day study period"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10))

ggsave(
  "figures/steam_visibility_breakdown.png",
  p2,
  width = 10,
  height = 6,
  dpi = 300
)

cat("Visualizations saved to:\n")
cat("  - figures/steam_visibility_distribution.png\n")
cat("  - figures/steam_visibility_breakdown.png\n\n")

# Demonstration of calculate_visibility_coverage function ----------------

cat("=== FUNCTION DEMONSTRATION ===\n\n")
cat(
  "The calculate_steam_visibility_coverage() function calculates visibility for individual participants.\n\n"
)

# Pick example participants from different categories
example_pids <- c()

# Get one from "Confirmed public only"
if (nrow(confirmed_public) > 0) {
  example_pids <- c(example_pids, confirmed_public$pid[1])
}

# Get one from "Public with private periods"
if (nrow(public_with_private) > 0) {
  example_pids <- c(example_pids, public_with_private$pid[1])
}

# Show examples
if (length(example_pids) > 0) {
  cat("Example calculations:\n\n")

  for (i in seq_along(example_pids)) {
    result <- calculate_steam_visibility_coverage(
      participant_id = example_pids[i],
      visibility_data = steam_visibility,
      gaming_data = steam_gaming,
      enrollment_data = intake
    )

    cat("Participant", example_pids[i], ":\n")
    cat("  Hours visible:", result$hours_visible, "\n")
    cat("  Hours hidden:", result$hours_hidden, "\n")
    cat("  Hours unknown:", result$hours_unknown, "\n")
    cat("  Hours total:", result$hours_total, "\n")
    cat(
      "  Proportion visible:",
      sprintf("%.1f%%", 100 * result$proportion_visible),
      "\n"
    )
    cat("  Has gaming data:", result$has_gaming_data, "\n")
    cat("  Has visibility events:", result$has_visibility_events, "\n\n")
  }

  cat("Usage:\n")
  cat("  result <- calculate_steam_visibility_coverage(\n")
  cat("    participant_id = 'p123456',\n")
  cat("    visibility_data = steam_visibility,\n")
  cat("    gaming_data = steam_gaming,\n")
  cat("    enrollment_data = intake\n")
  cat("  )\n\n")
}
