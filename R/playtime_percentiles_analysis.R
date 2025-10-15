# Playtime Percentiles Analysis
# This script calculates total playtime across platforms and visualizes
# daily gaming patterns for participants at 25th, 50th, and 75th percentiles

# ============================================================================
# 1. Setup and Data Loading
# ============================================================================

library(tidyverse)
library(lubridate)
library(patchwork)

# Load custom functions
source("R/helpers.R")

set.seed(8675309)
options(scipen = 999)

# Apply theme settings from index.qmd
theme_set(theme_minimal())
theme_update(
  strip.background = element_rect(fill = "black"),
  strip.text = element_text(color = "white", size = 10),
  panel.grid.minor = element_blank(),
  panel.border = element_rect(
    colour = "black",
    fill = NA,
    linewidth = 1
  ),
)

# Load data
cat("Loading data...\n")
intake <- read_csv("data/clean/intake.csv.gz", show_col_types = FALSE)
panel <- read_csv("data/clean/panel.csv.gz", show_col_types = FALSE)
nintendo <- read_csv("data/clean/nintendo.csv.gz", show_col_types = FALSE)
steam <- read_csv("data/clean/steam.csv.gz", show_col_types = FALSE)
xbox <- read_csv("data/clean/xbox.csv.gz", show_col_types = FALSE)

# ============================================================================
# 2. Replicate Preprocessing from index.qmd
# ============================================================================

cat("Preprocessing telemetry data...\n")

# Merge with the local time zone offsets from intake
tz_map <- intake |>
  transmute(pid = as.character(pid), off = offset_secs(local_timezone)) |>
  distinct(pid, .keep_all = TRUE)

# --- 1) SESSION-LEVEL (Nintendo + Xbox) ------------------------------------
sessions_all <- bind_rows(
  xbox %>%
    transmute(
      pid = paste0("p", as.character(pid)),  # Add "p" prefix to Xbox PIDs
      platform = "Xbox",
      start_utc = session_start,
      end_utc = session_end
    ),
  nintendo %>%
    transmute(
      pid = as.character(pid),
      platform = "Nintendo",
      start_utc = session_start,
      end_utc = session_end
    )
) %>%
  left_join(tz_map, by = "pid") %>%
  mutate(
    start_local = start_utc + off,
    end_local = end_utc + off,
    duration_min = as.numeric(difftime(end_utc, start_utc, units = "mins"))
  ) %>%
  filter(
    !is.na(start_utc),
    !is.na(end_utc),
    end_utc > start_utc,
    duration_min >= 1
  )

# --- 2) HOURLY (Nintendo + Xbox expanded) ----------------------------------
# expand sessions into local-hour bins, compute overlap minutes, and add UTC hour
hourly_from_sessions <- sessions_all %>%
  filter(!is.na(off)) |>
  mutate(
    h0_local = floor_date(start_local, "hour"),
    h1_local = floor_date(end_local - seconds(1), "hour"),
    n_hours = as.integer(difftime(h1_local, h0_local, units = "hours")) + 1
  ) %>%
  tidyr::uncount(n_hours, .remove = FALSE, .id = "k") |>
  mutate(
    hour_start_local = h0_local + hours(k - 1),
    minutes = pmax(
      0,
      as.numeric(difftime(
        pmin(end_local, hour_start_local + hours(1)),
        pmax(start_local, hour_start_local),
        units = "mins"
      ))
    ),
    hour_start_utc = hour_start_local - off
  ) |>
  select(pid, platform, hour_start_local, hour_start_utc, minutes) %>%
  group_by(pid, platform, hour_start_local, hour_start_utc) %>%
  summarise(minutes = sum(minutes, na.rm = TRUE), .groups = "drop")

hourly_from_steam <- steam |>
  select(pid, datetime_hour_start, minutes) |>
  mutate(pid = as.character(pid)) |>
  left_join(tz_map, by = "pid") |>
  transmute(
    pid,
    platform = "Steam",
    hour_start_utc = datetime_hour_start,
    hour_start_local = datetime_hour_start + off,
    minutes
  ) %>%
  group_by(pid, platform, hour_start_local, hour_start_utc) %>%
  summarise(minutes = sum(minutes, na.rm = TRUE), .groups = "drop")

hourly_all <- bind_rows(hourly_from_sessions, hourly_from_steam)

# --- 3) DAILY (Nintendo + Xbox + Steam; collapse hourly to days) -----------
daily_all <- hourly_all |>
  mutate(
    day_local = as.Date(hour_start_local),
  ) |>
  group_by(pid, platform, day_local) |>
  summarise(minutes = sum(minutes, na.rm = TRUE), .groups = "drop")

# ============================================================================
# 3. Determine Study Start Date and Calculate Total Playtime
# ============================================================================

cat("Calculating study start dates and total playtime...\n")

# Find each participant's study start date (first panel wave)
study_start_dates <- panel |>
  group_by(pid) |>
  summarise(study_start = min(date, na.rm = TRUE), .groups = "drop") |>
  mutate(pid = as.character(pid))

# Join study start dates and calculate study day
daily_with_study_day <- daily_all |>
  left_join(study_start_dates, by = "pid") |>
  filter(!is.na(study_start)) |>
  mutate(
    study_day = as.integer(floor(as.numeric(difftime(day_local, study_start, units = "days")))) + 1
  ) |>
  filter(study_day >= 1 & study_day <= 84)

# Calculate total playtime per participant (sum across all platforms and days 1-84)
total_playtime <- daily_with_study_day |>
  group_by(pid) |>
  summarise(
    total_minutes = sum(minutes, na.rm = TRUE),
    n_platforms = n_distinct(platform[minutes > 0]),
    .groups = "drop"
  )

# Calculate percentiles
percentiles <- quantile(total_playtime$total_minutes, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
cat("\nPlaytime Percentiles (minutes):\n")
cat(sprintf("  25th percentile: %.1f\n", percentiles[1]))
cat(sprintf("  50th percentile: %.1f\n", percentiles[2]))
cat(sprintf("  75th percentile: %.1f\n", percentiles[3]))

# Find participant closest to each percentile based on total playtime
find_closest_participant <- function(target_value, data) {
  data |>
    mutate(distance = abs(total_minutes - target_value)) |>
    slice_min(distance, n = 1, with_ties = FALSE) |>
    pull(pid)
}

selected_pids <- c(
  "p25" = find_closest_participant(percentiles[1], total_playtime),
  "p50" = find_closest_participant(percentiles[2], total_playtime),
  "p75" = find_closest_participant(percentiles[3], total_playtime)
)

cat("\nSelected Participants:\n")
for (i in 1:3) {
  pname <- names(selected_pids)[i]
  pid <- selected_pids[i]
  playtime <- total_playtime |> filter(pid == !!pid) |> pull(total_minutes)
  cat(sprintf("  %s: PID %s (%.1f minutes total)\n", pname, pid, playtime))
}

# ============================================================================
# 4. Prepare Wellbeing Data
# ============================================================================

cat("\nPreparing wellbeing data...\n")

# Process panel data to get wellbeing scores
# Panel waves occur every 14 days starting from study start
panel_wellbeing <- panel |>
  mutate(pid = as.character(pid)) |>
  left_join(study_start_dates, by = "pid") |>
  filter(!is.na(study_start)) |>
  mutate(
    study_day = as.integer(floor(as.numeric(difftime(date, study_start, units = "days")))) + 1
  ) |>
  filter(pid %in% selected_pids) |>
  # Calculate wellbeing composite scores
  rowwise() |>
  mutate(
    # WEMWBS - mental wellbeing (14 items, scale 1-5 each, summed for total score 14-70)
    wemwbs = sum(c_across(starts_with("wemwbs_")), na.rm = TRUE)
  ) |>
  ungroup() |>
  select(pid, study_day, wave, wemwbs) |>
  mutate(
    percentile = case_when(
      pid == selected_pids["p25"] ~ "25th Percentile",
      pid == selected_pids["p50"] ~ "50th Percentile (Median)",
      pid == selected_pids["p75"] ~ "75th Percentile",
      TRUE ~ NA_character_
    ),
    percentile = factor(percentile, levels = c("25th Percentile", "50th Percentile (Median)", "75th Percentile"))
  )

cat("\nWellbeing data summary:\n")
print(panel_wellbeing |> select(pid, study_day, wave, wemwbs), n = 20)

# ============================================================================
# 5. Create Visualization
# ============================================================================

cat("\nCreating visualization...\n")

# Diagnostic: Check if selected PIDs exist in daily_with_study_day
cat("\nDiagnostic - Checking for selected PIDs in daily_with_study_day:\n")
for (i in 1:3) {
  pname <- names(selected_pids)[i]
  pid <- selected_pids[i]
  count <- daily_with_study_day |> filter(pid == !!pid) |> nrow()
  cat(sprintf("  %s (%s): %d rows found\n", pname, pid, count))
}

# Prepare data for selected participants
plot_data <- daily_with_study_day |>
  filter(pid %in% selected_pids) |>
  mutate(
    percentile = case_when(
      pid == selected_pids["p25"] ~ "25th Percentile",
      pid == selected_pids["p50"] ~ "50th Percentile (Median)",
      pid == selected_pids["p75"] ~ "75th Percentile",
      TRUE ~ NA_character_
    ),
    percentile = factor(percentile, levels = c("25th Percentile", "50th Percentile (Median)", "75th Percentile"))
  )

cat("\nDiagnostic - plot_data before join:\n")
cat(sprintf("  Total rows: %d\n", nrow(plot_data)))
cat(sprintf("  Total minutes: %.1f\n", sum(plot_data$minutes, na.rm = TRUE)))
print(head(plot_data |> select(pid, platform, study_day, minutes), 10))

# Create a complete grid of study days for each participant
complete_grid <- expand_grid(
  pid = selected_pids,
  study_day = 1:84,
  platform = unique(daily_all$platform)
) |>
  mutate(
    percentile = case_when(
      pid == selected_pids["p25"] ~ "25th Percentile",
      pid == selected_pids["p50"] ~ "50th Percentile (Median)",
      pid == selected_pids["p75"] ~ "75th Percentile",
      TRUE ~ NA_character_
    ),
    percentile = factor(percentile, levels = c("25th Percentile", "50th Percentile (Median)", "75th Percentile"))
  )

# Fill in missing days with 0 minutes
plot_data_complete <- complete_grid |>
  left_join(plot_data |> select(pid, study_day, platform, minutes), 
            by = c("pid", "study_day", "platform")) |>
  mutate(minutes = replace_na(minutes, 0))

# Diagnostic: Check data availability
cat("\nDiagnostic - Data summary for selected participants:\n")
data_summary <- plot_data_complete |>
  group_by(percentile, platform) |>
  summarise(
    total_minutes = sum(minutes, na.rm = TRUE),
    days_with_play = sum(minutes > 0, na.rm = TRUE),
    .groups = "drop"
  )
print(data_summary, n = Inf)

# No transformation needed - we'll use original WEMWBS scale (1-5)
# We just need to know the max playtime for each percentile to set up the secondary axis
max_playtime_by_percentile <- plot_data_complete |>
  group_by(percentile, study_day) |>
  summarise(daily_total = sum(minutes, na.rm = TRUE), .groups = "drop") |>
  group_by(percentile) |>
  summarise(max_minutes = max(daily_total, na.rm = TRUE), .groups = "drop")

# Create separate plots for each percentile to handle dual y-axes properly
plots_list <- list()

pct_levels <- levels(plot_data_complete$percentile)

for (i in seq_along(pct_levels)) {
  pct_level <- pct_levels[i]
  is_bottom <- (i == length(pct_levels))
  is_middle <- (i == 2)  # Middle facet for y-axis labels
  
  # Filter data for this percentile
  plot_data_pct <- plot_data_complete |> filter(percentile == pct_level)
  wellbeing_data_pct <- panel_wellbeing |> filter(percentile == pct_level)
  
  # Get max playtime for scaling
  max_playtime <- max_playtime_by_percentile |> 
    filter(percentile == pct_level) |> 
    pull(max_minutes)
  
  # We want the wellbeing axis to always show 14-70
  # Calculate coefficient so that wellbeing 70 maps to the max playtime value
  # For primary axis: (wellbeing - 14) * coeff
  # For wellbeing=70: (70-14) * coeff = max_playtime
  # So: coeff = max_playtime / 56
  coeff <- max_playtime / 56
  
  # The primary axis limit that corresponds to wellbeing=70 on secondary axis
  # This is: (70 - 14) * coeff = 56 * coeff = max_playtime
  # We add a tiny buffer to ensure 70 is visible
  y_max_for_wb70 <- (70 - 14) * coeff
  
  p_temp <- ggplot() +
    # Playtime bars (primary y-axis)
    geom_col(
      data = plot_data_pct,
      aes(x = study_day, y = minutes, fill = platform),
      width = 1
    ) +
    # Wellbeing line (secondary y-axis) - transform to playtime scale
    geom_line(
      data = wellbeing_data_pct,
      aes(x = study_day, y = (wemwbs - 14) * coeff, group = 1),
      color = "#FF6B35",
      linewidth = 1.2,
      linetype = "solid"
    ) +
    geom_point(
      data = wellbeing_data_pct,
      aes(x = study_day, y = (wemwbs - 14) * coeff, group = 1),
      color = "#FF6B35",
      size = 3,
      shape = 21,
      fill = "white",
      stroke = 1.5
    ) +
    scale_fill_manual(
      values = c(
        "Nintendo" = "#E60012",  # Nintendo red
        "Steam" = "#1B2838",     # Steam dark blue
        "Xbox" = "#107C10"       # Xbox green
      ),
      name = "Platform"
    ) +
    scale_x_continuous(
      name = if (is_bottom) "Study Day" else NULL,
      breaks = seq(0, 84, by = 14),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      name = if (is_middle) "Total Daily Playtime (minutes)" else NULL,
      limits = c(0, y_max_for_wb70 * 1.05),
      expand = c(0, 0),
      sec.axis = sec_axis(
        transform = ~ . / coeff + 14,
        name = if (is_middle) "Wellbeing (WEMWBS)" else NULL,
        breaks = seq(14, 70, by = 7)
      )
    ) +
    ggtitle(pct_level) +
    theme(
      axis.title.y.right = if (is_middle) element_text(color = "#FF6B35", size = 10) else element_blank(),
      axis.text.y.right = element_text(color = "#FF6B35", size = 9),
      axis.title.y.left = if (is_middle) element_text(size = 10) else element_blank(),
      plot.title = element_text(size = 11, hjust = 0.5),
      axis.title.x = if (is_bottom) element_text() else element_blank()
    )
  
  plots_list[[pct_level]] <- p_temp
}

# Combine plots using patchwork
p <- wrap_plots(plots_list, ncol = 1, guides = "collect") +
  plot_annotation(
    title = "Daily Gaming Patterns and Wellbeing Across Platforms",
    subtitle = "Sample participant timeseries with biweekly wellbeing (WEMWBS) in orange",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11)
    )
  ) &
  theme(legend.position = "bottom")

# ============================================================================
# 6. Save Output
# ============================================================================

# Create output directory if it doesn't exist
if (!dir.exists("outputs")) {
  dir.create("outputs")
}

# Save the plot
output_file <- "outputs/playtime_percentiles_plot.png"
ggsave(
  output_file,
  plot = p,
  width = 12,
  height = 10,
  dpi = 300,
  bg = "white"
)

cat(sprintf("\nPlot saved to: %s\n", output_file))

# Save summary statistics
summary_stats <- total_playtime |>
  filter(pid %in% selected_pids) |>
  mutate(
    percentile = case_when(
      pid == selected_pids["p25"] ~ "25th",
      pid == selected_pids["p50"] ~ "50th",
      pid == selected_pids["p75"] ~ "75th"
    ),
    total_hours = total_minutes / 60
  ) |>
  select(percentile, pid, total_minutes, total_hours) |>
  arrange(match(percentile, c("25th", "50th", "75th")))

cat("\n=== Summary Statistics ===\n")
print(summary_stats, n = Inf)

cat("\nAnalysis complete!\n")

