idle_games <- c(
  "AdVenture Capitalist",
  "Banana",
  "Bard Idle",
  "Car Clicker",
  "Casting Clicker",
  "Clicker Clicker Clicker",
  "Clicker Guild",
  "Clicker Heroes",
  "Cookie Clicker",
  "Crusaders of the Lost Idols",
  "DPS IDLE",
  "DPS IDLE 2",
  "Dino Clicker",
  "Dog Clicker",
  "Farmer Against Potatoes Idle",
  "Fish Idle 2: Underwater Mystery",
  "Forager",
  "Galaxy Idle Clicker",
  "Honey Peach Clicker",
  "IDLE BOSS RUSH Demo",
  "Idle Awakening: Mages Path Demo",
  "Idle Baker Boss",
  "Idle Cave Miner",
  "Idle Champions of the Forgotten Realms",
  "Idle Circles Demo",
  "Idle Clans",
  "Idle Colony",
  "Idle Colony Demo",
  "Idle Dice 2",
  "Idle Fields",
  "Idle Fishing",
  "Idle Gem Quest",
  "Idle Grid",
  "Idle Hero TD",
  "Idle Research",
  "Idle Skilling",
  "Idle Slayer",
  "Idle Sphere",
  "Idle Spiral",
  "Idle Waters",
  "Incremental Adventures",
  "Incremental Cubes",
  "Incremental Epic Hero 2",
  "Industry Idle",
  "Kiwi Clicker",
  "Leaf Blower Revolution - Idle Game",
  "Melvor Idle",
  "Military Incremental Complex Demo",
  "Milky Way Idle",
  "Mini Idle Dice Monster",
  "Monster Hunting: Incremental Grind Forever",
  "Mr.Mine",
  "NGU IDLE",
  "Nomad Idle Demo",
  "Pickle Clicker",
  "Realm Grinder",
  "Reborn: An Idle Roguelike RPG",
  "Revolution Idle",
  "Ropuka's Idle Island",
  "Soda Dungeon",
  "Soda Dungeon 2",
  "Supply Chain Idle",
  "Time Clickers",
  "Trimps",
  "Typing Incremental",
  "Unnamed Space Idle",
  "Wizard And Minion Idle",
  "World of Talesworth: Idle MMO Simulator",
  "Zero IDLE",
  "Bongo Cat",
  "Cock",
  "Legends of Idleon MMO",
  "NGU INDUSTRIES"
)

non_games <- c("SteamVR", "tModLoader", "Stream Avatars", "Soundpad")

print_num <- function(num, accuracy = .1) {
  scales::number(num, accuracy = accuracy, scale_cut = scales::cut_long_scale())
}

offset_secs <- function(z) {
  z <- toupper(sub("^GMT", "", z))
  m <- regexec("^([+-])(\\d{2})(\\d{2})$", z)
  p <- regmatches(z, m)
  sapply(
    p,
    \(x) {
      if (length(x)) {
        (ifelse(x[2] == "-", -1, 1)) *
          (as.numeric(x[3]) * 3600 + as.numeric(x[4]) * 60)
      } else {
        0
      }
    }
  )
}

clip_to_window <- function(df) {
  df |>
    filter(end > window_start, start < window_end) |>
    mutate(
      start = pmax(start, window_start),
      end = pmin(end, window_end)
    ) |>
    filter(end > start)
}


merge_adjacent_sessions <- function(df, tol_sec = 60) {
  df2 <- df |>
    filter(
      !is.na(session_start),
      !is.na(session_end),
      session_end > session_start
    ) |>
    mutate(
      pid = as.character(pid),
      session_start = as.POSIXct(session_start, tz = "UTC"),
      session_end = as.POSIXct(session_end, tz = "UTC")
    )

  if (nrow(df2) == 0L) {
    return(df2[0, ])
  } # prevents min/max warnings

  # choose metadata columns once (before we add helpers), exclude boundaries and keys
  meta_cols <- setdiff(
    names(df2),
    c("pid", "title_id", "session_start", "session_end")
  )

  lazy_dt(df2, immutable = TRUE) |>
    arrange(pid, title_id, session_start, session_end) |>
    group_by(pid, title_id) |>
    mutate(
      start_num = as.numeric(session_start),
      end_num = as.numeric(session_end),
      gap_sec = start_num - lag(end_num),
      grp = cumsum(if_else(is.na(gap_sec) | gap_sec > tol_sec, 1L, 0L))
    ) |>
    group_by(pid, title_id, grp) |>
    summarise(
      session_start = min(session_start),
      session_end = max(session_end),
      across(all_of(meta_cols), first),
      .groups = "drop"
    ) |>
    mutate(
      duration = as.numeric(difftime(
        session_end,
        session_start,
        units = "mins"
      ))
    ) |>
    arrange(pid, session_start) |>
    as_tibble()
}


resolve_overlaps <- function(df) {
  base <- df |>
    filter(
      !is.na(session_start),
      !is.na(session_end),
      session_end > session_start
    ) |>
    mutate(
      pid = as.character(pid),
      session_start = as.POSIXct(session_start, tz = "UTC"),
      session_end = as.POSIXct(session_end, tz = "UTC")
    ) |>
    arrange(pid, session_start, session_end)

  if (nrow(base) == 0L) {
    return(
      base |>
        mutate(.had_overlap = FALSE, .max_titles = 1L, .n_slices = 1L)
    )
  }

  # --- atomic segments (tidyverse) ---
  segs <- base |>
    group_by(pid) |>
    reframe(cuts = sort(unique(c(session_start, session_end)))) |>
    mutate(seg_start = cuts, seg_end = lead(cuts)) |>
    filter(!is.na(seg_end), seg_end > seg_start) |>
    select(pid, seg_start, seg_end) |>
    ungroup()

  # --- non-equi overlap join (tidyverse; dtplyr doesn't translate this) ---
  olap <- inner_join(
    segs,
    base,
    by = join_by(pid, seg_start < session_end, seg_end > session_start)
  )

  if (nrow(olap) == 0L) {
    return(
      base |>
        mutate(.had_overlap = FALSE, .max_titles = 1L, .n_slices = 1L) |>
        arrange(pid, session_start)
    )
  }

  # --- choose rows for segments with <3 titles (dtplyr-accelerated) ---
  assigned <- olap |>
    lazy_dt(immutable = TRUE) |>
    group_by(pid, seg_start, seg_end) |>
    mutate(.n_titles_seg = n_distinct(title_id)) |>
    filter(.n_titles_seg < 3L) |>
    slice_max(session_start, with_ties = FALSE) |>
    ungroup() |>
    mutate(
      session_start = seg_start,
      session_end = seg_end
    ) |>
    select(-seg_start, -seg_end) |>
    as_tibble()

  if (nrow(assigned) == 0L) {
    return(
      base[0, ] |>
        tibble::add_column(
          .had_overlap = logical(),
          .max_titles = integer(),
          .n_slices = integer()
        )
    )
  }

  # set metadata columns once; avoid helpers in summarise
  meta_cols <- setdiff(
    names(assigned),
    c("pid", "title_id", "session_start", "session_end", ".n_titles_seg")
  )

  # --- stitch contiguous slices per pid+title_id (dtplyr-accelerated) ---
  out <- assigned |>
    lazy_dt(immutable = TRUE) |>
    arrange(pid, title_id, session_start, session_end) |>
    group_by(pid, title_id) |>
    mutate(
      gap0 = as.numeric(session_start) - lag(as.numeric(session_end)),
      grp = cumsum(if_else(is.na(gap0) | gap0 > 0, 1L, 0L))
    ) |>
    group_by(pid, title_id, grp) |>
    summarise(
      session_start = min(session_start),
      session_end = max(session_end),
      .had_overlap = any(.n_titles_seg >= 2L),
      .max_titles = max(.n_titles_seg, na.rm = TRUE),
      .n_slices = n(),
      across(any_of(meta_cols), first),
      .groups = "drop"
    ) |>
    mutate(
      duration = as.numeric(difftime(
        session_end,
        session_start,
        units = "mins"
      ))
    ) |>
    arrange(pid, session_start) |>
    as_tibble()

  out
}


avg_days_platform <- function(pltf, pids_filter = NULL) {
  data <- daily_all |> filter(platform == pltf)
  if (!is.null(pids_filter)) {
    data <- data |> filter(pid %in% pids_filter)
  }
  data |>
    group_by(pid) |>
    summarise(days = sum(minutes > 0, na.rm = TRUE), .groups = "drop") |>
    summarise(avg = median(days, na.rm = TRUE), .groups = "drop") |>
    pull(avg) %||%
    NA_real_
}

total_hours <- function(df) {
  mm <- if ("duration" %in% names(df)) {
    df$duration
  } else if ("minutes" %in% names(df)) {
    df$minutes
  } else {
    NULL
  }
  if (is.null(mm)) NA_real_ else sum(mm, na.rm = TRUE) / 60
}

summ_basic <- function(df, pid, label, avg_days = NA_real_) {
  by_pid <- df |> count({{ pid }}, name = "events")
  tibble(
    `Data type` = label,
    Participants = n_distinct(pull(df, {{ pid }})),
    Events = nrow(df),
    Hours = total_hours(df),
    `Median events / user` = median(by_pid$events, na.rm = TRUE),
    `Median active days` = avg_days
  )
}

fmt_row <- \(region, type, meas, n_total, n_eligible, max_possible, miss_vec) {
  tot_miss <- sum(miss_vec, na.rm = TRUE)
  tibble(
    region = region,
    data_type = type,
    measure = meas,
    n_total = n_total,
    n_participants = n_eligible, # "N with survey data" later
    maximum_possible = max_possible,
    total_observed = max(0, max_possible - tot_miss),
    total_missing = tot_miss,
    median_missing_per_participant = suppressWarnings(median(
      miss_vec,
      na.rm = TRUE
    )),
    max_missing_per_participant = suppressWarnings(max(miss_vec, na.rm = TRUE))
  )
}
