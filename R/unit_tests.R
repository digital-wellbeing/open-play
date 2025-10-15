suppressPackageStartupMessages({
  library(testthat)
  library(dplyr)
  library(tibble)
  library(lubridate)
})

source("R/helpers.R")

# Helpers ---------------------------------------------------------------
# Full UTC datetime, e.g. dt("2024-01-01 10:00:00")
dt <- function(x) ymd_hms(x, tz = "UTC")

# Time-of-day on a fixed date (avoids ambiguity across tests)
# e.g. t("10:00:00") -> 2024-01-01 10:00:00 UTC
t <- function(hms, date = "2024-01-01") ymd_hms(paste(date, hms), tz = "UTC")

# Use a summary reporter so the script exits non-zero on failures
rep <- SummaryReporter$new()
with_reporter(rep, {

  test_that("non-overlapping sessions remain unchanged", {
    input <- tibble(
      pid = c("u1", "u1"),
      title_id = c("g1", "g2"),
      session_start = dt(c("2024-01-01 10:00:00", "2024-01-01 12:00:00")),
      session_end   = dt(c("2024-01-01 11:00:00", "2024-01-01 13:00:00"))
    )

    out <- canonicalize_xbox_sessions(input)

    expect_equal(nrow(out), 2)
    expect_equal(out$title_id, c("g1","g2"))
    expect_true(all(tz(out$session_start) == "UTC"))
    expect_true(all(tz(out$session_end)   == "UTC"))
  })

  test_that("partially overlapping sessions: latest-start wins per atomic segment", {
    input <- tibble(
      pid = "u1",
      title_id = c("A","B"),
      session_start = t(c("10:00:00", "10:30:00")),
      session_end   = t(c("11:00:00", "11:30:00"))
    )

    out <- canonicalize_xbox_sessions(input, tol_sec = 0)

    # Expect [10:00-10:30]=A, [10:30-11:00]=B, [11:00-11:30]=B
    expect_equal(out$title_id, c("A","B"))
    expect_equal(out$session_start, t(c("10:00:00","10:30:00")))
    expect_equal(out$session_end,   t(c("10:30:00","11:30:00")))
  })

  test_that("fully nested session is split correctly", {
    input <- tibble(
      pid = "u1",
      title_id = c("outer","inner"),
      session_start = t(c("10:00:00","10:15:00")),
      session_end   = t(c("11:00:00","10:45:00"))
    )

    out <- canonicalize_xbox_sessions(input, tol_sec = 0)

    # [10:00-10:15]=outer, [10:15-10:45]=inner, [10:45-11:00]=outer
    expect_equal(out$title_id, c("outer","inner","outer"))
    expect_equal(out$session_start, t(c("10:00:00","10:15:00","10:45:00")))
    expect_equal(out$session_end,   t(c("10:15:00","10:45:00","11:00:00")))
  })

  test_that("segments with >=3 titles are dropped", {
    input <- tibble(
      pid = "u1",
      title_id = c("A","B","C"),
      session_start = t(c("10:00:00","10:10:00","10:20:00")),
      session_end   = t(c("10:40:00","10:30:00","10:25:00"))
    )

    out <- canonicalize_xbox_sessions(input, tol_sec = 0)

    # Nothing should cover [10:20-10:25]
    expect_true(all(out$session_end <= t("10:20:00") | out$session_start >= t("10:25:00")))
    expect_gt(nrow(out), 0)
  })

  test_that("tolerance merges short gaps only", {
    input <- tibble(
      pid = "u1",
      title_id = c("g1","g1"),
      session_start = t(c("10:00:00","11:00:05")),  # 5s gap
      session_end   = t(c("11:00:00","12:00:00"))
    )

    merged   <- canonicalize_xbox_sessions(input, tol_sec = 10)
    separate <- canonicalize_xbox_sessions(input, tol_sec = 4)

    expect_equal(nrow(merged), 1)
    expect_equal(merged$session_start[1], t("10:00:00"))
    expect_equal(merged$session_end[1],   t("12:00:00"))

    expect_equal(nrow(separate), 2)
  })

  test_that("invalid / NA / zero-length intervals are removed", {
    input <- tibble(
      pid = c("u1","u1","u1"),
      title_id = c("g1","g2","g3"),
      session_start = dt(c(NA, "2024-01-01 10:00:00", "2024-01-01 11:00:00")),
      session_end   = dt(c("2024-01-01 10:00:00", "2024-01-01 09:00:00", NA))
    )

    out <- canonicalize_xbox_sessions(input)
    expect_equal(nrow(out), 0)
  })

  test_that("metadata columns are preserved from first row when merging", {
    input <- tibble(
      pid = "u1",
      title_id = c("g1","g1"),
      platform = c("xbox-one","series-x"),   # meta column
      session_start = t(c("10:00:00","11:00:05")),
      session_end   = t(c("11:00:00","12:00:00"))
    )

    out <- canonicalize_xbox_sessions(input, tol_sec = 10)

    expect_equal(nrow(out), 1)
    expect_identical(out$platform[1], "xbox-one")
  })

  test_that("multiple users are processed independently", {
    input <- tibble(
      pid = c("u1","u1","u2","u2"),
      title_id = c("g1","g1","g2","g2"),
      session_start = t(c("10:00:00","11:00:00","10:00:00","10:30:00")),
      session_end   = t(c("10:30:00","11:30:00","10:30:00","11:00:00"))
    )

    out <- canonicalize_xbox_sessions(input, tol_sec = 60)

    expect_equal(nrow(filter(out, pid == "u1")), 2)
    expect_equal(nrow(filter(out, pid == "u2")), 1) # merged
  })

  test_that("empty input returns empty with same columns", {
    input <- tibble(
      pid = character(),
      title_id = character(),
      session_start = as_datetime(character(), tz = "UTC"),
      session_end   = as_datetime(character(), tz = "UTC")
    )

    out <- canonicalize_xbox_sessions(input)

    expect_equal(nrow(out), 0)
    expect_true(all(names(out) %in% names(input)))
  })
})

rep$end_reporter()
