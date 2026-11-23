# diary_dates.R: repairs for the two dating defects of the time-use diary.
#
# The diary describes one day from 04:00 to 04:00 the next morning. The
# instrument built each episode's date from the device clock when a block was
# placed or resized, which causes two defects:
#
#   Month roll-over. On the 1st of a month, times after midnight were stamped
#   with the 1st of the PREVIOUS month: an episode crossing midnight ends a
#   month before it starts (negative duration), and one placed wholly after
#   midnight is a month early. repair_rollover() adds the month back.
#
#   Midnight. "Yesterday" was the calendar day before the device's date, which
#   changes at midnight, four hours before the diary day does. A diary filled
#   in between 00:00 and 03:59 was therefore dated as the day still in
#   progress, one day late, and one filled in across midnight was split over
#   two days. repair_midnight() moves such episodes back to the day before the
#   one in which the diary was filled in.
#
# Both are fixed in the instrument from O-TUD v1.1.0, which records one
# reference day per diary. Times are the participant's local wall clock.
# The functions use dplyr and lubridate only.

# Adds one month to the start or end times stamped a month early. Within each
# diary (`by`) the anchor is the earliest start at or after 04:00 on the clock,
# the start of the day described. A time before the anchor by more than a week
# gets one month added (the month-early stamps lie 27 to 30 days before it;
# an ordinary episode just before 04:00 is never moved). Adds the logical
# column `rollover` (the row was changed).
repair_rollover <- function(ep, by) {
  anchor_of <- function(t) {
    t <- t[hour(t) >= 4]
    if (length(t) == 0) return(as.POSIXct(NA, tz = "UTC"))
    min(t)
  }
  early <- function(t, anchor) {
    !is.na(anchor) & t < anchor & difftime(anchor, t, units = "days") > 7
  }
  ep |>
    group_by(across(all_of(by))) |>
    mutate(.anchor = anchor_of(start_time)) |>
    ungroup() |>
    mutate(.fix_start = early(start_time, .anchor),
           .fix_end   = early(end_time, .anchor),
           rollover   = .fix_start | .fix_end,
           start_time = if_else(.fix_start, start_time %m+% months(1),
                                start_time),
           end_time   = if_else(.fix_end, end_time %m+% months(1), end_time)) |>
    select(-c(.anchor, .fix_start, .fix_end))
}

# Moves back the episodes dated on the day in which their diary was filled in,
# or later. `filled` names a Date column: the 04:00-to-04:00 day in which the
# diary was filled in on the participant's clock (for a diary filled in at
# 01:30 on the 21st, the 20th). A diary of "yesterday" describes the day before
# it, so an episode whose own 04:00 day is that day or later is moved back by
# the difference plus one day. Rows without a day of filling are left as they
# are. Run it after repair_rollover(). Adds the logical column `moved_midnight`.
repair_midnight <- function(ep, filled = "filled_day") {
  ep |>
    mutate(.shift = as.integer(as_date(start_time - hours(4)) -
                                 .data[[filled]]) + 1L,
           moved_midnight = !is.na(.shift) & .shift > 0L,
           .shift     = if_else(moved_midnight, .shift, 0L),
           start_time = start_time - days(.shift),
           end_time   = end_time - days(.shift)) |>
    select(-.shift)
}

# Local wall clock for UTC instants, by the rule used for the telemetry
# (get_iana_timezone() and get_dst_offset() in R/helpers.R). NA where the
# participant has no usable time zone; a missing local_timezone is tested on
# its own, because get_iana_timezone() would read it as GMT+0000.
to_local_clock <- function(utc, country, local_timezone) {
  has_zone <- !is.na(utc) & !is.na(local_timezone) &
    !is.na(get_iana_timezone(country, local_timezone))
  out <- utc
  out[has_zone] <- utc[has_zone] +
    get_dst_offset(utc[has_zone], country[has_zone], local_timezone[has_zone])
  out[!has_zone] <- NA
  out
}
