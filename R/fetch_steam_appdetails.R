# Pull rich per-game metadata from the Steam Store appdetails API for every
# unique Steam appid in data/raw/games.csv.gz. Long-running (~10k appids at a
# conservative pace = several hours) against an unauthenticated public
# endpoint with an undocumented rate limit, so each appid's raw JSON response
# is cached to disk as it goes (data/raw/steam_appdetails/<appid>.json) and
# skipped if already cached - safe to interrupt and rerun. Persistent
# failures (after retries) are logged to data/raw/steam_appdetails_failed.txt
# and skipped on subsequent runs too; delete lines from that file to retry.

library(tidyverse)
library(httr2)
library(jsonlite)

APPDETAILS_CACHE_DIR <- "data/raw/steam_appdetails"
APPDETAILS_FAILED_LOG <- "data/raw/steam_appdetails_failed.txt"
APPDETAILS_REQUEST_DELAY_SECONDS <- 1.3
APPDETAILS_MAX_RETRIES <- 3

fetch_appdetails_one <- function(appid) {
  req <- request("https://store.steampowered.com/api/appdetails") |>
    req_url_query(appids = appid, cc = "us", l = "english") |>
    req_timeout(20) |>
    req_retry(max_tries = APPDETAILS_MAX_RETRIES, backoff = ~ 10)

  resp <- tryCatch(req_perform(req), error = function(e) NULL)
  if (is.null(resp) || resp_status(resp) != 200) {
    return(FALSE)
  }

  writeLines(resp_body_string(resp), file.path(APPDETAILS_CACHE_DIR, paste0(appid, ".json")))
  TRUE
}

# Fetch appdetails for every Steam appid not already cached or failed.
fetch_appdetails <- function() {
  dir.create(APPDETAILS_CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

  appids <- read_csv("data/raw/games.csv.gz", show_col_types = FALSE) |>
    filter(platform == "Steam") |>
    distinct(steam_id) |>
    pull(steam_id) |>
    as.character()

  already_cached <- str_remove(list.files(APPDETAILS_CACHE_DIR, pattern = "\\.json$"), "\\.json$")
  already_failed <- if (file.exists(APPDETAILS_FAILED_LOG)) readLines(APPDETAILS_FAILED_LOG) else character(0)
  todo <- setdiff(appids, union(already_cached, already_failed))

  cat(glue::glue(
    "{length(appids)} unique appids total | {length(already_cached)} cached | ",
    "{length(already_failed)} previously failed | {length(todo)} to fetch\n"
  ))

  n_ok <- 0
  n_fail <- 0

  for (i in seq_along(todo)) {
    appid <- todo[i]
    ok <- fetch_appdetails_one(appid)

    if (ok) {
      n_ok <- n_ok + 1
    } else {
      n_fail <- n_fail + 1
      write(appid, APPDETAILS_FAILED_LOG, append = TRUE)
    }

    if (i %% 50 == 0 || i == length(todo)) {
      cat(glue::glue(
        "[{i}/{length(todo)}] ok={n_ok} fail={n_fail} ",
        "({format(Sys.time(), '%H:%M:%S')})\n"
      ))
    }

    Sys.sleep(APPDETAILS_REQUEST_DELAY_SECONDS)
  }

  cat("Done fetching appdetails.\n")
}

# Parse cached appdetails JSON into one row per steam_id.
parse_appdetails <- function() {
  cache_files <- list.files(APPDETAILS_CACHE_DIR, pattern = "\\.json$", full.names = TRUE)

  parse_one <- function(path) {
    appid <- str_remove(basename(path), "\\.json$")
    raw <- tryCatch(fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
    entry <- raw[[appid]]

    if (is.null(entry) || !isTRUE(entry$success) || is.null(entry$data)) {
      return(tibble(steam_id = appid))
    }

    d <- entry$data

    tibble(
      steam_id = appid,
      store_name = d$name %||% NA_character_,
      store_type = d$type %||% NA_character_,
      genres = d$genres |> map_chr(~ .x$description %||% NA_character_) |> paste(collapse = "; ") |> na_if(""),
      categories = d$categories |> map_chr(~ .x$description %||% NA_character_) |> paste(collapse = "; ") |> na_if(""),
      metacritic_score = d$metacritic$score %||% NA_integer_,
      recommendations_total = d$recommendations$total %||% NA_integer_,
      release_date = d$release_date$date %||% NA_character_,
      coming_soon = d$release_date$coming_soon %||% NA,
      required_age = if (is.null(d$required_age)) NA_integer_ else suppressWarnings(as.integer(d$required_age)),
      dlc_count = length(d$dlc %||% list()),
      is_free = d$is_free %||% NA,
      price_usd_final = if (!is.null(d$price_overview$final)) d$price_overview$final / 100 else NA_real_,
      short_description = d$short_description %||% NA_character_
    )
  }

  map(cache_files, parse_one) |> list_rbind()
}

if (sys.nframe() == 0) {
  fetch_appdetails()
}
