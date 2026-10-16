# Pull Steam's review-score summary (sentiment, not just volume) for every
# unique Steam appid in data/raw/games.csv.gz. appdetails' metacritic_score
# is a sparse critic score and recommendations.total is just a count - this
# is the actual user positive/negative signal, via num_per_page=0 (summary
# only, no review text). Same resumable-cache pattern as
# fetch_steam_appdetails.R.

library(tidyverse)
library(httr2)
library(jsonlite)

REVIEWS_CACHE_DIR <- "data/raw/steam_reviews"
REVIEWS_FAILED_LOG <- "data/raw/steam_reviews_failed.txt"
REVIEWS_REQUEST_DELAY_SECONDS <- 1.3
REVIEWS_MAX_RETRIES <- 3

fetch_reviews_one <- function(appid) {
  req <- request(glue::glue("https://store.steampowered.com/appreviews/{appid}")) |>
    req_url_query(json = 1, num_per_page = 0, language = "all") |>
    req_timeout(20) |>
    req_retry(max_tries = REVIEWS_MAX_RETRIES, backoff = ~ 10)

  resp <- tryCatch(req_perform(req), error = function(e) NULL)
  if (is.null(resp) || resp_status(resp) != 200) {
    return(FALSE)
  }

  writeLines(resp_body_string(resp), file.path(REVIEWS_CACHE_DIR, paste0(appid, ".json")))
  TRUE
}

# Fetch review-score summaries for every Steam appid not already cached or failed.
fetch_reviews <- function() {
  dir.create(REVIEWS_CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

  appids <- read_csv("data/raw/games.csv.gz", show_col_types = FALSE) |>
    filter(platform == "Steam") |>
    distinct(steam_id) |>
    pull(steam_id) |>
    as.character()

  already_cached <- str_remove(list.files(REVIEWS_CACHE_DIR, pattern = "\\.json$"), "\\.json$")
  already_failed <- if (file.exists(REVIEWS_FAILED_LOG)) readLines(REVIEWS_FAILED_LOG) else character(0)
  todo <- setdiff(appids, union(already_cached, already_failed))

  cat(glue::glue(
    "{length(appids)} unique appids total | {length(already_cached)} cached | ",
    "{length(already_failed)} previously failed | {length(todo)} to fetch\n"
  ))

  n_ok <- 0
  n_fail <- 0

  for (i in seq_along(todo)) {
    appid <- todo[i]
    ok <- fetch_reviews_one(appid)

    if (ok) {
      n_ok <- n_ok + 1
    } else {
      n_fail <- n_fail + 1
      write(appid, REVIEWS_FAILED_LOG, append = TRUE)
    }

    if (i %% 50 == 0 || i == length(todo)) {
      cat(glue::glue(
        "[{i}/{length(todo)}] ok={n_ok} fail={n_fail} ",
        "({format(Sys.time(), '%H:%M:%S')})\n"
      ))
    }

    Sys.sleep(REVIEWS_REQUEST_DELAY_SECONDS)
  }

  cat("Done fetching reviews.\n")
}

# Parse cached review-score JSON into one row per steam_id.
parse_reviews <- function() {
  review_files <- list.files(REVIEWS_CACHE_DIR, pattern = "\\.json$", full.names = TRUE)

  parse_one <- function(path) {
    appid <- str_remove(basename(path), "\\.json$")
    raw <- tryCatch(fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
    qs <- raw$query_summary

    if (is.null(qs)) {
      return(tibble(steam_id = appid))
    }

    total_positive <- qs$total_positive %||% NA_integer_
    total_reviews <- qs$total_reviews %||% NA_integer_

    tibble(
      steam_id = appid,
      review_score = qs$review_score %||% NA_integer_,
      review_score_desc = qs$review_score_desc %||% NA_character_,
      total_positive = total_positive,
      total_negative = qs$total_negative %||% NA_integer_,
      total_reviews = total_reviews,
      pct_positive = if (!is.na(total_positive) && !is.na(total_reviews) && total_reviews > 0) {
        total_positive / total_reviews
      } else {
        NA_real_
      }
    )
  }

  map(review_files, parse_one) |> list_rbind()
}

if (sys.nframe() == 0) {
  fetch_reviews()
}
