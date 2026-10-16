# Pull crowd-voted Steam Store tags from SteamSpy's third-party API for every
# unique Steam appid in data/raw/games.csv.gz. These are a much richer,
# player-generated vocabulary than IGDB's fixed genre/theme/keyword lists
# (e.g. "Difficult", "Replay Value", "Base Building", "Relaxing"). Same
# resumable-cache pattern as fetch_steam_appdetails.R. SteamSpy asks for
# roughly 1 request/second.

library(tidyverse)
library(httr2)
library(jsonlite)

STEAMSPY_CACHE_DIR <- "data/raw/steamspy"
STEAMSPY_FAILED_LOG <- "data/raw/steamspy_failed.txt"
STEAMSPY_REQUEST_DELAY_SECONDS <- 1.1
STEAMSPY_MAX_RETRIES <- 3

fetch_steamspy_one <- function(appid) {
  req <- request("https://steamspy.com/api.php") |>
    req_url_query(request = "appdetails", appid = appid) |>
    req_timeout(20) |>
    req_retry(max_tries = STEAMSPY_MAX_RETRIES, backoff = ~ 10)

  resp <- tryCatch(req_perform(req), error = function(e) NULL)
  if (is.null(resp) || resp_status(resp) != 200) {
    return(FALSE)
  }

  writeLines(resp_body_string(resp), file.path(STEAMSPY_CACHE_DIR, paste0(appid, ".json")))
  TRUE
}

# Fetch SteamSpy tags for every Steam appid not already cached or failed.
fetch_steamspy <- function() {
  dir.create(STEAMSPY_CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

  appids <- read_csv("data/raw/games.csv.gz", show_col_types = FALSE) |>
    filter(platform == "Steam") |>
    distinct(steam_id) |>
    pull(steam_id) |>
    as.character()

  already_cached <- str_remove(list.files(STEAMSPY_CACHE_DIR, pattern = "\\.json$"), "\\.json$")
  already_failed <- if (file.exists(STEAMSPY_FAILED_LOG)) readLines(STEAMSPY_FAILED_LOG) else character(0)
  todo <- setdiff(appids, union(already_cached, already_failed))

  cat(glue::glue(
    "{length(appids)} unique appids total | {length(already_cached)} cached | ",
    "{length(already_failed)} previously failed | {length(todo)} to fetch\n"
  ))

  n_ok <- 0
  n_fail <- 0

  for (i in seq_along(todo)) {
    appid <- todo[i]
    ok <- fetch_steamspy_one(appid)

    if (ok) {
      n_ok <- n_ok + 1
    } else {
      n_fail <- n_fail + 1
      write(appid, STEAMSPY_FAILED_LOG, append = TRUE)
    }

    if (i %% 50 == 0 || i == length(todo)) {
      cat(glue::glue(
        "[{i}/{length(todo)}] ok={n_ok} fail={n_fail} ",
        "({format(Sys.time(), '%H:%M:%S')})\n"
      ))
    }

    Sys.sleep(STEAMSPY_REQUEST_DELAY_SECONDS)
  }

  cat("Done fetching SteamSpy tags.\n")
}

# Parse cached SteamSpy JSON into one row per steam_id.
parse_steamspy <- function() {
  steamspy_files <- list.files(STEAMSPY_CACHE_DIR, pattern = "\\.json$", full.names = TRUE)

  parse_one <- function(path) {
    appid <- str_remove(basename(path), "\\.json$")
    d <- tryCatch(fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
    tag_names <- names(d$tags)

    tibble(
      steam_id = appid,
      steamspy_tags = if (length(tag_names) > 0) paste(tag_names, collapse = "; ") else NA_character_
    )
  }

  map(steamspy_files, parse_one) |> list_rbind()
}

if (sys.nframe() == 0) {
  fetch_steamspy()
}
