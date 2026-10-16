# Build the full game metadata release file: IGDB genres/themes/ratings for
# Steam, Nintendo and Xbox (label_genres.R), plus Steam Store / review /
# SteamSpy enrichment for Steam only (fetch_steam_appdetails.R,
# fetch_steam_reviews.R, fetch_steamspy_tags.R). Each fetch stage caches its
# own API responses under data/raw/ and resumes automatically, so the whole
# thing is safe to interrupt and rerun from the top. Long-running (several
# hours total, dominated by the Steam Store and SteamSpy fetches) - run this
# directly rather than sourcing it from anything else.
#
# Steam-only columns from the three fetch stages are prefixed with `steam_`
# on output, since IGDB already contributes `genres`/`release_date` for all
# three platforms.

library(tidyverse)

source("R/label_genres.R")
source("R/fetch_steam_appdetails.R")
source("R/fetch_steam_reviews.R")
source("R/fetch_steamspy_tags.R")

## 1. IGDB metadata (genres, themes, ratings) - Steam, Nintendo, Xbox
igdb_results <- run_extraction()
game_metadata <- harmonize_genres(igdb_results)

## 2-4. Steam Store appdetails, review-score summary, SteamSpy tags
fetch_appdetails()
fetch_reviews()
fetch_steamspy()

steam_extra <- parse_appdetails() |>
  left_join(parse_reviews(), by = "steam_id") |>
  left_join(parse_steamspy(), by = "steam_id") |>
  rename_with(~ paste0("steam_", .x), -steam_id)

## 5. Attach Steam extras via the title_id/steam_id crosswalk in games.csv.gz.
## Keyed on platform too, not just title_id - some titles (e.g. "Bayonetta")
## exist on both Nintendo and Steam under the identical name, and matching on
## title_id alone would leak Steam metadata onto the non-Steam row.
crosswalk <- read_csv("data/raw/games.csv.gz", show_col_types = FALSE) |>
  filter(platform == "Steam") |>
  distinct(platform, title_id, steam_id) |>
  mutate(steam_id = as.character(steam_id))

game_metadata <- game_metadata |>
  left_join(crosswalk, by = c("platform", "original_name" = "title_id")) |>
  left_join(steam_extra, by = "steam_id")

dir.create("data/clean", showWarnings = FALSE, recursive = TRUE)
write_csv(game_metadata, OUTPUT_FILE)

message(glue::glue(
  "Saved {nrow(game_metadata)} games to {OUTPUT_FILE} ",
  "({sum(!is.na(game_metadata$steam_id))} with a Steam extras match)"
))
