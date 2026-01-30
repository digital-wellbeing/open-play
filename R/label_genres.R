# label_genres.R
# Retrieve game metadata from IGDB API and harmonize genres across platforms

library(tidyverse)
library(httr)
library(httr2)
library(stringdist)
library(glue)

# Configuration
TEST_MODE <- FALSE
TEST_N <- 10
INPUT_FILE <- "data/raw/games.csv.gz"
OUTPUT_FILE <- "data/clean/game_metadata.csv.gz"
PROGRESS_FILE <- "data/qc/game_metadata_progress.rds"
RATE_LIMIT_DELAY <- 0.26

# Platforms to skip IGDB lookup (e.g., blinded identifiers instead of titles)
SKIP_PLATFORMS <- c("Xbox")

# Xbox telemetry file (for merging existing genre labels)
XBOX_DATA_FILE <- "data/clean/xbox.csv.gz"

# Genre mapping: Xbox/Microsoft genre → IGDB genre
# Maps Xbox taxonomy to IGDB for harmonization
GENRE_MAPPING <- tribble(
  ~xbox_genre                        , ~igdb_genre          ,
  "Shooter"                          , "Shooter"            ,
  "Role Playing"                     , "Role-playing (RPG)" ,
  "Action + Adventure"               , "Adventure"          ,
  "Puzzle + Trivia"                  , "Puzzle"             ,
  "Sports"                           , "Sport"              ,
  "Simulation"                       , "Simulator"          ,
  "Racing + Flying"                  , "Racing"             ,
  "Multi-Player Online Battle Arena" , "MOBA"               ,
  "Platformer"                       , "Platform"           ,
  "Fighting"                         , "Fighting"           ,
  "Casino"                           , "Card & Board Game"  ,
  "Strategy"                         , "Strategy"           ,
  "Classics"                         , "Arcade"             ,
  "Music"                            , "Music"              ,
  "Card + Board"                     , "Card & Board Game"  ,
  "Family + Kids"                    , NA_character_        ,
  "Other"                            , NA_character_
)

# Collapse list field to comma-separated string
collapse_names <- function(x) {
  if (length(x) == 0) {
    return(NA_character_)
  }
  paste(map_chr(x, "name"), collapse = ", ")
}

# IGDB organization names for age ratings
RATING_ORGS <- c("ESRB" = "ESRB", "PEGI" = "PEGI")

# Extract age rating from fetched ratings list
# ratings_data should have organization.name and rating_category.rating expanded
extract_rating <- function(ratings_data, org_name) {

  for (ar in ratings_data %||% list()) {
    if (isTRUE(ar$organization$name == org_name)) {
      return(ar$rating_category$rating %||% NA_character_)
    }
  }
  NA_character_
}

# Fetch age ratings by IDs with expanded organization and rating_category
get_age_ratings <- function(age_rating_ids, auth) {
  if (length(age_rating_ids) == 0) {
    return(list())
  }
  ids_str <- paste(age_rating_ids, collapse = ",")
  igdb_query(
    "age_ratings",
    glue("fields organization.name,rating_category.rating; where id = ({ids_str});"),
    auth
  )
}

# Authenticate with Twitch for IGDB access
authenticate_twitch <- function() {
  id <- Sys.getenv("IGDB_ID")
  secret <- Sys.getenv("IGDB_SECRET")

  if (id == "" || secret == "") {
    stop("Set IGDB_ID and IGDB_SECRET in .Renviron")
  }

  auth <- POST(glue(
    "https://id.twitch.tv/oauth2/token?client_id={id}&client_secret={secret}&grant_type=client_credentials"
  ))
  message("Twitch authentication successful")
  list(client_id = id, token = content(auth)$access_token)
}

# Generic IGDB API request
igdb_query <- function(endpoint, body, auth) {
  Sys.sleep(RATE_LIMIT_DELAY)

  resp <- request(glue("https://api.igdb.com/v4/{endpoint}")) |>
    req_headers(
      `Client-ID` = auth$client_id,
      Authorization = glue("Bearer {auth$token}")
    ) |>
    req_body_raw(body) |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()

  if (resp_status(resp) != 200) {
    return(NULL)
  }
  resp_body_json(resp)
}

# Look up IGDB ID from Steam app ID
lookup_steam_id <- function(steam_id, auth) {
  result <- igdb_query(
    "external_games",
    glue('fields game; where uid = "{steam_id}" & category = 1;'),
    auth
  )
  if (is.null(result) || length(result) == 0) NA_integer_ else result[[1]]$game
}

# Search IGDB by game name
search_by_name <- function(name, auth) {
  clean_name <- name |>
    str_remove_all("[™®©]") |>
    str_remove_all("\\s*\\([^)]*[Dd]emo[^)]*\\)") |>
    str_remove("\\s*[Dd]emo\\s*$") |>
    str_squish()

  result <- igdb_query(
    "games",
    glue(
      'search "{clean_name}"; fields name,platforms.name,first_release_date,aggregated_rating_count; limit 10;'
    ),
    auth
  )
  if (is.null(result) || length(result) == 0) {
    return(NULL)
  }

  map_dfr(result, \(g) {
    tibble(
      igdb_id = g$id,
      igdb_name = g$name,
      platforms = collapse_names(g$platforms),
      rating_count = g$aggregated_rating_count %||% 0L
    )
  })
}

# Score and select best match from candidates
select_match <- function(query, candidates, platform, verbose = TRUE) {
  if (is.null(candidates) || nrow(candidates) == 0) {
    return(list(id = NA_integer_, confidence = 0))
  }

  # Pokemon dual-version: normalize to first title (e.g., "Scarlet / Violet" → "Scarlet")
  if (str_detect(query, regex("pokemon|pokémon", ignore_case = TRUE))) {
    query <- query |>
      str_remove("\\s*/\\s*Violet") |>
      str_remove("\\s*/\\s*Shield") |>
      str_remove("\\s*/\\s*Moon") |>
      str_remove("\\s*(/|and)\\s*Y") |>
      str_remove("\\s*/\\s*Alpha Sapphire") |>
      str_remove("\\s*/\\s*Ultra Moon") |>
      str_remove("\\s*/\\s*Let's Go.*Eevee") |>
      str_remove("\\s*/\\s*Shining Pearl")
    if (verbose) message(glue("  Pokemon normalized: {query}"))
  }

  platform_pattern <- case_match(
    platform,
    "Nintendo" ~ "Switch|Nintendo|Wii|3DS",
    "Steam" ~ "PC|Windows|Linux|Mac",
    .default = ""
  )

  scored <- candidates |>
    mutate(
      clean_query = str_to_lower(str_remove_all(query, "[™®©()\\[\\]]")),
      clean_name = str_to_lower(str_remove_all(igdb_name, "[™®©()\\[\\]]")),
      str_sim = 1 - stringdist(clean_query, clean_name, method = "jw"),
      platform_match = !is.na(platforms) &
        str_detect(platforms, regex(platform_pattern, ignore_case = TRUE)),
      pop_score = pmin(0.2, log10(replace_na(rating_count, 0) + 1) / 10),
      score = (str_sim * 0.6) +
        if_else(platform_match, 0.3, 0, missing = 0) +
        pop_score
    ) |>
    arrange(desc(score))

  best <- scored |> slice(1)

  if (verbose) {
    message(glue("  Query: {query}"))
    message(glue(
      "  Best: {best$igdb_name} (score: {round(best$score, 3)}, sim: {round(best$str_sim, 3)}, plat: {best$platform_match})"
    ))
    if (nrow(scored) > 1) {
      others <- scored |> slice(2:min(4, n()))
      walk2(others$igdb_name, others$score, \(n, s) {
        message(glue("    - {n} ({round(s, 3)})"))
      })
    }
  }

  list(id = best$igdb_id, confidence = best$score)
}

# Get full game metadata
get_metadata <- function(igdb_id, auth) {
  fields <- c(
    "name",
    "summary",
    "storyline",
    "first_release_date",
    "aggregated_rating",
    "aggregated_rating_count",
    "rating",
    "rating_count",
    "total_rating",
    "total_rating_count",
    "hypes",
    "follows",
    "genres.name",
    "themes.name",
    "keywords.name",
    "game_modes.name",
    "player_perspectives.name",
    "platforms.name",
    "franchises.name",
    "collections.name",
    "age_ratings",
    "involved_companies.company",
    "involved_companies.developer",
    "involved_companies.publisher",
    "game_engines.name",
    "url"
  ) |>
    paste(collapse = ",")

  igdb_query("games", glue("fields {fields}; where id = {igdb_id};"), auth)[[1]]
}

# Get company names from IDs
get_companies <- function(ids, auth) {
  if (length(ids) == 0) {
    return(tibble(id = integer(), name = character()))
  }
  result <- igdb_query(
    "companies",
    glue('fields id,name; where id = ({paste(ids, collapse = ",")});'),
    auth
  )
  map_dfr(result %||% list(), \(c) tibble(id = c$id, name = c$name))
}

# Extract formatted metadata from API response
format_metadata <- function(
  data,
  companies,
  age_ratings_data,
  original_name,
  platform,
  confidence = NA_real_
) {
  # Helper to extract company names by role
  extract_companies <- function(involved, role) {
    involved |>
      filter(.data[[role]]) |>
      pull(name) |>
      paste(collapse = ", ") |>
      na_if("")
  }

  # Companies
  devs <- pubs <- NA_character_
  if (length(data$involved_companies) && nrow(companies) > 0) {
    involved <- tibble(
      company = map_int(data$involved_companies, "company"),
      developer = map_lgl(data$involved_companies, \(x) x$developer %||% FALSE),
      publisher = map_lgl(data$involved_companies, \(x) x$publisher %||% FALSE)
    ) |>
      left_join(companies, by = c("company" = "id"))
    devs <- extract_companies(involved, "developer")
    pubs <- extract_companies(involved, "publisher")
  }

  tibble(
    original_name = original_name,
    platform = platform,
    igdb_id = data$id,
    igdb_name = data$name,
    match_confidence = confidence,
    release_date = if (length(data$first_release_date)) {
      as_datetime(data$first_release_date)
    } else {
      NA_POSIXct_
    },
    genres = collapse_names(data$genres),
    themes = collapse_names(data$themes),
    keywords = collapse_names(data$keywords),
    game_modes = collapse_names(data$game_modes),
    player_perspectives = collapse_names(data$player_perspectives),
    platforms = collapse_names(data$platforms),
    franchise = pluck(data$franchises, 1, "name", .default = NA_character_),
    collection = pluck(data$collections, 1, "name", .default = NA_character_),
    game_engines = collapse_names(data$game_engines),
    summary = data$summary %||% NA_character_,
    storyline = data$storyline %||% NA_character_,
    critics_rating = data$aggregated_rating,
    critics_rating_count = data$aggregated_rating_count,
    user_rating = data$rating,
    user_rating_count = data$rating_count,
    total_rating = data$total_rating,
    total_rating_count = data$total_rating_count,
    hypes = data$hypes,
    follows = data$follows,
    esrb_rating = extract_rating(age_ratings_data, "ESRB"),
    pegi_rating = extract_rating(age_ratings_data, "PEGI"),
    developers = devs,
    publishers = pubs,
    igdb_url = data$url
  )
}

# Process a single game
process_game <- function(row, auth, verbose = TRUE) {
  platform <- row$platform
  title <- row$title_id
  steam_id <- row$steam_id

  if (verbose) {
    message(glue("\n[{platform}] {title}"))
  }

  igdb_id <- NA_integer_
  confidence <- NA_real_

  # Steam: direct lookup
  if (platform == "Steam" && !is.na(steam_id) && steam_id != "NA") {
    if (verbose) {
      message(glue("  Steam ID: {steam_id}"))
    }
    igdb_id <- lookup_steam_id(steam_id, auth)
    if (!is.na(igdb_id)) {
      confidence <- 1.0
      if (verbose) message(glue("  IGDB ID: {igdb_id}"))
    }
  }

  # Name search fallback
  if (is.na(igdb_id)) {
    match <- select_match(title, search_by_name(title, auth), platform, verbose)
    igdb_id <- match$id
    confidence <- match$confidence
  }

  # No match
  if (is.na(igdb_id)) {
    if (verbose) {
      message("  No match")
    }
    return(tibble(
      original_name = title,
      platform = platform,
      igdb_id = NA_integer_
    ))
  }

  # Get full metadata
  data <- get_metadata(igdb_id, auth)
  if (is.null(data)) {
    return(tibble(
      original_name = title,
      platform = platform,
      igdb_id = igdb_id
    ))
  }

  company_ids <- map_int(data$involved_companies %||% list(), "company")
  companies <- get_companies(company_ids, auth)


  # Fetch age ratings (API returns IDs as plain integers, not expanded objects)
  age_rating_ids <- as.integer(data$age_ratings %||% integer())
  age_ratings_data <- get_age_ratings(age_rating_ids, auth)

  format_metadata(data, companies, age_ratings_data, title, platform, confidence)
}

# Main extraction function
run_extraction <- function(test_mode = TEST_MODE, resume = TRUE) {
  message(strrep("=", 60))
  message(glue(
    "IGDB Metadata Extraction | Mode: {if (test_mode) glue('TEST ({TEST_N}/platform)') else 'FULL'}"
  ))
  message(strrep("=", 60))

  auth <- authenticate_twitch()
  games <- read_csv(INPUT_FILE, show_col_types = FALSE)
  message(glue(
    "Loaded {nrow(games)} games | Platforms: {paste(unique(games$platform), collapse = ', ')}"
  ))

  # Filter out platforms that should be skipped (e.g., Xbox with blinded IDs)
  if (length(SKIP_PLATFORMS) > 0) {
    n_before <- nrow(games)
    games <- games |> filter(!platform %in% SKIP_PLATFORMS)
    message(glue(
      "Skipping {n_before - nrow(games)} games from: {paste(SKIP_PLATFORMS, collapse = ', ')}"
    ))
  }

  if (test_mode) {
    games <- games |>
      group_by(platform) |>
      slice_sample(prop = 1) |>
      slice_head(n = TEST_N) |>
      ungroup()
    message(glue("Test sample: {nrow(games)} games"))
  }

  # Resume from progress
  results <- tibble()
  processed <- character()
  if (resume && file.exists(PROGRESS_FILE)) {
    progress <- readRDS(PROGRESS_FILE)
    results <- progress$results
    processed <- progress$processed
    message(glue("Resuming: {length(processed)} already processed"))
  }

  to_process <- games |> filter(!title_id %in% processed)
  message(glue("To process: {nrow(to_process)}"))

  if (nrow(to_process) == 0) {
    return(results)
  }

  # Process games
  for (i in seq_len(nrow(to_process))) {
    row <- to_process[i, ]
    message(glue("\n[{i}/{nrow(to_process)}]"), appendLF = FALSE)

    result <- tryCatch(
      process_game(row, auth),
      error = \(e) {
        message(glue("  ERROR: {e$message}"))
        tibble(original_name = row$title_id, platform = row$platform)
      }
    )

    results <- bind_rows(results, result)
    processed <- c(processed, row$title_id)

    if (i %% 50 == 0) {
      saveRDS(list(results = results, processed = processed), PROGRESS_FILE)
      message(glue("\n[Saved progress: {length(processed)} games]"))
    }
  }

  saveRDS(list(results = results, processed = processed), PROGRESS_FILE)

  # Summary
  message(strrep("=", 60))
  results |>
    group_by(platform) |>
    summarise(
      total = n(),
      matched = sum(!is.na(igdb_id)),
      high_conf = sum(match_confidence >= 0.7, na.rm = TRUE)
    ) |>
    print()
  message(glue(
    "Overall: {sum(!is.na(results$igdb_id))}/{nrow(results)} matched"
  ))

  results
}


# Map Xbox genre to IGDB genre
map_xbox_to_igdb <- function(xbox_genre) {
  if (is.na(xbox_genre)) {
    return(NA_character_)
  }
  match <- GENRE_MAPPING$igdb_genre[GENRE_MAPPING$xbox_genre == xbox_genre]
  if (length(match) > 0) match[1] else NA_character_
}

# Get unique Xbox games with their genre labels
load_xbox_genres <- function(path = XBOX_DATA_FILE) {
  read_csv(path, show_col_types = FALSE) |>
    distinct(title_id, .keep_all = TRUE) |>
    select(
      original_name = title_id,
      xbox_genre = genres,
      xbox_primary_genre = publisher_primary_genre,
      xbox_subgenre = publisher_subgenre
    ) |>
    mutate(platform = "Xbox")
}

# Harmonize genres across all platforms (IGDB genre as target taxonomy)
harmonize_genres <- function(igdb_results) {
  message(strrep("=", 60))
  message("Harmonizing genres across platforms")
  message(strrep("=", 60))

  message(glue("Using {nrow(GENRE_MAPPING)} Xbox → IGDB genre mappings"))

  # Steam/Nintendo: already have IGDB genres, add empty xbox_genre columns
  igdb_with_xbox <- igdb_results |>
    mutate(
      xbox_genre = NA_character_,
      xbox_primary_genre = NA_character_,
      xbox_subgenre = NA_character_
    )

  # Load Xbox games with existing genre labels
  xbox_games <- load_xbox_genres()
  message(glue("Loaded {nrow(xbox_games)} unique Xbox games"))

  # Xbox games: map xbox_genre → IGDB genre
  xbox_harmonized <- xbox_games |>
    mutate(
      genres = map_chr(xbox_genre, map_xbox_to_igdb),
      igdb_id = NA_integer_,
      igdb_name = NA_character_,
      match_confidence = NA_real_
    )

  # Combine all platforms
  combined <- bind_rows(
    igdb_with_xbox,
    xbox_harmonized
  )

  # Summary
  message(strrep("-", 40))
  combined |>
    group_by(platform) |>
    summarise(
      total = n(),
      has_genre = sum(!is.na(genres)),
      pct = scales::percent(has_genre / total, accuracy = 0.1)
    ) |>
    print()

  combined
}

# Run if executed directly
if (sys.nframe() == 0) {
  results <- run_extraction()

  if (nrow(results) > 0) {
    # Harmonize genres and combine with Xbox data
    final <- harmonize_genres(results)

    dir.create(dirname(OUTPUT_FILE), showWarnings = FALSE, recursive = TRUE)
    write_csv(final, OUTPUT_FILE)
    message(glue("Saved {nrow(final)} games to {OUTPUT_FILE}"))
  }
}
