# supplemental_data.R
# Fills seasons newer than the installed Lahman package from the MLB Stats API
# so the pipeline keeps working between Lahman releases (Lahman lags a season).
#
# For every season present in the Baseball-Reference WAR feed but absent from
# Lahman, this builds Lahman-shaped rows for the tables the pipeline uses:
#   Appearances   games by position (MLB Stats API fielding splits, incl. DH)
#   Fielding      games by position, LF/CF/RF folded into OF (same source)
#   Pitching      G / GS per stint (from the Baseball-Reference pitching feed)
#   AllstarFull   All-Star selections (MLB awards ALAS / NLAS)
#   AwardsPlayers MVP and Cy Young (MLB awards ALMVP / NLMVP / ALCY / NLCY)
#   SeriesPost    completed postseason series (MLB postseason schedule)
#
# Player IDs are translated MLB id -> Baseball-Reference id using the mlb_ID
# column that the Baseball-Reference feed already carries. Team IDs in the
# supplement rows are the site's own franchise codes (e.g. "OAK"), which
# lahman_franchise() passes through unchanged.
#
# Results are cached as CSVs under data_cache/ so re-runs are offline and
# reproducible. Pass refresh = TRUE to re-download (e.g. after the World Series
# or once MVP / Cy Young winners are announced in November).

SUPP_DIR <- "data_cache"
MLB_API  <- "https://statsapi.mlb.com/api/v1/"

# MLB Stats API team abbreviations -> site franchise codes (identity otherwise)
.mlb_abbr_to_code <- c(AZ = "ARI", KC = "KCR", WSH = "WSN", SD = "SDP", SF = "SFG",
                       TB = "TBR", CWS = "CHW", ATH = "OAK")

# GET a Stats API path and parse the JSON. A 404 means "no such object yet"
# (e.g. an award not announced for the season) and returns NULL.
mlb_api_get <- function(path, retries = 3) {
  url <- paste0(MLB_API, path)
  for (attempt in seq_len(retries)) {
    res <- tryCatch(curl::curl_fetch_memory(url), error = function(e) NULL)
    if (!is.null(res)) {
      if (res$status_code == 404) return(NULL)
      if (res$status_code == 200) {
        return(jsonlite::fromJSON(rawToChar(res$content), simplifyVector = FALSE))
      }
    }
    Sys.sleep(2 * attempt)
  }
  stop("MLB Stats API request failed after ", retries, " attempts: ", url)
}

mlb_team_codes <- function(year) {
  teams <- mlb_api_get(sprintf("teams?sportId=1&season=%d", year))$teams
  ids   <- vapply(teams, function(t) t$id, numeric(1))
  abbr  <- vapply(teams, function(t) t$abbreviation, character(1))
  code  <- ifelse(abbr %in% names(.mlb_abbr_to_code), .mlb_abbr_to_code[abbr], abbr)
  setNames(unname(code), ids)
}

# mlb_ID -> player_ID lookup from the Baseball-Reference feeds
br_id_map <- function(war_bat, war_pitch) {
  ids <- bind_rows(
    war_bat   %>% select(mlb_ID, player_ID),
    war_pitch %>% select(mlb_ID, player_ID)
  ) %>%
    filter(!is.na(mlb_ID)) %>%
    distinct(mlb_ID, .keep_all = TRUE)
  setNames(ids$player_ID, as.character(ids$mlb_ID))
}

.supp_cache_path <- function(table, year) {
  file.path(SUPP_DIR, sprintf("supp_%s_%d.csv", table, year))
}

# ---- Appearances + Fielding ---------------------------------------------

fetch_supp_fielding_splits <- function(year, id_map, team_codes) {
  res <- mlb_api_get(sprintf(
    "stats?stats=season&group=fielding&season=%d&sportId=1&limit=10000&playerPool=all", year))
  splits <- res$stats[[1]]$splits
  rows <- lapply(splits, function(s) {
    data.frame(
      mlb_ID  = s$player$id,
      team_id = if (!is.null(s$team$id)) s$team$id else NA_real_,
      POS     = s$position$abbreviation,
      G       = s$stat$games,
      stringsAsFactors = FALSE
    )
  })
  bind_rows(rows) %>%
    mutate(
      playerID = unname(id_map[as.character(mlb_ID)]),
      teamID   = unname(team_codes[as.character(team_id)]),
      yearID   = year
    ) %>%
    filter(!is.na(playerID))
}

build_supp_appearances <- function(splits) {
  wide <- splits %>%
    mutate(POS = tolower(POS)) %>%
    group_by(playerID, yearID, teamID, POS) %>%
    summarise(G = sum(G), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = POS, values_from = G, values_fill = 0,
                       names_prefix = "G_")
  for (col in c("G_p","G_c","G_1b","G_2b","G_3b","G_ss","G_lf","G_cf","G_rf","G_dh")) {
    if (!col %in% names(wide)) wide[[col]] <- 0L
  }
  wide %>%
    mutate(
      G_of      = G_lf + G_cf + G_rf,
      G_defense = G_p + G_c + G_1b + G_2b + G_3b + G_ss + G_lf + G_cf + G_rf,
      G_all     = G_defense + G_dh,
      lgID      = NA_character_
    ) %>%
    select(yearID, teamID, lgID, playerID, G_all, G_defense, G_p, G_c, G_1b, G_2b,
           G_3b, G_ss, G_lf, G_cf, G_rf, G_of, G_dh)
}

build_supp_fielding <- function(splits) {
  splits %>%
    filter(POS != "DH") %>%
    mutate(POS = if_else(POS %in% c("LF", "CF", "RF"), "OF", POS)) %>%
    group_by(playerID, yearID, teamID, POS) %>%
    summarise(G = sum(G), .groups = "drop") %>%
    mutate(stint = 1L, lgID = NA_character_) %>%
    select(playerID, yearID, stint, teamID, lgID, POS, G)
}

# ---- Pitching (from Baseball-Reference feed, per stint) -----------------

build_supp_pitching <- function(year, war_pitch) {
  war_pitch %>%
    filter(year_ID == year) %>%
    transmute(playerID = player_ID, yearID = year_ID, stint = stint_ID,
              teamID = franchise_map(team_ID, year_ID),
              G = as.integer(G), GS = as.integer(GS)) %>%
    filter(!is.na(teamID))
}

# ---- Awards and All-Stars -----------------------------------------------

fetch_award_recipients <- function(award_id, year, id_map) {
  res <- mlb_api_get(sprintf("awards/%s/recipients?season=%d", award_id, year))
  recs <- if (is.null(res)) list() else res$awards
  if (length(recs) == 0) return(character(0))
  mlb_ids <- vapply(recs, function(r) r$player$id, numeric(1))
  unname(id_map[as.character(mlb_ids)])
}

build_supp_allstar <- function(year, id_map) {
  ids <- c(fetch_award_recipients("ALAS", year, id_map),
           fetch_award_recipients("NLAS", year, id_map))
  ids <- unique(ids[!is.na(ids)])
  data.frame(playerID = ids, yearID = rep(year, length(ids)), stringsAsFactors = FALSE)
}

build_supp_awards <- function(year, id_map) {
  spec <- c(ALMVP = "Most Valuable Player", NLMVP = "Most Valuable Player",
            ALCY = "Cy Young Award", NLCY = "Cy Young Award")
  rows <- lapply(names(spec), function(a) {
    ids <- fetch_award_recipients(a, year, id_map)
    ids <- ids[!is.na(ids)]
    if (length(ids) == 0) return(NULL)
    data.frame(playerID = ids, awardID = spec[[a]], yearID = year,
               lgID = substr(a, 1, 2), stringsAsFactors = FALSE)
  })
  out <- bind_rows(rows)
  if (nrow(out) == 0) {
    out <- data.frame(playerID = character(0), awardID = character(0),
                      yearID = integer(0), lgID = character(0))
  }
  out
}

# ---- Postseason series --------------------------------------------------

build_supp_seriespost <- function(year, team_codes) {
  res <- mlb_api_get(sprintf("schedule/postseason/series?season=%d&sportId=1", year))
  if (is.null(res)) res <- list(series = list())
  wins_needed <- c(F = 2, D = 3, L = 4, W = 4)   # Wild Card, Division, LCS, World Series
  rows <- lapply(res$series, function(s) {
    gtype <- s$series$gameType
    if (!gtype %in% names(wins_needed)) return(NULL)
    games <- Filter(function(g) identical(g$status$abstractGameState, "Final"), s$games)
    if (length(games) == 0) return(NULL)
    wins <- list()
    for (g in games) {
      for (side in c("home", "away")) {
        t <- g$teams[[side]]
        if (isTRUE(t$isWinner)) wins[[as.character(t$team$id)]] <- (wins[[as.character(t$team$id)]] %||% 0) + 1
      }
    }
    if (length(wins) == 0) return(NULL)
    winner <- names(wins)[which.max(unlist(wins))]
    if (max(unlist(wins)) < wins_needed[[gtype]]) return(NULL)   # series still in progress
    team_ids <- unique(unlist(lapply(games, function(g) c(g$teams$home$team$id, g$teams$away$team$id))))
    loser <- setdiff(as.character(team_ids), winner)[1]
    league <- substr(games[[1]]$seriesDescription, 1, 2)   # "AL" / "NL" / "Wo"
    round <- switch(gtype,
      F = paste0(league, "WC"), D = paste0(league, "DS"),
      L = paste0(league, "CS"), W = "WS")
    data.frame(yearID = year, round = round,
               teamIDwinner = unname(team_codes[winner]), lgIDwinner = NA_character_,
               teamIDloser = unname(team_codes[loser]), lgIDloser = NA_character_,
               wins = max(unlist(wins)), losses = sum(unlist(wins)) - max(unlist(wins)),
               ties = 0L, stringsAsFactors = FALSE)
  })
  out <- bind_rows(rows)
  if (nrow(out) == 0) {
    out <- data.frame(yearID = integer(0), round = character(0), teamIDwinner = character(0),
                      teamIDloser = character(0))
  }
  out
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- Orchestration ------------------------------------------------------

# Build (or load from cache) all supplement tables for one season.
get_supplement <- function(year, war_bat, war_pitch, refresh = FALSE) {
  dir.create(SUPP_DIR, showWarnings = FALSE, recursive = TRUE)
  tables <- c("Appearances", "Fielding", "Pitching", "AllstarFull", "AwardsPlayers", "SeriesPost")
  paths  <- setNames(lapply(tables, .supp_cache_path, year = year), tables)

  if (!refresh && all(file.exists(unlist(paths)))) {
    message(sprintf("  Using cached %d supplement from %s/", year, SUPP_DIR))
    return(lapply(paths, function(p) readr::read_csv(p, show_col_types = FALSE)))
  }

  message(sprintf("  Fetching %d supplement from MLB Stats API...", year))
  id_map     <- br_id_map(war_bat, war_pitch)
  team_codes <- mlb_team_codes(year)
  splits     <- fetch_supp_fielding_splits(year, id_map, team_codes)

  supp <- list(
    Appearances   = build_supp_appearances(splits),
    Fielding      = build_supp_fielding(splits),
    Pitching      = build_supp_pitching(year, war_pitch),
    AllstarFull   = build_supp_allstar(year, id_map),
    AwardsPlayers = build_supp_awards(year, id_map),
    SeriesPost    = build_supp_seriespost(year, team_codes)
  )
  for (t in tables) readr::write_csv(supp[[t]], paths[[t]])
  message(sprintf("    %d appearances, %d all-stars, %d awards, %d postseason series",
                  nrow(supp$Appearances), nrow(supp$AllstarFull),
                  nrow(supp$AwardsPlayers), nrow(supp$SeriesPost)))
  supp
}

# Lahman tables with supplements appended for every season the WAR feed has
# but Lahman lacks.
load_reference_tables <- function(war_bat, war_pitch, refresh = FALSE) {
  lahman_max <- max(Lahman::Appearances$yearID)
  br_years   <- sort(unique(c(war_bat$year_ID, war_pitch$year_ID)))
  missing    <- br_years[br_years > lahman_max]

  tabs <- list(
    Appearances   = Lahman::Appearances,
    Fielding      = Lahman::Fielding,
    Pitching      = Lahman::Pitching,
    AllstarFull   = Lahman::AllstarFull,
    AwardsPlayers = Lahman::AwardsPlayers,
    SeriesPost    = Lahman::SeriesPost
  )
  if (length(missing) == 0) {
    message(sprintf("Lahman %s covers through %d; no supplement needed.",
                    packageVersion("Lahman"), lahman_max))
    return(tabs)
  }
  message(sprintf("Lahman %s ends at %d; supplementing %s.",
                  packageVersion("Lahman"), lahman_max, paste(missing, collapse = ", ")))
  tabs <- lapply(tabs, function(d) mutate(d, across(where(is.factor), as.character)))
  for (y in missing) {
    supp <- get_supplement(y, war_bat, war_pitch, refresh = refresh)
    for (t in names(tabs)) tabs[[t]] <- bind_ref_rows(tabs[[t]], supp[[t]])
  }
  tabs
}

# bind_rows with the supplement coerced to the Lahman column types (cached CSVs
# come back with all-NA columns typed as logical, which vctrs refuses to bind).
bind_ref_rows <- function(base, supp) {
  for (col in intersect(names(base), names(supp))) {
    if (is.numeric(base[[col]]) && !is.numeric(supp[[col]])) {
      supp[[col]] <- as.numeric(supp[[col]])
    } else if (is.character(base[[col]]) && !is.character(supp[[col]])) {
      supp[[col]] <- as.character(supp[[col]])
    }
  }
  bind_rows(base, supp)
}
