# supplemental_data.R
# Fills seasons newer than the installed Lahman package from the MLB Stats API
# so the pipeline keeps working between Lahman releases (Lahman lags a season),
# and reconciles Lahman's player ids with Baseball-Reference's.
#
# For every season present in the Baseball-Reference WAR feed but absent from
# Lahman, this builds Lahman-shaped rows for the tables the pipeline uses:
#   Appearances   games by position per team (MLB Stats API fielding splits,
#                 queried PER TEAM so traded players are split correctly; the
#                 league-wide endpoint reports a traded player's season total
#                 under his last team only), plus games played from the hitting
#                 splits for G_all
#   Fielding      games by position per team, LF/CF/RF folded into OF
#   Pitching      G / GS per stint (from the Baseball-Reference pitching feed)
#   AllstarFull   All-Star selections (MLB awards ALAS / NLAS)
#   AwardsPlayers MVP and Cy Young (MLB awards ALMVP / NLMVP / ALCY / NLCY)
#   SeriesPost    completed postseason series (MLB postseason schedule), with
#                 Lahman's round codes (ALWC1, NLDS2, ALCS, WS, ...)
#
# Player IDs are translated MLB id -> Baseball-Reference id using the mlb_ID
# column that the Baseball-Reference feed already carries. Team IDs in the
# supplement rows are the site's own franchise codes (e.g. "OAK"), which
# lahman_franchise() passes through unchanged.
#
# Lahman's own playerIDs differ from Baseball-Reference ids for ~500 players
# (e.g. Riley Greene is greenri02 in Lahman, greenri03 on BR). Because the WAR
# feeds key everything by BR id, load_reference_tables() rewrites those Lahman
# ids to People$bbrefID so positions, All-Star and award rows join.
#
# Results are cached as CSVs under data_cache/ so re-runs are offline and
# reproducible. The cache for a season that may still change (postseason not
# over, awards not announced) expires after SUPP_MAX_AGE_HOURS; closed seasons
# are cached permanently. Pass refresh = TRUE to force a re-download.

SUPP_DIR <- "data_cache"
MLB_API  <- "https://statsapi.mlb.com/api/v1/"
SUPP_MAX_AGE_HOURS <- 12          # same horizon as the Baseball-Reference feed cache
SEASON_CLOSED_MD   <- "-12-01"    # MVP / Cy Young are announced by late November

# MLB Stats API team abbreviations -> site franchise codes (identity otherwise)
.mlb_abbr_to_code <- c(AZ = "ARI", KC = "KCR", WSH = "WSN", SD = "SDP", SF = "SFG",
                       TB = "TBR", CWS = "CHW", ATH = "OAK")
.mlb_league_id <- c(`103` = "AL", `104` = "NL")

`%||%` <- function(a, b) if (is.null(a)) b else a

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

# The season's MLB clubs: MLB team id, site franchise code, league
mlb_teams <- function(year) {
  teams <- mlb_api_get(sprintf("teams?sportId=1&season=%d", year))$teams
  if (length(teams) != 30) stop(sprintf("MLB Stats API lists %d teams for %d, expected 30", length(teams), year))
  abbr <- vapply(teams, function(t) t$abbreviation, character(1))
  data.frame(
    id   = vapply(teams, function(t) as.numeric(t$id), numeric(1)),
    code = unname(ifelse(abbr %in% names(.mlb_abbr_to_code), .mlb_abbr_to_code[abbr], abbr)),
    lg   = unname(.mlb_league_id[as.character(vapply(teams, function(t) as.numeric(t$league$id), numeric(1)))]),
    stringsAsFactors = FALSE)
}
mlb_team_codes <- function(year) { t <- mlb_teams(year); setNames(t$code, t$id) }

# mlb_ID -> player_ID lookup from the Baseball-Reference feeds ("NULL" = unknown)
br_id_map <- function(war_bat, war_pitch) {
  ids <- bind_rows(
    war_bat   %>% select(mlb_ID, player_ID),
    war_pitch %>% select(mlb_ID, player_ID)
  ) %>%
    mutate(mlb_ID = suppressWarnings(as.numeric(as.character(mlb_ID)))) %>%
    filter(!is.na(mlb_ID)) %>%
    distinct(mlb_ID, .keep_all = TRUE)
  setNames(ids$player_ID, as.character(ids$mlb_ID))
}

.supp_cache_path <- function(table, year) {
  file.path(SUPP_DIR, sprintf("supp_%s_%d.csv", table, year))
}

# ---- Per-team season splits ------------------------------------------------

# One team's season splits for a stat group ("fielding" or "hitting"). Querying
# per team is what makes multi-team players come back split by team.
fetch_team_splits <- function(year, group, team_id) {
  res <- mlb_api_get(sprintf(
    "stats?stats=season&group=%s&season=%d&sportId=1&limit=10000&playerPool=all&teamId=%d",
    group, year, team_id))
  st <- res$stats
  if (length(st) == 0) return(list())
  splits <- st[[1]]$splits
  total  <- st[[1]]$totalSplits
  if (!is.null(total) && total != length(splits)) {
    stop(sprintf("MLB Stats API truncated %s splits for team %d in %d (%d of %d)", group, team_id, year, length(splits), total))
  }
  splits
}

fetch_supp_fielding_splits <- function(year, id_map, teams) {
  rows <- lapply(seq_len(nrow(teams)), function(i) {
    splits <- fetch_team_splits(year, "fielding", teams$id[i])
    if (length(splits) == 0) return(NULL)
    data.frame(
      mlb_ID   = vapply(splits, function(s) as.numeric(s$player$id), numeric(1)),
      teamID   = teams$code[i],
      POS      = vapply(splits, function(s) s$position$abbreviation, character(1)),
      G        = vapply(splits, function(s) as.numeric(s$stat$games), numeric(1)),
      numTeams = vapply(splits, function(s) as.numeric(s$numTeams %||% 1), numeric(1)),
      stringsAsFactors = FALSE)
  })
  out <- bind_rows(rows)
  if (any(out$numTeams != 1)) stop("per-team fielding query returned multi-team splits; cannot attribute games to teams")
  out %>%
    mutate(playerID = unname(id_map[as.character(mlb_ID)]), yearID = year) %>%
    filter(!is.na(playerID)) %>%
    select(playerID, yearID, teamID, POS, G)
}

# Games played per player-team from the hitting splits (Lahman's G_all)
fetch_supp_games <- function(year, id_map, teams) {
  rows <- lapply(seq_len(nrow(teams)), function(i) {
    splits <- fetch_team_splits(year, "hitting", teams$id[i])
    if (length(splits) == 0) return(NULL)
    data.frame(
      mlb_ID = vapply(splits, function(s) as.numeric(s$player$id), numeric(1)),
      teamID = teams$code[i],
      G_all  = vapply(splits, function(s) as.numeric(s$stat$gamesPlayed %||% NA), numeric(1)),
      stringsAsFactors = FALSE)
  })
  bind_rows(rows) %>%
    mutate(playerID = unname(id_map[as.character(mlb_ID)]), yearID = year) %>%
    filter(!is.na(playerID)) %>%
    select(playerID, yearID, teamID, G_all)
}

build_supp_appearances <- function(splits, games) {
  wide <- splits %>%
    mutate(POS = tolower(POS)) %>%
    group_by(playerID, yearID, teamID, POS) %>%
    summarise(G = sum(G), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = POS, values_from = G, values_fill = 0,
                       names_prefix = "G_")
  for (col in c("G_p","G_c","G_1b","G_2b","G_3b","G_ss","G_lf","G_cf","G_rf","G_dh")) {
    if (!col %in% names(wide)) wide[[col]] <- 0
  }
  # Lahman's G_defense counts distinct games in the field; the splits only give
  # games per position. Bound it: at least the most games at any one position,
  # at most the sum over positions, and at most games played minus DH games.
  wide %>%
    left_join(games, by = c("playerID", "yearID", "teamID")) %>%
    mutate(
      G_of      = G_lf + G_cf + G_rf,
      pos_sum   = G_p + G_c + G_1b + G_2b + G_3b + G_ss + G_lf + G_cf + G_rf,
      pos_max   = pmax(G_p, G_c, G_1b, G_2b, G_3b, G_ss, G_lf, G_cf, G_rf),
      G_all     = pmax(coalesce(G_all, pos_sum + G_dh), pos_max + G_dh),
      G_defense = pmax(pos_max, pmin(pos_sum, G_all - G_dh)),
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
  mlb_ids <- vapply(recs, function(r) as.numeric(r$player$id), numeric(1))
  ids <- unname(id_map[as.character(mlb_ids)])
  if (any(is.na(ids))) {
    message(sprintf("    %s %d: %d recipient(s) have no Baseball-Reference id and are dropped", award_id, year, sum(is.na(ids))))
  }
  ids
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

# Completed series only. Round codes follow Lahman: WS, ALCS/NLCS, ALDS1/ALDS2,
# ALWC1/ALWC2 (numbered within league in schedule order).
build_supp_seriespost <- function(year, teams) {
  res <- mlb_api_get(sprintf("schedule/postseason/series?season=%d&sportId=1", year))
  if (is.null(res)) res <- list(series = list())
  wins_needed <- c(F = 2, D = 3, L = 4, W = 4)   # Wild Card, Division, LCS, World Series
  code_of <- setNames(teams$code, teams$id); lg_of <- setNames(teams$lg, teams$id)
  rows <- lapply(res$series, function(s) {
    gtype <- s$series$gameType
    if (!gtype %in% names(wins_needed)) return(NULL)
    games <- Filter(function(g) identical(g$status$abstractGameState, "Final"), s$games)
    if (length(games) == 0) return(NULL)
    wins <- list()
    for (g in games) for (side in c("home", "away")) {
      t <- g$teams[[side]]
      if (isTRUE(t$isWinner)) wins[[as.character(t$team$id)]] <- (wins[[as.character(t$team$id)]] %||% 0) + 1
    }
    if (length(wins) == 0) return(NULL)
    winner <- names(wins)[which.max(unlist(wins))]
    if (max(unlist(wins)) < wins_needed[[gtype]]) {
      message(sprintf("    %d %s series (%s) not yet decided: %s", year, gtype, s$series$id %||% "?",
                      paste(sprintf("%s %d", code_of[names(wins)], unlist(wins)), collapse = ", ")))
      return(NULL)
    }
    team_ids <- unique(unlist(lapply(games, function(g) c(g$teams$home$team$id, g$teams$away$team$id))))
    loser <- setdiff(as.character(team_ids), winner)[1]
    data.frame(yearID = year, gtype = gtype, lg = if (gtype == "W") "" else unname(lg_of[winner]),
               order = as.numeric(s$sortNumber %||% games[[1]]$gamePk),
               teamIDwinner = unname(code_of[winner]), lgIDwinner = unname(lg_of[winner]),
               teamIDloser = unname(code_of[loser]), lgIDloser = unname(lg_of[loser]),
               wins = max(unlist(wins)), losses = sum(unlist(wins)) - max(unlist(wins)),
               ties = 0L, stringsAsFactors = FALSE)
  })
  out <- bind_rows(rows)
  if (nrow(out) == 0) {
    return(data.frame(yearID = integer(0), round = character(0), teamIDwinner = character(0), lgIDwinner = character(0),
                      teamIDloser = character(0), lgIDloser = character(0), wins = integer(0), losses = integer(0), ties = integer(0)))
  }
  out %>%
    group_by(yearID, gtype, lg) %>%
    arrange(order, .by_group = TRUE) %>%
    mutate(n_in_round = n(), k = row_number(),
           round = case_when(
             gtype == "W" ~ "WS",
             gtype == "L" ~ paste0(lg, "CS"),
             gtype == "D" ~ paste0(lg, "DS", if_else(n_in_round > 1, as.character(k), "")),
             gtype == "F" ~ paste0(lg, "WC", if_else(n_in_round > 1, as.character(k), "")))) %>%
    ungroup() %>%
    arrange(match(gtype, c("W", "L", "D", "F")), lg, k) %>%
    select(yearID, round, teamIDwinner, lgIDwinner, teamIDloser, lgIDloser, wins, losses, ties)
}

# ---- Orchestration ------------------------------------------------------

# A season can still change (postseason, awards) until SEASON_CLOSED_MD of that
# year; until then its cache expires like the WAR feeds do.
.supp_cache_fresh <- function(paths, year, max_age_hours = SUPP_MAX_AGE_HOURS) {
  if (!all(file.exists(paths))) return(FALSE)
  if (Sys.Date() > as.Date(paste0(year, SEASON_CLOSED_MD))) return(TRUE)
  age <- as.numeric(difftime(Sys.time(), min(file.mtime(paths)), units = "hours"))
  age < max_age_hours
}

# Build (or load from cache) all supplement tables for one season.
get_supplement <- function(year, war_bat, war_pitch, refresh = FALSE) {
  dir.create(SUPP_DIR, showWarnings = FALSE, recursive = TRUE)
  tables <- c("Appearances", "Fielding", "Pitching", "AllstarFull", "AwardsPlayers", "SeriesPost")
  paths  <- setNames(lapply(tables, .supp_cache_path, year = year), tables)

  if (!refresh && .supp_cache_fresh(unlist(paths), year)) {
    message(sprintf("  Using cached %d supplement from %s/ (%s)", year, SUPP_DIR,
                    format(min(file.mtime(unlist(paths))), "%Y-%m-%d %H:%M")))
    return(lapply(paths, function(p) readr::read_csv(p, show_col_types = FALSE)))
  }

  message(sprintf("  Fetching %d supplement from MLB Stats API (per-team splits)...", year))
  id_map <- br_id_map(war_bat, war_pitch)
  teams  <- mlb_teams(year)
  splits <- fetch_supp_fielding_splits(year, id_map, teams)
  games  <- fetch_supp_games(year, id_map, teams)

  supp <- list(
    Appearances   = build_supp_appearances(splits, games),
    Fielding      = build_supp_fielding(splits),
    Pitching      = build_supp_pitching(year, war_pitch),
    AllstarFull   = build_supp_allstar(year, id_map),
    AwardsPlayers = build_supp_awards(year, id_map),
    SeriesPost    = build_supp_seriespost(year, teams)
  )
  for (t in tables) readr::write_csv(supp[[t]], paths[[t]])
  message(sprintf("    %d appearances, %d all-stars, %d awards, %d postseason series",
                  nrow(supp$Appearances), nrow(supp$AllstarFull),
                  nrow(supp$AwardsPlayers), nrow(supp$SeriesPost)))
  supp
}

# Lahman playerID -> Baseball-Reference id where they differ (People$bbrefID)
.lahman_to_bbref <- local({
  p <- Lahman::People
  p <- p[!is.na(p$bbrefID) & as.character(p$bbrefID) != as.character(p$playerID), c("playerID", "bbrefID")]
  setNames(as.character(p$bbrefID), as.character(p$playerID))
})
remap_lahman_ids <- function(d) {
  if (!"playerID" %in% names(d)) return(d)
  id <- as.character(d$playerID)
  hit <- id %in% names(.lahman_to_bbref)
  id[hit] <- unname(.lahman_to_bbref[id[hit]])
  d$playerID <- id
  d
}

# Lahman tables keyed by Baseball-Reference ids, with supplements appended for
# every season the WAR feed has but Lahman lacks.
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
  tabs <- lapply(tabs, function(d) remap_lahman_ids(mutate(d, across(where(is.factor), as.character))))
  if (length(missing) == 0) {
    message(sprintf("Lahman %s covers through %d; no supplement needed.",
                    packageVersion("Lahman"), lahman_max))
    return(tabs)
  }
  message(sprintf("Lahman %s ends at %d; supplementing %s.",
                  packageVersion("Lahman"), lahman_max, paste(missing, collapse = ", ")))
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
