# fangraphs_data.R
# FanGraphs WAR as an alternate source for every plot.
#
# FanGraphs has no public API and its leaderboard endpoint sits behind
# Cloudflare bot protection, so the data is pulled through a real browser
# session (one request per season and stat type, "split teams" mode) and
# cached as data_cache/fg/{bat|pit}_{year}.json. Each file holds
# {season, stats, totalCount, rows: [{Season, TeamName, xMLBAMID, playerid,
# PlayerName, WAR, G, GS, IP, PA, Pos, position}, ...]}.
#
# load_fg_war_data() reshapes those rows to the same columns the pipeline
# uses for the Baseball-Reference feeds and maps players to Baseball-Reference
# IDs through MLBAM ids (the BR feeds carry mlb_ID for every player), so
# positions, awards, postseason and franchise mapping work unchanged.

FG_DIR <- file.path(SUPP_DIR, "fg")

# FanGraphs team abbreviations differ from Baseball-Reference's for several
# historical clubs, and some FanGraphs codes are reused across unrelated
# franchises in different eras, so the translation is year-aware. Output codes
# are Baseball-Reference's, which franchise_map() understands. Anything not
# listed passes through unchanged; unmapped post-1900 codes are reported by
# load_fg_war_data().
fg_team_to_br <- function(fg_team, year) {
  case_when(
    fg_team == "WAS" & year >= 1901 & year <= 1960 ~ "WSH",  # original Senators -> MIN
    fg_team == "WAS" & year >= 1961 & year <= 1971 ~ "WSA",  # expansion Senators -> TEX
    fg_team == "MIL" & year >= 1953 & year <= 1965 ~ "MLN",  # Milwaukee Braves -> ATL
    fg_team == "SEA" & year == 1969                 ~ "SEP",  # Seattle Pilots -> MIL
    fg_team == "NYH" & year >= 1903 & year <= 1912 ~ "NYY",  # Highlanders -> NYY
    fg_team == "CHI" & year >= 1876 & year <= 1902 ~ "CHC",  # Chicago NL -> CHC
    TRUE ~ fg_team
  )
}

read_fg_season <- function(path) {
  d <- jsonlite::fromJSON(path, simplifyVector = TRUE)
  rows <- d$rows
  if (length(rows) == 0 || is.null(nrow(rows)) || nrow(rows) == 0) return(NULL)
  for (col in c("GS", "IP", "PA", "G", "Pos", "position", "xMLBAMID", "playerid")) {
    if (!col %in% names(rows)) rows[[col]] <- NA
  }
  tibble::tibble(
    year_ID   = as.integer(rows$Season),
    fg_team   = as.character(rows$TeamName),
    mlb_ID    = suppressWarnings(as.integer(rows$xMLBAMID)),
    fg_id     = as.character(rows$playerid),
    name_common = as.character(rows$PlayerName),
    WAR       = suppressWarnings(as.numeric(rows$WAR)),
    G         = suppressWarnings(as.integer(rows$G)),
    GS        = suppressWarnings(as.integer(rows$GS)),
    PA        = suppressWarnings(as.numeric(rows$PA)),
    fg_pos    = as.character(if (!all(is.na(rows$Pos))) rows$Pos else rows$position)
  )
}

load_fg_table <- function(stats) {
  files <- list.files(FG_DIR, pattern = sprintf("^%s_\\d{4}\\.json$", stats), full.names = TRUE)
  if (length(files) == 0) stop("No FanGraphs ", stats, " files in ", FG_DIR,
                               "; run the browser pull first (see fangraphs_data.R header).")
  bind_rows(lapply(files, read_fg_season))
}

# Returns list(batting, pitching) shaped like load_war_data()'s output.
# `war_bat_br` / `war_pitch_br` are the Baseball-Reference feeds, used for the
# MLBAM -> BR id crosswalk and for primary positions.
load_fg_war_data <- function(war_bat_br, war_pitch_br) {
  message("Loading FanGraphs WAR from ", FG_DIR, "...")
  id_map <- br_id_map(war_bat_br, war_pitch_br)

  # Secondary crosswalk (FanGraphs id -> BR id, from the Chadwick register) for
  # the few rows that carry no MLBAM id. Built once; see data_cache/fg_id_crosswalk.csv.
  xwalk_path <- file.path(SUPP_DIR, "fg_id_crosswalk.csv")
  fg_xwalk <- if (file.exists(xwalk_path)) {
    x <- readr::read_csv(xwalk_path, show_col_types = FALSE, col_types = "cc")
    setNames(x$bbref_id, x$fg_id)
  } else c()

  convert <- function(d, player_type) {
    d <- d %>%
      mutate(
        team_ID   = fg_team_to_br(fg_team, year_ID),
        player_ID = coalesce(unname(id_map[as.character(mlb_ID)]), unname(fg_xwalk[fg_id])),
        stint_ID  = 1L,
        player_type = player_type
      ) %>%
      group_by(player_ID, year_ID, team_ID) %>%
      mutate(stint_ID = row_number()) %>%
      ungroup()
    unmatched <- d %>% filter(is.na(player_ID))
    if (nrow(unmatched) > 0) {
      message(sprintf("  %s: %d of %d rows (%.1f WAR) have no Baseball-Reference id and are dropped",
                      player_type, nrow(unmatched), nrow(d), sum(unmatched$WAR, na.rm = TRUE)))
    }
    d %>% filter(!is.na(player_ID))
  }

  bat   <- convert(load_fg_table("bat"), "batter")
  pitch <- convert(load_fg_table("pit"), "pitcher") %>% mutate(primary_pos = "P")

  # Same primary-position rule as the BR path (most career games at a position)
  primary_position <- ref_table("Fielding") %>%
    group_by(playerID, POS) %>%
    summarise(games = sum(G), .groups = "drop") %>%
    group_by(playerID) %>%
    slice_max(order_by = games, n = 1, with_ties = FALSE) %>%
    select(playerID, primary_pos = POS)
  bat <- bat %>% left_join(primary_position, by = c("player_ID" = "playerID"))

  unmapped <- bind_rows(bat, pitch) %>%
    mutate(franchise = franchise_map(team_ID, year_ID)) %>%
    filter(is.na(franchise), year_ID >= 1901) %>%
    count(team_ID, name = "rows")
  if (nrow(unmapped) > 0) {
    message("  FanGraphs team codes not in franchise_map (post-1900 rows dropped downstream): ",
            paste(sprintf("%s(%d)", unmapped$team_ID, unmapped$rows), collapse = " "))
  }

  message(sprintf("FanGraphs data loaded: seasons %d-%d, %d batting + %d pitching rows.",
                  min(bat$year_ID), max(bat$year_ID), nrow(bat), nrow(pitch)))
  list(batting = bat, pitching = pitch)
}
