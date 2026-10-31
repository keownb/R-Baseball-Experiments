# pipeline_jobs.R
# Single source of truth for WHAT the site needs rendered. The renderer
# (render.R) executes these jobs; the validator (pipeline_checks.R) checks that
# every one of them exists on disk and matches the data. The HTML pages build
# the same filenames from their controls, and validate_html_contract() checks
# that those filenames are a subset of what is rendered.
#
# Filename contracts (see README):
#   plots{_fg}/{pos}_{era}[_zscore][_awards|_postseason|_all].svg
#   data{_fg}/{pos}_{era}.csv
#   team_breakdowns{_fg}/{TEAM}_{linear|log}_{const|zscore}[_awards|_postseason|_all].svg
#   plots{_fg}/all_1901.png, team_breakdowns{_fg}/SEA_linear_const.png  (landing cards)

PIPELINE_ERAS <- c(1901, 1961, 1969, 1977, 1993, 1998)

GRID_POSITIONS <- list(
  all      = list(source = "combined", filter = NULL, label = "All Players"),
  batters  = list(source = "batting",  filter = NULL, label = "Position Players"),
  pitchers = list(source = "pitching", filter = NULL, label = "Pitchers"),
  C  = list(source = "batting",  filter = "C",  label = "Position Players"),
  `1B` = list(source = "batting", filter = "1B", label = "Position Players"),
  `2B` = list(source = "batting", filter = "2B", label = "Position Players"),
  SS = list(source = "batting",  filter = "SS", label = "Position Players"),
  `3B` = list(source = "batting", filter = "3B", label = "Position Players"),
  OF = list(source = "batting",  filter = "OF", label = "Position Players"),
  SP = list(source = "pitching", filter = "SP", label = "Pitchers"),
  RP = list(source = "pitching", filter = "RP", label = "Pitchers")
)

# Positions that can get a z-score (variable line width) grid variant. The
# explorer page dropped its line-width control in March 2026 (commit d6b5d58),
# so these 192 files per source are not rendered unless GRID_ZSCORE is TRUE.
Z_POSITIONS <- c("C", "1B", "2B", "SS", "3B", "OF", "SP", "RP")
GRID_ZSCORE <- FALSE

OVERLAYS <- list(
  list(suffix = "",            awards = FALSE, postseason = FALSE),
  list(suffix = "_awards",     awards = TRUE,  postseason = FALSE),
  list(suffix = "_postseason", awards = FALSE, postseason = TRUE),
  list(suffix = "_all",        awards = TRUE,  postseason = TRUE)
)

TEAMS <- c(
  BAL = "Baltimore Orioles", BOS = "Boston Red Sox", NYY = "New York Yankees",
  TBR = "Tampa Bay Rays", TOR = "Toronto Blue Jays", CHW = "Chicago White Sox",
  CLE = "Cleveland Guardians", DET = "Detroit Tigers", KCR = "Kansas City Royals",
  MIN = "Minnesota Twins", HOU = "Houston Astros", LAA = "Los Angeles Angels",
  OAK = "Oakland Athletics", SEA = "Seattle Mariners", TEX = "Texas Rangers",
  ATL = "Atlanta Braves", MIA = "Miami Marlins", NYM = "New York Mets",
  PHI = "Philadelphia Phillies", WSN = "Washington Nationals", CHC = "Chicago Cubs",
  CIN = "Cincinnati Reds", MIL = "Milwaukee Brewers", PIT = "Pittsburgh Pirates",
  STL = "St. Louis Cardinals", ARI = "Arizona Diamondbacks", COL = "Colorado Rockies",
  LAD = "Los Angeles Dodgers", SDP = "San Diego Padres", SFG = "San Francisco Giants"
)

WAR_SOURCES <- c("br", "fg")

source_suffix <- function(src) if (identical(src, "fg")) "_fg" else ""

# Output directories for one WAR source under a site root (docs/ or a staging dir)
output_dirs <- function(src, root = "docs") {
  sfx <- source_suffix(src)
  list(plots      = file.path(root, paste0("plots", sfx)),
       data       = file.path(root, paste0("data", sfx)),
       breakdowns = file.path(root, paste0("team_breakdowns", sfx)))
}

grid_jobs <- function(src, root = "docs", ext = "svg") {
  dirs <- output_dirs(src, root)
  jobs <- list()
  for (era in PIPELINE_ERAS) {
    for (pos_name in names(GRID_POSITIONS)) {
      pc <- GRID_POSITIONS[[pos_name]]
      bn <- sprintf("%s_%d", tolower(pos_name), era)
      widths <- list(list(n = "", vw = FALSE))
      if (GRID_ZSCORE && !is.null(pc$filter) && pc$filter %in% Z_POSITIONS) widths[[2]] <- list(n = "_zscore", vw = TRUE)
      for (wm in widths) for (oc in OVERLAYS) {
        jobs[[length(jobs) + 1]] <- list(
          kind = "grid", id = sprintf("%s%s%s", bn, wm$n, oc$suffix),
          fname = file.path(dirs$plots, sprintf("%s%s%s.%s", bn, wm$n, oc$suffix, ext)),
          sn = pc$source, tl = pc$label, era = era, pf = pc$filter,
          vw = wm$vw, sa = oc$awards, sp = oc$postseason)
      }
      jobs[[length(jobs) + 1]] <- list(
        kind = "grid_csv", id = paste0(bn, ".csv"),
        fname = file.path(dirs$data, paste0(bn, ".csv")),
        sn = pc$source, tl = pc$label, era = era, pf = pc$filter)
    }
  }
  # Raster copy of the default view for the landing-page card and og:image
  jobs[[length(jobs) + 1]] <- list(
    kind = "grid", id = "all_1901.png", fname = file.path(dirs$plots, "all_1901.png"),
    sn = "combined", tl = "All Players", era = 1901, pf = NULL,
    vw = FALSE, sa = FALSE, sp = FALSE)
  jobs
}

breakdown_jobs <- function(src, root = "docs", ext = "svg") {
  dirs <- output_dirs(src, root)
  jobs <- list()
  for (tc in names(TEAMS)) for (sc in c("linear", "log")) for (wn in c("const", "zscore")) {
    for (oc in OVERLAYS) {
      jobs[[length(jobs) + 1]] <- list(
        kind = "breakdown", id = sprintf("%s_%s_%s%s", tc, sc, wn, oc$suffix),
        fname = file.path(dirs$breakdowns, sprintf("%s_%s_%s%s.%s", tc, sc, wn, oc$suffix, ext)),
        tc = tc, tn = unname(TEAMS[[tc]]), il = (sc == "log"), iz = (wn == "zscore"),
        sa = oc$awards, sp = oc$postseason)
    }
  }
  jobs[[length(jobs) + 1]] <- list(
    kind = "breakdown", id = "SEA_linear_const.png",
    fname = file.path(dirs$breakdowns, "SEA_linear_const.png"),
    tc = "SEA", tn = unname(TEAMS[["SEA"]]), il = FALSE, iz = FALSE, sa = FALSE, sp = FALSE)
  jobs
}

all_jobs <- function(kind = c("grid", "breakdowns"), src, root = "docs", ext = "svg") {
  kind <- match.arg(kind)
  if (kind == "grid") grid_jobs(src, root, ext) else breakdown_jobs(src, root, ext)
}

# Every file the site expects for one source (plots, CSVs, breakdowns, cards)
expected_files <- function(src, root = "docs", ext = "svg") {
  vapply(c(grid_jobs(src, root, ext), breakdown_jobs(src, root, ext)),
         function(j) j$fname, character(1))
}

# Path of the per-run manifest written by render.R and read by the validator
manifest_path <- function(kind, src, root = "docs") {
  file.path(root, "build", sprintf("manifest_%s_%s.json", kind, src))
}
