# generate_team_breakdowns.R
# Batch generation of team position breakdown charts
# Uses parallel::mclapply (fork-based) + ragg for maximum throughput

source("franchise_war_functions.R")
source("team_position_breakdown.R")

# =============================================================================
# CONFIGURATION
# =============================================================================

OUTPUT_DIR <- "docs/team_breakdowns"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

N_CORES <- parallel::detectCores()  # all of them
PLOT_WIDTH <- 20
PLOT_DPI <- 150

TEAMS <- list(
  "BAL" = "Baltimore Orioles", "BOS" = "Boston Red Sox",
  "NYY" = "New York Yankees", "TBR" = "Tampa Bay Rays", "TOR" = "Toronto Blue Jays",
  "CHW" = "Chicago White Sox", "CLE" = "Cleveland Guardians",
  "DET" = "Detroit Tigers", "KCR" = "Kansas City Royals", "MIN" = "Minnesota Twins",
  "HOU" = "Houston Astros", "LAA" = "Los Angeles Angels",
  "OAK" = "Oakland Athletics", "SEA" = "Seattle Mariners", "TEX" = "Texas Rangers",
  "ATL" = "Atlanta Braves", "MIA" = "Miami Marlins",
  "NYM" = "New York Mets", "PHI" = "Philadelphia Phillies", "WSN" = "Washington Nationals",
  "CHC" = "Chicago Cubs", "CIN" = "Cincinnati Reds",
  "MIL" = "Milwaukee Brewers", "PIT" = "Pittsburgh Pirates", "STL" = "St. Louis Cardinals",
  "ARI" = "Arizona Diamondbacks", "COL" = "Colorado Rockies",
  "LAD" = "Los Angeles Dodgers", "SDP" = "San Diego Padres", "SFG" = "San Francisco Giants"
)

OVERLAY_COMBOS <- list(
  list(suffix = "",            awards = FALSE, postseason = FALSE),
  list(suffix = "_awards",     awards = TRUE,  postseason = FALSE),
  list(suffix = "_postseason", awards = FALSE, postseason = TRUE),
  list(suffix = "_all",        awards = TRUE,  postseason = TRUE)
)

# =============================================================================
# LOAD SHARED DATA (loaded once in parent, shared via fork COW)
# =============================================================================

message("=== Loading shared data ===")
pos_year_stats <- compute_position_year_stats(WAR_bat, WAR_pitch)
award_data <- load_award_data()

use_ragg <- requireNamespace("ragg", quietly = TRUE)

# =============================================================================
# BUILD LIGHTWEIGHT JOB LIST
# =============================================================================

jobs <- list()
for (team_code in names(TEAMS)) {
  team_name <- TEAMS[[team_code]]
  for (scale_name in c("linear", "log")) {
    for (width_name in c("const", "zscore")) {
      for (oc in OVERLAY_COMBOS) {
        jobs[[length(jobs) + 1]] <- list(
          fname = sprintf("%s/%s_%s_%s%s.png", OUTPUT_DIR, team_code, scale_name, width_name, oc$suffix),
          team_code = team_code, team_name = team_name,
          is_log = (scale_name == "log"), is_zscore = (width_name == "zscore"),
          show_awards = oc$awards, show_postseason = oc$postseason
        )
      }
    }
  }
}

message(sprintf("\n=== %d images | %d cores | %s ===\n",
                length(jobs), N_CORES, if (use_ragg) "ragg" else "default png"))

# =============================================================================
# PARALLEL GENERATION (mclapply = raw fork, zero serialization overhead)
# =============================================================================

render_job <- function(job) {
  p <- generate_team_position_plot(
    WAR_bat, WAR_pitch,
    team_code = job$team_code, team_name = job$team_name,
    show_postseason = job$show_postseason,
    log_scale = job$is_log,
    variable_width = job$is_zscore,
    pos_year_stats = if (job$is_zscore) pos_year_stats else NULL,
    show_awards = job$show_awards,
    award_data = if (job$show_awards) award_data else NULL
  )

  n_pos <- attr(p, "n_positions")
  plot_height <- 3 * n_pos + 2

  if (use_ragg) {
    ggsave(job$fname, plot = p, width = PLOT_WIDTH, height = plot_height,
           dpi = PLOT_DPI, device = ragg::agg_png)
  } else {
    ggsave(job$fname, plot = p, width = PLOT_WIDTH, height = plot_height, dpi = PLOT_DPI)
  }
  return(job$fname)
}

t0 <- Sys.time()
results <- parallel::mclapply(jobs, render_job, mc.cores = N_CORES)
elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

errors <- sapply(results, inherits, "try-error")
message(sprintf("\n=== Done! %d images in %.0fs (%.1f/s) | %d errors ===",
                length(results), elapsed, length(results) / elapsed, sum(errors)))
