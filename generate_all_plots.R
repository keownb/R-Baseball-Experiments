# generate_all_plots.R
# Batch generation of all WAR plots for GH Pages
# Uses parallel::mclapply (fork-based) + ragg for maximum throughput

source("franchise_war_functions.R")

# =============================================================================
# CONFIGURATION
# =============================================================================

OUTPUT_DIR <- "docs/plots"
DATA_DIR <- "docs/data"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(DATA_DIR, showWarnings = FALSE, recursive = TRUE)

N_CORES <- parallel::detectCores()  # all of them
ERAS <- c(1901, 1961, 1969, 1977, 1993, 1998)

POSITIONS <- list(
  "all" = list(source = "combined", filter = NULL),
  "batters" = list(source = "batting", filter = NULL),
  "pitchers" = list(source = "pitching", filter = NULL),
  "C" = list(source = "batting", filter = "C"),
  "1B" = list(source = "batting", filter = "1B"),
  "2B" = list(source = "batting", filter = "2B"),
  "SS" = list(source = "batting", filter = "SS"),
  "3B" = list(source = "batting", filter = "3B"),
  "OF" = list(source = "batting", filter = "OF"),
  "SP" = list(source = "pitching", filter = "SP"),
  "RP" = list(source = "pitching", filter = "RP")
)

PLOT_WIDTH <- 20
PLOT_HEIGHT <- 24
PLOT_DPI <- 150

OVERLAY_COMBOS <- list(
  list(suffix = "",            awards = FALSE, postseason = FALSE),
  list(suffix = "_awards",     awards = TRUE,  postseason = FALSE),
  list(suffix = "_postseason", awards = FALSE, postseason = TRUE),
  list(suffix = "_all",        awards = TRUE,  postseason = TRUE)
)

# =============================================================================
# LOAD DATA (loaded once in parent, shared via fork COW)
# =============================================================================

message("=== Loading WAR data ===")
war_data <- load_war_data()
WAR_bat <- war_data$batting
WAR_pitch <- war_data$pitching

common_cols <- c("year_ID", "player_ID", "name_common", "team_ID", "WAR", "player_type", "primary_pos")
WAR_combined <- bind_rows(
  WAR_bat %>% select(all_of(common_cols)),
  WAR_pitch %>% select(all_of(common_cols))
)

message("=== Loading shared data (z-scores, awards) ===")
pos_year_stats <- compute_position_year_stats(WAR_bat, WAR_pitch)
award_data <- load_award_data()

use_ragg <- requireNamespace("ragg", quietly = TRUE)

# =============================================================================
# BUILD LIGHTWEIGHT JOB LIST (no data refs, just params)
# =============================================================================

jobs <- list()
for (era in ERAS) {
  for (pos_name in names(POSITIONS)) {
    pos_config <- POSITIONS[[pos_name]]
    pos_filter <- pos_config$filter
    can_zscore <- !is.null(pos_filter) && pos_filter %in% c("C","1B","2B","SS","3B","OF","SP","RP")
    base_name <- sprintf("%s_%d", tolower(pos_name), era)

    width_modes <- list(list(name = "", vw = FALSE))
    if (can_zscore) width_modes[[2]] <- list(name = "_zscore", vw = TRUE)

    for (wm in width_modes) {
      for (oc in OVERLAY_COMBOS) {
        jobs[[length(jobs) + 1]] <- list(
          fname = sprintf("%s/%s%s%s.png", OUTPUT_DIR, base_name, wm$name, oc$suffix),
          source_name = pos_config$source, type_label = switch(pos_config$source,
            "batting" = "Position Players", "pitching" = "Pitchers", "combined" = "All Players"),
          era = era, pos_filter = pos_filter, vw = wm$vw,
          show_awards = oc$awards, show_postseason = oc$postseason
        )
      }
    }

    # CSV export job
    jobs[[length(jobs) + 1]] <- list(
      csv_path = sprintf("%s/%s.csv", DATA_DIR, base_name),
      source_name = pos_config$source, type_label = switch(pos_config$source,
        "batting" = "Position Players", "pitching" = "Pitchers", "combined" = "All Players"),
      era = era, pos_filter = pos_filter
    )
  }
}

message(sprintf("\n=== %d items | %d cores | %s ===\n",
                length(jobs), N_CORES, if (use_ragg) "ragg" else "default png"))

# =============================================================================
# PARALLEL GENERATION (mclapply = raw fork, zero serialization overhead)
# =============================================================================

render_job <- function(job) {
  war_src <- switch(job$source_name,
    "batting" = WAR_bat, "pitching" = WAR_pitch, "combined" = WAR_combined)

  if (!is.null(job$csv_path)) {
    p <- generate_franchise_war_plot(war_src, job$type_label,
      start_year = job$era, position_filter = job$pos_filter)
    export_plot_data(p, job$csv_path)
    return(job$csv_path)
  }

  p <- generate_franchise_war_plot(war_src, job$type_label,
    start_year = job$era, position_filter = job$pos_filter,
    variable_width = job$vw, pos_year_stats = if (job$vw) pos_year_stats else NULL,
    show_awards = job$show_awards,
    award_data = if (job$show_awards) award_data else NULL,
    show_postseason = job$show_postseason)

  if (use_ragg) {
    ggsave(job$fname, plot = p, width = PLOT_WIDTH, height = PLOT_HEIGHT,
           dpi = PLOT_DPI, device = ragg::agg_png)
  } else {
    ggsave(job$fname, plot = p, width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
  }
  return(job$fname)
}

t0 <- Sys.time()
results <- parallel::mclapply(jobs, render_job, mc.cores = N_CORES)
elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

errors <- sapply(results, inherits, "try-error")
message(sprintf("\n=== Done! %d items in %.0fs (%.1f/s) | %d errors ===",
                length(results), elapsed, length(results) / elapsed, sum(errors)))
