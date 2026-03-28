# generate_all_plots.R
# Batch generation of all WAR plots for GH Pages
# Generates base images (constant + z-score width) and transparent overlays

source("franchise_war_functions.R")

# =============================================================================
# CONFIGURATION
# =============================================================================

OUTPUT_DIR <- "docs/plots"
DATA_DIR <- "docs/data"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(DATA_DIR, showWarnings = FALSE, recursive = TRUE)

ERAS <- c(1901, 1961, 1969, 1977, 1993, 1998)

# Position config: data source + filter
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

# =============================================================================
# LOAD DATA
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

# =============================================================================
# GENERATE ALL PLOTS
# =============================================================================

# For each position × era, generate all overlay combos as full pre-rendered images.
# Naming: {pos}_{era}[_zscore][_awards|_postseason|_all].png
# This avoids overlay alignment issues — each image is self-contained.

OVERLAY_COMBOS <- list(
  list(suffix = "",            awards = FALSE, postseason = FALSE),
  list(suffix = "_awards",     awards = TRUE,  postseason = FALSE),
  list(suffix = "_postseason", awards = FALSE, postseason = TRUE),
  list(suffix = "_all",        awards = TRUE,  postseason = TRUE)
)

n_positions <- length(POSITIONS)
n_eras <- length(ERAS)
current <- 0
total <- n_positions * n_eras

message(sprintf("\n=== Generating plots for %d positions × %d eras ===\n", n_positions, n_eras))

for (era in ERAS) {
  for (pos_name in names(POSITIONS)) {
    current <- current + 1
    
    pos_config <- POSITIONS[[pos_name]]
    pos_filter <- pos_config$filter
    can_zscore <- !is.null(pos_filter) && pos_filter %in% c("C","1B","2B","SS","3B","OF","SP","RP")
    
    war_source <- switch(pos_config$source,
      "batting" = WAR_bat, "pitching" = WAR_pitch, "combined" = WAR_combined)
    type_label <- switch(pos_config$source,
      "batting" = "Position Players", "pitching" = "Pitchers", "combined" = "All Players")
    
    base_name <- sprintf("%s_%d", tolower(pos_name), era)
    message(sprintf("[%d/%d] %s", current, total, base_name))
    
    width_modes <- list(list(name = "", vw = FALSE, stats = NULL))
    if (can_zscore) {
      width_modes[[2]] <- list(name = "_zscore", vw = TRUE, stats = pos_year_stats)
    }
    
    for (wm in width_modes) {
      for (oc in OVERLAY_COMBOS) {
        fname <- sprintf("%s/%s%s%s.png", OUTPUT_DIR, base_name, wm$name, oc$suffix)
        
        p <- generate_franchise_war_plot(
          war_source, type_label, start_year = era, position_filter = pos_filter,
          variable_width = wm$vw, pos_year_stats = wm$stats,
          show_awards = oc$awards, award_data = if (oc$awards) award_data else NULL,
          show_postseason = oc$postseason)
        ggsave(fname, plot = p, width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
      }
    }
    
    # CSV export (just the base data, once per position/era)
    p_csv <- generate_franchise_war_plot(
      war_source, type_label, start_year = era, position_filter = pos_filter)
    export_plot_data(p_csv, sprintf("%s/%s.csv", DATA_DIR, base_name))
  }
}

message(sprintf("\n=== Done! Generated plots in %s ===", OUTPUT_DIR))
