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

# For each position × era, generate:
#   1. Base constant-width PNG
#   2. Base z-score-width PNG (single positions only)
#   3. Awards overlay (transparent)
#   4. Postseason overlay (transparent)
#   5. CSV data export

n_positions <- length(POSITIONS)
n_eras <- length(ERAS)
current <- 0

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
    message(sprintf("[%d/%d] %s", current, n_positions * n_eras, base_name))
    
    # --- 1. Constant-width base ---
    p_const <- generate_franchise_war_plot(
      war_source, type_label, start_year = era, position_filter = pos_filter)
    ggsave(sprintf("%s/%s.png", OUTPUT_DIR, base_name),
           plot = p_const, width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
    export_plot_data(p_const, sprintf("%s/%s.csv", DATA_DIR, base_name))
    
    # --- 2. Z-score-width base (single positions only) ---
    if (can_zscore) {
      p_zscore <- generate_franchise_war_plot(
        war_source, type_label, start_year = era, position_filter = pos_filter,
        variable_width = TRUE, pos_year_stats = pos_year_stats)
      ggsave(sprintf("%s/%s_zscore.png", OUTPUT_DIR, base_name),
             plot = p_zscore, width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
    }
    
    # --- 3. Awards overlay ---
    p_awards <- generate_franchise_war_plot(
      war_source, type_label, start_year = era, position_filter = pos_filter,
      show_awards = TRUE, award_data = award_data)
    award_layer <- p_awards$layers[sapply(p_awards$layers, function(l) {
      inherits(l$geom, "GeomPoint")
    })]
    if (length(award_layer) > 0) {
      save_overlay_png(award_layer, p_const,
        sprintf("%s/%s_awards.png", OUTPUT_DIR, base_name),
        width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
    }
    
    # --- 4. Postseason overlay ---
    p_post <- generate_franchise_war_plot(
      war_source, type_label, start_year = era, position_filter = pos_filter,
      show_postseason = TRUE)
    post_layer <- p_post$layers[sapply(p_post$layers, function(l) {
      inherits(l$geom, "GeomVline")
    })]
    if (length(post_layer) > 0) {
      save_overlay_png(post_layer, p_const,
        sprintf("%s/%s_postseason.png", OUTPUT_DIR, base_name),
        width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
    }
  }
}

message(sprintf("\n=== Done! Generated plots in %s ===", OUTPUT_DIR))
