# generate_all_plots.R
# Batch generation of all WAR plots for GH Pages
# Run this after you're happy with the look/feel in franchise_war_analysis.R

source("franchise_war_functions.R")

# =============================================================================
# CONFIGURATION
# =============================================================================

OUTPUT_DIR <- "docs/plots"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# Eras to generate
ERAS <- c(1901, 1961, 1969, 1977, 1993, 1998)

# Single unified position list (replaces separate type + position dropdowns)
# Format: display_name = list(data_source, position_filter)
#   data_source: "batting", "pitching", or "combined"
#   position_filter: NULL for all, or specific position code
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

# Create data output directory
DATA_DIR <- "docs/data"
dir.create(DATA_DIR, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# GENERATE ALL PLOTS
# =============================================================================

total_plots <- length(ERAS) * length(POSITIONS)
current_plot <- 0

message(sprintf("\n=== Generating %d plots ===\n", total_plots))

for (era in ERAS) {
  for (pos_name in names(POSITIONS)) {
    current_plot <- current_plot + 1
    
    pos_config <- POSITIONS[[pos_name]]
    pos_filter <- pos_config$filter
    
    # Select appropriate data source
    war_source <- switch(pos_config$source,
      "batting" = WAR_bat,
      "pitching" = WAR_pitch,
      "combined" = WAR_combined
    )
    
    # Label for plot title
    type_label <- switch(pos_config$source,
      "batting" = "Position Players",
      "pitching" = "Pitchers",
      "combined" = "All Players"
    )
    
    base_name <- sprintf("%s_%d", tolower(pos_name), era)
    png_file <- sprintf("%s/%s.png", OUTPUT_DIR, base_name)
    csv_file <- sprintf("%s/%s.csv", DATA_DIR, base_name)
    message(sprintf("[%d/%d] %s", current_plot, total_plots, png_file))
    
    p <- generate_franchise_war_plot(
      war_source, 
      type_label, 
      start_year = era, 
      position_filter = pos_filter
    )
    ggsave(png_file, plot = p, width = 20, height = 24, units = "in", dpi = 150)
    export_plot_data(p, csv_file)
  }
}

message(sprintf("\n=== Done! Generated %d plots in %s ===", total_plots, OUTPUT_DIR))
message(sprintf("Data tables exported to %s", DATA_DIR))
