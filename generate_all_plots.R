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

# Positions for batters (NULL = all positions)
BATTER_POSITIONS <- list(
  "All" = NULL,
  "C" = "C",
  "1B" = "1B",
  "2B" = "2B",
  "SS" = "SS",
  "3B" = "3B",
  "OF" = "OF"
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

total_plots <- length(ERAS) * (length(BATTER_POSITIONS) + 2)  # batters + pitchers + combined
current_plot <- 0

message(sprintf("\n=== Generating %d plots ===\n", total_plots))

for (era in ERAS) {
  
  # --- Position Players (with position filtering) ---
  for (pos_name in names(BATTER_POSITIONS)) {
    current_plot <- current_plot + 1
    pos_filter <- BATTER_POSITIONS[[pos_name]]
    
    base_name <- sprintf("batters_%s_%d", tolower(pos_name), era)
    png_file <- sprintf("%s/%s.png", OUTPUT_DIR, base_name)
    csv_file <- sprintf("%s/%s.csv", DATA_DIR, base_name)
    message(sprintf("[%d/%d] %s", current_plot, total_plots, png_file))
    
    p <- generate_franchise_war_plot(
      WAR_bat, 
      "Position Players", 
      start_year = era, 
      position_filter = pos_filter
    )
    ggsave(png_file, plot = p, width = 20, height = 24, units = "in", dpi = 150)
    export_plot_data(p, csv_file)
  }
  
  # --- Pitchers ---
  current_plot <- current_plot + 1
  base_name <- sprintf("pitchers_%d", era)
  png_file <- sprintf("%s/%s.png", OUTPUT_DIR, base_name)
  csv_file <- sprintf("%s/%s.csv", DATA_DIR, base_name)
  message(sprintf("[%d/%d] %s", current_plot, total_plots, png_file))
  
  p <- generate_franchise_war_plot(WAR_pitch, "Pitchers", start_year = era)
  ggsave(png_file, plot = p, width = 20, height = 24, units = "in", dpi = 150)
  export_plot_data(p, csv_file)
  
  # --- Combined ---
  current_plot <- current_plot + 1
  base_name <- sprintf("combined_%d", era)
  png_file <- sprintf("%s/%s.png", OUTPUT_DIR, base_name)
  csv_file <- sprintf("%s/%s.csv", DATA_DIR, base_name)
  message(sprintf("[%d/%d] %s", current_plot, total_plots, png_file))
  
  p <- generate_franchise_war_plot(WAR_combined, "All Players", start_year = era)
  ggsave(png_file, plot = p, width = 20, height = 24, units = "in", dpi = 150)
  export_plot_data(p, csv_file)
}

message(sprintf("\n=== Done! Generated %d plots in %s ===", total_plots, OUTPUT_DIR))
message(sprintf("Data tables exported to %s", DATA_DIR))
