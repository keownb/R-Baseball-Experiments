# generate_team_breakdowns.R
# Batch generation of team position breakdown charts
# Generates linear and log scale versions with postseason overlay

source("franchise_war_functions.R")
source("team_position_breakdown.R")

# =============================================================================
# CONFIGURATION
# =============================================================================

OUTPUT_DIR <- "docs/team_breakdowns"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# All 30 MLB teams with full names
TEAMS <- list(
  # AL East
  "BAL" = "Baltimore Orioles",
  "BOS" = "Boston Red Sox",
  "NYY" = "New York Yankees",
  "TBR" = "Tampa Bay Rays",
  "TOR" = "Toronto Blue Jays",
  # AL Central
  "CHW" = "Chicago White Sox",
  "CLE" = "Cleveland Guardians",
  "DET" = "Detroit Tigers",
  "KCR" = "Kansas City Royals",
  "MIN" = "Minnesota Twins",
  # AL West
  "HOU" = "Houston Astros",
  "LAA" = "Los Angeles Angels",
  "OAK" = "Oakland Athletics",
  "SEA" = "Seattle Mariners",
  "TEX" = "Texas Rangers",
  # NL East
  "ATL" = "Atlanta Braves",
  "MIA" = "Miami Marlins",
  "NYM" = "New York Mets",
  "PHI" = "Philadelphia Phillies",
  "WSN" = "Washington Nationals",
  # NL Central
  "CHC" = "Chicago Cubs",
  "CIN" = "Cincinnati Reds",
  "MIL" = "Milwaukee Brewers",
  "PIT" = "Pittsburgh Pirates",
  "STL" = "St. Louis Cardinals",
  # NL West
  "ARI" = "Arizona Diamondbacks",
  "COL" = "Colorado Rockies",
  "LAD" = "Los Angeles Dodgers",
  "SDP" = "San Diego Padres",
  "SFG" = "San Francisco Giants"
)

# =============================================================================
# BATCH GENERATE
# =============================================================================

# Generate two versions per team:
# 1. Linear scale with postseason overlay (primary)
# 2. Log scale with postseason overlay (for comparison)

total_plots <- length(TEAMS) * 2
current_plot <- 0

message(sprintf("\n=== Generating %d team breakdown plots ===\n", total_plots))

for (team_code in names(TEAMS)) {
  team_name <- TEAMS[[team_code]]
  
  # --- Linear scale version ---
  current_plot <- current_plot + 1
  png_file <- sprintf("%s/%s_linear.png", OUTPUT_DIR, team_code)
  message(sprintf("[%d/%d] %s (linear)", current_plot, total_plots, team_code))
  
  p <- generate_team_position_plot(
    WAR_bat,
    WAR_pitch,
    team_code = team_code,
    team_name = team_name,
    show_postseason = TRUE,
    log_scale = FALSE
  )
  
  n_pos <- attr(p, "n_positions")
  ggsave(
    png_file, 
    plot = p, 
    width = 20, 
    height = 3 * n_pos + 2,
    units = "in",
    dpi = 150
  )
  
  # --- Log scale version ---
  current_plot <- current_plot + 1
  png_file <- sprintf("%s/%s_log.png", OUTPUT_DIR, team_code)
  message(sprintf("[%d/%d] %s (log)", current_plot, total_plots, team_code))
  
  p <- generate_team_position_plot(
    WAR_bat,
    WAR_pitch,
    team_code = team_code,
    team_name = team_name,
    show_postseason = TRUE,
    log_scale = TRUE
  )
  
  ggsave(
    png_file, 
    plot = p, 
    width = 20, 
    height = 3 * n_pos + 2,
    units = "in",
    dpi = 150
  )
}

message(sprintf("\n=== Done! Generated %d plots in %s ===", total_plots, OUTPUT_DIR))
