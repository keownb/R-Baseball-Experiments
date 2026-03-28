# generate_team_breakdowns.R
# Batch generation of team position breakdown charts
# Generates: constant + z-score bases (× linear/log), plus overlays

source("franchise_war_functions.R")
source("team_position_breakdown.R")

# =============================================================================
# CONFIGURATION
# =============================================================================

OUTPUT_DIR <- "docs/team_breakdowns"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

PLOT_WIDTH <- 20
PLOT_DPI <- 150

TEAMS <- list(
  # AL East
  "BAL" = "Baltimore Orioles", "BOS" = "Boston Red Sox",
  "NYY" = "New York Yankees", "TBR" = "Tampa Bay Rays", "TOR" = "Toronto Blue Jays",
  # AL Central
  "CHW" = "Chicago White Sox", "CLE" = "Cleveland Guardians",
  "DET" = "Detroit Tigers", "KCR" = "Kansas City Royals", "MIN" = "Minnesota Twins",
  # AL West
  "HOU" = "Houston Astros", "LAA" = "Los Angeles Angels",
  "OAK" = "Oakland Athletics", "SEA" = "Seattle Mariners", "TEX" = "Texas Rangers",
  # NL East
  "ATL" = "Atlanta Braves", "MIA" = "Miami Marlins",
  "NYM" = "New York Mets", "PHI" = "Philadelphia Phillies", "WSN" = "Washington Nationals",
  # NL Central
  "CHC" = "Chicago Cubs", "CIN" = "Cincinnati Reds",
  "MIL" = "Milwaukee Brewers", "PIT" = "Pittsburgh Pirates", "STL" = "St. Louis Cardinals",
  # NL West
  "ARI" = "Arizona Diamondbacks", "COL" = "Colorado Rockies",
  "LAD" = "Los Angeles Dodgers", "SDP" = "San Diego Padres", "SFG" = "San Francisco Giants"
)

# =============================================================================
# LOAD SHARED DATA
# =============================================================================

message("=== Loading shared data ===")
pos_year_stats <- compute_position_year_stats(WAR_bat, WAR_pitch)
award_data <- load_award_data()

# =============================================================================
# BATCH GENERATE
# =============================================================================

# Per team: 2 scales × 2 widths × 4 overlay combos = 16 images
# 30 teams × 16 = 480 total

OVERLAY_COMBOS <- list(
  list(suffix = "",            awards = FALSE, postseason = FALSE),
  list(suffix = "_awards",     awards = TRUE,  postseason = FALSE),
  list(suffix = "_postseason", awards = FALSE, postseason = TRUE),
  list(suffix = "_all",        awards = TRUE,  postseason = TRUE)
)

n_teams <- length(TEAMS)
scales <- c("linear", "log")
widths <- c("const", "zscore")
total <- n_teams * length(scales) * length(widths) * length(OVERLAY_COMBOS)
current <- 0

message(sprintf("\n=== Generating %d team breakdown images ===\n", total))

for (team_code in names(TEAMS)) {
  team_name <- TEAMS[[team_code]]
  
  for (scale_name in scales) {
    is_log <- scale_name == "log"
    
    for (width_name in widths) {
      is_zscore <- width_name == "zscore"
      
      for (oc in OVERLAY_COMBOS) {
        current <- current + 1
        fname <- sprintf("%s/%s_%s_%s%s.png", OUTPUT_DIR, team_code, scale_name, width_name, oc$suffix)
        message(sprintf("[%d/%d] %s", current, total, basename(fname)))
        
        p <- generate_team_position_plot(
          WAR_bat, WAR_pitch,
          team_code = team_code, team_name = team_name,
          show_postseason = oc$postseason,
          log_scale = is_log,
          variable_width = is_zscore,
          pos_year_stats = if (is_zscore) pos_year_stats else NULL,
          show_awards = oc$awards,
          award_data = if (oc$awards) award_data else NULL
        )
        
        n_pos <- attr(p, "n_positions")
        plot_height <- 3 * n_pos + 2
        ggsave(fname, plot = p, width = PLOT_WIDTH, height = plot_height, dpi = PLOT_DPI)
      }
    }
  }
}

message(sprintf("\n=== Done! Generated %d images in %s ===", total, OUTPUT_DIR))
