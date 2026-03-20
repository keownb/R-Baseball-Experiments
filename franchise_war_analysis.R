# franchise_war_analysis.R
# Interactive testing script - use this to tweak and preview single plots
# Edit look/feel in franchise_war_functions.R

source("franchise_war_functions.R")

# =============================================================================
# LOAD DATA (only need to do once per session)
# =============================================================================

war_data <- load_war_data()
WAR_bat <- war_data$batting
WAR_pitch <- war_data$pitching

# Combined data for "All Players" view
common_cols <- c("year_ID", "player_ID", "name_common", "team_ID", "WAR", "player_type", "primary_pos")
WAR_combined <- bind_rows(
  WAR_bat %>% select(all_of(common_cols)),
  WAR_pitch %>% select(all_of(common_cols))
)

# =============================================================================
# TEST PLOTS - Uncomment what you want to preview
# =============================================================================

# Position Players - Full history
p <- generate_franchise_war_plot(WAR_bat, "Position Players", start_year = 1901)
print(p)

# Pitchers
# p <- generate_franchise_war_plot(WAR_pitch, "Pitchers", start_year = 1901)
# print(p)

# Combined
# p <- generate_franchise_war_plot(WAR_combined, "All Players", start_year = 1901)
# print(p)

# Position filtered examples
# p <- generate_franchise_war_plot(WAR_bat, "Position Players", start_year = 1901, position_filter = "SS")
# print(p)

# Era examples
# p <- generate_franchise_war_plot(WAR_bat, "Position Players", start_year = 1969)
# print(p)

# Save current test plot (optional)
# ggsave("docs/test_plot.png", plot = p, width = 20, height = 24, units = "in", dpi = 150)
