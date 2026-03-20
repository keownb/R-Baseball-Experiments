# team_position_breakdown.R
# Deep dive into one team's history by position
# Shows all players as ghost lines, top 5 per position highlighted

source("franchise_war_functions.R")

# =============================================================================
# LOAD DATA
# =============================================================================

war_data <- load_war_data()
WAR_bat <- war_data$batting
WAR_pitch <- war_data$pitching

# =============================================================================
# TEAM POSITION BREAKDOWN FUNCTION
# =============================================================================

generate_team_position_plot <- function(
    war_batting,
    war_pitching, 
    team_code,
    team_name = NULL,
    top_n = 5,
    include_dh = TRUE  # Include DH for AL teams
) {
  
  # Get team name if not provided
  if (is.null(team_name)) {
    team_name <- team_code
  }
  
  # Get primary position including DH from Appearances
  # DH is based on most games at DH vs other positions
  dh_players <- Lahman::Appearances %>%
    group_by(playerID) %>%
    summarise(
      dh_games = sum(G_dh, na.rm = TRUE),
      field_games = sum(G_defense, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(dh_games > field_games * 0.5) %>%  # Primarily DH if >50% DH
    select(playerID, is_primary_dh = dh_games) %>%
    mutate(is_primary_dh = TRUE)
  
  # Get SP vs RP classification PER TEAM from Lahman Pitching
  # Map Lahman teamID to our franchise codes
  pitcher_type <- Lahman::Pitching %>%
    mutate(franchise = franchise_map(teamID)) %>%
    filter(franchise == team_code) %>%  # Only this team's data
    group_by(playerID) %>%
    summarise(
      total_G = sum(G, na.rm = TRUE),
      total_GS = sum(GS, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(pitcher_role = if_else(total_GS > total_G * 0.5, "SP", "RP")) %>%
    select(playerID, pitcher_role)
  
  # Combine batting and pitching data
  common_cols <- c("year_ID", "player_ID", "name_common", "team_ID", "WAR", "primary_pos")
  
  war_all <- bind_rows(
    war_batting %>% select(all_of(common_cols)),
    war_pitching %>% select(all_of(common_cols))
  )
  
  # Filter to this franchise and clean
  team_data <- war_all %>%
    mutate(franchise = franchise_map(team_ID)) %>%
    filter(franchise == team_code, !is.na(WAR), !is.na(primary_pos)) %>%
    # Join DH info
    left_join(dh_players, by = c("player_ID" = "playerID")) %>%
    # Join pitcher type (SP/RP)
    left_join(pitcher_type, by = c("player_ID" = "playerID")) %>%
    # Consolidate OF positions and assign DH, split P into SP/RP
    mutate(position = case_when(
      include_dh & !is.na(is_primary_dh) ~ "DH",
      primary_pos %in% c("LF", "CF", "RF") ~ "OF",
      primary_pos == "P" & pitcher_role == "RP" ~ "RP",
      primary_pos == "P" ~ "SP",
      TRUE ~ primary_pos
    )) %>%
    # Keep standard positions
    filter(position %in% c("C", "1B", "2B", "SS", "3B", "OF", "DH", "SP", "RP"))
  
  # Determine team's start year
  start_year <- min(team_data$year_ID)
  
  # Aggregate by player-position-year (handles multiple stints)
  team_agg <- team_data %>%
    group_by(player_ID, name_common, position, year_ID) %>%
    summarise(WAR = sum(WAR, na.rm = TRUE), .groups = "drop")
  
  # Calculate cumulative WAR by player-position
  team_cum <- team_agg %>%
    group_by(player_ID, position) %>%
    arrange(year_ID) %>%
    mutate(
      cumWAR = cumsum(WAR),
      max_year = max(year_ID),
      total_WAR = sum(WAR)
    ) %>%
    ungroup()
  
  # Identify top N players per position
  top_players <- team_cum %>%
    group_by(position, player_ID) %>%
    summarise(
      total_WAR = sum(WAR, na.rm = TRUE),
      years_played = n_distinct(year_ID),
      .groups = "drop"
    ) %>%
    filter(years_played >= 2) %>%
    group_by(position) %>%
    slice_max(order_by = total_WAR, n = top_n) %>%
    mutate(is_top = TRUE) %>%
    select(position, player_ID, is_top)
  
  team_cum <- team_cum %>%
    left_join(top_players, by = c("position", "player_ID")) %>%
    mutate(is_top = replace_na(is_top, FALSE))
  
  # Order positions logically - only include positions that have data
  position_order <- c("C", "1B", "2B", "SS", "3B", "OF", "DH", "SP", "RP")
  positions_with_data <- intersect(position_order, unique(team_cum$position))
  team_cum <- team_cum %>%
    filter(position %in% positions_with_data) %>%
    mutate(position = factor(position, levels = positions_with_data))
  
  # Get team color
  team_color <- primary_colors[[team_code]]
  if (is.null(team_color)) team_color <- "#333333"
  
  # Create the plot
  p <- ggplot(team_cum, aes(x = year_ID, y = cumWAR)) +
    # Ghost lines for ALL players (more visible)
    geom_line(
      aes(group = player_ID),
      color = "gray60",
      linewidth = 0.4,
      alpha = 0.6
    ) +
    # Highlighted lines for top players
    geom_line(
      data = filter(team_cum, is_top),
      aes(group = player_ID),
      color = team_color,
      linewidth = 1.2
    ) +
    # Labels for top players
    geom_label_repel(
      data = filter(team_cum, is_top & year_ID == max_year),
      aes(label = name_common),
      color = team_color,
      size = 2.5,
      fill = alpha("white", 0.9),
      label.size = 0,
      segment.size = 0.3,
      segment.alpha = 0.5,
      max.overlaps = 10,
      nudge_y = 2,
      box.padding = 0.2
    ) +
    # Facet by position - one row per position, same y-axis, x-axis on all
    ggh4x::facet_wrap2(~ position, ncol = 1, axes = "x") +
    # Axis settings
    coord_cartesian(xlim = c(start_year, 2026)) +
    scale_x_continuous(breaks = seq(1980, 2020, 10)) +
    # Theme - light grid for ghost line contrast
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.spacing = unit(1, "lines"),
      strip.text = element_text(face = "bold", size = 11),
      axis.text = element_text(size = 8),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "#666")
    ) +
    labs(
      title = paste0(team_name, " — Cumulative WAR by Position"),
      subtitle = paste0("Top ", top_n, " players per position highlighted (", start_year, "-Present)"),
      x = "Year",
      y = "Cumulative WAR",
      caption = "Source: Baseball Reference | Ghost lines show all players"
    )
  
  # Attach position count for dynamic sizing
  attr(p, "n_positions") <- length(positions_with_data)
  
  return(p)
}

# =============================================================================
# TEST: SEATTLE MARINERS
# =============================================================================

p_sea <- generate_team_position_plot(
  WAR_bat,
  WAR_pitch,
  team_code = "SEA",
  team_name = "Seattle Mariners"
)

# print(p_sea)  # Skip if plot window is small - just save directly

# Save - height based on number of positions
n_pos <- attr(p_sea, "n_positions")
message(sprintf("Positions found: %d", n_pos))
ggsave(
  "docs/team_breakdown_SEA.png",
  plot = p_sea,
  width = 20,
  height = 3 * n_pos + 2,  # ~3 inches per position + margins
  units = "in",
  dpi = 150
)
