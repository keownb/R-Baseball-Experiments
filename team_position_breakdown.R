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
    include_dh = TRUE,
    log_scale = FALSE,  # Use pseudo-log scale for y-axis (helps see RPs vs Ruth)
    show_postseason = FALSE  # Show vertical lines for postseason achievements
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
  # Also identify "stints" - continuous runs of years with the team
  team_cum <- team_agg %>%
    group_by(player_ID, position) %>%
    arrange(year_ID) %>%
    mutate(
      cumWAR = cumsum(WAR),
      max_year = max(year_ID),
      total_WAR = sum(WAR),
      # Detect gaps: if year jumps by more than 1, start a new stint
      year_gap = year_ID - lag(year_ID, default = first(year_ID)),
      stint = cumsum(year_gap > 1)
    ) %>%
    ungroup() %>%
    # Create unique stint ID for grouping lines
    mutate(stint_id = paste(player_ID, position, stint, sep = "_"))
  
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
  
  # For years where no top-5 player is active, identify the "best ghost"
  # First: find which years have at least one top player active per position
  years_with_top <- team_cum %>%
    filter(is_top) %>%
    distinct(position, year_ID)
  
  # Find the best non-top player in years WITHOUT a top player
  best_ghosts <- team_cum %>%
    filter(!is_top) %>%
    anti_join(years_with_top, by = c("position", "year_ID")) %>%
    group_by(position, year_ID) %>%
    slice_max(order_by = cumWAR, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(is_best_ghost = TRUE) %>%
    select(position, year_ID, player_ID, is_best_ghost)
  
  team_cum <- team_cum %>%
    left_join(best_ghosts, by = c("position", "year_ID", "player_ID")) %>%
    mutate(is_best_ghost = replace_na(is_best_ghost, FALSE))
  
  # Order positions logically - only include positions that have data
  position_order <- c("C", "1B", "2B", "SS", "3B", "OF", "DH", "SP", "RP")
  positions_with_data <- intersect(position_order, unique(team_cum$position))
  team_cum <- team_cum %>%
    filter(position %in% positions_with_data) %>%
    mutate(position = factor(position, levels = positions_with_data))
  
  # Get team color and secondary color for "best ghost" (runner-up) players
  team_color <- primary_colors[[team_code]]
  if (is.null(team_color)) team_color <- "#333333"
  ghost_highlight_color <- secondary_colors[[team_code]]
  if (is.null(ghost_highlight_color)) ghost_highlight_color <- "#C4CED4"
  
  # Get postseason achievements if requested
  # Map our franchise codes to Lahman team codes (reverse mapping)
  lahman_codes_map <- list(
    "NYY" = c("NYA"),
    "LAD" = c("LAN", "BRO"),
    "SFG" = c("SFN", "NYG"),
    "ATL" = c("ATL", "MLN", "BSN"),
    "BAL" = c("BAL", "SLA", "MLA"),
    "MIN" = c("MIN", "WS1", "WS2"),
    "OAK" = c("OAK", "PHA", "KCA"),
    "TEX" = c("TEX", "WS2"),
    "MIL" = c("MIL", "SEA"),
    "WSN" = c("WAS", "MON"),
    "CHC" = c("CHN"),
    "CIN" = c("CIN"),
    "STL" = c("SLN"),
    "PIT" = c("PIT"),
    "PHI" = c("PHI"),
    "BOS" = c("BOS"),
    "CLE" = c("CLE"),
    "DET" = c("DET"),
    "CHW" = c("CHA"),
    "SEA" = c("SEA"),
    "TBR" = c("TBA"),
    "TOR" = c("TOR"),
    "HOU" = c("HOU"),
    "LAA" = c("ANA", "CAL", "LAA"),
    "KCR" = c("KCA"),
    "ARI" = c("ARI"),
    "COL" = c("COL"),
    "SDP" = c("SDN"),
    "MIA" = c("FLO", "MIA")
  )
  lahman_codes <- lahman_codes_map[[team_code]]
  if (is.null(lahman_codes)) lahman_codes <- team_code
  
  postseason_data <- NULL
  if (show_postseason) {
    postseason_data <- Lahman::SeriesPost %>%
      filter(teamIDwinner %in% lahman_codes) %>%
      mutate(achievement = case_when(
        round == "WS" ~ "World Series",
        round %in% c("CS", "ALCS", "NLCS") ~ "Pennant/LCS",
        grepl("DS|DIV", round) ~ "Division Series",
        grepl("WC", round) ~ "Wild Card",
        TRUE ~ "Pennant/LCS"  # Historical pennants
      )) %>%
      distinct(yearID, achievement) %>%
      # Keep only highest achievement per year
      mutate(achievement = factor(achievement, 
        levels = c("World Series", "Pennant/LCS", "Division Series", "Wild Card"))) %>%
      group_by(yearID) %>%
      slice_min(achievement, n = 1) %>%
      ungroup()
  }
  
  # Create player category for legend
  team_cum <- team_cum %>%
    mutate(player_category = case_when(
      is_top ~ "Top 5 (Team WAR)",
      is_best_ghost ~ "Best in Gap Years (2+ yr)",
      TRUE ~ "Other Players"
    ))
  
  # Create the plot
  p <- ggplot(team_cum, aes(x = year_ID, y = cumWAR)) +
    # Ghost lines for ALL players (group by stint to break at gaps)
    geom_line(
      data = filter(team_cum, player_category == "Other Players"),
      aes(group = stint_id),
      color = "gray60",
      linewidth = 0.4,
      alpha = 0.6
    )
  
  # Add postseason vertical lines if requested
  if (show_postseason && !is.null(postseason_data) && nrow(postseason_data) > 0) {
    p <- p + geom_vline(
      data = postseason_data,
      aes(xintercept = yearID, linetype = achievement),
      alpha = 0.4,
      linewidth = 0.5
    ) +
    scale_linetype_manual(
      name = "Postseason",
      values = c(
        "World Series" = "solid",
        "Pennant/LCS" = "dashed",
        "Division Series" = "dotted",
        "Wild Card" = "dotdash"
      )
    )
  }
  
  p <- p +
    # Best ghost lines (years with no top player)
    geom_line(
      data = filter(team_cum, player_category == "Best in Gap Years (2+ yr)"),
      aes(group = stint_id, color = player_category),
      linewidth = 0.8,
      show.legend = TRUE
    ) +
    # Highlighted lines for top players
    geom_line(
      data = filter(team_cum, player_category == "Top 5 (Team WAR)"),
      aes(group = stint_id, color = player_category),
      linewidth = 1.2,
      show.legend = TRUE
    ) +
    # Color scale for legend
    scale_color_manual(
      name = NULL,
      values = c(
        "Top 5 (Team WAR)" = team_color,
        "Best in Gap Years (2+ yr)" = ghost_highlight_color
      ),
      guide = guide_legend(override.aes = list(linewidth = 2))
    ) +
    # Labels for top players (only stints with 2+ years)
    geom_label_repel(
      data = team_cum %>%
        filter(is_top) %>%
        group_by(position, player_ID, stint_id) %>%
        filter(n() >= 2) %>%  # Only label stints with 2+ years
        filter(year_ID == max(year_ID)) %>%
        ungroup(),
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
    # Labels for best ghosts (only if 2+ years as best ghost, per stint)
    geom_label_repel(
      data = team_cum %>%
        filter(is_best_ghost) %>%
        group_by(position, player_ID, stint_id) %>%
        filter(n() >= 2) %>%  # Only label if 2+ years as best ghost in this stint
        filter(year_ID == max(year_ID)) %>%
        ungroup(),
      aes(label = name_common),
      color = ghost_highlight_color,
      size = 2,
      fill = alpha("white", 0.85),
      label.size = 0,
      segment.size = 0.2,
      segment.alpha = 0.4,
      max.overlaps = 5,
      nudge_y = -1,
      box.padding = 0.15
    ) +
    # Facet by position - one row per position, same y-axis, x-axis on all
    ggh4x::facet_wrap2(~ position, ncol = 1, axes = "x") +
    # Axis settings - dynamic breaks based on team start year
    coord_cartesian(xlim = c(start_year, 2026)) +
    scale_x_continuous(
      breaks = seq(ceiling(start_year / 10) * 10, 2020, 10)  # Start at first decade
    )
  
  # Conditional y-scale: log or linear
  if (log_scale) {
    p <- p + scale_y_continuous(
      trans = "pseudo_log",  # Handles 0 and negative values
      breaks = c(0, 5, 10, 20, 50, 100, 150)
    )
  }
  
  p <- p +
    # Theme - light grid for ghost line contrast
    theme_minimal() +
    theme(
      legend.position = "top",
      legend.justification = "left",
      legend.margin = margin(0, 0, 10, 0),
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
      caption = "Source: Baseball Reference | Gray lines show all other players"
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

# =============================================================================
# TEST: NEW YORK YANKEES (with postseason overlay)
# =============================================================================

p_nyy <- generate_team_position_plot(
  WAR_bat,
  WAR_pitch,
  team_code = "NYY",
  team_name = "New York Yankees",
  show_postseason = TRUE,
  log_scale = FALSE  # Set TRUE to compress y-axis
)

n_pos <- attr(p_nyy, "n_positions")
message(sprintf("Positions found: %d", n_pos))
ggsave(
  "docs/team_breakdown_NYY.png",
  plot = p_nyy,
  width = 20,
  height = 3 * n_pos + 2,
  units = "in",
  dpi = 150
)
