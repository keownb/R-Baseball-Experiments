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
    log_scale = FALSE,
    show_postseason = FALSE,
    variable_width = FALSE,
    pos_year_stats = NULL,
    show_awards = FALSE,
    award_data = NULL
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
    mutate(franchise = franchise_map(teamID, yearID)) %>%
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
    mutate(franchise = franchise_map(team_ID, year_ID)) %>%
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
  
  # Order positions — filter first, set factor AFTER z-score join
  position_order <- c("C", "1B", "2B", "SS", "3B", "OF", "DH", "SP", "RP")
  positions_with_data <- intersect(position_order, unique(team_cum$position))
  team_cum <- team_cum %>%
    filter(position %in% positions_with_data)
  
  team_color <- primary_colors[[team_code]]
  if (is.null(team_color)) team_color <- "#333333"
  ghost_highlight_color <- secondary_colors[[team_code]]
  if (is.null(ghost_highlight_color)) ghost_highlight_color <- "#C4CED4"
  
  # Player category
  team_cum <- team_cum %>%
    mutate(player_category = case_when(
      is_top ~ "Top 5 (Team WAR)",
      is_best_ghost ~ "Best in Gap Years (2+ yr)",
      TRUE ~ "Other Players"
    ))
  
  # --- Z-score width for top players ---
  top_segments <- NULL
  if (variable_width && !is.null(pos_year_stats)) {
    team_cum <- team_cum %>%
      left_join(pos_year_stats, by = c("position", "year_ID")) %>%
      mutate(
        war_zscore = (WAR - mean_WAR) / sd_WAR,
        war_zscore_clamped = pmin(pmax(war_zscore, 0), 4)
      )
    
    top_segments <- team_cum %>%
      filter(is_top) %>%
      group_by(stint_id) %>%
      arrange(year_ID) %>%
      mutate(
        next_year = lead(year_ID),
        next_cumWAR = lead(cumWAR),
        width_z = lead(war_zscore_clamped)
      ) %>%
      filter(!is.na(next_year), !is.na(width_z)) %>%
      ungroup() %>%
      mutate(position = factor(position, levels = positions_with_data))
  }
  
  # Set position factor levels AFTER z-score join
  team_cum <- team_cum %>%
    mutate(position = factor(position, levels = positions_with_data))
  
  # --- Award markers ---
  award_markers <- NULL
  if (show_awards && !is.null(award_data)) {
    award_markers <- team_cum %>%
      filter(is_top) %>%
      inner_join(award_data, by = c("player_ID" = "playerID", "year_ID" = "yearID"),
                 relationship = "many-to-many") %>%
      filter(award %in% c("MVP", "CYA", "AS")) %>%
      mutate(
        award = factor(award, levels = c("MVP", "CYA", "AS")),
        position = factor(position, levels = positions_with_data)
      )
  }
  
  # --- Postseason data (using shared helper) ---
  postseason_data <- NULL
  if (show_postseason) {
    postseason_data <- get_postseason_data(team_code)
  }
  
  # Create the plot
  p <- ggplot(team_cum, aes(x = year_ID, y = cumWAR)) +
    geom_line(
      data = filter(team_cum, player_category == "Other Players"),
      aes(group = stint_id),
      color = "gray75", linewidth = 0.3, alpha = 0.5
    )
  
  # Postseason vertical lines
  if (show_postseason && !is.null(postseason_data) && nrow(postseason_data) > 0) {
    p <- p + geom_vline(
      data = postseason_data,
      aes(xintercept = yearID, linetype = achievement),
      alpha = 0.35, linewidth = 0.4
    ) +
    scale_linetype_manual(name = "Postseason",
      values = c("World Series" = "solid", "Pennant/LCS" = "dashed",
                 "Division Series" = "dotted", "Wild Card" = "dotdash"))
  }
  
  # Best ghost lines
  p <- p + geom_line(
    data = filter(team_cum, player_category == "Best in Gap Years (2+ yr)"),
    aes(group = stint_id, color = player_category),
    linewidth = 0.7, show.legend = TRUE
  )
  
  # Top players: z-score segments or constant lines
  if (!is.null(top_segments) && nrow(top_segments) > 0) {
    p <- p + geom_segment(
      data = top_segments,
      aes(x = year_ID, xend = next_year, y = cumWAR, yend = next_cumWAR,
          linewidth = width_z, color = player_category),
      lineend = "round", linejoin = "round", show.legend = TRUE
    ) +
    scale_linewidth_continuous(
      name = "Positional Dominance (σ above mean)",
      range = c(0.3, 5),
      breaks = c(0, 1, 2, 3, 4),
      labels = c("Avg", "+1σ", "+2σ", "+3σ", "+4σ")
    )
  } else {
    p <- p + geom_line(
      data = filter(team_cum, player_category == "Top 5 (Team WAR)"),
      aes(group = stint_id, color = player_category),
      linewidth = 1.2, show.legend = TRUE
    )
  }
  
  # Award markers
  if (!is.null(award_markers) && nrow(award_markers) > 0) {
    p <- p + geom_point(
      data = award_markers,
      aes(shape = award, size = award, fill = award),
      color = "black", stroke = 0.5
    ) +
    scale_shape_manual(name = "Awards",
      values = c("MVP" = 23, "CYA" = 24, "AS" = 21)) +
    scale_size_manual(name = "Awards",
      values = c("MVP" = 4, "CYA" = 4, "AS" = 1.5)) +
    scale_fill_manual(name = "Awards",
      values = c("MVP" = "gold", "CYA" = "gold", "AS" = "gray80"))
  }
  
  # Color scale + labels
  p <- p +
    scale_color_manual(name = NULL,
      values = c("Top 5 (Team WAR)" = team_color,
                 "Best in Gap Years (2+ yr)" = ghost_highlight_color),
      guide = guide_legend(override.aes = list(linewidth = 2))
    ) +
    geom_label_repel(
      data = team_cum %>%
        filter(is_top) %>%
        group_by(position, player_ID, stint_id) %>%
        filter(n() >= 2) %>%
        filter(year_ID == max(year_ID)) %>%
        ungroup(),
      aes(label = name_common),
      color = team_color,
      size = 2.5, fill = alpha("white", 0.9), label.size = 0,
      segment.size = 0.3, segment.alpha = 0.5,
      max.overlaps = 10, nudge_y = 2, box.padding = 0.2
    ) +
    geom_label_repel(
      data = team_cum %>%
        filter(is_best_ghost) %>%
        group_by(position, player_ID, stint_id) %>%
        filter(n() >= 2) %>%
        filter(year_ID == max(year_ID)) %>%
        ungroup(),
      aes(label = name_common),
      color = ghost_highlight_color,
      size = 2, fill = alpha("white", 0.85), label.size = 0,
      segment.size = 0.2, segment.alpha = 0.4,
      max.overlaps = 5, nudge_y = -1, box.padding = 0.15
    ) +
    ggh4x::facet_wrap2(~ position, ncol = 1, axes = "x") +
    coord_cartesian(xlim = c(start_year, 2026)) +
    scale_x_continuous(
      breaks = seq(ceiling(start_year / 10) * 10, 2020, 10)
    )
  
  if (log_scale) {
    p <- p + scale_y_continuous(
      trans = "pseudo_log",
      breaks = c(0, 5, 10, 20, 50, 100, 150)
    )
  }
  
  # Build subtitle
  subtitle_parts <- paste0("Top ", top_n, " players per position highlighted")
  if (!is.null(top_segments) && nrow(top_segments) > 0) {
    subtitle_parts <- paste0(subtitle_parts, " | Line width = positional dominance (σ)")
  }
  
  p <- p +
    theme_minimal() +
    theme(
      legend.position = "top",
      legend.justification = "left",
      legend.box = "horizontal",
      legend.box.just = "left",
      legend.margin = margin(0, 0, 5, 0),
      legend.spacing.x = unit(0.8, "cm"),
      panel.spacing = unit(0.8, "lines"),
      strip.text = element_text(face = "bold", size = 11),
      axis.text = element_text(size = 8),
      panel.grid.major = element_line(color = "gray92", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "#666")
    ) +
    guides(
      color = guide_legend(order = 1),
      linetype = guide_legend(order = 2),
      shape = guide_legend(order = 3),
      size = guide_legend(order = 3),
      fill = guide_legend(order = 3),
      linewidth = guide_legend(order = 4)
    ) +
    labs(
      title = paste0(team_name, " — Cumulative WAR by Position"),
      subtitle = paste0(subtitle_parts, " (", start_year, "-Present)"),
      x = "Year", y = "Cumulative WAR",
      caption = "Source: Baseball Reference + Lahman"
    )
  
  # Attach position count for dynamic sizing
  attr(p, "n_positions") <- length(positions_with_data)
  
  return(p)
}

# =============================================================================
# USAGE EXAMPLE (uncomment to test individual teams)
# =============================================================================

# # Load data first
# war_data <- load_war_data()
# WAR_bat <- war_data$batting
# WAR_pitch <- war_data$pitching
#
# # Generate a single team plot
# p <- generate_team_position_plot(
#   WAR_bat,
#   WAR_pitch,
#   team_code = "SEA",
#   team_name = "Seattle Mariners",
#   show_postseason = TRUE,
#   log_scale = FALSE
# )
#
# # Save it
# n_pos <- attr(p, "n_positions")
# ggsave("test_output.png", plot = p, width = 20, height = 3 * n_pos + 2, dpi = 150)

