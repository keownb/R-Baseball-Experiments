# experiment_density.R
# Prototype: variable line width (seasonal WAR) + award markers
# Test with Mariners first

source("franchise_war_functions.R")

war_data <- load_war_data()
WAR_bat <- war_data$batting
WAR_pitch <- war_data$pitching

# =============================================================================
# AWARD DATA
# =============================================================================

# Major individual awards from Lahman
awards_raw <- Lahman::AwardsPlayers %>%
  filter(awardID %in% c(
    "Most Valuable Player", "Cy Young Award",
    "Gold Glove", "Silver Slugger"
  )) %>%
  mutate(award = case_when(
    awardID == "Most Valuable Player" ~ "MVP",
    awardID == "Cy Young Award" ~ "CYA",
    awardID == "Gold Glove" ~ "GG",
    awardID == "Silver Slugger" ~ "SS"
  )) %>%
  select(playerID, yearID, award)

allstars <- Lahman::AllstarFull %>%
  distinct(playerID, yearID) %>%
  mutate(award = "AS")

all_awards <- bind_rows(awards_raw, allstars)

# =============================================================================
# LEAGUE-WIDE POSITION-YEAR WAR DISTRIBUTIONS (for z-score width)
# =============================================================================
# Compute mean/SD of WAR per position per year across the entire league
# so we can show "how dominant was this player at their position that year?"

# Need league-wide position assignments
# Pitchers: SP vs RP career-wide
league_pitcher_type <- Lahman::Pitching %>%
  group_by(playerID) %>%
  summarise(
    total_G = sum(G, na.rm = TRUE),
    total_GS = sum(GS, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(pitcher_role = if_else(total_GS > total_G * 0.5, "SP", "RP")) %>%
  select(playerID, pitcher_role)

# DH detection (same as team-level)
league_dh_players <- Lahman::Appearances %>%
  group_by(playerID) %>%
  summarise(
    dh_games = sum(G_dh, na.rm = TRUE),
    field_games = sum(G_defense, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(dh_games > field_games * 0.5) %>%
  mutate(is_primary_dh = TRUE) %>%
  select(playerID, is_primary_dh)

# Build league-wide WAR by position-year
common_cols <- c("year_ID", "player_ID", "name_common", "team_ID", "WAR", "primary_pos")
league_war <- bind_rows(
  WAR_bat %>% select(all_of(common_cols)),
  WAR_pitch %>% select(all_of(common_cols))
) %>%
  filter(!is.na(WAR), !is.na(primary_pos)) %>%
  left_join(league_dh_players, by = c("player_ID" = "playerID")) %>%
  left_join(league_pitcher_type, by = c("player_ID" = "playerID")) %>%
  mutate(position = case_when(
    !is.na(is_primary_dh) ~ "DH",
    primary_pos %in% c("LF", "CF", "RF") ~ "OF",
    primary_pos == "P" & pitcher_role == "RP" ~ "RP",
    primary_pos == "P" ~ "SP",
    TRUE ~ primary_pos
  )) %>%
  filter(position %in% c("C", "1B", "2B", "SS", "3B", "OF", "DH", "SP", "RP")) %>%
  # Aggregate player-year-position (handle mid-season trades)
  group_by(player_ID, position, year_ID) %>%
  summarise(WAR = sum(WAR, na.rm = TRUE), .groups = "drop")

# Position-year distributions
pos_year_stats <- league_war %>%
  group_by(position, year_ID) %>%
  summarise(
    mean_WAR = mean(WAR, na.rm = TRUE),
    sd_WAR = sd(WAR, na.rm = TRUE),
    n_players = n(),
    .groups = "drop"
  ) %>%
  # Floor SD to avoid division by near-zero in thin years
  mutate(sd_WAR = pmax(sd_WAR, 0.5))

message(sprintf("Computed position-year stats: %d position-years", nrow(pos_year_stats)))

# =============================================================================
# DENSE TEAM POSITION PLOT
# =============================================================================

generate_dense_team_plot <- function(
    war_batting, war_pitching, team_code, team_name = NULL,
    top_n = 5, include_dh = TRUE, log_scale = FALSE,
    show_postseason = TRUE,
    variable_width = TRUE,
    show_awards = TRUE
) {
  if (is.null(team_name)) team_name <- team_code
  
  # --- Data pipeline (same as team_position_breakdown.R) ---
  
  dh_players <- Lahman::Appearances %>%
    group_by(playerID) %>%
    summarise(
      dh_games = sum(G_dh, na.rm = TRUE),
      field_games = sum(G_defense, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(dh_games > field_games * 0.5) %>%
    mutate(is_primary_dh = TRUE) %>%
    select(playerID, is_primary_dh)
  
  pitcher_type <- Lahman::Pitching %>%
    mutate(franchise = franchise_map(teamID)) %>%
    filter(franchise == team_code) %>%
    group_by(playerID) %>%
    summarise(
      total_G = sum(G, na.rm = TRUE),
      total_GS = sum(GS, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(pitcher_role = if_else(total_GS > total_G * 0.5, "SP", "RP")) %>%
    select(playerID, pitcher_role)
  
  common_cols <- c("year_ID", "player_ID", "name_common", "team_ID", "WAR", "primary_pos")
  
  war_all <- bind_rows(
    war_batting %>% select(all_of(common_cols)),
    war_pitching %>% select(all_of(common_cols))
  )
  
  team_data <- war_all %>%
    mutate(franchise = franchise_map(team_ID)) %>%
    filter(franchise == team_code, !is.na(WAR), !is.na(primary_pos)) %>%
    left_join(dh_players, by = c("player_ID" = "playerID")) %>%
    left_join(pitcher_type, by = c("player_ID" = "playerID")) %>%
    mutate(position = case_when(
      include_dh & !is.na(is_primary_dh) ~ "DH",
      primary_pos %in% c("LF", "CF", "RF") ~ "OF",
      primary_pos == "P" & pitcher_role == "RP" ~ "RP",
      primary_pos == "P" ~ "SP",
      TRUE ~ primary_pos
    )) %>%
    filter(position %in% c("C", "1B", "2B", "SS", "3B", "OF", "DH", "SP", "RP"))
  
  start_year <- min(team_data$year_ID)
  
  team_agg <- team_data %>%
    group_by(player_ID, name_common, position, year_ID) %>%
    summarise(WAR = sum(WAR, na.rm = TRUE), .groups = "drop")
  
  team_cum <- team_agg %>%
    group_by(player_ID, position) %>%
    arrange(year_ID) %>%
    mutate(
      cumWAR = cumsum(WAR),
      max_year = max(year_ID),
      total_WAR = sum(WAR),
      year_gap = year_ID - lag(year_ID, default = first(year_ID)),
      stint = cumsum(year_gap > 1)
    ) %>%
    ungroup() %>%
    mutate(stint_id = paste(player_ID, position, stint, sep = "_"))
  
  # Top N per position
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
  
  # Best ghosts
  years_with_top <- team_cum %>%
    filter(is_top) %>%
    distinct(position, year_ID)
  
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
  
  # Position ordering — filter first, set factor levels AFTER all joins
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
      is_best_ghost ~ "Best in Gap Years",
      TRUE ~ "Other Players"
    ))
  
  # --- NEW: Segment data for variable line width (z-score) ---
  # Width = how many SDs above mean WAR for that position-year league-wide
  # Decouples from slope: slope = raw WAR, width = positional dominance
  if (variable_width) {
    # Join league-wide stats and compute z-score (join before factoring position)
    team_cum <- team_cum %>%
      left_join(pos_year_stats, by = c("position", "year_ID")) %>%
      mutate(
        war_zscore = (WAR - mean_WAR) / sd_WAR,
        # Clamp: below-average seasons get minimum width, cap at 4 SD
        war_zscore_clamped = pmin(pmax(war_zscore, 0), 4)
      )
    
    top_segments <- team_cum %>%
      filter(is_top) %>%
      group_by(stint_id) %>%
      arrange(year_ID) %>%
      mutate(
        next_year = lead(year_ID),
        next_cumWAR = lead(cumWAR),
        # Width encodes the DESTINATION year's z-score
        width_z = lead(war_zscore_clamped)
      ) %>%
      filter(!is.na(next_year), !is.na(width_z)) %>%
      ungroup() %>%
      mutate(position = factor(position, levels = positions_with_data))
  }
  
  # NOW set position factor levels (after all joins that touch position are done)
  team_cum <- team_cum %>%
    mutate(position = factor(position, levels = positions_with_data))
  
  # --- NEW: Award markers for top players ---
  if (show_awards) {
    award_markers <- team_cum %>%
      filter(is_top) %>%
      inner_join(all_awards, by = c("player_ID" = "playerID", "year_ID" = "yearID")) %>%
      mutate(
        award = factor(award, levels = c("MVP", "CYA", "GG", "SS", "AS")),
        position = factor(position, levels = positions_with_data)
      )
  }
  
  # --- Postseason data (same as before) ---
  lahman_codes_map <- list(
    "NYY" = c("NYA"), "LAD" = c("LAN", "BRO"), "SFG" = c("SFN", "NYG"),
    "ATL" = c("ATL", "MLN", "BSN"), "BAL" = c("BAL", "SLA", "MLA"),
    "MIN" = c("MIN", "WS1", "WS2"), "OAK" = c("OAK", "PHA", "KCA"),
    "TEX" = c("TEX", "WS2"), "MIL" = c("MIL", "SEA"),
    "WSN" = c("WAS", "MON"), "CHC" = c("CHN"), "CIN" = c("CIN"),
    "STL" = c("SLN"), "PIT" = c("PIT"), "PHI" = c("PHI"),
    "BOS" = c("BOS"), "CLE" = c("CLE"), "DET" = c("DET"),
    "CHW" = c("CHA"), "SEA" = c("SEA"), "TBR" = c("TBA"),
    "TOR" = c("TOR"), "HOU" = c("HOU"), "LAA" = c("ANA", "CAL", "LAA"),
    "KCR" = c("KCA"), "ARI" = c("ARI"), "COL" = c("COL"),
    "SDP" = c("SDN"), "MIA" = c("FLO", "MIA")
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
        TRUE ~ "Pennant/LCS"
      )) %>%
      distinct(yearID, achievement) %>%
      mutate(achievement = factor(achievement,
        levels = c("World Series", "Pennant/LCS", "Division Series", "Wild Card"))) %>%
      group_by(yearID) %>%
      slice_min(achievement, n = 1) %>%
      ungroup()
  }
  
  # ==========================================================================
  # BUILD PLOT
  # ==========================================================================
  
  p <- ggplot(team_cum, aes(x = year_ID, y = cumWAR))
  
  # Layer 1: Ghost lines (thin, constant width)
  p <- p + geom_line(
    data = filter(team_cum, player_category == "Other Players"),
    aes(group = stint_id),
    color = "gray75", linewidth = 0.3, alpha = 0.5
  )
  
  # Layer 2: Postseason verticals
  if (show_postseason && !is.null(postseason_data) && nrow(postseason_data) > 0) {
    p <- p + geom_vline(
      data = postseason_data,
      aes(xintercept = yearID, linetype = achievement),
      alpha = 0.35, linewidth = 0.4
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
  
  # Layer 3: Best ghost lines (constant width)
  p <- p + geom_line(
    data = filter(team_cum, player_category == "Best in Gap Years"),
    aes(group = stint_id, color = player_category),
    linewidth = 0.7, show.legend = TRUE
  )
  
  # Layer 4: Top players - variable width segments OR constant lines
  if (variable_width) {
    p <- p + geom_segment(
      data = top_segments,
      aes(
        x = year_ID, xend = next_year,
        y = cumWAR, yend = next_cumWAR,
        linewidth = width_z,
        color = player_category
      ),
      lineend = "round",
      linejoin = "round",
      show.legend = TRUE
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
  
  # Layer 5: Award markers — single geom_point, vary size/fill via aesthetics
  if (show_awards && nrow(award_markers) > 0) {
    award_display <- award_markers %>%
      filter(award %in% c("MVP", "CYA", "AS"))
    
    if (nrow(award_display) > 0) {
      p <- p + geom_point(
        data = award_display,
        aes(shape = award, size = award, fill = award),
        color = "black",
        stroke = 0.5
      ) +
      scale_shape_manual(
        name = "Awards",
        values = c("MVP" = 23, "CYA" = 24, "AS" = 21)
      ) +
      scale_size_manual(
        name = "Awards",
        values = c("MVP" = 4, "CYA" = 4, "AS" = 1.5)
      ) +
      scale_fill_manual(
        name = "Awards",
        values = c("MVP" = "gold", "CYA" = "gold", "AS" = "gray80")
      )
    }
  }
  
  # Dynamic nudge: ~10% of the max cumWAR so connectors are visible at any scale
  max_cum <- max(team_cum$cumWAR, na.rm = TRUE)
  label_nudge <- max(max_cum * 0.10, 5)
  ghost_nudge <- -max(max_cum * 0.07, 3)
  
  # Layer 6: Labels
  p <- p +
    scale_color_manual(
      name = NULL,
      values = c(
        "Top 5 (Team WAR)" = team_color,
        "Best in Gap Years" = ghost_highlight_color
      ),
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
    )
  
  # Facet + axes
  p <- p +
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
  
  # Theme
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
      linewidth = guide_legend(order = 4, override.aes = list(color = team_color))
    ) +
    labs(
      title = paste0(team_name, " — Cumulative WAR by Position"),
      subtitle = paste0("Line width = positional dominance (σ above league mean) | Top ", top_n, 
                        " per position | Awards marked (", start_year, "-Present)"),
      x = "Year",
      y = "Cumulative WAR",
      caption = "Source: Baseball Reference + Lahman"
    )
  
  attr(p, "n_positions") <- length(positions_with_data)
  return(p)
}

# =============================================================================
# GENERATE TEST: MARINERS
# =============================================================================

message("Generating Mariners dense plot...")
p <- generate_dense_team_plot(
  WAR_bat, WAR_pitch,
  team_code = "SEA",
  team_name = "Seattle Mariners",
  show_postseason = TRUE,
  variable_width = TRUE,
  show_awards = TRUE,
  log_scale = FALSE
)

n_pos <- attr(p, "n_positions")
ggsave(
  "experiment_mariners_dense.png",
  plot = p, width = 20, height = 3 * n_pos + 2, dpi = 150
)
message("Saved: experiment_mariners_dense.png")

# Also do Yankees for comparison (long history, big numbers)
message("Generating Yankees dense plot...")
p2 <- generate_dense_team_plot(
  WAR_bat, WAR_pitch,
  team_code = "NYY",
  team_name = "New York Yankees",
  show_postseason = TRUE,
  variable_width = TRUE,
  show_awards = TRUE,
  log_scale = FALSE
)

n_pos2 <- attr(p2, "n_positions")
ggsave(
  "experiment_yankees_dense.png",
  plot = p2, width = 20, height = 3 * n_pos2 + 2, dpi = 150
)
# Also do log scale versions
message("Generating Mariners dense plot (log)...")
p3 <- generate_dense_team_plot(
  WAR_bat, WAR_pitch,
  team_code = "SEA",
  team_name = "Seattle Mariners",
  show_postseason = TRUE,
  variable_width = TRUE,
  show_awards = TRUE,
  log_scale = TRUE
)
n_pos3 <- attr(p3, "n_positions")
ggsave("experiment_mariners_dense_log.png", plot = p3, width = 20, height = 3 * n_pos3 + 2, dpi = 150)
message("Saved: experiment_mariners_dense_log.png")

message("Generating Yankees dense plot (log)...")
p4 <- generate_dense_team_plot(
  WAR_bat, WAR_pitch,
  team_code = "NYY",
  team_name = "New York Yankees",
  show_postseason = TRUE,
  variable_width = TRUE,
  show_awards = TRUE,
  log_scale = TRUE
)
n_pos4 <- attr(p4, "n_positions")
ggsave("experiment_yankees_dense_log.png", plot = p4, width = 20, height = 3 * n_pos4 + 2, dpi = 150)
message("Saved: experiment_yankees_dense_log.png")
