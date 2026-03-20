# franchise_war_functions.R
# Core functions and data for WAR visualizations
# Edit look/feel here, then test in franchise_war_analysis.R

# =============================================================================
# PACKAGES
# =============================================================================

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(ggrepel)
library(ggh4x)
library(Lahman)

# =============================================================================
# FRANCHISE MAPPING - All historical team IDs to current 30
# =============================================================================

franchise_map <- function(team_id) {
  case_when(
    # AL West
    team_id %in% c("LAA", "ANA", "CAL") ~ "LAA",
    team_id %in% c("OAK", "KCA", "PHA") ~ "OAK",
    team_id == "SEA" ~ "SEA",
    team_id == "HOU" ~ "HOU",
    team_id %in% c("TEX", "WSA") ~ "TEX",
    
    # AL Central
    team_id == "CHW" ~ "CHW",
    team_id == "CLE" ~ "CLE",
    team_id == "DET" ~ "DET",
    team_id == "KCR" ~ "KCR",
    team_id %in% c("MIN", "WSH") ~ "MIN",
    
    # AL East
    team_id %in% c("BAL", "SLB", "MLA") ~ "BAL",
    team_id == "BOS" ~ "BOS",
    team_id %in% c("NYY", "NYA") ~ "NYY",
    team_id %in% c("TBR", "TBD") ~ "TBR",
    team_id == "TOR" ~ "TOR",
    
    # NL West
    team_id == "ARI" ~ "ARI",
    team_id == "COL" ~ "COL",
    team_id %in% c("LAD", "BRO") ~ "LAD",
    team_id == "SDP" ~ "SDP",
    team_id %in% c("SFG", "NYG") ~ "SFG",
    
    # NL Central
    team_id == "CHC" ~ "CHC",
    team_id == "CIN" ~ "CIN",
    team_id %in% c("MIL", "SEP") ~ "MIL",
    team_id == "PIT" ~ "PIT",
    team_id == "STL" ~ "STL",
    
    # NL East
    team_id %in% c("ATL", "MLN", "BSN") ~ "ATL",
    team_id %in% c("MIA", "FLA") ~ "MIA",
    team_id == "NYM" ~ "NYM",
    team_id == "PHI" ~ "PHI",
    team_id %in% c("WSN", "MON") ~ "WSN",
    
    TRUE ~ NA_character_
  )
}

# =============================================================================
# TEAM COLORS
# =============================================================================

team_colors <- tribble(
  ~franchise, ~primary, ~secondary,
  # AL West
  "LAA", "#BA0021", "#003263",
  "HOU", "#EB6E1F", "#002D62",
  "OAK", "#003831", "#EFB21E",
  "SEA", "#005C5C", "#0C2C56",
  "TEX", "#003278", "#C0111F",
  # AL Central
  "CHW", "#27251F", "#C4CED4",
  "CLE", "#E50022", "#00385D",
  "DET", "#FA4616", "#0C2340",
  "KCR", "#004687", "#BD9B60",
  "MIN", "#D31145", "#002B5C",
  # AL East
  "BAL", "#DF4601", "#000000",
  "BOS", "#BD3039", "#0D2B56",
  "NYY", "#003087", "#C4CED4",
  "TBR", "#1A8CFF", "#092C5C",
  "TOR", "#134A8E", "#1D2D5C",
  # NL West
  "ARI", "#A71930", "#E3D4AD",
  "COL", "#6B5B95", "#333366",
  "LAD", "#005A9C", "#A5ACAF",
  "SDP", "#FFC425", "#2F241D",
  "SFG", "#FD5A1E", "#27251F",
  # NL Central
  "CHC", "#0E3386", "#CC3433",
  "CIN", "#C6011F", "#000000",
  "MIL", "#B6922E", "#12284B",
  "PIT", "#FDB827", "#27251F",
  "STL", "#C41E3A", "#0C2340",
  # NL East
  "ATL", "#CE1141", "#13274F",
  "MIA", "#00A3E0", "#EF3340",
  "NYM", "#FF5910", "#002D72",
  "PHI", "#E81828", "#002D72",
  "WSN", "#AB0003", "#14225A"
)

primary_colors <- setNames(team_colors$primary, team_colors$franchise)
secondary_colors <- setNames(team_colors$secondary, team_colors$franchise)

# =============================================================================
# DIVISION MAPPING
# =============================================================================

divisions <- tribble(
  ~franchise, ~league, ~division, ~div_order,
  # AL West
  "LAA", "AL", "West", 1,
  "HOU", "AL", "West", 1,
  "OAK", "AL", "West", 1,
  "SEA", "AL", "West", 1,
  "TEX", "AL", "West", 1,
  # AL Central
  "CHW", "AL", "Central", 2,
  "CLE", "AL", "Central", 2,
  "DET", "AL", "Central", 2,
  "KCR", "AL", "Central", 2,
  "MIN", "AL", "Central", 2,
  # AL East
  "BAL", "AL", "East", 3,
  "BOS", "AL", "East", 3,
  "NYY", "AL", "East", 3,
  "TBR", "AL", "East", 3,
  "TOR", "AL", "East", 3,
  # NL West
  "ARI", "NL", "West", 4,
  "COL", "NL", "West", 4,
  "LAD", "NL", "West", 4,
  "SDP", "NL", "West", 4,
  "SFG", "NL", "West", 4,
  # NL Central
  "CHC", "NL", "Central", 5,
  "CIN", "NL", "Central", 5,
  "MIL", "NL", "Central", 5,
  "PIT", "NL", "Central", 5,
  "STL", "NL", "Central", 5,
  # NL East
  "ATL", "NL", "East", 6,
  "MIA", "NL", "East", 6,
  "NYM", "NL", "East", 6,
  "PHI", "NL", "East", 6,
  "WSN", "NL", "East", 6
)

# =============================================================================
# DATA LOADING FUNCTIONS
# =============================================================================

load_war_data <- function() {
  message("Loading WAR data from Baseball Reference...")
  
  # Get primary position for each player from Lahman
  primary_position <- Fielding %>%
    group_by(playerID, POS) %>%
    summarise(games = sum(G), .groups = "drop") %>%
    group_by(playerID) %>%
    slice_max(order_by = games, n = 1, with_ties = FALSE) %>%
    select(playerID, primary_pos = POS)
  
  # Load batting WAR
  WAR_bat <- read_csv(
    "https://www.baseball-reference.com/data/war_daily_bat.txt",
    show_col_types = FALSE
  ) %>%
    mutate(player_type = "batter", WAR = as.numeric(WAR)) %>%
    left_join(primary_position, by = c("player_ID" = "playerID"))
  
  # Load pitching WAR
  WAR_pitch <- read_csv(
    "https://www.baseball-reference.com/data/war_daily_pitch.txt",
    show_col_types = FALSE
  ) %>%
    mutate(player_type = "pitcher", WAR = as.numeric(WAR), primary_pos = "P")
  
  message("Data loaded successfully.")
  
  list(batting = WAR_bat, pitching = WAR_pitch)
}

# =============================================================================
# MAIN PLOTTING FUNCTION
# =============================================================================

generate_franchise_war_plot <- function(
    war_data, 
    player_type_label, 
    start_year = 1901, 
    top_n = 5,
    position_filter = NULL  # NULL = all, or "C", "1B", "2B", "SS", "3B", "OF", "P"
) {
  
  # Filter by position if specified
  if (!is.null(position_filter)) {
    war_data <- war_data %>% filter(primary_pos == position_filter)
    player_type_label <- paste0(player_type_label, " (", position_filter, ")")
  }
  
  # Determine primary position per player (the one with most WAR)
  player_positions <- war_data %>%
    filter(!is.na(WAR)) %>%
    group_by(player_ID, primary_pos) %>%
    summarise(pos_WAR = sum(WAR, na.rm = TRUE), .groups = "drop") %>%
    group_by(player_ID) %>%
    slice_max(pos_WAR, n = 1, with_ties = FALSE) %>%
    select(player_ID, primary_pos)
  
  # Apply franchise mapping and aggregate by player-franchise-year
  WAR_clean <- war_data %>%
    mutate(franchise = franchise_map(team_ID)) %>%
    filter(!is.na(franchise), !is.na(WAR)) %>%
    group_by(player_ID, name_common, franchise, year_ID) %>%
    summarise(WAR = sum(WAR, na.rm = TRUE), .groups = "drop") %>%
    left_join(player_positions, by = "player_ID") %>%
    left_join(divisions, by = "franchise") %>%
    mutate(division_full = paste(league, division))
  
  # Calculate cumulative WAR by player-franchise
  WAR_cum <- WAR_clean %>%
    group_by(player_ID, franchise) %>%
    arrange(year_ID) %>%
    mutate(
      cumWAR_franchise = cumsum(WAR),
      totalWAR_franchise = sum(WAR),
      max_year = max(year_ID)
    ) %>%
    ungroup()
  
  # Identify top N players per franchise (within period, 2+ years)
  top_players <- WAR_cum %>%
    filter(year_ID >= start_year) %>%
    group_by(franchise, player_ID) %>%
    summarise(
      totalWAR = sum(WAR, na.rm = TRUE),
      years_played = n_distinct(year_ID),
      name = first(name_common),
      .groups = "drop"
    ) %>%
    filter(years_played >= 2) %>%
    group_by(franchise) %>%
    slice_max(order_by = totalWAR, n = top_n) %>%
    mutate(is_top = TRUE) %>%
    select(franchise, player_ID, is_top)
  
  WAR_cum <- WAR_cum %>%
    left_join(top_players, by = c("franchise", "player_ID")) %>%
    mutate(is_top = replace_na(is_top, FALSE))
  
  # Prepare plot data - only add position to label if showing multiple positions
  plot_data <- WAR_cum %>%
    filter(year_ID >= start_year) %>%
    mutate(name_label = if (is.null(position_filter)) {
      paste0(name_common, " (", primary_pos, ")")
    } else {
      name_common
    })
  
  # Adjust x-axis breaks based on era
  if (start_year >= 1990) {
    x_breaks <- seq(2000, 2020, 10)
  } else if (start_year >= 1960) {
    x_breaks <- seq(1980, 2020, 20)
  } else {
    x_breaks <- seq(1920, 2020, 40)
  }
  
  # Create the faceted plot
  p <- ggplot(filter(plot_data, is_top), aes(x = year_ID, y = cumWAR_franchise)) +
    geom_line(aes(group = player_ID, color = franchise), linewidth = 1) +
    geom_label_repel(
      data = filter(plot_data, is_top & year_ID == max_year),
      aes(label = name_label, color = franchise),
      size = 2.2,
      fill = alpha("white", 0.85),
      label.size = 0,
      segment.size = 0.3,
      segment.alpha = 0.5,
      max.overlaps = 15,
      nudge_y = 3,
      box.padding = 0.15,
      point.padding = 0.1,
      min.segment.length = 0.2
    ) +
    scale_color_manual(values = primary_colors) +
    scale_x_continuous(breaks = x_breaks) +
    ggh4x::facet_wrap2(~ reorder(franchise, div_order), ncol = 5, axes = "x") +
    coord_cartesian(xlim = c(start_year, 2026), ylim = c(0, 175)) +  # Clip display, don't remove data
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.spacing.x = unit(1.2, "lines"),
      panel.spacing.y = unit(0.8, "lines"),
      strip.text = element_text(face = "bold", size = 9),
      axis.text = element_text(size = 6),
      axis.text.x = element_text(angle = 0),
      panel.grid.minor = element_blank()
    ) +
    labs(
      title = paste0("Cumulative WAR by Franchise — ", player_type_label, " (", start_year, "-Present)"),
      subtitle = paste0("Top ", top_n, " per franchise highlighted"),
      x = "Year",
      y = "Cumulative WAR",
      caption = "Source: Baseball Reference"
    )
  
  # Attach the top players data as an attribute for export
  # Filter to start_year so CSV matches the era shown in plot
  top_summary <- WAR_cum %>%
    filter(is_top, year_ID >= start_year) %>%
    group_by(franchise, player_ID, name_common, primary_pos) %>%
    summarise(
      total_WAR = round(sum(WAR, na.rm = TRUE), 1),
      first_year = min(year_ID),
      last_year = max(year_ID),
      .groups = "drop"
    ) %>%
    arrange(franchise, desc(total_WAR))
  
  attr(p, "top_players") <- top_summary
  attr(p, "start_year") <- start_year
  
  return(p)
}

# =============================================================================
# HELPER: Export top players data to CSV
# =============================================================================

export_plot_data <- function(plot, filename) {
  top_players <- attr(plot, "top_players")
  if (!is.null(top_players)) {
    write_csv(top_players, filename)
    message(sprintf("Exported: %s", filename))
  }
}

# =============================================================================
# CONFIGURATION
# =============================================================================

ERAS <- c(1901, 1961, 1969, 1977, 1993, 1998)
POSITIONS <- c("All", "C", "1B", "2B", "SS", "3B", "OF")
PLAYER_TYPES <- c("batters", "pitchers", "combined")
