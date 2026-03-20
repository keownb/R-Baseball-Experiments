# franchise_war_analysis.R
# Cumulative WAR by franchise - Modern Era (1901-present)
# Maps all historical team IDs to current 30 MLB franchises

# Load packages individually (avoids ragg dependency issues)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(ggrepel)
library(gghighlight)
library(ggh4x)    # For facet_wrap2 with axes on all panels
library(Lahman)   # For position data

# =============================================================================
# DATA LOADING
# =============================================================================

# Load batting WAR (position players)
WAR_bat <- read_csv("https://www.baseball-reference.com/data/war_daily_bat.txt")

# Load pitching WAR
WAR_pitch <- read_csv("https://www.baseball-reference.com/data/war_daily_pitch.txt")

# Combine with a player_type indicator
WAR_bat <- WAR_bat %>% mutate(player_type = "batter")
WAR_pitch <- WAR_pitch %>% mutate(player_type = "pitcher")

# Use batting data for now (position players only)
WAR_data <- WAR_bat

# =============================================================================
# GET PRIMARY POSITION FROM LAHMAN
# =============================================================================

# Get primary position for each player (most games played at position)
primary_position <- Fielding %>%
  group_by(playerID, POS) %>%
  summarise(games = sum(G), .groups = "drop") %>%
  group_by(playerID) %>%
  slice_max(order_by = games, n = 1) %>%
  select(playerID, primary_pos = POS)

# Rename player_ID column to match (bbref uses underscores)
WAR_data <- WAR_data %>%
  left_join(primary_position, by = c("player_ID" = "playerID"))

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
    team_id == "NYY" ~ "NYY",
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
# TEAM COLORS - Primary and Secondary
# =============================================================================

team_colors <- tribble(
  ~franchise, ~primary, ~secondary,
  # AL West
  "LAA", "#BA0021", "#003263",
  "HOU", "#EB6E1F", "#002D62",
  "OAK", "#003831", "#EFB21E",
  "SEA", "#005C5C", "#0C2C56",           # Teal instead of navy
  "TEX", "#003278", "#C0111F",
  # AL Central
  "CHW", "#27251F", "#C4CED4",
  "CLE", "#E50022", "#00385D",           # Red instead of navy
  "DET", "#FA4616", "#0C2340",           # Orange instead of navy
  "KCR", "#004687", "#BD9B60",
  "MIN", "#D31145", "#002B5C",           # Red instead of navy
  # AL East
  "BAL", "#DF4601", "#000000",
  "BOS", "#BD3039", "#0D2B56",
  "NYY", "#003087", "#C4CED4",           # Keep navy (iconic)
  "TBR", "#8FBCE6", "#092C5C",           # Light blue instead of navy
  "TOR", "#134A8E", "#1D2D5C",
  # NL West
  "ARI", "#A71930", "#E3D4AD",
  "COL", "#C4CED4", "#333366",           # Silver instead of purple
  "LAD", "#005A9C", "#A5ACAF",
  "SDP", "#FFC425", "#2F241D",           # Gold instead of brown
  "SFG", "#FD5A1E", "#27251F",
  # NL Central
  "CHC", "#0E3386", "#CC3433",
  "CIN", "#C6011F", "#000000",
  "MIL", "#B6922E", "#12284B",           # Gold instead of navy
  "PIT", "#FDB827", "#27251F",
  "STL", "#C41E3A", "#0C2340",
  # NL East
  "ATL", "#CE1141", "#13274F",
  "MIA", "#00A3E0", "#EF3340",
  "NYM", "#FF5910", "#002D72",           # Orange instead of navy
  "PHI", "#E81828", "#002D72",
  "WSN", "#AB0003", "#14225A"
)

# Create named vectors for ggplot scales
primary_colors <- setNames(team_colors$primary, team_colors$franchise)
secondary_colors <- setNames(team_colors$secondary, team_colors$franchise)

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
# APPLY MAPPINGS
# =============================================================================

WAR_clean <- WAR_data %>%
  mutate(
    WAR = as.numeric(WAR),
    franchise = franchise_map(team_ID)
  ) %>%
  filter(!is.na(franchise)) %>%  # Keep only current 30 franchises
  left_join(divisions, by = "franchise") %>%
  mutate(division_full = paste(league, division))

# =============================================================================
# CALCULATE CUMULATIVE WAR BY PLAYER-FRANCHISE
# =============================================================================

WAR_cum <- WAR_clean %>%
  group_by(player_ID, franchise) %>%
  arrange(year_ID) %>%
  mutate(
    cumWAR_franchise = cumsum(WAR),
    totalWAR_franchise = sum(WAR),
    max_year = max(year_ID)
  ) %>%
  ungroup()

# =============================================================================
# IDENTIFY TOP N PLAYERS PER FRANCHISE
# =============================================================================

TopN <- 5

# Filter to modern era first
start_year <- 1901

# Calculate top players based on WAR within the filtered period
# Also require at least 2 years of play to avoid one-year edge cases
top_players <- WAR_cum %>%
  filter(year_ID >= start_year) %>%
  group_by(franchise, player_ID) %>%
  summarise(
    totalWAR = sum(WAR, na.rm = TRUE),  # Sum WAR in period
    years_played = n_distinct(year_ID),
    name = first(name_common),
    .groups = "drop"
  ) %>%
  filter(years_played >= 2) %>%  # Must have 2+ years in period
  group_by(franchise) %>%
  slice_max(order_by = totalWAR, n = TopN) %>%
  mutate(is_top = TRUE) %>%
  select(franchise, player_ID, is_top)

WAR_cum <- WAR_cum %>%
  left_join(top_players, by = c("franchise", "player_ID")) %>%
  mutate(is_top = replace_na(is_top, FALSE))

# =============================================================================
# PLOT: ALL 30 TEAMS (6 rows x 5 cols by division)
# =============================================================================

plot_data <- WAR_cum %>%
  filter(year_ID >= start_year) %>%
  # Create label with name and position
  mutate(name_with_pos = paste0(name_common, " (", primary_pos, ")"))

# Create the faceted plot
p <- ggplot(filter(plot_data, is_top), aes(x = year_ID, y = cumWAR_franchise)) +
  # Lines colored by franchise
  geom_line(
    aes(group = player_ID, color = franchise),
    linewidth = 1
  ) +
  # Labels with white background for readability on any color
  geom_label_repel(
    data = filter(plot_data, is_top & year_ID == max_year),
    aes(label = name_with_pos, color = franchise),
    size = 2.2,
    fill = alpha("white", 0.85),
    label.size = 0,  # no border
    segment.size = 0.3,
    segment.alpha = 0.5,
    max.overlaps = 15,
    nudge_y = 3,
    box.padding = 0.15,
    point.padding = 0.1,
    min.segment.length = 0.2
  ) +
  # Map franchise to team primary colors
  scale_color_manual(values = primary_colors) +
  # Fixed x-axis for consistency (1901-2025 for all)
  scale_x_continuous(limits = c(1901, 2026), breaks = c(1920, 1960, 2000)) +
  # Facet by team, ordered by division - with x-axis on all rows
  ggh4x::facet_wrap2(
    ~ reorder(franchise, div_order),
    ncol = 5,
    axes = "x"  # Show x-axis on all panels
  ) +
  # Consistent y-axis for comparison
  scale_y_continuous(limits = c(0, 175)) +
  # Styling with more horizontal separation
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.spacing.x = unit(1.2, "lines"),  # More horizontal space
    panel.spacing.y = unit(0.8, "lines"),
    strip.text = element_text(face = "bold", size = 9),
    axis.text = element_text(size = 6),
    axis.text.x = element_text(angle = 0),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = paste0("Cumulative WAR by Franchise — Position Players (", start_year, "-Present)"),
    subtitle = paste0("Top ", TopN, " position players per franchise highlighted"),
    x = "Year",
    y = "Cumulative WAR",
    caption = "Source: Baseball Reference"
  )

print(p)

# Save high-res version
ggsave(
  "docs/all_30_franchises_war.png",
  plot = p,
  width = 20,
  height = 24,
  units = "in",
  dpi = 150
)

# =============================================================================
# EXAMPLE: SINGLE TEAM DEEP DIVE
# =============================================================================

single_team <- "SEA"
team_data <- plot_data %>% filter(franchise == single_team)

p_team <- ggplot(team_data, aes(x = year_ID, y = cumWAR_franchise)) +
  geom_line(aes(group = player_ID), alpha = 0.2) +
  geom_line(
    data = filter(team_data, is_top),
    aes(group = player_ID, color = name_common),
    linewidth = 1.2
  ) +
  geom_label_repel(
    data = filter(team_data, is_top & year_ID == max_year),
    aes(label = name_common, color = name_common),
    size = 4,
    max.overlaps = 15
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = paste0(single_team, " - Cumulative WAR"),
    x = "Year",
    y = "Cumulative WAR"
  )

print(p_team)
