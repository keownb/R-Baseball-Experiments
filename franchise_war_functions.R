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

franchise_map <- function(team_id, year_id = NA) {
  case_when(
    # AL West
    # AL West (BR + Lahman codes)
    team_id %in% c("LAA", "ANA", "CAL") ~ "LAA",
    team_id %in% c("OAK", "PHA") ~ "OAK",
    team_id == "KCA" & year_id < 1969 ~ "OAK",
    team_id == "SEA" ~ "SEA",
    team_id == "HOU" ~ "HOU",
    team_id %in% c("TEX", "WSA", "WS2") ~ "TEX",
    
    # AL Central
    team_id %in% c("CHW", "CHA") ~ "CHW",
    team_id == "CLE" ~ "CLE",
    team_id == "DET" ~ "DET",
    team_id %in% c("KCR", "KCA") ~ "KCR",
    team_id %in% c("MIN", "WSH", "WS1") ~ "MIN",
    
    # AL East
    team_id %in% c("BAL", "SLB", "MLA", "SLA") ~ "BAL",
    team_id == "BOS" ~ "BOS",
    team_id %in% c("NYY", "NYA") ~ "NYY",
    team_id %in% c("TBR", "TBD", "TBA") ~ "TBR",
    team_id == "TOR" ~ "TOR",
    
    # NL West
    team_id == "ARI" ~ "ARI",
    team_id == "COL" & year_id >= 1993 ~ "COL",
    team_id %in% c("LAD", "BRO", "LAN") ~ "LAD",
    team_id %in% c("SDP", "SDN") ~ "SDP",
    team_id %in% c("SFG", "NYG", "SFN") ~ "SFG",
    
    # NL Central
    team_id %in% c("CHC", "CHN") ~ "CHC",
    team_id == "CIN" ~ "CIN",
    team_id %in% c("MIL", "SEP") ~ "MIL",
    team_id == "PIT" ~ "PIT",
    team_id %in% c("STL", "SLN") ~ "STL",
    
    # NL East
    team_id %in% c("ATL", "MLN", "BSN") ~ "ATL",
    team_id %in% c("MIA", "FLA", "FLO") ~ "MIA",
    team_id %in% c("NYM", "NYN") ~ "NYM",
    team_id == "PHI" ~ "PHI",
    team_id %in% c("WSN", "MON", "WAS") ~ "WSN",
    
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
  "SEA", "#005C5C", "#8A8D8F",
  "TEX", "#003278", "#C0111F",
  # AL Central
  "CHW", "#27251F", "#888888",
  "CLE", "#E50022", "#00385D",
  "DET", "#FA4616", "#0C2340",
  "KCR", "#004687", "#BD9B60",
  "MIN", "#D31145", "#002B5C",
  # AL East
  "BAL", "#DF4601", "#000000",
  "BOS", "#BD3039", "#0D2B56",
  "NYY", "#003087", "#888888",
  "TBR", "#1A8CFF", "#092C5C",
  "TOR", "#134A8E", "#629DD1",
  # NL West
  "ARI", "#A71930", "#C49B6A",
  "COL", "#6B5B95", "#9B8BB4",
  "LAD", "#005A9C", "#7A8B8B",
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
    position_filter = NULL,  # NULL = all, or "C", "1B", "2B", "SS", "3B", "OF", "SP", "RP"
    variable_width = FALSE,  # Z-score line width
    pos_year_stats = NULL,   # Required if variable_width = TRUE
    show_awards = FALSE,
    award_data = NULL,       # Required if show_awards = TRUE
    show_postseason = FALSE
) {
  
 # For SP/RP filtering, we need to classify pitchers first
  if (!is.null(position_filter) && position_filter %in% c("SP", "RP")) {
    pitcher_type <- Lahman::Pitching %>%
      group_by(playerID) %>%
      summarise(
        total_G = sum(G, na.rm = TRUE),
        total_GS = sum(GS, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(pitcher_role = if_else(total_GS > total_G * 0.5, "SP", "RP")) %>%
      select(playerID, pitcher_role)
    
    war_data <- war_data %>%
      left_join(pitcher_type, by = c("player_ID" = "playerID")) %>%
      filter(pitcher_role == position_filter)
    
    player_type_label <- paste0(player_type_label, " (", position_filter, ")")
  } else if (!is.null(position_filter)) {
    war_data <- war_data %>% filter(primary_pos == position_filter)
    player_type_label <- paste0(player_type_label, " (", position_filter, ")")
  }
  
  # Determine primary position per player
  player_positions <- war_data %>%
    filter(!is.na(WAR)) %>%
    group_by(player_ID, primary_pos) %>%
    summarise(pos_WAR = sum(WAR, na.rm = TRUE), .groups = "drop") %>%
    group_by(player_ID) %>%
    slice_max(pos_WAR, n = 1, with_ties = FALSE) %>%
    select(player_ID, primary_pos)
  
  WAR_clean <- war_data %>%
    mutate(franchise = franchise_map(team_ID, year_ID)) %>%
    filter(!is.na(franchise), !is.na(WAR)) %>%
    group_by(player_ID, name_common, franchise, year_ID) %>%
    summarise(WAR = sum(WAR, na.rm = TRUE), .groups = "drop") %>%
    left_join(player_positions, by = "player_ID") %>%
    left_join(divisions, by = "franchise") %>%
    mutate(division_full = paste(league, division))
  
  WAR_cum <- WAR_clean %>%
    group_by(player_ID, franchise) %>%
    arrange(year_ID) %>%
    mutate(
      cumWAR_franchise = cumsum(WAR),
      totalWAR_franchise = sum(WAR),
      max_year = max(year_ID)
    ) %>%
    ungroup()
  
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
  
  plot_data <- WAR_cum %>%
    filter(year_ID >= start_year) %>%
    mutate(name_label = if (is.null(position_filter)) {
      paste0(name_common, " (", primary_pos, ")")
    } else {
      name_common
    })
  
  # x-axis breaks
  if (start_year >= 1990) {
    x_breaks <- seq(2000, 2020, 10)
  } else if (start_year >= 1960) {
    x_breaks <- seq(1980, 2020, 20)
  } else {
    x_breaks <- seq(1920, 2020, 40)
  }
  
  # --- Z-score segments for variable width ---
  top_segments <- NULL
  if (variable_width && !is.null(pos_year_stats)) {
    zscore_pos <- if (!is.null(position_filter) && position_filter %in% 
                      c("C", "1B", "2B", "SS", "3B", "OF", "SP", "RP")) {
      position_filter
    } else {
      NULL  # Can't z-score mixed positions
    }
    
    if (!is.null(zscore_pos)) {
      pos_stats_filtered <- pos_year_stats %>% filter(position == zscore_pos)
      
      top_segments <- plot_data %>%
        filter(is_top) %>%
        left_join(pos_stats_filtered, by = c("year_ID")) %>%
        mutate(
          war_zscore = (WAR - mean_WAR) / sd_WAR,
          war_zscore_clamped = pmin(pmax(war_zscore, 0), 4)
        ) %>%
        group_by(player_ID, franchise) %>%
        arrange(year_ID) %>%
        mutate(
          next_year = lead(year_ID),
          next_cumWAR = lead(cumWAR_franchise),
          width_z = lead(war_zscore_clamped)
        ) %>%
        filter(!is.na(next_year), !is.na(width_z)) %>%
        ungroup()
    }
  }
  
  # --- Build the plot ---
  top_plot_data <- filter(plot_data, is_top)
  
  p <- ggplot(top_plot_data, aes(x = year_ID, y = cumWAR_franchise))
  
  if (!is.null(top_segments) && nrow(top_segments) > 0) {
    p <- p + geom_segment(
      data = top_segments,
      aes(x = year_ID, xend = next_year,
          y = cumWAR_franchise, yend = next_cumWAR,
          linewidth = width_z, color = franchise),
      lineend = "round", linejoin = "round"
    ) +
    scale_linewidth_continuous(
      name = "Positional Dominance",
      range = c(0.3, 4),
      breaks = c(0, 1, 2, 3, 4),
      labels = c("Avg", "+1σ", "+2σ", "+3σ", "+4σ"),
      guide = guide_legend(override.aes = list(color = "gray30"))
    )
  } else {
    p <- p + geom_line(
      data = top_plot_data,
      aes(group = player_ID, color = franchise),
      linewidth = 1
    )
  }
  
  # Award markers (before labels so labels render on top)
  if (show_awards && !is.null(award_data)) {
    award_markers <- top_plot_data %>%
      filter(is_top) %>%
      inner_join(award_data, by = c("player_ID" = "playerID", "year_ID" = "yearID"),
                 relationship = "many-to-many") %>%
      filter(award %in% c("MVP", "CYA", "AS")) %>%
      mutate(award = factor(award, levels = c("MVP", "CYA", "AS")))
    
    if (nrow(award_markers) > 0) {
      p <- p + geom_point(
        data = award_markers,
        aes(shape = award, size = award, fill = award),
        color = "black", stroke = 0.5
      ) +
      scale_shape_manual(name = "Awards",
        values = c("MVP" = 23, "CYA" = 24, "AS" = 21)) +
      scale_size_manual(name = "Awards",
        values = c("MVP" = 3, "CYA" = 3, "AS" = 1.2)) +
      scale_fill_manual(name = "Awards",
        values = c("MVP" = "gold", "CYA" = "gold", "AS" = "gray80"))
    }
  }
  
  # Postseason vlines per franchise facet
  if (show_postseason) {
    all_postseason <- bind_rows(
      lapply(unique(WAR_clean$franchise), function(fc) {
        ps <- get_postseason_data(fc)
        if (nrow(ps) > 0) {
          ps %>%
            mutate(franchise = fc) %>%
            left_join(divisions, by = "franchise") %>%
            filter(yearID >= start_year)
        }
      })
    )
    
    if (nrow(all_postseason) > 0) {
      p <- p + geom_vline(
        data = all_postseason,
        aes(xintercept = yearID, linetype = achievement),
        alpha = 0.3, linewidth = 0.3
      ) +
      scale_linetype_manual(name = "Postseason",
        values = c("World Series" = "solid", "Pennant/LCS" = "dashed",
                   "Division Series" = "dotted", "Wild Card" = "dotdash"))
    }
  }
  
  # Labels (after awards/postseason so labels render on top)
  p <- p + geom_label_repel(
    data = filter(top_plot_data, year_ID == max_year),
    aes(label = name_label, color = franchise),
    size = 2.2, fill = alpha("white", 0.85), label.size = 0,
    segment.size = 0.3, segment.alpha = 0.5,
    max.overlaps = 15, nudge_y = 3,
    box.padding = 0.15, point.padding = 0.1,
    min.segment.length = 0.2
  )
  
  # Scales, facets, theme
  subtitle_parts <- paste0("Top ", top_n, " per franchise")
  if (!is.null(top_segments) && nrow(top_segments) > 0) {
    subtitle_parts <- paste0(subtitle_parts, " | Line width = positional dominance (σ)")
  }
  
  p <- p +
    scale_color_manual(values = primary_colors, guide = "none") +
    scale_x_continuous(breaks = x_breaks) +
    ggh4x::facet_wrap2(~ reorder(franchise, div_order), ncol = 5, axes = "x") +
    coord_cartesian(xlim = c(start_year, 2026), ylim = c(0, ceiling(max(top_plot_data$cumWAR_franchise, na.rm = TRUE) * 1.1 / 5) * 5)) +
    theme_minimal() +
    theme(
      legend.position = "top",
      legend.justification = "left",
      legend.box = "horizontal",
      legend.box.just = "left",
      panel.spacing.x = unit(1.2, "lines"),
      panel.spacing.y = unit(0.8, "lines"),
      strip.text = element_text(face = "bold", size = 9),
      axis.text = element_text(size = 6),
      axis.text.x = element_text(angle = 0),
      panel.grid.minor = element_blank()
    ) +
    guides(
      shape = guide_legend(order = 1),
      size = guide_legend(order = 1),
      fill = guide_legend(order = 1),
      linetype = guide_legend(order = 2),
      linewidth = guide_legend(order = 3)
    ) +
    labs(
      title = paste0("Cumulative WAR by Franchise — ", player_type_label,
                     " (", start_year, "-Present)"),
      subtitle = subtitle_parts,
      x = "Year", y = "Cumulative WAR",
      caption = "Source: Baseball Reference + Lahman"
    )
  
  # Attach data for CSV export
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

# =============================================================================
# AWARD DATA (loaded once, reused by all views)
# =============================================================================

load_award_data <- function() {
  awards_raw <- Lahman::AwardsPlayers %>%
    filter(awardID %in% c("Most Valuable Player", "Cy Young Award")) %>%
    mutate(award = case_when(
      awardID == "Most Valuable Player" ~ "MVP",
      awardID == "Cy Young Award" ~ "CYA"
    )) %>%
    select(playerID, yearID, award)
  
  allstars <- Lahman::AllstarFull %>%
    distinct(playerID, yearID) %>%
    mutate(award = "AS")
  
  bind_rows(awards_raw, allstars)
}

# =============================================================================
# LEAGUE-WIDE Z-SCORE STATS (loaded once, reused by all views)
# =============================================================================

compute_position_year_stats <- function(war_batting, war_pitching) {
  message("Computing league-wide position-year WAR distributions...")

  # --- Step 1: Year-specific position assignment from Appearances ---
  # Classify each player-year by the position where they played the most games
  # that season, rather than career-wide primary position. This ensures the
  # comparison pool reflects the actual positional landscape each year.
  pos_games <- Lahman::Appearances %>%
    transmute(
      playerID, yearID,
      C  = G_c,
      `1B` = G_1b,
      `2B` = G_2b,
      SS = G_ss,
      `3B` = G_3b,
      OF = G_lf + G_cf + G_rf,
      DH = G_dh,
      P  = G_p
    ) %>%
    pivot_longer(cols = c(C, `1B`, `2B`, SS, `3B`, OF, DH, P),
                 names_to = "position", values_to = "games") %>%
    filter(games > 0) %>%
    group_by(playerID, yearID) %>%
    slice_max(order_by = games, n = 1, with_ties = FALSE) %>%
    ungroup()

  # --- Step 2: SP/RP split for pitchers (year-specific) ---
  pitcher_yearly <- Lahman::Pitching %>%
    group_by(playerID, yearID) %>%
    summarise(G = sum(G, na.rm = TRUE), GS = sum(GS, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(pitcher_role = if_else(GS > G * 0.5, "SP", "RP"))

  pos_games <- pos_games %>%
    left_join(pitcher_yearly %>% select(playerID, yearID, pitcher_role, GS),
              by = c("playerID", "yearID")) %>%
    mutate(position = case_when(
      position == "P" & pitcher_role == "RP" ~ "RP",
      position == "P" ~ "SP",
      TRUE ~ position
    ))

  # --- Step 3: Apply minimum-games thresholds ---
  # Field positions: 60 games (~37% of season) — captures regulars
  # DH: 30 games — lower bar since even full-time DHs get rested/platooned
  # SP: 15 GS — roughly one start every 10 days
  # RP: 30 games
  qualified <- pos_games %>%
    filter(
      (position %in% c("C", "1B", "2B", "SS", "3B", "OF") & games >= 60) |
      (position == "DH" & games >= 30) |
      (position == "SP" & !is.na(GS) & GS >= 15) |
      (position == "RP" & games >= 30)
    )

  # --- Step 4: Join WAR values ---
  all_war <- bind_rows(
    war_batting  %>% select(year_ID, player_ID, WAR),
    war_pitching %>% select(year_ID, player_ID, WAR)
  ) %>%
    mutate(WAR = as.numeric(WAR)) %>%
    filter(!is.na(WAR)) %>%
    group_by(player_ID, year_ID) %>%
    summarise(WAR = sum(WAR, na.rm = TRUE), .groups = "drop")

  league_war <- qualified %>%
    inner_join(all_war,
               by = c("playerID" = "player_ID", "yearID" = "year_ID"))

  # --- Step 5: Compute raw position-year stats ---
  pos_year_raw <- league_war %>%
    group_by(position, yearID) %>%
    summarise(
      mean_WAR = mean(WAR, na.rm = TRUE),
      sd_WAR   = sd(WAR, na.rm = TRUE),
      n_players = n(),
      .groups = "drop"
    ) %>%
    mutate(sd_WAR = replace_na(sd_WAR, 0))

  # --- Step 6: 3-year centered rolling average ---
  # Smooths year-to-year noise, especially valuable for thin pools (DH, early RP).
  # Edge years use available neighbors via coalesce fallback.
  pos_year_stats <- pos_year_raw %>%
    group_by(position) %>%
    arrange(yearID) %>%
    mutate(
      mean_WAR = (lag(mean_WAR, default = first(mean_WAR)) +
                  mean_WAR +
                  lead(mean_WAR, default = last(mean_WAR))) / 3,
      sd_WAR   = (lag(sd_WAR, default = first(sd_WAR)) +
                  sd_WAR +
                  lead(sd_WAR, default = last(sd_WAR))) / 3
    ) %>%
    ungroup() %>%
    mutate(sd_WAR = pmax(sd_WAR, 0.5)) %>%
    rename(year_ID = yearID)

  message(sprintf("  %d position-years computed (3-yr rolling, qualified pools)",
                  nrow(pos_year_stats)))
  pos_year_stats
}

# =============================================================================
# POSTSEASON DATA HELPER
# =============================================================================

# Lahman team code mapping (reverse of franchise_map)
LAHMAN_CODES <- list(
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
  "SDP" = c("SDN"), "MIA" = c("FLO", "MIA"), "NYM" = c("NYN")
)

get_postseason_data <- function(team_code) {
  lahman_codes <- LAHMAN_CODES[[team_code]]
  if (is.null(lahman_codes)) lahman_codes <- team_code
  
  Lahman::SeriesPost %>%
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

# =============================================================================
# OVERLAY GENERATION HELPER
# =============================================================================

# Save a transparent overlay PNG that is pixel-aligned with the base plot.
# Builds an overlay plot using the same scales/facets/coords/dimensions,
# but with transparent backgrounds and invisible text (preserving spacing).

