# generate_team_breakdowns.R
# Batch generation of team position breakdown charts
# Runs N_CORES parallel R worker processes via PSOCK cluster.
# No fork (macOS safe), no serialization overhead per job. Uses ragg if available.

source("franchise_war_functions.R")
source("team_position_breakdown.R")

OUTPUT_DIR <- "docs/team_breakdowns"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

N_CORES <- parallel::detectCores()
PLOT_WIDTH <- 20; PLOT_DPI <- 150

TEAMS <- list(
  "BAL"="Baltimore Orioles","BOS"="Boston Red Sox","NYY"="New York Yankees",
  "TBR"="Tampa Bay Rays","TOR"="Toronto Blue Jays","CHW"="Chicago White Sox",
  "CLE"="Cleveland Guardians","DET"="Detroit Tigers","KCR"="Kansas City Royals",
  "MIN"="Minnesota Twins","HOU"="Houston Astros","LAA"="Los Angeles Angels",
  "OAK"="Oakland Athletics","SEA"="Seattle Mariners","TEX"="Texas Rangers",
  "ATL"="Atlanta Braves","MIA"="Miami Marlins","NYM"="New York Mets",
  "PHI"="Philadelphia Phillies","WSN"="Washington Nationals","CHC"="Chicago Cubs",
  "CIN"="Cincinnati Reds","MIL"="Milwaukee Brewers","PIT"="Pittsburgh Pirates",
  "STL"="St. Louis Cardinals","ARI"="Arizona Diamondbacks","COL"="Colorado Rockies",
  "LAD"="Los Angeles Dodgers","SDP"="San Diego Padres","SFG"="San Francisco Giants"
)

OVERLAY_COMBOS <- list(
  list(suffix="", awards=FALSE, postseason=FALSE),
  list(suffix="_awards", awards=TRUE, postseason=FALSE),
  list(suffix="_postseason", awards=FALSE, postseason=TRUE),
  list(suffix="_all", awards=TRUE, postseason=TRUE)
)

message("=== Loading shared data ===")
pos_year_stats <- compute_position_year_stats(WAR_bat, WAR_pitch)
award_data <- load_award_data()

jobs <- list()
for (tc in names(TEAMS)) {
  tn <- TEAMS[[tc]]
  for (sc in c("linear","log")) {
    for (wn in c("const","zscore")) {
      for (oc in OVERLAY_COMBOS) {
        jobs[[length(jobs)+1]] <- list(
          fname=sprintf("%s/%s_%s_%s%s.png",OUTPUT_DIR,tc,sc,wn,oc$suffix),
          tc=tc, tn=tn, il=(sc=="log"), iz=(wn=="zscore"), sa=oc$awards, sp=oc$postseason)
      }
    }
  }
}

message(sprintf("\n=== %d images | %d cores ===\n", length(jobs), N_CORES))

cl <- parallel::makeCluster(N_CORES)

parallel::clusterExport(cl, c("WAR_bat","WAR_pitch","pos_year_stats","award_data",
  "PLOT_WIDTH","PLOT_DPI"))

parallel::clusterEvalQ(cl, {
  suppressPackageStartupMessages({
    library(ggplot2); library(dplyr); library(tidyr); library(ggrepel); library(ggh4x)
    source("franchise_war_functions.R"); source("team_position_breakdown.R")
  })
})

render_job <- function(job) {
  p <- generate_team_position_plot(WAR_bat, WAR_pitch,
    team_code=job$tc, team_name=job$tn, show_postseason=job$sp, log_scale=job$il,
    variable_width=job$iz, pos_year_stats=if(job$iz) pos_year_stats else NULL,
    show_awards=job$sa, award_data=if(job$sa) award_data else NULL)
  n_pos <- attr(p, "n_positions"); ph <- 3 * n_pos + 2
  ggsave(job$fname, plot=p, width=PLOT_WIDTH, height=ph, dpi=PLOT_DPI, device=grDevices::png)
  return(job$fname)
}

t0 <- Sys.time()
results <- parallel::parLapply(cl, jobs, render_job)
elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
parallel::stopCluster(cl)

message(sprintf("\n=== Done! %d images in %.0fs (%.1f/s) ===", length(results), elapsed, length(results)/elapsed))
