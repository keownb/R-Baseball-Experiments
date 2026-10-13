# generate_team_breakdowns.R
# Batch generation of team position breakdown charts
# Runs N_CORES parallel R worker processes via PSOCK cluster (macOS safe). Data
# is loaded once in the parent and exported to the workers. Optional pngquant pass (see save_plot_png).

source("franchise_war_functions.R")
source("team_position_breakdown.R")

# Usage: Rscript generate_team_breakdowns.R [br|fg]   (WAR source; default br)
SRC <- if (length(commandArgs(trailingOnly = TRUE)) >= 1) commandArgs(trailingOnly = TRUE)[1] else "br"

message("=== Loading WAR data ===")
war_data <- load_war_data(source = SRC)
WAR_bat <- war_data$batting; WAR_pitch <- war_data$pitching

OUTPUT_DIR <- paste0("docs/team_breakdowns", if (SRC == "fg") "_fg" else "")
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
  "REF_TABLES","WAR_SOURCE","PLOT_WIDTH","PLOT_DPI"))

invisible(parallel::clusterEvalQ(cl, {
  suppressPackageStartupMessages({
    library(ggplot2); library(dplyr); library(tidyr); library(ggrepel); library(ggh4x)
    source("franchise_war_functions.R"); source("team_position_breakdown.R")
  })
}))

render_job <- function(job) {
  p <- generate_team_position_plot(WAR_bat, WAR_pitch,
    team_code=job$tc, team_name=job$tn, show_postseason=job$sp, log_scale=job$il,
    variable_width=job$iz, pos_year_stats=if(job$iz) pos_year_stats else NULL,
    show_awards=job$sa, award_data=if(job$sa) award_data else NULL)
  n_pos <- attr(p, "n_positions"); ph <- 3 * n_pos + 2
  save_plot_png(job$fname, p, PLOT_WIDTH, ph, PLOT_DPI)
  return(job$fname)
}

t0 <- Sys.time()
results <- parallel::parLapplyLB(cl, jobs, render_job)  # load-balanced: workers pull jobs as they finish
elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
parallel::stopCluster(cl)

message(sprintf("\n=== Done! %d images in %.0fs (%.1f/s) ===", length(results), elapsed, length(results)/elapsed))
