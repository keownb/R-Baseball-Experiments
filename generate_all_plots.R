# generate_all_plots.R
# Batch generation of all WAR plots for GH Pages
# Runs N_CORES parallel R worker processes, each loading data independently.
# No fork (macOS safe), no serialization overhead. Uses ragg if available.

source("franchise_war_functions.R")

OUTPUT_DIR <- "docs/plots"
DATA_DIR <- "docs/data"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(DATA_DIR, showWarnings = FALSE, recursive = TRUE)

N_CORES <- parallel::detectCores()
ERAS <- c(1901, 1961, 1969, 1977, 1993, 1998)

POSITIONS <- list(
  "all" = list(source = "combined", filter = NULL),
  "batters" = list(source = "batting", filter = NULL),
  "pitchers" = list(source = "pitching", filter = NULL),
  "C" = list(source = "batting", filter = "C"),
  "1B" = list(source = "batting", filter = "1B"),
  "2B" = list(source = "batting", filter = "2B"),
  "SS" = list(source = "batting", filter = "SS"),
  "3B" = list(source = "batting", filter = "3B"),
  "OF" = list(source = "batting", filter = "OF"),
  "SP" = list(source = "pitching", filter = "SP"),
  "RP" = list(source = "pitching", filter = "RP")
)

PLOT_WIDTH <- 20; PLOT_HEIGHT <- 24; PLOT_DPI <- 150

OVERLAY_COMBOS <- list(
  list(suffix = "",            awards = FALSE, postseason = FALSE),
  list(suffix = "_awards",     awards = TRUE,  postseason = FALSE),
  list(suffix = "_postseason", awards = FALSE, postseason = TRUE),
  list(suffix = "_all",        awards = TRUE,  postseason = TRUE)
)

# Build lightweight job list
message("=== Loading WAR data ===")
war_data <- load_war_data()
WAR_bat <- war_data$batting; WAR_pitch <- war_data$pitching
common_cols <- c("year_ID","player_ID","name_common","team_ID","WAR","player_type","primary_pos")
WAR_combined <- bind_rows(WAR_bat %>% select(all_of(common_cols)),
                          WAR_pitch %>% select(all_of(common_cols)))

message("=== Loading shared data ===")
pos_year_stats <- compute_position_year_stats(WAR_bat, WAR_pitch)
award_data <- load_award_data()
use_ragg <- requireNamespace("ragg", quietly = TRUE)

jobs <- list()
for (era in ERAS) {
  for (pos_name in names(POSITIONS)) {
    pc <- POSITIONS[[pos_name]]
    pf <- pc$filter
    can_z <- !is.null(pf) && pf %in% c("C","1B","2B","SS","3B","OF","SP","RP")
    bn <- sprintf("%s_%d", tolower(pos_name), era)
    tl <- switch(pc$source, "batting"="Position Players", "pitching"="Pitchers", "combined"="All Players")

    wms <- list(list(n="", vw=FALSE))
    if (can_z) wms[[2]] <- list(n="_zscore", vw=TRUE)

    for (wm in wms) {
      for (oc in OVERLAY_COMBOS) {
        jobs[[length(jobs)+1]] <- list(fname=sprintf("%s/%s%s%s.png",OUTPUT_DIR,bn,wm$n,oc$suffix),
          sn=pc$source, tl=tl, era=era, pf=pf, vw=wm$vw, sa=oc$awards, sp=oc$postseason)
      }
    }
    jobs[[length(jobs)+1]] <- list(csv=sprintf("%s/%s.csv",DATA_DIR,bn), sn=pc$source, tl=tl, era=era, pf=pf)
  }
}

message(sprintf("\n=== %d items | %d cores | %s ===\n", length(jobs), N_CORES, if(use_ragg)"ragg"else"png"))

# Chunk jobs across cores and use parLapply (PSOCK cluster = separate processes)
cl <- parallel::makeCluster(N_CORES)

# Export everything workers need
parallel::clusterExport(cl, c("WAR_bat","WAR_pitch","WAR_combined","pos_year_stats",
  "award_data","use_ragg","PLOT_WIDTH","PLOT_HEIGHT","PLOT_DPI"))

# Each worker loads the function library once
parallel::clusterEvalQ(cl, {
  suppressPackageStartupMessages({
    library(ggplot2); library(dplyr); library(tidyr); library(ggrepel); library(ggh4x)
    source("franchise_war_functions.R")
    if (requireNamespace("ragg", quietly = TRUE)) library(ragg)
  })
})

render_job <- function(job) {
  war_src <- switch(job$sn, "batting"=WAR_bat, "pitching"=WAR_pitch, "combined"=WAR_combined)
  if (!is.null(job$csv)) {
    p <- generate_franchise_war_plot(war_src, job$tl, start_year=job$era, position_filter=job$pf)
    export_plot_data(p, job$csv)
    return(job$csv)
  }
  p <- generate_franchise_war_plot(war_src, job$tl, start_year=job$era, position_filter=job$pf,
    variable_width=job$vw, pos_year_stats=if(job$vw) pos_year_stats else NULL,
    show_awards=job$sa, award_data=if(job$sa) award_data else NULL, show_postseason=job$sp)
  if (use_ragg) {
    ggsave(job$fname, plot=p, width=PLOT_WIDTH, height=PLOT_HEIGHT, dpi=PLOT_DPI, device=ragg::agg_png)
  } else {
    ggsave(job$fname, plot=p, width=PLOT_WIDTH, height=PLOT_HEIGHT, dpi=PLOT_DPI)
  }
  return(job$fname)
}

t0 <- Sys.time()
results <- parallel::parLapply(cl, jobs, render_job)
elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
parallel::stopCluster(cl)

message(sprintf("\n=== Done! %d items in %.0fs (%.1f/s) ===", length(results), elapsed, length(results)/elapsed))
