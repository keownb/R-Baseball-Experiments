# render.R
# Renders one family of plots (grid explorer or team breakdowns) for one WAR
# source, in parallel, and writes a manifest describing exactly what was
# rendered from which data. Replaces generate_all_plots.R / generate_team_breakdowns.R
# (kept as thin wrappers).
#
#   Rscript render.R grid       br|fg [--root DIR] [--cores N] [--only REGEX]
#   Rscript render.R breakdowns br|fg [--root DIR] [--cores N] [--only REGEX]
#
# --root  site root to write under (default docs/; build.sh uses a staging dir
#         and swaps it in only after validation passes)
# --only  render just the jobs whose id matches REGEX (debugging)
#
# Safety properties:
#   * the data is validated (pipeline_checks.R) before any plot is rendered;
#   * workers source the function library FIRST and receive the parent's data
#     and reference tables AFTER, then prove they hold the same seasons as the
#     parent (the previous scripts did this in the other order, so workers
#     silently fell back to bare Lahman tables and the default WAR source);
#   * every job error is collected and the run fails if any job failed;
#   * a manifest (root/build/manifest_<kind>_<src>.json) records inputs and
#     per-plot facts (seasons, postseason years drawn, caption) for validation.

suppressPackageStartupMessages({
  source("franchise_war_functions.R")
  source("team_position_breakdown.R")
  source("pipeline_jobs.R")
  source("pipeline_checks.R")
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop("usage: Rscript render.R grid|breakdowns br|fg [--root DIR] [--cores N] [--only REGEX]")
KIND <- match.arg(args[1], c("grid", "breakdowns"))
SRC  <- match.arg(args[2], WAR_SOURCES)
opt  <- function(flag, default) { i <- match(flag, args); if (is.na(i) || i == length(args)) default else args[i + 1] }
ROOT    <- opt("--root", "docs")
N_CORES <- as.integer(opt("--cores", parallel::detectCores()))
ONLY    <- opt("--only", NULL)

PLOT_WIDTH <- 20; GRID_HEIGHT <- 24; PLOT_DPI <- 150

message(sprintf("=== render %s / %s -> %s ===", KIND, SRC, ROOT))
war_data <- load_war_data(source = SRC)
WAR_bat <- war_data$batting; WAR_pitch <- war_data$pitching
common_cols <- c("year_ID", "player_ID", "name_common", "team_ID", "WAR", "player_type", "primary_pos")
WAR_combined <- bind_rows(WAR_bat %>% select(all_of(common_cols)),
                          WAR_pitch %>% select(all_of(common_cols)))

# Refuse to render from data that fails the hard checks
validate_war_data(war_data, SRC, stop_on_failure = TRUE)
DATA_MAX_YEAR <- max(WAR_bat$year_ID)
REF_MAX_YEARS <- vapply(REF_TABLES, function(t) max(t$yearID, na.rm = TRUE), numeric(1))

message("=== Shared data ===")
pos_year_stats <- compute_position_year_stats(WAR_bat, WAR_pitch)
award_data <- load_award_data()

jobs <- all_jobs(KIND, SRC, ROOT, PLOT_EXT)
if (!is.null(ONLY)) jobs <- Filter(function(j) grepl(ONLY, j$id), jobs)
for (d in unique(dirname(vapply(jobs, function(j) j$fname, character(1))))) dir.create(d, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(ROOT, "build"), showWarnings = FALSE, recursive = TRUE)
message(sprintf("=== %d jobs | %d cores ===", length(jobs), N_CORES))

# ---- Workers: source FIRST, then export state, then verify -------------------
cl <- parallel::makeCluster(N_CORES)
on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
invisible(parallel::clusterEvalQ(cl, suppressPackageStartupMessages({
  source("franchise_war_functions.R"); source("team_position_breakdown.R")
})))
parallel::clusterExport(cl, c("WAR_bat", "WAR_pitch", "WAR_combined", "pos_year_stats", "award_data",
                              "REF_TABLES", "WAR_SOURCE", "PLOT_WIDTH", "GRID_HEIGHT", "PLOT_DPI"))
worker_state <- parallel::clusterCall(cl, function() {
  list(src = WAR_SOURCE, ref_null = is.null(REF_TABLES),
       ref_max = vapply(REF_TABLES, function(t) max(t$yearID, na.rm = TRUE), numeric(1)),
       bat_max = max(WAR_bat$year_ID), pitch_max = max(WAR_pitch$year_ID))
})
for (w in worker_state) {
  if (!identical(w$src, SRC) || w$ref_null || !identical(w$ref_max[names(REF_MAX_YEARS)], REF_MAX_YEARS) ||
      w$bat_max != DATA_MAX_YEAR) {
    stop("worker state does not match parent: ", paste(capture.output(str(w)), collapse = "\n"))
  }
}
message("workers verified: source=", SRC, ", reference tables through ",
        paste(sprintf("%s=%d", names(REF_MAX_YEARS), REF_MAX_YEARS), collapse = " "))

# ---- Job runner ---------------------------------------------------------------
render_job <- function(job) {
  t0 <- Sys.time()
  out <- tryCatch({
    if (job$kind == "grid" || job$kind == "grid_csv") {
      war_src <- switch(job$sn, batting = WAR_bat, pitching = WAR_pitch, combined = WAR_combined)
      if (job$kind == "grid_csv") {
        p <- generate_franchise_war_plot(war_src, job$tl, start_year = job$era, position_filter = job$pf)
        export_plot_data(p, job$fname)
      } else {
        p <- generate_franchise_war_plot(war_src, job$tl, start_year = job$era, position_filter = job$pf,
          variable_width = job$vw, pos_year_stats = if (job$vw) pos_year_stats else NULL,
          show_awards = job$sa, award_data = if (job$sa) award_data else NULL, show_postseason = job$sp)
        save_plot(job$fname, p, PLOT_WIDTH, GRID_HEIGHT, PLOT_DPI)
      }
    } else {
      p <- generate_team_position_plot(WAR_bat, WAR_pitch, team_code = job$tc, team_name = job$tn,
        show_postseason = job$sp, log_scale = job$il,
        variable_width = job$iz, pos_year_stats = if (job$iz) pos_year_stats else NULL,
        show_awards = job$sa, award_data = if (job$sa) award_data else NULL)
      save_plot(job$fname, p, PLOT_WIDTH, 3 * attr(p, "n_positions") + 2, PLOT_DPI)
    }
    meta <- attr(p, "meta")
    list(id = job$id, file = job$fname, ok = TRUE, bytes = file.info(job$fname)$size,
         seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")),
         max_year = meta$max_year, caption = meta$caption,
         postseason_years = meta$postseason_years, postseason_max_year = meta$postseason_max_year,
         n_top = meta$n_top)
  }, error = function(e) list(id = job$id, file = job$fname, ok = FALSE, error = conditionMessage(e)))
  out
}

t0 <- Sys.time()
results <- parallel::parLapplyLB(cl, jobs, render_job)
elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
parallel::stopCluster(cl); on.exit()

failed <- Filter(function(r) !isTRUE(r$ok), results)
manifest <- list(
  kind = KIND, source = SRC, root = ROOT, rendered_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  r_version = R.version.string, lahman_version = as.character(packageVersion("Lahman")),
  data_max_year = DATA_MAX_YEAR, ref_max_years = as.list(REF_MAX_YEARS),
  br_feed_mtime = lapply(c(bat = "bat", pitch = "pitch"), function(w)
    format(file.mtime(file.path(SUPP_DIR, sprintf("br_war_daily_%s.txt", w))), "%Y-%m-%dT%H:%M:%S%z")),
  n_jobs = length(jobs), n_failed = length(failed), seconds = round(elapsed, 1),
  plots = results)
jsonlite::write_json(manifest, manifest_path(KIND, SRC, ROOT), auto_unbox = TRUE, pretty = TRUE, null = "null")

if (length(failed) > 0) {
  for (f in failed) message(sprintf("FAILED %s: %s", f$id, f$error))
  stop(sprintf("%d of %d jobs failed", length(failed), length(jobs)))
}
message(sprintf("=== Done: %d items in %.0fs (%.1f/s); manifest %s ===",
                length(results), elapsed, length(results) / elapsed, manifest_path(KIND, SRC, ROOT)))
