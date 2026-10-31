# generate_team_breakdowns.R — compatibility wrapper. The renderer now lives in
# render.R (shared job catalog in pipeline_jobs.R, checks in pipeline_checks.R);
# use ./build.sh for a full, validated site update.
#
#   Rscript generate_team_breakdowns.R [br|fg]   ==   Rscript render.R breakdowns [br|fg]
src <- if (length(commandArgs(trailingOnly = TRUE)) >= 1) commandArgs(trailingOnly = TRUE)[1] else "br"
status <- system2(file.path(R.home("bin"), "Rscript"), c("render.R", "breakdowns", src))
if (status != 0) stop("render.R failed with status ", status)
