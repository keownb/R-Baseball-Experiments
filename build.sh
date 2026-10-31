#!/usr/bin/env bash
# build.sh — the one way to update the site.
#
#   ./build.sh                 refresh data, validate, render both WAR sources into a
#                              staging dir, validate the outputs, swap them into docs/
#   ./build.sh --deploy        ...then `npx wrangler deploy` and verify the live site
#   ./build.sh --no-refresh    reuse cached feeds (debugging only; freshness check becomes a warning)
#   ./build.sh --br-only | --fg-only
#   ./build.sh --from-stage [--deploy]   skip refresh and render; validate and install the existing build/stage
#
# Every step must succeed for the next to run (set -e, pipefail). Rendering goes
# to build/stage/ and only replaces docs/ after pipeline_checks.R has verified
# the rendered files against the data, so docs/ is never left half-updated.
# Logs for each build are kept under build/logs/<stamp>/.
set -euo pipefail
cd "$(dirname "$0")"

RSCRIPT=${RSCRIPT:-$([ -x /opt/homebrew/bin/Rscript ] && echo /opt/homebrew/bin/Rscript || command -v Rscript)}
DEPLOY=0; REFRESH=1; FROM_STAGE=0; SOURCES="br fg"
for a in "$@"; do
  case "$a" in
    --deploy) DEPLOY=1 ;;
    --from-stage) FROM_STAGE=1; REFRESH=0 ;;
    --no-refresh) REFRESH=0 ;;
    --br-only) SOURCES="br" ;;
    --fg-only) SOURCES="fg" ;;
    *) echo "unknown argument: $a" >&2; exit 2 ;;
  esac
done

STAMP=$(date +%Y%m%d%H%M)
STAGE=build/stage
LOG=build/logs/$STAMP
if [ "$FROM_STAGE" = 1 ]; then
  [ -d "$STAGE/build" ] || { echo "no $STAGE to resume from" >&2; exit 2; }
else
  rm -rf "$STAGE"; mkdir -p "$STAGE"
fi
mkdir -p "$LOG"
log() { printf '[%s] %s\n' "$(date +%H:%M:%S)" "$*" | tee -a "$LOG/build.log"; }
log "build $STAMP  sources: $SOURCES  refresh: $REFRESH  deploy: $DEPLOY  R: $RSCRIPT"

# 1. Fresh data: Baseball-Reference feeds and the MLB Stats API supplement for
#    seasons newer than the installed Lahman package.
if [ "$REFRESH" = 1 ]; then
  log "step 1/7  refreshing data"
  "$RSCRIPT" -e 'suppressMessages(source("franchise_war_functions.R")); invisible(load_war_data(refresh = TRUE))' 2>&1 | tee "$LOG/refresh.log"
else
  log "step 1/7  skipping data refresh (--no-refresh)"
fi

# 2. Hard data checks per source (season coverage, mappings, positions, postseason, awards)
FRESH_FLAG=""; [ "$REFRESH" = 1 ] && FRESH_FLAG="--require-fresh"
[ "$FROM_STAGE" = 1 ] || for s in $SOURCES; do
  log "step 2/7  validating data ($s)"
  "$RSCRIPT" pipeline_checks.R data "$s" $FRESH_FLAG 2>&1 | tee "$LOG/data_$s.log"
done

# 3. Render into the staging directory
if [ "$FROM_STAGE" = 1 ]; then log "step 3/7  reusing rendered $STAGE (--from-stage)"; fi
[ "$FROM_STAGE" = 1 ] || for s in $SOURCES; do
  for k in grid breakdowns; do
    log "step 3/7  rendering $k ($s)"
    "$RSCRIPT" render.R "$k" "$s" --root "$STAGE" 2>&1 | tee "$LOG/render_${k}_$s.log"
  done
done

# 4. Validate the rendered files against the data (completeness, manifests,
#    captions, latest postseason lines actually drawn, HTML filename contract)
log "step 4/7  validating outputs"
"$RSCRIPT" pipeline_checks.R outputs "$STAGE" --sources "$(echo $SOURCES | tr ' ' ,)" 2>&1 | tee "$LOG/outputs.log"

# 5. Install staged output into docs/ (old dir is moved aside first, then removed).
#    The stage is copied, not moved, so --from-stage can re-validate and redeploy it.
log "step 5/7  installing into docs/"
for s in $SOURCES; do
  sfx=""; [ "$s" = fg ] && sfx="_fg"
  for d in "plots$sfx" "data$sfx" "team_breakdowns$sfx"; do
    rm -rf "docs/$d.old"
    [ -d "docs/$d" ] && mv "docs/$d" "docs/$d.old"
    cp -R "$STAGE/$d" "docs/$d"
    rm -rf "docs/$d.old"
  done
done
mkdir -p docs/build; cp "$STAGE"/build/*.json docs/build/

# 6. Stamp: cache-buster in the pages and a build record the live check reads back
log "step 6/7  stamping build $STAMP"
perl -pi -e "s/\?v=\d+/?v=$STAMP/g" docs/war_explorer.html docs/team_breakdown.html
"$RSCRIPT" -e '
  suppressPackageStartupMessages(source("pipeline_jobs.R"))
  args <- commandArgs(TRUE); stamp <- args[1]; srcs <- strsplit(args[2], " ")[[1]]
  m <- lapply(srcs, function(s) jsonlite::fromJSON(manifest_path("grid", s, "docs"), simplifyVector = FALSE))
  b <- list(build = stamp, built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), sources = srcs,
            data_max_year = m[[1]]$data_max_year, lahman_version = m[[1]]$lahman_version,
            br_feed_mtime = m[[1]]$br_feed_mtime, r_version = m[[1]]$r_version,
            files = sum(vapply(srcs, function(s) length(expected_files(s, "docs")), numeric(1))))
  jsonlite::write_json(b, "docs/build.json", auto_unbox = TRUE, pretty = TRUE)' "$STAMP" "$SOURCES"
cat docs/build.json | tee -a "$LOG/build.log"

# 7. Deploy and verify the live site serves exactly what is in docs/
if [ "$DEPLOY" = 1 ]; then
  log "step 7/7  deploying"
  npx wrangler deploy 2>&1 | tee "$LOG/deploy.log"
  SITE=${SITE:-https://baseball.bricekeown.net}
  log "verifying live site"
  live=$(curl -fsS "$SITE/build.json?nocache=$STAMP" | "$RSCRIPT" -e 'cat(jsonlite::fromJSON(file("stdin"))$build)')
  [ "$live" = "$STAMP" ] || { log "LIVE CHECK FAILED: build.json says $live, expected $STAMP"; exit 1; }
  # HTML is served extensionless (/war_explorer); assets by exact path
  for f in war_explorer.html team_breakdown.html index.html plots/all_1901_postseason.svg team_breakdowns/MIL_linear_const_postseason.svg; do
    [ -f "docs/$f" ] || continue
    url="$SITE/${f%.html}"; [ "$f" = index.html ] && url="$SITE/"
    want=$(shasum -a 256 "docs/$f" | cut -d' ' -f1)
    got=$(curl -fsSL "$url?nocache=$STAMP" | shasum -a 256 | cut -d' ' -f1)
    [ "$want" = "$got" ] || { log "LIVE CHECK FAILED: $f differs from docs/"; exit 1; }
  done
  log "live site verified"
else
  log "step 7/7  not deploying (pass --deploy)"
fi
log "build $STAMP complete"
