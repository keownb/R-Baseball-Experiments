# R-Baseball-Experiments

Cumulative WAR by franchise, rendered as static SVG charts and served from
`docs/` by Cloudflare Workers static assets at https://baseball.bricekeown.net
(the old GitHub Pages URL redirects there).

## How the site works

Two interactive pages swap between pre-rendered SVGs by filename:

- `docs/plots/{pos}_{era}[_awards|_postseason|_all].svg` — the 30-team grid explorer
  (`war_explorer.html`); `pos` is `all`, `batters`, `pitchers`, `c`, `1b`, `2b`, `ss`,
  `3b`, `of`, `sp`, `rp`; `era` is 1901, 1961, 1969, 1977, 1993 or 1998
- `docs/team_breakdowns/{TEAM}_{linear|log}_{const|zscore}[_awards|_postseason|_all].svg`
  — one team, nine position panels (`team_breakdown.html`)
- `docs/data/{pos}_{era}.csv` — the top-5 table behind each grid view
- `plots/all_1901.png`, `team_breakdowns/SEA_linear_const.png` — landing-page cards / `og:image`
- the same four with an `_fg` suffix for the FanGraphs WAR variants

`pipeline_jobs.R` is the single definition of that file set. The plot
directories are not versioned (they added ~1 GB of git history per
regeneration); they are rebuilt locally and deployed.

## Updating the site

```bash
./build.sh            # refresh data, validate, render both WAR sources, validate outputs, install into docs/
./build.sh --deploy   # ...and `npx wrangler deploy`, then verify the live site matches docs/
```

`build.sh` runs these steps and stops at the first failure:

1. **Refresh** — re-download the Baseball-Reference WAR feeds and rebuild the MLB
   Stats API supplement for seasons the installed Lahman package lacks.
2. **Validate data** (`pipeline_checks.R data`) — the feeds reach the current
   season, all 30 franchises are present, every post-1900 team code maps to a
   franchise, the reference tables cover the latest season, batters have
   positions, the latest postseason is complete once the World Series is over,
   MVP/Cy Young rows exist once awards season has passed (a warning before then).
3. **Render** (`render.R`) into `build/stage/`, in parallel. Workers source the
   library first and receive the parent's data second, then prove they hold the
   same seasons as the parent. Every job is wrapped so one failure cannot leave a
   half-written directory; a manifest records what was rendered from which data.
4. **Validate outputs** (`pipeline_checks.R outputs`) — every expected file
   exists and is complete, no stale files, manifests agree with the data,
   captions credit the right WAR source, legends match the overlay, the latest
   World Series winner has its line drawn in the rendered SVGs (the image itself
   is parsed), the CSVs have 30 franchises × 5 players, and every filename the
   HTML pages can construct exists.
5. **Install** — swap the staged directories into `docs/`.
6. **Stamp** — bump the `?v=` cache-buster in both pages and write `docs/build.json`.
7. **Deploy** (with `--deploy`) and confirm the live `build.json` and a few
   files match what is on disk.

Renders are deterministic (label placement is seeded), so unchanged data gives
byte-identical files.

### After the season

| When | What | Command |
|---|---|---|
| Regular season over | Final WAR, positions | `./build.sh --deploy` |
| World Series over | Postseason lines | `./build.sh --deploy` |
| Late November | MVP / Cy Young markers | `./build.sh --deploy` (data check fails after Nov 30 until they are present) |
| New Lahman release | Supplement no longer needed for that season | `install.packages("Lahman")`, then `./build.sh --deploy` |

The supplement cache for a season that can still change expires after 12 hours;
closed seasons are cached permanently in `data_cache/supp_*_{year}.csv`
(committed, so builds are reproducible).

## Data sources

- WAR: Baseball-Reference daily WAR feeds (`war_daily_bat.txt`, `war_daily_pitch.txt`),
  cached in `data_cache/` for 12 hours.
- FanGraphs WAR: leaderboard JSON pulled through a browser session into
  `data_cache/fg/` (see `fangraphs_data.R`; the endpoint is behind bot protection).
- Positions, awards, All-Stars, postseason: the `Lahman` R package, with Lahman
  player ids rewritten to Baseball-Reference ids (they differ for ~500 players).
- Seasons newer than the installed Lahman release: MLB Stats API via
  `supplemental_data.R` (per-team fielding and games splits, awards, postseason series).

## Requirements

R 4.x for your CPU architecture (Apple Silicon: `brew install r` or the arm64
CRAN build) and:

```r
install.packages(c("dplyr","tidyr","readr","ggplot2","ggrepel","ggh4x",
                   "Lahman","jsonlite","curl","svglite"))
# optional, shrinks the two preview PNGs: brew install pngquant
```

`npx wrangler` (logged in) for deploys.

## Files

| File | Purpose |
|---|---|
| `build.sh` | The one entry point: refresh → validate → render → validate → install → stamp → deploy |
| `pipeline_jobs.R` | Definition of every output file (eras, positions, teams, overlays) |
| `pipeline_checks.R` | Hard data checks before rendering and output checks after |
| `render.R` | Parallel renderer with worker verification and a manifest |
| `franchise_war_functions.R` | Data loading, franchise mapping, grid plot, z-score pools, awards, postseason |
| `team_position_breakdown.R` | Single-team, per-position plot function |
| `supplemental_data.R` | MLB Stats API fill-in for post-Lahman seasons; Lahman → BR id reconciliation |
| `fangraphs_data.R` | FanGraphs WAR loader |
| `wrangler.jsonc` | Cloudflare deploy config (assets = `docs/`, custom domain); `docs/.assetsignore` lists legacy files kept out of the deploy |
| `generate_all_plots.R`, `generate_team_breakdowns.R` | Thin wrappers around `render.R` |
| `franchise_war_analysis.R` | Interactive scratch script for previewing one plot |
| `experiment_density.R`, `*.Rmd` | Earlier experiments; not part of the pipeline |

Plots are vector (SVG). `save_plot()` picks the device by extension, so a `.pdf`
filename gives a print/plotter-ready PDF with embedded fonts.
