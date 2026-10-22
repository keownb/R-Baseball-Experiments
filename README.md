# R-Baseball-Experiments

Cumulative WAR by franchise, rendered as static PNGs and served from `docs/`
on GitHub Pages: https://keownb.github.io/R-Baseball-Experiments/

## How the site works

The site is static HTML in `docs/` plus pre-rendered SVG plots, served from
Cloudflare Workers static assets at https://baseball.bricekeown.net
(config in `wrangler.jsonc`). The two interactive pages
(`war_explorer.html`, `team_breakdown.html`) swap between PNGs by filename:

- `docs/plots/{pos}_{era}[_zscore][_awards|_postseason|_all].svg` (grid explorer)
- `docs/team_breakdowns/{TEAM}_{linear|log}_{const|zscore}[_awards|_postseason|_all].svg`
- one PNG of each default view (`all_1901.png`, `SEA_linear_const.png`) for the
  landing-page cards and `og:image` tags
- `docs/data/{pos}_{era}.csv` (top-5 tables behind each grid view)
- the same three with an `_fg` suffix for the FanGraphs WAR variants

The plot directories are not versioned (they added ~1 GB of git history per
regeneration). A stats update is: render locally, then `npx wrangler deploy`.
The old GitHub Pages URL redirects here from the orphan `gh-pages` branch.

## Data sources

- WAR: Baseball-Reference daily WAR feeds (`war_daily_bat.txt`, `war_daily_pitch.txt`),
  cached in `data_cache/` for 12 hours by `load_war_data()`.
- Positions, awards, All-Stars, postseason: the `Lahman` R package.
- Seasons newer than the installed Lahman release are filled from the MLB Stats
  API by `supplemental_data.R` and cached as `data_cache/supp_*_{year}.csv`
  (committed, so builds are reproducible). Re-fetch with
  `load_war_data(refresh = TRUE)` after the World Series or once MVP / Cy Young
  winners are announced.

## Building

Requires R 4.x for your CPU architecture (on Apple Silicon: `brew install r`,
or the arm64 build from CRAN) and these packages:

```r
install.packages(c("dplyr","tidyr","readr","ggplot2","ggrepel","ggh4x",
                   "Lahman","jsonlite","curl","svglite"))
# optional, shrinks the two preview PNGs: brew install pngquant
```

Then, from the repo root:

```bash
Rscript generate_all_plots.R        # 456 grid SVGs + 66 CSVs (~2 min on 10 cores)
Rscript generate_team_breakdowns.R  # 480 team-breakdown SVGs (~1 min)
```

Add `fg` as the argument to either script to render the FanGraphs variants
(needs `data_cache/fg/`, see `fangraphs_data.R`). Then bump the `?v=YYYYMMDD`
cache-buster in `docs/war_explorer.html` and `docs/team_breakdown.html`,
commit the source and `data_cache/` changes, and deploy:

```bash
npx wrangler deploy
```

## Files

| File | Purpose |
|---|---|
| `franchise_war_functions.R` | Data loading, franchise mapping, grid plot, z-score pools, awards, postseason |
| `team_position_breakdown.R` | Single-team, per-position plot function |
| `supplemental_data.R` | MLB Stats API fill-in for post-Lahman seasons |
| `fangraphs_data.R` | FanGraphs WAR loader (data pulled through a browser session) |

Plots are vector (SVG). `save_plot()` in `franchise_war_functions.R` picks the
device by extension, so a `.pdf` filename gives a print/plotter-ready PDF with
embedded fonts.
| `wrangler.jsonc` | Cloudflare deploy config (assets = docs/, custom domain) |
| `generate_all_plots.R` | Batch driver for the grid explorer |
| `generate_team_breakdowns.R` | Batch driver for team breakdowns |
| `franchise_war_analysis.R` | Interactive scratch script for previewing one plot |
| `experiment_density.R`, `*.Rmd` | Earlier experiments; not part of the pipeline |
