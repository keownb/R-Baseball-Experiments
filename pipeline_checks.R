# pipeline_checks.R
# Hard checks for the two places the pipeline can silently go wrong:
#   validate_war_data()  — the loaded data is complete and current BEFORE rendering
#   validate_outputs()   — the rendered files are complete and reflect that data AFTER rendering
#
# CLI:
#   Rscript pipeline_checks.R data br|fg [--require-fresh]
#   Rscript pipeline_checks.R outputs [ROOT] [--sources br,fg]
# Exit status is non-zero when any check fails. Warnings never fail the run.
#
# Every check is a plain, inspectable rule; when a rule has to know about the
# calendar (postseason over? awards announced?) the dates are explicit below.

if (!exists("franchise_map")) suppressPackageStartupMessages(source("franchise_war_functions.R"))
if (!exists("grid_jobs")) source("pipeline_jobs.R")

# ---- Calendar rules -----------------------------------------------------------
# A season's data is "final" once the World Series is over; MVP / Cy Young are
# announced in the second half of November. These are deliberately generous.
SEASON_START_MD  <- "-04-01"   # BR feed carries the new season from Opening Day
POSTSEASON_END_MD <- "-11-08"  # World Series has always ended by then
AWARDS_DONE_MD   <- "-11-30"
ALLSTAR_DONE_MD  <- "-07-25"

# ---- Check collector -----------------------------------------------------------
new_report <- function() structure(list(fail = character(0), warn = character(0), ok = character(0)), class = "check_report")
.add <- function(rep, kind, msg) { rep[[kind]] <- c(rep[[kind]], msg); rep }
check <- function(rep, cond, msg, warn_only = FALSE) {
  if (isTRUE(cond)) .add(rep, "ok", msg) else .add(rep, if (warn_only) "warn" else "fail", msg)
}
print_report <- function(rep, title) {
  cat(sprintf("\n== %s: %d ok, %d warnings, %d FAILURES ==\n", title, length(rep$ok), length(rep$warn), length(rep$fail)))
  for (m in rep$warn) cat("  WARN  ", m, "\n")
  for (m in rep$fail) cat("  FAIL  ", m, "\n")
  invisible(rep)
}
finish_report <- function(rep, title, stop_on_failure = TRUE) {
  print_report(rep, title)
  if (length(rep$fail) > 0 && stop_on_failure) stop(sprintf("%s: %d check(s) failed", title, length(rep$fail)), call. = FALSE)
  invisible(length(rep$fail) == 0)
}

# Season the BR feed should currently contain
expected_current_season <- function(today = Sys.Date()) {
  y <- as.integer(format(today, "%Y"))
  if (today >= as.Date(paste0(y, SEASON_START_MD))) y else y - 1L
}
after <- function(year, md, today = Sys.Date()) today > as.Date(paste0(year, md))

# ---- Data checks ----------------------------------------------------------------
validate_war_data <- function(war_data, src = WAR_SOURCE, stop_on_failure = TRUE,
                              today = Sys.Date(), require_fresh = FALSE, max_age_hours = 24) {
  rep <- new_report()
  bat <- war_data$batting; pit <- war_data$pitching
  max_year <- max(bat$year_ID, na.rm = TRUE)
  cur <- expected_current_season(today)

  # Coverage of the season we expect
  rep <- check(rep, max_year == max(pit$year_ID, na.rm = TRUE),
               sprintf("batting and pitching feeds end in the same season (%d)", max_year))
  rep <- check(rep, max_year >= cur, sprintf("data reaches the current season (have %d, expect %d)", max_year, cur))
  rep <- check(rep, min(bat$year_ID) <= 1901, "data starts by 1901")

  # Freshness of the BR feeds (only enforced when the build asked for it)
  for (w in c("bat", "pitch")) {
    p <- file.path(SUPP_DIR, sprintf("br_war_daily_%s.txt", w))
    age <- as.numeric(difftime(Sys.time(), file.mtime(p), units = "hours"))
    rep <- check(rep, file.exists(p) && age <= max_age_hours,
                 sprintf("BR %s feed downloaded within %dh (age %.1fh)", w, max_age_hours, age), warn_only = !require_fresh)
  }

  # Franchise mapping: every AL/NL team-season from 1901 on belongs to one of the
  # 30 franchises (Federal League and Negro League clubs are not AL/NL)
  lg_col <- if ("lg_ID" %in% names(bat)) "lg_ID" else NULL
  fr <- bind_rows(bat %>% select(year_ID, team_ID, WAR, any_of("lg_ID")), pit %>% select(year_ID, team_ID, WAR, any_of("lg_ID"))) %>%
    filter(year_ID >= 1901) %>%
    mutate(franchise = franchise_map(team_ID, year_ID))
  if (!is.null(lg_col)) {
    # The 1901-02 Baltimore Orioles (BLA) are a defunct AL club that maps to no
    # modern franchise on purpose (see .defunct_as_na in franchise_war_functions.R).
    unmapped <- fr %>% filter(is.na(franchise), lg_ID %in% c("AL", "NL"), !(team_ID == "BLA" & year_ID %in% 1901:1902)) %>%
      count(team_ID, year_ID, name = "rows")
    rep <- check(rep, nrow(unmapped) == 0, sprintf("every 1901+ AL/NL team-season maps to a franchise (unmapped: %s)",
                 if (nrow(unmapped)) paste(head(sprintf("%s %d", unmapped$team_ID, unmapped$year_ID), 8), collapse = ",") else "none"))
  }
  # Nothing may be attributed to a franchise before it existed (per Lahman::Teams)
  first_year <- Lahman::Teams %>% mutate(franchise = .franchid_code(franchID)) %>% filter(franchise %in% names(primary_colors)) %>%
    group_by(franchise) %>% summarise(first = min(yearID), .groups = "drop")
  early <- fr %>% filter(!is.na(franchise)) %>% group_by(franchise) %>% summarise(have = min(year_ID), .groups = "drop") %>%
    inner_join(first_year, by = "franchise") %>% filter(have < first)
  rep <- check(rep, nrow(early) == 0, sprintf("no WAR attributed to a franchise before its first Lahman season (%s)",
               if (nrow(early)) paste(sprintf("%s %d<%d", early$franchise, early$have, early$first), collapse = ",") else "none"))
  n_fr <- fr %>% filter(year_ID == max_year, !is.na(franchise)) %>% distinct(franchise) %>% nrow()
  rep <- check(rep, n_fr == 30, sprintf("30 franchises have %d WAR rows (have %d)", max_year, n_fr))
  for (code in names(primary_colors)) {
    yrs <- fr %>% filter(franchise == code) %>% summarise(n = n_distinct(year_ID)) %>% pull(n)
    rep <- check(rep, yrs >= 28, sprintf("franchise %s has %d seasons of data", code, yrs))
  }

  # Reference tables cover the latest season
  for (tbl in c("Appearances", "Fielding", "Pitching")) {
    tmax <- max(ref_table(tbl)$yearID, na.rm = TRUE)
    rep <- check(rep, tmax == max_year, sprintf("%s table reaches %d (has %d)", tbl, max_year, tmax))
  }

  # Positions: every batter with meaningful WAR in the latest season has a primary position
  no_pos <- bat %>% filter(year_ID == max_year, is.na(primary_pos), !is.na(WAR)) %>%
    group_by(player_ID, name_common) %>% summarise(WAR = sum(WAR), .groups = "drop") %>% filter(abs(WAR) >= 0.5)
  rep <- check(rep, nrow(no_pos) == 0,
               sprintf("%d batters with no primary position in %d (|WAR| >= 0.5): %s", max_year, nrow(no_pos),
                       paste(head(no_pos$name_common, 5), collapse = ", ")))
  # ...and across history: a position player with a real career has a position
  no_pos_all <- bat %>% filter(year_ID >= 1901, is.na(primary_pos), !is.na(WAR)) %>%
    group_by(player_ID, name_common) %>% summarise(WAR = sum(WAR), .groups = "drop") %>% filter(WAR >= 3)
  rep <- check(rep, nrow(no_pos_all) == 0,
               sprintf("%d batters with >= 3 career WAR lack a primary position (ids not joining Lahman?): %s",
                       nrow(no_pos_all), paste(head(no_pos_all$name_common, 5), collapse = ", ")))
  # Pitcher roles: every latest-season pitcher appears in the Pitching table that season
  pit_ids <- pit %>% filter(year_ID == max_year, !is.na(WAR)) %>% distinct(player_ID) %>% pull()
  ref_pit <- ref_table("Pitching") %>% filter(yearID == max_year) %>% distinct(playerID) %>% pull()
  miss_p <- setdiff(pit_ids, ref_pit)
  rep <- check(rep, length(miss_p) == 0, sprintf("%d pitchers of %d missing from the Pitching table (G/GS roles)", length(miss_p), max_year))

  # Postseason for the latest season
  sp <- ref_table("SeriesPost") %>% filter(yearID == max_year)
  if (after(max_year, POSTSEASON_END_MD, today) || any(sp$round == "WS")) {
    rep <- check(rep, sum(sp$round == "WS") == 1, sprintf("%d World Series result present", max_year))
    if (max_year >= 2022) rep <- check(rep, nrow(sp) == 11, sprintf("%d postseason has all 11 series (has %d)", max_year, nrow(sp)))
    ps_codes <- unique(c(sp$teamIDwinner, sp$teamIDloser))
    ps_fr <- lahman_franchise(ps_codes, rep(max_year, length(ps_codes)))
    rep <- check(rep, all(!is.na(ps_fr)), sprintf("%d postseason team codes all map to franchises (%s)", max_year,
                 paste(ps_codes[is.na(ps_fr)], collapse = ",")))
  } else if (after(max_year, "-10-01", today)) {
    rep <- .add(rep, "warn", sprintf("%d postseason may still be in progress (%d series so far); rerun after the World Series", max_year, nrow(sp)))
  }

  # Awards for the latest season
  aw <- ref_table("AwardsPlayers") %>% filter(yearID == max_year, awardID %in% c("Most Valuable Player", "Cy Young Award"))
  rep <- check(rep, nrow(aw) >= 4, sprintf("%d MVP + Cy Young awards present (%d rows)", max_year, nrow(aw)),
               warn_only = !after(max_year, AWARDS_DONE_MD, today))
  as_n <- ref_table("AllstarFull") %>% filter(yearID == max_year) %>% nrow()
  rep <- check(rep, as_n >= 50, sprintf("%d All-Star selections present (%d)", max_year, as_n),
               warn_only = !after(max_year, ALLSTAR_DONE_MD, today))

  # Duplicate rows in the feeds would double-count WAR
  dup_b <- bat %>% count(player_ID, year_ID, team_ID, stint_ID) %>% filter(n > 1) %>% nrow()
  dup_p <- pit %>% count(player_ID, year_ID, team_ID, stint_ID) %>% filter(n > 1) %>% nrow()
  rep <- check(rep, dup_b == 0 && dup_p == 0, sprintf("no duplicate player-season-stint rows (%d bat, %d pitch)", dup_b, dup_p))

  # FanGraphs-specific: season totals are normalized to ~1000 WAR league-wide
  if (identical(src, "fg")) {
    tot <- bind_rows(bat %>% select(year_ID, WAR), pit %>% select(year_ID, WAR)) %>%
      filter(year_ID >= 1998, year_ID <= max_year, !(year_ID %in% c(2020))) %>%  # 2020: 60 games
      group_by(year_ID) %>% summarise(WAR = sum(WAR, na.rm = TRUE), .groups = "drop")
    off <- tot %>% filter(WAR < 900 | WAR > 1100)
    rep <- check(rep, nrow(off) == 0, sprintf("FanGraphs season totals within 900-1100 WAR for 1998+ (off: %s)",
                 if (nrow(off)) paste(sprintf("%d=%.0f", off$year_ID, off$WAR), collapse = " ") else "none"))
    # Both sources must agree on which franchise-seasons exist from 1901 on
    br <- bind_rows(
      readr::read_csv(br_feed_path("bat"), show_col_types = FALSE, col_select = c(year_ID, team_ID)),
      readr::read_csv(br_feed_path("pitch"), show_col_types = FALSE, col_select = c(year_ID, team_ID))) %>%
      filter(year_ID >= 1901) %>% mutate(franchise = franchise_map(team_ID, year_ID)) %>% filter(!is.na(franchise)) %>% distinct(franchise, year_ID)
    fgy <- bind_rows(bat %>% select(year_ID, team_ID), pit %>% select(year_ID, team_ID)) %>%
      filter(year_ID >= 1901) %>% mutate(franchise = franchise_map(team_ID, year_ID)) %>% filter(!is.na(franchise)) %>% distinct(franchise, year_ID)
    only_br <- anti_join(br, fgy, by = c("franchise", "year_ID")); only_fg <- anti_join(fgy, br, by = c("franchise", "year_ID"))
    rep <- check(rep, nrow(only_br) == 0 && nrow(only_fg) == 0,
                 sprintf("FanGraphs and Baseball-Reference agree on franchise-seasons 1901+ (BR-only %d, FG-only %d: %s)", nrow(only_br), nrow(only_fg),
                         paste(head(c(sprintf("%s %d", only_br$franchise, only_br$year_ID), sprintf("%s %d", only_fg$franchise, only_fg$year_ID)), 6), collapse = ",")))
    files <- list.files(FG_DIR, pattern = "^(bat|pit)_\\d{4}\\.json$")
    yrs <- 1901:max_year
    missing <- c(setdiff(sprintf("bat_%d.json", yrs), files), setdiff(sprintf("pit_%d.json", yrs), files))
    rep <- check(rep, length(missing) == 0, sprintf("FanGraphs files present for every season 1901-%d (missing %d)", max_year, length(missing)))
  }

  finish_report(rep, sprintf("data checks (%s)", src), stop_on_failure)
}

# ---- SVG inspection ---------------------------------------------------------------
# svglite writes axis tick labels as <text text-anchor='middle' x= y=>YEAR</text>
# and geom_vline as <line x1=X y1 x2=X y2>. From two tick labels in a panel
# column we recover the year->x mapping and can ask whether a vertical line was
# drawn at a given year. This is an end-to-end check of the rendered image.
svg_text_nodes <- function(svg) {
  m <- gregexpr("<text[^>]*>[^<]*</text>", svg)[[1]]
  nodes <- regmatches(svg, list(m))[[1]]
  tibble::tibble(
    x = as.numeric(sub(".*\\bx='([-0-9.]+)'.*", "\\1", nodes)),
    y = as.numeric(sub(".*\\by='([-0-9.]+)'.*", "\\1", nodes)),
    anchor = ifelse(grepl("text-anchor='middle'", nodes), "middle", ifelse(grepl("text-anchor='end'", nodes), "end", "start")),
    text = sub(".*>([^<]*)</text>$", "\\1", nodes))
}
svg_vlines <- function(svg) {
  m <- gregexpr("<line [^>]*/>", svg)[[1]]
  nodes <- regmatches(svg, list(m))[[1]]
  d <- tibble::tibble(
    x1 = as.numeric(sub(".*x1='([-0-9.]+)'.*", "\\1", nodes)), y1 = as.numeric(sub(".*y1='([-0-9.]+)'.*", "\\1", nodes)),
    x2 = as.numeric(sub(".*x2='([-0-9.]+)'.*", "\\1", nodes)), y2 = as.numeric(sub(".*y2='([-0-9.]+)'.*", "\\1", nodes)),
    dashed = grepl("stroke-dasharray", nodes))
  d %>% filter(abs(x1 - x2) < 0.01, abs(y2 - y1) > 20)
}
# Year ticks grouped into panel columns: for each tick-row y, the k-th smallest x
# of the min-year label and of the max-year label belong to the same column.
svg_year_axes <- function(ticks) {
  yrs <- ticks %>% filter(anchor == "middle", grepl("^[12][0-9]{3}$", text)) %>% mutate(year = as.integer(text))
  if (nrow(yrs) == 0) return(NULL)
  out <- list()
  for (yy in unique(round(yrs$y, 1))) {
    row <- yrs %>% filter(abs(y - yy) < 0.5)
    lo <- row %>% filter(year == min(year)) %>% arrange(x); hi <- row %>% filter(year == max(year)) %>% arrange(x)
    if (nrow(lo) != nrow(hi) || min(row$year) == max(row$year)) next
    for (k in seq_len(nrow(lo))) {
      b <- (hi$x[k] - lo$x[k]) / (hi$year[k] - lo$year[k])
      out[[length(out) + 1]] <- tibble::tibble(tick_y = yy, col = k, x0 = lo$x[k], y0 = lo$year[k], px_per_year = b,
                                               col_xmin = lo$x[k] - 5 * b, col_xmax = hi$x[k] + 40 * b)
    }
  }
  bind_rows(out)
}
# Vertical lines at `year` inside the panel whose strip label is `strip` (or any panel if NULL)
svg_vlines_at_year <- function(svg, year, strip = NULL, tol_px = 1.5) {
  ticks <- svg_text_nodes(svg); axes <- svg_year_axes(ticks); vl <- svg_vlines(svg)
  if (is.null(axes) || nrow(axes) == 0) stop("no year axis ticks found in SVG")
  if (!is.null(strip)) {
    s <- ticks %>% filter(text == strip, anchor == "middle") %>% arrange(desc(y)) %>% slice(1)   # strip label of that facet
    if (nrow(s) == 0) stop("facet strip '", strip, "' not found in SVG")
    ax <- axes %>% filter(tick_y > s$y, col_xmin <= s$x, col_xmax >= s$x) %>% arrange(tick_y) %>% slice(1)
    if (nrow(ax) == 0) stop("no axis found under strip '", strip, "'")
    xt <- ax$x0 + (year - ax$y0) * ax$px_per_year
    vl %>% filter(abs(x1 - xt) <= tol_px, pmin(y1, y2) >= s$y - 5, pmax(y1, y2) <= ax$tick_y)
  } else {
    hits <- lapply(seq_len(nrow(axes)), function(i) {
      xt <- axes$x0[i] + (year - axes$y0[i]) * axes$px_per_year[i]
      vl %>% filter(abs(x1 - xt) <= tol_px, pmax(y1, y2) <= axes$tick_y[i] + 1)
    })
    bind_rows(hits) %>% distinct()
  }
}
svg_caption <- function(svg) { t <- svg_text_nodes(svg); t$text[grepl("^Source: ", t$text)] }

# ---- Output checks -----------------------------------------------------------------
# `root` is the site root holding plots*/, data*/, team_breakdowns*/ and build/.
# Needs the data loaded (for the facts the images must reflect); loads it if not.
validate_outputs <- function(root = "docs", sources = WAR_SOURCES, stop_on_failure = TRUE, today = Sys.Date()) {
  rep <- new_report()
  if (is.null(REF_TABLES)) invisible(load_war_data(source = "br"))
  max_year <- max(ref_table("Appearances")$yearID)

  # Facts the postseason plots must show for the latest season, computed directly
  # from the SeriesPost table (not through get_postseason_data()).
  sp <- ref_table("SeriesPost") %>% filter(yearID == max_year) %>%
    mutate(fr_w = lahman_franchise(teamIDwinner, yearID), fr_l = lahman_franchise(teamIDloser, yearID))
  ws_winner <- sp %>% filter(round == "WS") %>% pull(fr_w)
  series_winners <- unique(na.omit(sp$fr_w))
  ps_teams <- unique(na.omit(c(sp$fr_w, sp$fr_l)))
  postseason_final <- after(max_year, POSTSEASON_END_MD, today) || length(ws_winner) == 1

  for (src in sources) {
    label <- if (src == "fg") "FanGraphs" else "Baseball Reference"
    dirs <- output_dirs(src, root)
    exp <- expected_files(src, root, PLOT_EXT)

    # 1. Every expected file exists with a sane size; no unexpected files
    missing <- exp[!file.exists(exp)]
    rep <- check(rep, length(missing) == 0, sprintf("[%s] all %d expected files exist (missing %d: %s)", src, length(exp),
                 length(missing), paste(head(basename(missing), 5), collapse = ", ")))
    present <- exp[file.exists(exp)]
    sizes <- file.info(present)$size
    small <- present[(grepl("\\.svg$", present) & sizes < 20000) | (grepl("\\.png$", present) & sizes < 10000) | (grepl("\\.csv$", present) & sizes < 500)]
    rep <- check(rep, length(small) == 0, sprintf("[%s] no suspiciously small outputs (%d)", src, length(small)))
    on_disk <- unlist(lapply(unlist(dirs), list.files, full.names = TRUE))
    orphans <- setdiff(normalizePath(on_disk, mustWork = FALSE), normalizePath(exp, mustWork = FALSE))
    rep <- check(rep, length(orphans) == 0, sprintf("[%s] no stale files in output dirs (%d: %s)", src, length(orphans),
                 paste(head(basename(orphans), 5), collapse = ", ")))

    # 2. Manifests: both runs completed, from the same data season, no failed jobs
    for (kind in c("grid", "breakdowns")) {
      mp <- manifest_path(kind, src, root)
      if (!file.exists(mp)) { rep <- .add(rep, "fail", sprintf("[%s] manifest missing: %s", src, mp)); next }
      m <- jsonlite::fromJSON(mp, simplifyVector = FALSE)
      rep <- check(rep, identical(m$source, src) && m$n_failed == 0, sprintf("[%s/%s] manifest: source %s, %d failed jobs", src, kind, m$source, m$n_failed))
      rep <- check(rep, m$data_max_year == max_year, sprintf("[%s/%s] rendered from %d data (now %d)", src, kind, m$data_max_year, max_year))
      rep <- check(rep, all(unlist(m$ref_max_years[c("Appearances", "Fielding", "Pitching")]) == max_year),
                   sprintf("[%s/%s] workers held reference tables through %d", src, kind, max_year))
      caps <- unique(unlist(lapply(m$plots, function(p) p$caption)))
      rep <- check(rep, length(caps) >= 1 && all(grepl(label, caps, fixed = TRUE)), sprintf("[%s/%s] plot captions credit %s", src, kind, label))
      if (postseason_final) {
        # grid: every postseason overlay spans all clubs; breakdowns: only the clubs that won a series
        # that October (a club that lost its first series draws no line under the postseason rule)
        ps_plots <- Filter(function(p) isTRUE(p$ok) && grepl("_postseason|_all", p$id) &&
                             (kind == "grid" || sub("_.*", "", p$id) %in% series_winners), m$plots)
        n_ok <- sum(vapply(ps_plots, function(p) !is.null(p$postseason_max_year) && p$postseason_max_year == max_year, logical(1)))
        rep <- check(rep, length(ps_plots) > 0 && n_ok == length(ps_plots),
                     sprintf("[%s/%s] %d of %d postseason overlays include %d (per manifest)", src, kind, n_ok, length(ps_plots), max_year))
      }
      ids <- vapply(m$plots, function(p) p$id, character(1))
      exp_ids <- vapply(all_jobs(kind, src, root, PLOT_EXT), function(j) j$id, character(1))
      rep <- check(rep, setequal(ids, exp_ids), sprintf("[%s/%s] manifest lists exactly the expected %d jobs", src, kind, length(exp_ids)))
    }

    # 3. Every rendered SVG: complete file, right caption, legends the overlay promises
    svgs <- present[grepl("\\.svg$", present)]
    scan_one <- function(f) {
      svg <- readChar(f, file.info(f)$size, useBytes = TRUE)
      problems <- character(0)
      if (!grepl("</svg>\\s*$", svg)) problems <- c(problems, "truncated")
      if (!grepl(paste0(">Source: ", label, " WAR"), svg, fixed = TRUE)) problems <- c(problems, "caption")
      if (grepl("_postseason|_all", basename(f)) && !grepl(">Postseason<", svg, fixed = TRUE)) problems <- c(problems, "no postseason legend")
      if (grepl("_awards|_all", basename(f)) && !grepl(">Awards<", svg, fixed = TRUE)) problems <- c(problems, "no awards legend")
      if (grepl("^plots", basename(dirname(f)))) {
        strips <- regmatches(svg, gregexpr("font-weight: bold;fill: #[0-9A-Fa-f]{6}; font-family: \"Arial\";' textLength='[0-9.]+px' lengthAdjust='spacingAndGlyphs'>[A-Z]{3}</text>", svg, perl = TRUE))[[1]]
        if (length(unique(sub(".*>([A-Z]{3})</text>$", "\\1", strips))) != 30) problems <- c(problems, "not 30 facets")
      }
      if (length(problems)) sprintf("%s (%s)", basename(f), paste(problems, collapse = ", ")) else NULL
    }
    bad <- unlist(parallel::mclapply(svgs, scan_one, mc.cores = max(1, parallel::detectCores() - 2)))
    rep <- check(rep, length(bad) == 0, sprintf("[%s] %d SVGs complete with %s captions and expected legends (%d bad: %s)",
                 src, length(svgs), label, length(bad), paste(head(bad, 4), collapse = "; ")))
    if (postseason_final && length(ws_winner) == 1) {
      # World Series winner: solid vertical line at max_year in its breakdown and in the all_1901 grid facet
      f <- file.path(dirs$breakdowns, sprintf("%s_linear_const_postseason.%s", ws_winner, PLOT_EXT))
      if (file.exists(f)) {
        svg <- readChar(f, file.info(f)$size, useBytes = TRUE)
        hits <- tryCatch(svg_vlines_at_year(svg, max_year), error = function(e) { rep <<- .add(rep, "fail", paste("svg parse:", conditionMessage(e))); NULL })
        rep <- check(rep, !is.null(hits) && nrow(hits) >= 1 && any(!hits$dashed),
                     sprintf("[%s] %s breakdown shows a solid %d World Series line (%d vlines found)", src, ws_winner, max_year, if (is.null(hits)) 0 else nrow(hits)))
        rep <- check(rep, grepl("World Series", svg, fixed = TRUE), sprintf("[%s] %s breakdown legend lists World Series", src, ws_winner))
      }
      g <- file.path(dirs$plots, sprintf("all_1901_postseason.%s", PLOT_EXT))
      if (file.exists(g)) {
        svg <- readChar(g, file.info(g)$size, useBytes = TRUE)
        hits <- tryCatch(svg_vlines_at_year(svg, max_year, strip = ws_winner), error = function(e) { rep <<- .add(rep, "fail", paste("svg parse:", conditionMessage(e))); NULL })
        rep <- check(rep, !is.null(hits) && nrow(hits) >= 1 && any(!hits$dashed),
                     sprintf("[%s] all_1901_postseason facet %s shows a solid %d line", src, ws_winner, max_year))
        # every franchise that won a series this postseason has a line in its facet
        miss <- character(0)
        for (fc in series_winners) {
          h <- tryCatch(svg_vlines_at_year(svg, max_year, strip = fc), error = function(e) NULL)
          if (is.null(h) || nrow(h) == 0) miss <- c(miss, fc)
        }
        rep <- check(rep, length(miss) == 0, sprintf("[%s] all_1901_postseason marks %d for every series winner (missing: %s)", src, max_year,
                     if (length(miss)) paste(miss, collapse = ",") else "none"))
      }
    }

    # 4. CSVs: 30 franchises, 5 (or more on ties) players each, no NA totals
    for (csv in exp[grepl("\\.csv$", exp)]) {
      if (!file.exists(csv)) next
      d <- suppressMessages(readr::read_csv(csv, show_col_types = FALSE))
      n_fr <- length(unique(d$franchise))
      ok <- n_fr == 30 && all(!is.na(d$total_WAR)) && all(table(d$franchise) >= 5)
      if (!ok) rep <- .add(rep, "fail", sprintf("[%s] %s: %d franchises, min %d players, %d NA", src, basename(csv), n_fr, min(table(d$franchise)), sum(is.na(d$total_WAR))))
    }
    rep <- .add(rep, "ok", sprintf("[%s] CSV tables checked", src))
  }

  # 5. HTML contract: every filename the pages can build is rendered
  rep <- validate_html_contract(root, rep)
  finish_report(rep, "output checks", stop_on_failure)
}

# The pages construct filenames from their <select> values; make sure each one exists.
validate_html_contract <- function(root = "docs", rep = new_report(), html_dir = "docs") {
  opt_values <- function(html, select_id) {
    block <- regmatches(html, regexpr(sprintf("(?s)<select id=\"%s\">.*?</select>", select_id), html, perl = TRUE))
    if (length(block) == 0) stop("no <select id=\"", select_id, "\"> in page")
    m <- gregexpr("<option value=\"([^\"]*)\"", block, perl = TRUE)
    sub("<option value=\"([^\"]*)\"", "\\1", regmatches(block, m)[[1]])
  }
  suffixes <- c("", "_awards", "_postseason", "_all")
  ex <- readChar(file.path(html_dir, "war_explorer.html"), 1e7, useBytes = TRUE)
  tb <- readChar(file.path(html_dir, "team_breakdown.html"), 1e7, useBytes = TRUE)
  want <- character(0)
  for (src in opt_values(ex, "war-source")) for (pos in opt_values(ex, "position")) for (era in opt_values(ex, "era")) for (s in suffixes)
    want <- c(want, file.path(output_dirs(src, root)$plots, sprintf("%s_%s%s.%s", pos, era, s, PLOT_EXT)))
  for (src in opt_values(tb, "war-source")) for (tm in opt_values(tb, "team")) for (sc in opt_values(tb, "scale")) for (w in opt_values(tb, "line-width")) for (s in suffixes)
    want <- c(want, file.path(output_dirs(src, root)$breakdowns, sprintf("%s_%s_%s%s.%s", tm, sc, w, s, PLOT_EXT)))
  missing <- want[!file.exists(want)]
  check(rep, length(want) > 500 && length(missing) == 0,
        sprintf("HTML controls reference %d files, all rendered (missing %d: %s)", length(want), length(missing), paste(head(basename(missing), 5), collapse = ", ")))
}

# ---- CLI ----------------------------------------------------------------------------
if (sys.nframe() == 0 && !interactive()) {
  a <- commandArgs(trailingOnly = TRUE)
  if (length(a) == 0) stop("usage: Rscript pipeline_checks.R data br|fg [--require-fresh] | outputs [ROOT] [--sources br,fg]")
  if (a[1] == "data") {
    src <- if (length(a) >= 2 && !startsWith(a[2], "--")) a[2] else "br"
    d <- load_war_data(source = src)
    validate_war_data(d, src, require_fresh = "--require-fresh" %in% a)
  } else if (a[1] == "outputs") {
    root <- if (length(a) >= 2 && !startsWith(a[2], "--")) a[2] else "docs"
    i <- match("--sources", a); srcs <- if (!is.na(i)) strsplit(a[i + 1], ",")[[1]] else WAR_SOURCES
    validate_outputs(root, srcs)
  } else stop("unknown command: ", a[1])
}
