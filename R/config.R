# One place for every threshold and assumption. ZH has approved neither the prior nor the
# calling thresholds; change them here and nowhere else.
needle_root <- function() {
  r <- Sys.getenv("NEEDLE_ROOT", "")
  if (nzchar(r)) return(r)
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(a)) return(normalizePath(file.path(dirname(sub("^--file=", "", a[1])), "..")))
  getwd()
}

CFG <- list(
  race = "NC-1",
  contest_prefix = "US HOUSE OF REPRESENTATIVES DISTRICT 01",
  contest_lid = "1369",
  candidates = list(D = "Don Davis", R = "Laurie Buckhout", L = "Tom Bailey"),
  polls_close = "2026-11-03T19:30:00-05:00",
  tz = "America/New_York",

  # --- Prior on the final margin. Approved by ZH 2026-11-03: adjusted poll average, wider uncertainty. ---
  # R/build_prior.R: 4 NC-1 polls since Aug 1 in the NYT file, partisan polls moved 4.5 points toward the other
  # party, sqrt(n) weights: D+4.54 (unadjusted D+5.56), rounded to 4.5. Scale 8 rather than 6 because there are only
  # four polls and three are partisan. t(5), scale 8: P(Davis) = pt(4.5/8, 5) = 0.701.
  # Before 2026-11-03 this was D+2, scale 6, an assumption rather than a poll average.
  prior_mid = 4.5, prior_sd = 8, t_df = 5,

  # --- Baseline blend: weight on Harris; the rest on Stein. From data/baseline/blend_regression.csv: Davis's own
  # 2024 cell margins load 0.77 (SE 0.02) on Harris, 0.77 within county x method. The 2022 analogue on Biden and Cooper
  # is weaker and differs (0.58 overall, 0.21 within), but those are other candidates. Rounded to 0.75. ---
  blend_harris = 0.75,
  baseline_shrink_k = 25,      # votes of county x method baseline added to each cell's own baseline

  # --- Swing model SDs (points). Sized from past swings on these lines (README, "Variance sizing"). ---
  tau_d = 10,       # method deviation. The ED minus early gap moved 2 points 2020-22 and 23 to 26 points 2022-24.
  d_prior_mean = c(ED = 0, EV = 0, Mail = 0, Prov = 0), # prior mean of method deviations; see README on the first-drop risk
  early_comp_adjust = FALSE,  # PROPOSAL, not approved: add data/baseline/comp_cells.csv `adj` (R/build_comp.R) to early/mail cell baselines
  tau_out = 4,      # shared error of outstanding votes within a method; added after the first backtest undercovered
  tau_c = 5,        # county effect: 3.0 to 8.2 across four past contests
  tau_cm = 3.5,     # county x method deviation: 3.2 to 3.8
  tau_e = 8,        # cell swing beyond sampling noise: ED cell residual SD was 10 to 12 at about 300 votes
  sig2 = 10000,     # sampling noise of a cell margin: sig2 / votes

  # --- Election Day turnout ratio: 2026 ED votes / 2024 ED votes, by precinct. ASSUMPTION. ---
  # 1.15 puts ED at about 113,000 and the total near 317,000, versus 288,694 in 2022 (Senate, these lines),
  # with 205,458 early and mail ballots already accepted against 160,345 votes in 2022.
  ed_ratio_prior = 1.15, ed_ratio_sd = 0.25,
  ed_ratio_county_sd = 0.12, ed_ratio_noise = 0.20,
  ed_partial_frac = 0.6,       # reported ED below this share of its expected votes is a partial precinct

  # --- Early and mail. ---
  # House votes per accepted ballot. Top-of-ticket votes per accepted ballot were 0.995 to 1.006 (2022, 2024) and
  # House votes per top-of-ticket vote were 0.995 in 2022 and 0.981 in 2024; recheck table in data/baseline/recheck.txt.
  rolloff_ev = 0.99, rolloff_mail = 0.99,
  ev_noise = 0.02, late_mail_frac = 0.05,   # ASSUMPTION: mail arriving after the voter file snapshot
  closed_frac = 0.985,         # county x method at this share of expected counts as closed
  prov_noise = 0.25,

  draws_live = 20000, draws_backtest = 2000,

  # --- Calling. mode "signoff" is the live default: nothing is called without data/live/call_signoff.json.
  # mode "auto_original" restores the briefed automatic rule; ZH rejected it, so it is only logged. ---
  call_mode = "signoff",
  orig = list(p_min = 0.99, ed_counties_min = 18, pct_counted_min = 80, projected_margin_min = 1, hold_minutes = 10),
  ccu = list(buffer_B = 10, lead_min_pts = 0.5, hold_minutes = 10),

  # --- Page groups panel (reporting only; the model and calls never see it). ---
  # ASSUMPTION, ZH must approve: a group whose pooled margin is within 3 points of its benchmark shows "Neither".
  # Groups with under 10% of their expected votes counted show "Too early".
  bench_threshold = 3, groups_min_counted = 0.10,

  # --- Loop. ---
  min_fetch_seconds = 120, stale_minutes = 30, max_consecutive_errors = 5,
  backoff_seconds = 900, user_agent = "needle_nc01 research poller (one process, 120 s)",
  json_request_spacing = 1,
  history_max = 1000,

  # --- Page additions 2026-11-03 (R/additions.R; reporting only, never fed to the model). New entries only. ---
  # Senate contest for Davis v Cooper and the redistricting comparison. lid 1383 read from the cached
  # er.ncsbe.gov precinct_15.txt (rows "Roy Cooper", "Michael Whatley"). LIB Shannon W. Bray and GRE Michael Dublin
  # count only in the denominator.
  senate_contest = "US SENATE",
  senate_lid = "1383",
  senate_candidates = list(D = "Roy Cooper", R = "Michael Whatley", L = "Shannon W. Bray", G = "Michael Dublin")
)
