# Turns a model result into state.json, atomically. Holds call state between updates.
# Page additions of 2026-11-03 (davis_v_cooper, redistricting, trailing_needs) live in R/additions.R.
if (!exists("state_additions")) tryCatch(source(file.path(needle_root(), "R/additions.R")),
  error = function(e) cat("additions.R not loaded:", conditionMessage(e), "\n", file = stderr()))
load_base_live <- function(root, cfg) {
  BD <- file.path(root, "data/baseline")
  cells <- fread(file.path(BD, "cells_2026.csv"), colClasses = list(character = c("unit", "alias")))
  ee <- fread(file.path(BD, "early_expected.csv"), colClasses = list(character = "unit"))
  cells <- merge(cells, ee[, .(county, unit, method, A)], by = c("county", "unit", "method"), all.x = TRUE)
  make_base(cells[, .(county, unit, method, real, b, A, edb, prvb, b_h, b_house24)], cfg)
}

# County groups for the page (R/build_groups.R). Group labels per dimension, in display order.
GROUP_DIMS <- list(
  list(dimension = "Area", col = "area", groups = c("Counties from the old NC-1", "Counties new to NC-1")),
  list(dimension = "2024 presidential result", col = "pres_lean", groups = c("Strong for Harris", "More competitive", "Strong for Trump")),
  list(dimension = "Race", col = "race", groups = c("More Black voters", "Fewer Black voters")),
  list(dimension = "Age", col = "age", groups = c("More older voters", "More younger voters")))
load_county_groups <- function(root = needle_root()) fread(file.path(root, "data/baseline/county_groups.csv"))

validate_state <- function(s) {
  req <- c("race", "status", "updated_at", "feed", "candidates", "counted_votes", "expected_total", "pct_counted_est",
           "estimate", "call", "by_method", "by_county", "history", "early_electorate", "sources", "groups")
  # Optional keys added 2026-11-03: allowed, null-tolerant, checked by validate_additions() when present.
  opt <- c("davis_v_cooper", "redistricting", "trailing_needs")
  err <- character()
  miss <- setdiff(req, names(s)); if (length(miss)) err <- c(err, paste("missing", miss))
  if (length(setdiff(names(s), c(req, opt)))) err <- c(err, paste("extra", setdiff(names(s), c(req, opt))))
  if (any(opt %in% names(s))) err <- c(err, if (exists("validate_additions")) validate_additions(s) else "validate_additions not loaded")
  if (length(err)) return(err)
  if (!s$status %in% c("pre", "counting", "final", "stale")) err <- c(err, "bad status")
  if (!s$feed$source %in% c("zip", "json")) err <- c(err, "bad source")
  if (!identical(sort(names(s$feed)), sort(c("source", "last_modified", "last_change_at")))) err <- c(err, "feed keys")
  if (!identical(sort(names(s$expected_total)), c("hi90", "lo90", "mid"))) err <- c(err, "expected_total keys")
  EK <- c("margin_mid", "margin_lo90", "margin_hi90", "margin_lo50", "margin_hi50", "margin_lo95", "margin_hi95", "p_davis", "p_buckhout")
  if (!identical(sort(names(s$estimate)), sort(EK))) err <- c(err, "estimate keys")
  e <- s$estimate
  for (k in EK) if (!is.numeric(e[[k]]) || !is.finite(e[[k]])) err <- c(err, paste("estimate", k))
  if (length(err)) return(err)
  if (e$p_davis < 0 || e$p_davis > 1 || abs(e$p_davis + e$p_buckhout - 1) > 1e-6) err <- c(err, "probabilities")
  if (!(e$margin_lo90 <= e$margin_mid && e$margin_mid <= e$margin_hi90)) err <- c(err, "interval order")
  if (!(e$margin_lo95 <= e$margin_lo90 && e$margin_lo90 <= e$margin_lo50 && e$margin_lo50 <= e$margin_mid &&
        e$margin_mid <= e$margin_hi50 && e$margin_hi50 <= e$margin_hi90 && e$margin_hi90 <= e$margin_hi95)) err <- c(err, "interval order 50/95")
  so <- s$sources
  if (!identical(sort(names(so)), c("actual", "model", "prior")) || !all(sapply(so, function(x) is.numeric(x) && length(x) == 1 && x >= 0 && x <= 1))) err <- c(err, "sources keys")
  else if (abs(so$prior + so$model + so$actual - 1) > 1e-3) err <- c(err, "sources sum")
  if (!is.numeric(s$counted_votes) || s$counted_votes != round(s$counted_votes)) err <- c(err, "counted_votes int")
  if (!s$call$status %in% c("none", "Davis", "Buckhout", "withdrawn")) err <- c(err, "call status")
  if (!identical(sort(names(s$call)), sort(c("status", "called_at", "withdrawn_at", "conditions")))) err <- c(err, "call keys")
  for (c0 in s$call$conditions) if (!identical(sort(names(c0)), sort(c("id", "label", "met", "value", "threshold"))) || !is.logical(c0$met) || is.na(c0$met)) err <- c(err, "condition")
  if (!"cannot_catch_up" %in% sapply(s$call$conditions, `[[`, "id") ) err <- c(err, "no cannot_catch_up condition")
  for (m in s$by_method) if (!identical(sort(names(m)), sort(c("method", "votes", "margin", "expected_votes", "baseline_margin")))) err <- c(err, "by_method keys")
  CK <- c("county", "precincts_reported", "precincts_total", "votes", "margin", "baseline_margin", "swing", "votes_d", "votes_r", "votes_l", "pres2024_margin", "swing_pres", "house2024_margin", "swing_house",
          "expected_total", "pct_counted_est", "est_margin_mid", "est_margin_lo50", "est_margin_hi50", "est_margin_lo95", "est_margin_hi95", "benchmark_margin", "vs_benchmark", "vs_benchmark_votes", "groups")
  num1 <- function(x) is.numeric(x) && length(x) == 1 && is.finite(x)
  for (m in s$by_county) {
    if (!identical(sort(names(m)), sort(CK))) { err <- c(err, paste("by_county keys", m$county)); next }
    if (!all(sapply(m[c("expected_total", "pct_counted_est", "est_margin_mid", "est_margin_lo50", "est_margin_hi50", "est_margin_lo95", "est_margin_hi95")], num1))) { err <- c(err, paste("by_county numbers", m$county)); next }
    if (!(m$est_margin_lo95 <= m$est_margin_lo50 && m$est_margin_lo50 <= m$est_margin_mid && m$est_margin_mid <= m$est_margin_hi50 && m$est_margin_hi50 <= m$est_margin_hi95)) err <- c(err, paste("county interval order", m$county))
    if (m$votes > 0) { if (!all(sapply(m[c("benchmark_margin", "vs_benchmark", "vs_benchmark_votes")], num1))) err <- c(err, paste("benchmark", m$county)) }
    else if (!all(sapply(m[c("benchmark_margin", "vs_benchmark", "vs_benchmark_votes")], is.null))) err <- c(err, paste("benchmark not null at zero votes", m$county))
    g <- m$groups
    if (!identical(sort(names(g)), c("age", "area", "pres_lean", "race"))) err <- c(err, paste("county groups keys", m$county))
    else for (d in GROUP_DIMS) if (!isTRUE(g[[d$col]] %in% d$groups)) err <- c(err, paste("county group label", m$county, d$col))
  }
  GK <- c("dimension", "group", "counties_reporting", "counties_total", "votes", "leader_margin", "benchmark_margin", "vs_benchmark", "status")
  if (!is.list(s$groups) || length(s$groups) != sum(sapply(GROUP_DIMS, function(d) length(d$groups)))) err <- c(err, "groups length")
  for (g in s$groups) {
    if (!identical(sort(names(g)), sort(GK))) { err <- c(err, "groups keys"); next }
    d <- Filter(function(d) identical(d$dimension, g$dimension), GROUP_DIMS)
    if (!length(d) || !g$group %in% d[[1]]$groups) err <- c(err, paste("group label", g$dimension, g$group))
    if (!g$status %in% c("Davis", "Buckhout", "Neither", "Too early")) err <- c(err, "group status")
    if (g$votes > 0) { if (!all(sapply(g[c("leader_margin", "benchmark_margin", "vs_benchmark")], num1))) err <- c(err, "group numbers") }
    else if (!all(sapply(g[c("leader_margin", "benchmark_margin", "vs_benchmark")], is.null)) || g$status != "Too early") err <- c(err, "group at zero votes")
  }
  if (length(s$by_method) != 4) err <- c(err, "by_method length")
  if (length(s$by_county) != 25) err <- c(err, "by_county length")
  for (m in s$candidates) if (!identical(sort(names(m)), sort(c("name", "party", "votes", "pct")))) err <- c(err, "candidates keys")
  # History entries written before 2026-11-02 have only the first four keys; both forms are valid.
  H0 <- c("t", "p_davis", "margin_mid", "counted_votes")
  H1 <- c(H0, "margin_lo50", "margin_hi50", "margin_lo95", "margin_hi95", "reported_margin", "pct_counted_est")
  for (h in s$history) if (!(identical(sort(names(h)), sort(H0)) || identical(sort(names(h)), sort(H1)))) err <- c(err, "history keys")
  ee <- s$early_electorate
  if (!identical(sort(names(ee)), sort(c("as_of", "total", "by_party", "by_race", "by_age", "by_gender")))) err <- c(err, "early keys")
  for (k in c("by_party", "by_race", "by_age", "by_gender")) for (g in ee[[k]]) {
    if (!identical(sort(names(g)), sort(c("group", "n", "share", "share_2024")))) err <- c(err, "early cell keys")
    if (g$n < 10) err <- c(err, "early cell under 10")
  }
  err
}

write_atomic_json <- function(obj, path) {
  tmp <- tempfile(tmpdir = dirname(path), fileext = ".tmp")
  write_json(obj, tmp, auto_unbox = TRUE, null = "null", na = "null", digits = 6, pretty = TRUE)
  file.rename(tmp, path)
}

# Build the state list. `live` is a cell table; base from load_base_live; prev is last state or NULL.
build_state <- function(live, base0, cfg, prev, rs, now, feed, early, cand, cgroups = load_county_groups(), senate = NULL) {
  al <- align_live(live, base0, cfg); base <- al$base
  D <- al$D; R <- al$R; V <- al$V; ce <- base$cells; ci <- ce$ci; mi <- ce$mi
  ot <- outstanding(base, cfg, D, R, V)
  seed <- as.integer(sum(V) %% 1e6) + 1L
  es <- estimate(base, cfg, D, R, V, ndraw = cfg$draws_live, seed = seed, out = ot)
  counted <- sum(V); lead <- sum(D) - sum(R)
  nown <- as.numeric(now)
  ed_counties <- uniqueN(ce$county[mi == 1 & V > 0])
  prov_out <- sum(ot$Ocm[, 4])
  orig <- orig_eval(es, cfg, counted, lead, ed_counties, prov_out)
  cu <- ccu_test(base, cfg, D, R, V, ot, Bs = cfg$ccu$buffer_B)
  ce1 <- ccu_eval(cu, cu$by_B[[1]])

  # Original rule: logged every update, published only in mode auto_original.
  hold_min <- function(m) m * 60
  was_o <- rs$orig$called
  rs$orig <- hold_step(rs$orig, nown, orig$pass, orig$leader, hold_min(cfg$orig$hold_minutes))
  rs$ccu <- hold_step(rs$ccu, nown, ce1$pass, ce1$leader, hold_min(cfg$ccu$hold_minutes))

  # Sign-off: the only route to a published call under the cannot-catch-up rule.
  so_path <- file.path(rs$live_dir, "call_signoff.json")
  so <- if (file.exists(so_path)) tryCatch(fromJSON(so_path), error = function(e) list()) else list()
  so_t <- parse_iso(so$signed_at %||% NA)
  so_valid <- !is.null(so$candidate) && so$candidate %in% c("Davis", "Buckhout") && !is.na(so_t)
  ccu_met <- isTRUE(rs$ccu$held)
  pub <- rs$pub %||% list(status = "none", called_at = NA_real_, withdrawn_at = NA_real_, signed_at = NA_real_)
  if (cfg$call_mode == "signoff") {
    live_call <- pub$status %in% c("Davis", "Buckhout")
    stale_so <- so_valid && !is.na(pub$withdrawn_at) && as.numeric(so_t) <= pub$withdrawn_at
    if (live_call && !(ccu_met && identical(rs$ccu$leader, pub$status))) {
      pub$status <- "withdrawn"; pub$withdrawn_at <- nown
    } else if (live_call && !(so_valid && identical(so$candidate, pub$status))) {
      pub$status <- "withdrawn"; pub$withdrawn_at <- nown     # ZH removed or changed the sign-off
    } else if (!live_call && so_valid && !stale_so && ccu_met && identical(rs$ccu$leader, so$candidate)) {
      pub$status <- so$candidate; pub$called_at <- nown; pub$signed_at <- as.numeric(so_t)
    }
  } else {
    if (rs$orig$called) { if (pub$status != rs$orig$called_leader) { pub$status <- rs$orig$called_leader; pub$called_at <- nown } }
    else if (pub$status %in% c("Davis", "Buckhout")) { pub$status <- "withdrawn"; pub$withdrawn_at <- nown }
  }
  rs$pub <- pub
  iso <- function(x) if (is.na(x)) NULL else iso_now(as.POSIXct(x, origin = "1970-01-01"))

  B <- cfg$ccu$buffer_B
  conds <- list(
    list(id = "cannot_catch_up", label = sprintf("Cannot catch up: the favourable-case test passed with a %g point buffer, lead above %g points, held 10 minutes", B, cfg$ccu$lead_min_pts),
         met = ccu_met, value = round(ce1$fav_lead), threshold = 0),
    list(id = "ccu_favourable_case", label = "Leader still ahead with every outstanding vote at the trailing candidate's favourable margin (votes)",
         met = isTRUE(ce1$fav), value = round(ce1$fav_lead), threshold = 0),
    list(id = "ccu_methods_tested", label = "Every vote method with votes outstanding has reports to test against", met = !isTRUE(cu$unstressed),
         value = as.integer(!isTRUE(cu$unstressed)), threshold = 1),
    list(id = "ccu_lead", label = sprintf("Lead in the count above %g points", cfg$ccu$lead_min_pts), met = isTRUE(ce1$lead_ok), value = round(ce1$lead_pts, 2), threshold = cfg$ccu$lead_min_pts),
    list(id = "ccu_hold", label = "Test passed on consecutive updates at least 10 minutes apart", met = ccu_met,
         value = if (is.na(rs$ccu$pending_since)) 0 else round((nown - rs$ccu$pending_since) / 60, 1), threshold = cfg$ccu$hold_minutes),
    list(id = "signoff", label = "Signed off by hand in data/live/call_signoff.json", met = so_valid, value = so$candidate %||% "none", threshold = "Davis or Buckhout"))
  o <- cfg$orig
  conds_orig <- list(
    list(id = "orig_prob", label = sprintf("Original rule: P(leader wins) at least %g", o$p_min), met = orig$c1, value = round(orig$p_lead, 4), threshold = o$p_min),
    list(id = "orig_ed_counties", label = sprintf("Original rule: Election Day votes in at least %d of 25 counties", o$ed_counties_min), met = orig$c2, value = ed_counties, threshold = o$ed_counties_min),
    list(id = "orig_pct_counted", label = sprintf("Original rule: at least %d%% of estimated votes counted", o$pct_counted_min), met = orig$c3, value = round(orig$pct, 1), threshold = o$pct_counted_min),
    list(id = "orig_lead", label = "Original rule: ahead by more than outstanding provisionals, projected margin above 1 point", met = orig$c4, value = round(orig$lead_abs), threshold = round(orig$prov_out)),
    list(id = "orig_hold", label = "Original rule: conditions held on consecutive updates at least 10 minutes apart", met = isTRUE(rs$orig$held),
         value = if (is.na(rs$orig$pending_since)) 0 else round((nown - rs$orig$pending_since) / 60, 1), threshold = o$hold_minutes))
  # ZH rejected the original rule: it stays in data/live/rule_log.csv only, off the public page.
  if (identical(cfg$call_mode, "auto_original")) conds <- c(conds, conds_orig)

  # by_method, by_county
  mt <- data.table(ci, mi, D, R, V, b = ce$b, E0 = ce$E0)
  byM <- lapply(1:4, function(m) {
    x <- mt[mi == m]; v <- sum(x$V)
    bl <- if (v > 0) sum(x$b * x$V) / v else if (sum(x$E0) > 0) sum(x$b * x$E0) / sum(x$E0) else mean(x$b)
    list(method = METHODS[m], votes = round(v), margin = if (v > 0) round(100 * sum(x$D - x$R) / v, 2) else NULL,
         expected_votes = round(v + es$method_out_mid[m]), baseline_margin = round(bl, 2))
  })
  # Harris minus Trump 2024, by cell. Cells new to the baseline take their county x method value.
  mt[, b_h := if ("b_h" %in% names(ce)) ce$b_h else NA_real_]
  bh_cm <- mt[!is.na(b_h), .(f = if (sum(E0) > 0) sum(b_h * E0) / sum(E0) else mean(b_h)), by = .(ci, mi)]
  mt[bh_cm, on = .(ci, mi), b_h := fcoalesce(b_h, i.f)]
  # 2024 House race, for the map only. NA in the seven counties outside that race stays NA; only cells new to the
  # baseline (E0 of zero, not a real precinct) take the county x method value.
  mt[, b_o := if ("b_house24" %in% names(ce)) ce$b_house24 else NA_real_]
  mt[, is_new := ce$E0 == 0 & ce$real == "N"]
  bo_cm <- mt[!is.na(b_o), .(f = if (sum(E0) > 0) sum(b_o * E0) / sum(E0) else mean(b_o)), by = .(ci, mi)]
  mt[bo_cm, on = .(ci, mi), b_o := fifelse(is.na(b_o) & is_new, i.f, b_o)]
  # Benchmark: the cell baseline shifted by one constant so the district, weighted by prior expected votes (E0), is exactly
  # even. Evaluated like for like on counted cells (weighted by votes reported per cell).
  mt[, bench := b - base$Bbar]
  ec <- es$county
  byC <- lapply(seq_len(base$nc), function(cc) {
    x <- mt[ci == cc]; v <- sum(x$V); ed <- ce[ci == cc & mi == 1]; edv <- V[ci == cc & mi == 1]
    bm <- if (v > 0) sum(x$bench * x$V) / v else NA_real_
    mg0 <- if (v > 0) 100 * sum(x$D - x$R) / v else NA_real_
    gr <- cgroups[county == base$counties[cc]]
    if (nrow(gr) != 1) stop("county_groups.csv has no row for ", base$counties[cc])
    k <- ec[ci == cc]
    extra <- list(expected_total = round(k$exp_mid), pct_counted_est = if (k$exp_mid > 0) round(100 * v / k$exp_mid, 2) else 0,
         est_margin_mid = round(k$mid, 2), est_margin_lo50 = round(k$lo50, 2), est_margin_hi50 = round(k$hi50, 2),
         est_margin_lo95 = round(k$lo95, 2), est_margin_hi95 = round(k$hi95, 2),
         benchmark_margin = if (v > 0) round(bm, 2) else NULL, vs_benchmark = if (v > 0) round(mg0 - bm, 2) else NULL,
         vs_benchmark_votes = if (v > 0) round((mg0 - bm) / 100 * v) else NULL,
         groups = list(pres_lean = gr$pres_lean, area = gr$area, race = gr$race, age = gr$age))
    xh <- x[!is.na(b_h)]; vh <- sum(xh$V)
    # Like for like: 2024 margin on the same cells and methods as the votes counted so far.
    pres <- if (vh > 0) sum(xh$b_h * xh$V) / vh else if (sum(xh$E0) > 0) sum(xh$b_h * xh$E0) / sum(xh$E0) else if (nrow(xh)) mean(xh$b_h) else NA_real_
    xo <- x[!is.na(b_o)]; vo <- sum(xo$V)
    house <- if (vo > 0) sum(xo$b_o * xo$V) / vo else if (sum(xo$E0) > 0) sum(xo$b_o * xo$E0) / sum(xo$E0) else NA_real_
    bl <- if (v > 0) sum(x$b * x$V) / v else if (sum(x$E0) > 0) sum(x$b * x$E0) / sum(x$E0) else mean(x$b)
    mg <- if (v > 0) 100 * sum(x$D - x$R) / v else NA
    list(county = base$counties[cc], precincts_reported = sum(edv > 0 & ed$edb > 0), precincts_total = sum(ed$edb > 0), votes = round(v),
         margin = if (v > 0) round(mg, 2) else NULL, baseline_margin = round(bl, 2), swing = if (v > 0) round(mg - bl, 2) else NULL,
         votes_d = round(sum(x$D)), votes_r = round(sum(x$R)), votes_l = round(v - sum(x$D) - sum(x$R)),
         pres2024_margin = if (is.finite(pres)) round(pres, 2) else NULL,
         swing_pres = if (v > 0 && is.finite(pres)) round(mg - pres, 2) else NULL,
         house2024_margin = if (is.finite(house)) round(house, 2) else NULL,
         swing_house = if (v > 0 && is.finite(house)) round(mg - house, 2) else NULL) |> c(extra)
  })
  # Groups of counties: pooled margin of counted votes against the pooled benchmark, like for like.
  cagg <- mt[, .(V = sum(V), L = sum(D - R), BV = sum(bench * V)), by = ci]
  cagg[, county := base$counties[ci]]
  cagg <- merge(cagg, cgroups, by = "county")
  cagg <- merge(cagg, ec[, .(ci, exp_mid)], by = "ci")
  grp <- list()
  for (d in GROUP_DIMS) for (g in d$groups) {
    x <- cagg[get(d$col) == g]; v <- sum(x$V)
    lm <- if (v > 0) 100 * sum(x$L) / v else NA_real_; bm <- if (v > 0) sum(x$BV) / v else NA_real_
    vs <- lm - bm
    st <- if (v < cfg$groups_min_counted * sum(x$exp_mid) || v == 0) "Too early" else if (abs(vs) < cfg$bench_threshold) "Neither" else if (vs > 0) "Davis" else "Buckhout"
    grp[[length(grp) + 1]] <- list(dimension = d$dimension, group = g, counties_reporting = sum(x$V > 0), counties_total = nrow(x), votes = round(v),
      leader_margin = if (v > 0) round(lm, 2) else NULL, benchmark_margin = if (v > 0) round(bm, 2) else NULL, vs_benchmark = if (v > 0) round(vs, 2) else NULL, status = st)
  }
  src <- source_shares(base, es, counted)
  sp <- round(src$prior, 4); sm <- round(src$model, 4)
  sources <- list(prior = sp, model = sm, actual = round(1 - sp - sm, 4))
  # status
  close_t <- as.numeric(parse_iso(cfg$polls_close))
  all_ed <- sum(ot$EDb) == 0
  all_early <- sum(ot$Ocm[, 2:3]) == 0
  last_change <- feed$last_change_num
  status <- if (counted > 0 && all_ed && all_early) "final"
            # "stale" only once votes exist: no votes 30 minutes after close is normal, not a dead feed.
            else if (counted > 0 && nown > close_t && (nown - max(last_change, close_t)) / 60 >= cfg$stale_minutes) "stale"
            else if (counted == 0) "pre" else "counting"
  hist <- prev$history %||% list()
  hist[[length(hist) + 1]] <- list(t = iso_now(now), p_davis = round(es$p_davis, 4), margin_mid = round(es$margin_mid, 2), counted_votes = round(counted),
    margin_lo50 = round(es$margin_lo50, 2), margin_hi50 = round(es$margin_hi50, 2), margin_lo95 = round(es$margin_lo95, 2), margin_hi95 = round(es$margin_hi95, 2),
    reported_margin = if (counted > 0) round(100 * lead / counted, 2) else NULL, pct_counted_est = round(es$pct_counted, 2))
  if (length(hist) > cfg$history_max) hist <- hist[seq.int(length(hist) - cfg$history_max + 1, length(hist))]
  total_cand <- sum(cand$votes)
  st <- list(race = cfg$race, status = status, updated_at = iso_now(now),
    feed = list(source = feed$source, last_modified = iso_now(feed$last_modified), last_change_at = iso_now(as.POSIXct(last_change, origin = "1970-01-01"))),
    candidates = lapply(seq_len(nrow(cand)), function(i) list(name = cand$name[i], party = cand$party[i], votes = round(cand$votes[i]), pct = if (total_cand > 0) round(100 * cand$votes[i] / total_cand, 2) else NULL)),
    counted_votes = round(counted),
    expected_total = list(mid = round(es$exp_mid), lo90 = round(es$exp_lo), hi90 = round(es$exp_hi)),
    pct_counted_est = round(es$pct_counted, 2),
    estimate = list(margin_mid = round(es$margin_mid, 2), margin_lo90 = round(es$margin_lo90, 2), margin_hi90 = round(es$margin_hi90, 2),
                    margin_lo50 = round(es$margin_lo50, 2), margin_hi50 = round(es$margin_hi50, 2), margin_lo95 = round(es$margin_lo95, 2), margin_hi95 = round(es$margin_hi95, 2),
                    p_davis = round(es$p_davis, 4), p_buckhout = round(1 - round(es$p_davis, 4), 4)),
    call = list(status = pub$status, called_at = iso(pub$called_at), withdrawn_at = iso(pub$withdrawn_at), conditions = conds),
    by_method = byM, by_county = byC, history = hist, early_electorate = early, sources = sources, groups = grp)
  # Page additions (R/additions.R): never break the main estimate; on any failure publish the null-shaped blocks.
  add <- tryCatch(state_additions(live, base, mt, es, D, R, V, senate, feed$source),
                  error = function(e) { cat("additions failed:", conditionMessage(e), "\n", file = stderr()); NULL })
  if (is.null(add)) add <- tryCatch(list(davis_v_cooper = dvc_null(), redistricting = red_null(), trailing_needs = tn_null()), error = function(e) NULL)
  if (!is.null(add)) st <- c(st, add)
  list(state = st, rs = rs, new_cells = al$new_cells, ccu = ce1, orig = orig, es = es)
}
