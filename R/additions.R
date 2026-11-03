# Page additions approved by ZH 2026-11-03: Davis v Cooper, the redistricting comparison, and what the trailing candidate
# needs. Reporting only: nothing here feeds the estimate, the prior or the calling rules. build_state() calls
# state_additions(), which never throws: any failure logs to stderr and publishes the null-shaped block.
# Margins are points, first-named minus second-named, as a share of ALL votes in that contest (third parties in the
# denominator), like the House margin everywhere else in state.json.
suppressPackageStartupMessages({library(data.table); library(jsonlite)})

# --- Senate readers ------------------------------------------------------------------------------------------------
# Zip: statewide, every county. Same cell table as contest_cells(): county, unit, real, method, D (Cooper), R (Whatley), T.
senate_from_zip <- function(path, cfg) {
  d <- read_results(path)
  d <- d[contest == cfg$senate_contest]
  if (!nrow(d)) stop("no rows for contest in zip: ", cfg$senate_contest)
  if (!all(c("DEM", "REP") %in% d$party)) stop("senate rows lack a DEM or REP party code")
  contest_cells(d)[]
}
# JSON: one file per NC-1 county, so the four old-NC-1 counties outside the district (Greene, Lenoir, Wayne, Wilson) are absent.
senate_from_json <- function(files, cty_map, cfg, geo = NULL) {
  out <- list()
  for (f in files) {
    id <- as.integer(sub(".*precinct_([0-9]+)\\.txt$", "\\1", f))
    j <- as.data.table(fromJSON(f))
    if (!nrow(j)) stop("empty JSON for ", basename(f))
    miss <- setdiff(c("lid", "aid", "bnm", "evc", "ovc", "avc", "pvc"), names(j))
    if (length(miss)) stop("json fields missing in ", basename(f), ": ", toString(miss))
    j <- j[as.character(lid) == cfg$senate_lid]
    if (!nrow(j)) stop("senate contest id ", cfg$senate_lid, " absent from ", basename(f))
    j[, county := cty_map$county[match(id, cty_map$id)]]
    out[[f]] <- j
  }
  j <- rbindlist(out, fill = TRUE)
  sc <- cfg$senate_candidates
  if (!all(c(sc$D, sc$R) %in% j$bnm)) stop("senate candidate names in json do not match CFG: ", paste(unique(j$bnm), collapse = ", "))
  j[, party := fifelse(bnm == sc$D, "DEM", fifelse(bnm == sc$R, "REP", "OTH"))]
  for (v in c("evc", "ovc", "avc", "pvc")) set(j, j = v, value = as.numeric(gsub(",", "", j[[v]])))
  x <- melt(j, id.vars = c("county", "aid", "party"), measure.vars = c("evc", "ovc", "avc", "pvc"), variable.name = "m", value.name = "v")
  x[, method := METHODS[match(m, c("evc", "ovc", "avc", "pvc"))]]
  r <- x[, .(D = sum(v[party == "DEM"], na.rm = TRUE), R = sum(v[party == "REP"], na.rm = TRUE), T = sum(v, na.rm = TRUE)),
         by = .(county, unit = trimws(aid), method)]
  # The JSON has no real-precinct flag. Units in geo_2026 take theirs; others are guessed from the name.
  if (!is.null(geo)) r <- merge(r, geo[, .(county, unit, real)], by = c("county", "unit"), all.x = TRUE) else r[, real := NA_character_]
  r[is.na(real), real := fifelse(is_pseudo_name(unit), "N", "Y")]
  r[]
}
is_pseudo_name <- function(u) grepl("ABSENTEE|PROVISIONAL|TRANSFER|SPARE|CURBSIDE|ONE STOP|ONESTOP|EARLY|^EV[ -]|^OS[ -]", toupper(u))

# Both feeds, read independently; NULL where a feed is missing or unreadable. Called from R/step.R.
read_senate <- function(zf, jf, ids, cfg, geo, log_line = function(...) message(...)) {
  z <- if (file.exists(zf)) tryCatch(senate_from_zip(zf, cfg), error = function(e) { log_line("senate feed: zip not usable (page additions only): ", conditionMessage(e)); NULL })
  j <- if (length(jf) && all(file.exists(jf))) tryCatch(senate_from_json(jf, ids, cfg, geo), error = function(e) { log_line("senate feed: json not usable (page additions only): ", conditionMessage(e)); NULL })
  list(zip = z, json = j)
}

# --- Helpers -------------------------------------------------------------------------------------------------------
r2 <- function(x) if (length(x) == 1 && is.finite(x)) round(x, 2) else NULL
r4 <- function(x) if (length(x) == 1 && is.finite(x)) round(x, 4) else NULL
mg_pts <- function(a, b, t) if (t > 0) 100 * (a - b) / t else NA_real_
pick_senate <- function(senate, src) {
  if (is.null(senate)) return(NULL)
  s <- senate[[src]]; used <- src
  if (is.null(s)) { used <- setdiff(c("zip", "json"), src); s <- senate[[used]] }
  if (is.null(s)) NULL else list(cells = s, source = used)
}
split_factors <- function(root) {
  f <- file.path(root, "data/baseline/split_factors.csv")
  if (file.exists(f)) fread(f, colClasses = list(character = "unit")) else data.table(county = character(), unit = character(), fact = numeric())
}

# --- 1. Davis v Cooper ----------------------------------------------------------------------------------------------
# Like for like: county x unit x method cells of NC-1 where both contests have votes. Senate votes in NC-1 units of split
# counties are scaled by split_factors.csv (Granville SALM 0.643, WOEL 0), the same factor the House baseline uses; a cell
# with factor 0 is dropped. Within a cell the factor does not change the Senate margin, only that cell's weight.
dvc_null <- function() list(available = FALSE, source = NULL, district = NULL, by_county = list())
davis_v_cooper <- function(live, sen, geo, counties, root = needle_root()) {
  if (is.null(sen)) return(dvc_null())
  s <- merge(sen$cells, geo[, .(county, unit)], by = c("county", "unit"))
  s <- merge(s, split_factors(root), by = c("county", "unit"), all.x = TRUE); s[is.na(fact), fact := 1]
  x <- merge(live[, .(county, unit, method, hD = D, hR = R, hT = T)], s[, .(county, unit, method, sD = D * fact, sR = R * fact, sT = T * fact, fact)],
             by = c("county", "unit", "method"))
  x <- x[hT > 0 & sT > 0 & fact > 0]
  one <- function(z, cty = NULL) {
    hv <- sum(z$hT); sv <- sum(z$sT)
    dm <- mg_pts(sum(z$hD), sum(z$hR), hv); cm <- mg_pts(sum(z$sD), sum(z$sR), sv)
    c(if (!is.null(cty)) list(county = cty), list(cells = nrow(z), votes_house = round(hv), votes_senate = round(sv),
      davis_margin = r2(dm), cooper_margin = r2(cm), davis_minus_cooper = if (nrow(z)) r2(dm - cm) else NULL))
  }
  list(available = TRUE, source = sen$source, district = one(x),
       by_county = lapply(counties, function(cc) one(x[county == cc], cc)))
}

# --- 2. Redistricting: Senate on the old and new NC-1 lines ---------------------------------------------------------
# Old lines from data/baseline/old_lines_cells.csv (R/build_old_lines.R). The Senate count on each set of lines is weighted
# by the unit's weight on those lines; the 2024 Harris minus Trump baseline is like for like, weighted by tonight's
# weighted Senate votes per cell. Whole 2024 figures are in data/baseline/lines_2024.csv.
red_null <- function() list(available = FALSE, source_old = NULL, source_new = NULL, low_match_flag = NULL, match_rate = NULL, coverage = NULL,
                            old_lines = NULL, new_lines = NULL, gap = NULL, old_lines_unavailable_reason = NULL)
# A county counts as reporting only where it has votes: the zip carries zero-vote rows for every unit before any count.
reporting_counties <- function(s) { v <- s[, .(v = sum(T, na.rm = TRUE)), by = county]; v[v > 0, county] }
lines_block <- function(z, votes24, pres24_full, counties_total, counties_missing) {
  v <- sum(z$wT); sm <- mg_pts(sum(z$wD), sum(z$wR), v)
  zb <- z[is.finite(b24) & wT > 0]; pl <- if (sum(zb$wT) > 0) sum(zb$b24 * zb$wT) / sum(zb$wT) else NA_real_
  list(counted_votes = round(v), votes_2024 = round(votes24), counted_share = r4(if (votes24 > 0) v / votes24 else NA_real_),
       senate_margin = r2(sm), pres2024_margin_counted = r2(pl), pres2024_margin = r2(pres24_full),
       senate_minus_pres2024 = r2(sm - pl), counties_total = counties_total, counties_missing = as.list(counties_missing))
}
redistricting <- function(senate, src, geo, new_b, root = needle_root(), log_line = function(...) message(...)) {
  BD <- file.path(root, "data/baseline")
  oc <- fread(file.path(BD, "old_lines_cells.csv"), colClasses = list(character = "unit"))
  l24 <- fread(file.path(BD, "lines_2024.csv"))
  OLD <- unique(oc$county)
  # Old lines: the zip is statewide, so prefer it; the JSON lacks the old counties outside the new district.
  so <- if (!is.null(senate$zip)) list(cells = senate$zip, source = "zip") else pick_senate(senate, "json")
  sn <- pick_senate(senate, src)
  if (is.null(so) && is.null(sn)) return(red_null())
  if (!is.null(so) && !is.null(sn) && !identical(so$source, sn$source))
    log_line("redistricting: old lines use the ", so$source, " Senate feed, the main House source is ", src, " (new lines use ", sn$source, ")")
  # The JSON lacks Greene, Lenoir, Wayne and Wilson, so an old-lines figure from it alone would be wrong: publish none.
  old_na <- if (!is.null(so) && identical(so$source, "json")) "needs the statewide zip" else NULL
  old <- NULL
  if (!is.null(so) && is.null(old_na)) {
    s <- so$cells[county %in% OLD]
    s <- merge(s, oc[unit != "*", .(county, unit, method, w, b_old)], by = c("county", "unit", "method"), all.x = TRUE)
    fb <- oc[unit == "*", .(county, method, w_fb = w, w_ps = w_pseudo, b_fb = b_old)]
    s <- merge(s, fb, by = c("county", "method"), all.x = TRUE)
    # Units new tonight: whole counties weight 1; split counties pro rata if a pseudo-precinct, otherwise 0.
    s[is.na(w), w := fifelse(!is.na(w_fb), w_fb, fifelse(real == "N" | is_pseudo_name(unit), fcoalesce(w_ps, 0), 0))]
    s[is.na(b_old), b_old := b_fb]
    z <- s[, .(wD = D * w, wR = R * w, wT = T * w, b24 = b_old)]
    old <- lines_block(z, l24[lines == "old", votes_2024_pres], l24[lines == "old", harris_minus_trump], length(OLD), setdiff(OLD, reporting_counties(s)))
  }
  new <- NULL
  if (!is.null(sn)) {
    s <- merge(sn$cells, geo[, .(county, unit)], by = c("county", "unit"))
    s <- merge(s, split_factors(root), by = c("county", "unit"), all.x = TRUE); s[is.na(fact), fact := 1]
    s <- merge(s, new_b, by = c("county", "unit", "method"), all.x = TRUE)
    z <- s[, .(wD = D * fact, wR = R * fact, wT = T * fact, b24 = b_h)]
    new <- lines_block(z, l24[lines == "new", votes_2024_pres], l24[lines == "new", harris_minus_trump], uniqueN(geo$county), setdiff(unique(geo$county), reporting_counties(s)))
  }
  mr <- l24[lines == "old", match_rate]
  d <- function(a, b) if (is.null(a) || is.null(b)) NULL else r2(a - b)
  list(available = TRUE, source_old = so$source, source_new = sn$source, low_match_flag = isTRUE(mr < 0.9), match_rate = r4(mr),
       coverage = r4(l24[lines == "old", coverage]), old_lines = old, new_lines = new,
       gap = if (is.null(old)) NULL else list(senate_old_minus_new = d(old$senate_margin, new$senate_margin),
                  pres2024_old_minus_new = d(old$pres2024_margin, new$pres2024_margin),
                  pres2024_counted_old_minus_new = d(old$pres2024_margin_counted, new$pres2024_margin_counted)),
       old_lines_unavailable_reason = old_na)
}

# --- 3. What the trailing candidate needs ---------------------------------------------------------------------------
# From the same draws as the estimate: remaining = simulated final total - counted; remaining margin (Davis minus Buckhout,
# points of remaining votes) = 100 (final lead - counted lead) / remaining. needed_margin = 100 x |lead| / remaining, the
# margin the trailing candidate must win the remaining votes by to tie, at the median remaining (and at its 90% bounds:
# the low bound of needed uses the high bound of remaining). expected_margin_* is the trailing candidate's margin on the
# remaining votes in the model (trailing minus leading), so the trailing candidate is on course when it exceeds needed.
tn_null <- function() list(available = FALSE, leader = NULL, trailer = NULL, lead_votes = NULL, lead_pts = NULL,
                           remaining_votes_mid = NULL, remaining_votes_lo90 = NULL, remaining_votes_hi90 = NULL,
                           needed_margin = NULL, needed_margin_lo90 = NULL, needed_margin_hi90 = NULL, needed_possible = NULL,
                           expected_margin_mid = NULL, expected_margin_lo90 = NULL, expected_margin_hi90 = NULL)
trailing_needs <- function(es, D, R, V) {
  counted <- sum(V); lead <- sum(D) - sum(R)
  if (counted <= 0) return(tn_null())
  out <- tn_null(); out$available <- TRUE
  sgn <- if (lead > 0) 1 else if (lead < 0) -1 else 0
  out$leader <- if (sgn > 0) "Davis" else if (sgn < 0) "Buckhout" else NULL
  out$trailer <- if (sgn > 0) "Buckhout" else if (sgn < 0) "Davis" else NULL
  out$lead_votes <- round(abs(lead)); out$lead_pts <- r2(100 * abs(lead) / counted)
  rem <- es$tot_draws - counted
  q <- quantile(rem, c(.05, .5, .95), names = FALSE)
  out$remaining_votes_mid <- round(max(q[2], 0)); out$remaining_votes_lo90 <- round(max(q[1], 0)); out$remaining_votes_hi90 <- round(max(q[3], 0))
  if (q[2] < 1) return(out)                      # nothing left to win: needed and expected stay null
  out$needed_possible <- 100 * abs(lead) / q[2] <= 100
  if (q[2] < 100) return(out)                    # under 100 votes left the margins are noise: needed and expected stay null
  out$needed_margin <- r2(100 * abs(lead) / q[2])
  out$needed_margin_lo90 <- r2(100 * abs(lead) / max(q[3], 1)); out$needed_margin_hi90 <- if (q[1] >= 1) r2(100 * abs(lead) / q[1]) else NULL
  ok <- rem >= 1
  rm_d <- 100 * (es$final_draws[ok] * es$tot_draws[ok] / 100 - lead) / rem[ok]
  tr <- if (sgn == 0) rm_d else -sgn * rm_d     # trailing minus leading; at a tie, Davis minus Buckhout
  qe <- pmin(pmax(quantile(tr, c(.05, .5, .95), names = FALSE), -100), 100)   # a margin on remaining votes lies in [-100, 100]
  out$expected_margin_mid <- r2(qe[2]); out$expected_margin_lo90 <- r2(qe[1]); out$expected_margin_hi90 <- r2(qe[3])
  out
}

# --- Wrapper used by build_state ------------------------------------------------------------------------------------
state_additions <- function(live, base, mt, es, D, R, V, senate, src, root = needle_root()) {
  safe <- function(expr, nul, what) tryCatch(expr, error = function(e) {
    cat("additions:", what, "failed:", conditionMessage(e), "\n", file = stderr()); nul })
  geo <- unique(data.table(county = base$counties[base$cells$ci], unit = base$cells$unit))
  list(
    davis_v_cooper = safe(davis_v_cooper(live, pick_senate(senate, src), geo, base$counties, root), dvc_null(), "davis_v_cooper"),
    redistricting = safe(redistricting(senate, src, geo, mt[, .(county = base$counties[ci], unit = base$cells$unit, method = METHODS[mi], b_h)], root), red_null(), "redistricting"),
    trailing_needs = safe(trailing_needs(es, D, R, V), tn_null(), "trailing_needs"))
}

# --- Schema check, tolerant of null -----------------------------------------------------------------------------------
validate_additions <- function(s) {
  err <- character()
  numn <- function(x) is.null(x) || (is.numeric(x) && length(x) == 1 && is.finite(x))
  keys <- function(x, k, lab) if (!identical(sort(names(x)), sort(k))) paste(lab, "keys") else character()
  if (!is.null(b <- s$davis_v_cooper)) {
    err <- c(err, keys(b, c("available", "source", "district", "by_county"), "davis_v_cooper"))
    if (!is.logical(b$available)) err <- c(err, "davis_v_cooper available")
    DK <- c("cells", "votes_house", "votes_senate", "davis_margin", "cooper_margin", "davis_minus_cooper")
    if (!is.null(b$district)) { err <- c(err, keys(b$district, DK, "davis_v_cooper district"))
      if (!all(sapply(b$district[DK], numn))) err <- c(err, "davis_v_cooper district numbers") }
    for (k in b$by_county) { err <- c(err, keys(k, c("county", DK), "davis_v_cooper by_county"))
      if (!all(sapply(k[DK], numn))) err <- c(err, "davis_v_cooper county numbers") }
    if (isTRUE(b$available) && length(b$by_county) != 25) err <- c(err, "davis_v_cooper by_county length")
  }
  if (!is.null(b <- s$redistricting)) {
    RK <- c("available", "source_old", "source_new", "low_match_flag", "match_rate", "coverage", "old_lines", "new_lines", "gap")
    err <- c(err, keys(b[setdiff(names(b), "old_lines_unavailable_reason")], RK, "redistricting"))   # reason key added 2026-11-03; older files lack it
    if (!is.null(b$old_lines_unavailable_reason) && !is.character(b$old_lines_unavailable_reason)) err <- c(err, "redistricting old_lines_unavailable_reason")
    LK <- c("counted_votes", "votes_2024", "counted_share", "senate_margin", "pres2024_margin_counted", "pres2024_margin", "senate_minus_pres2024", "counties_total", "counties_missing")
    for (nm in c("old_lines", "new_lines")) if (!is.null(l <- b[[nm]])) { err <- c(err, keys(l, LK, nm))
      if (!all(sapply(l[setdiff(LK, "counties_missing")], numn))) err <- c(err, paste(nm, "numbers"))
      if (!is.null(l$counted_share) && (l$counted_share < 0)) err <- c(err, paste(nm, "share")) }
    if (!is.null(b$gap)) { err <- c(err, keys(b$gap, c("senate_old_minus_new", "pres2024_old_minus_new", "pres2024_counted_old_minus_new"), "redistricting gap"))
      if (!all(sapply(b$gap, numn))) err <- c(err, "redistricting gap numbers") }
  }
  if (!is.null(b <- s$trailing_needs)) {
    err <- c(err, keys(b, names(tn_null()), "trailing_needs"))
    for (k in setdiff(names(tn_null()), c("available", "leader", "trailer", "needed_possible"))) if (!numn(b[[k]])) err <- c(err, paste("trailing_needs", k))
    if (!is.null(b$leader) && !b$leader %in% c("Davis", "Buckhout")) err <- c(err, "trailing_needs leader")
    if (!is.null(b$remaining_votes_mid) && !is.null(b$remaining_votes_lo90) && b$remaining_votes_lo90 > b$remaining_votes_mid) err <- c(err, "trailing_needs interval")
  }
  err
}
