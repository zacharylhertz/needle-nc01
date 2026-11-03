# Dress rehearsal: a fake timeline through the real loop (run_loop.sh --once -> R/step.R), no network, no push.
# Fake feed files are built from the 2022 Senate results on these lines relabelled as Davis/Buckhout/Bailey, on the 2026 template.
# Every step validates state.json against the schema. Rscript tests/rehearse.R
suppressPackageStartupMessages({library(data.table); library(jsonlite)})
a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
root <- normalizePath(file.path(dirname(sub("^--file=", "", a[1])), "..")); Sys.setenv(NEEDLE_ROOT = root)
for (f in c("config", "lib_data", "model", "call_rule", "fetch", "publish")) source(file.path(root, "R", paste0(f, ".R")))
realraw <- RAW
tmp <- tempfile("rehearse_"); dir.create(tmp); LD <- file.path(tmp, "live"); dir.create(LD); FF <- file.path(tmp, "feed"); dir.create(FF); RW <- file.path(tmp, "raw"); dir.create(RW)
file.copy(file.path(root, "data/live/early_electorate.json"), file.path(LD, "early_electorate.json"))
Sys.setenv(NEEDLE_LIVE_DIR = LD, NEEDLE_FAKE_FEED = FF, NEEDLE_RAW = RW)

d26 <- read_results(file.path(realraw, "results_pct_20261103.zip"))
tpl <- d26[startsWith(contest, CFG$contest_prefix)]
ids <- fread(file.path(root, "data/baseline/county_ids.csv"))
past <- readRDS(file.path(realraw, "past_results_geo.rds"))
base <- load_base_live(root, CFG); ce <- base$cells
set.seed(2026)
# Truth for every baseline cell: baseline + swing + noise, turnout from expectations. Davis ahead by about 6.
ce[, Tv := fifelse(mi == 1, edb * 1.1, fifelse(mi == 4, 0, A * .99))]
# County effects so the groups panel shows both sides of benchmark: counties new to NC-1 run 10 points toward Buckhout,
# old-NC-1 counties with more Black voters 4 toward Davis, other old counties 1 toward Davis.
cg <- fread(file.path(root, "data/baseline/county_groups.csv"))
ce[, ceff := cg[match(ce$county, cg$county), fifelse(area == "Counties new to NC-1", -10, fifelse(race == "More Black voters", 4, 1))]]
ce[, mg := pmin(pmax(b + 13 + fifelse(mi == 1, -4, 2) + ceff + rnorm(.N, 0, 4), -95), 95)]
ce[, `:=`(D = round(Tv * (.98 + mg / 100) / 2), R = round(Tv * (.98 - mg / 100) / 2), L = round(Tv * .02))]
# Senate (page additions): Cooper runs 3 points behind Davis with noise in NC-1 cells. Senate units outside the NC-1 baseline
# (the rest of Granville and Onslow; Greene, Lenoir, Wayne, Wilson on the old lines) take 2024 President votes and margin + 2.
tplS <- d26[contest == CFG$senate_contest & county %in% union(ids$county, fread(file.path(root, "data/baseline/old_lines_cells.csv"))$county)]
ce[, smg := pmin(pmax(mg - 3 + rnorm(.N, 0, 1.5), -95), 95)]
sv <- ce[, .(county, unit, method, Tv, smg, inbase = TRUE)]
p24 <- contest_cells(read_results(file.path(realraw, "results_pct_20241105.zip"))[contest == "US PRESIDENT"])
p24 <- p24[paste(county, unit) %in% tplS[, paste(county, unit)] & !paste(county, unit, method) %in% sv[, paste(county, unit, method)] & T > 0]
sv <- rbind(sv, p24[, .(county, unit, method, Tv = T, smg = pmin(pmax(100 * (D - R) / T + 2, -95), 95), inbase = FALSE)])
sv[, `:=`(C = round(Tv * (.98 + smg / 100) / 2), W = round(Tv * (.98 - smg / 100) / 2), SL = round(Tv * .012), SG = round(Tv * .008))]
wfeed <- function(frac_ed, ev_counties, extra_flip = 0, lm_zip, lm_json, write_zip = TRUE, write_json = TRUE, status = NULL) {
  v <- copy(ce)
  v[, on := fifelse(mi %in% 2:3, county %in% ev_counties, fifelse(mi == 1, frac_ed[match(paste(county, unit), paste(county, unit))] > 0, FALSE))]
  x <- tpl[, .(county, date, unit, gid, ctype, contest, choice, party, votefor, real)]
  m <- CJ(i = seq_len(nrow(x)), method = METHODS)
  x2 <- cbind(x[m$i], method = m$method)
  y <- merge(x2, v[on == TRUE, .(county, unit, method, D, R, L)], by = c("county", "unit", "method"), all.x = TRUE)
  y[, n := fifelse(party == "DEM", D, fifelse(party == "REP", R, L))]
  y[is.na(n), n := 0]
  if (extra_flip != 0) y[party == "DEM", n := pmax(n - extra_flip * n, 0)]
  # Senate rows: baseline cells report with their House cell; the rest with early (all 25 counties) or half of ED.
  s <- copy(sv)
  s[, on := fifelse(inbase, paste(county, unit, method) %in% v[on == TRUE, paste(county, unit, method)],
                    fifelse(method == "Election Day", mean(frac_ed > 0) >= .5, length(ev_counties) == 25 & method != "Provisional"))]
  xs <- tplS[, .(county, date, unit, gid, ctype, contest, choice, party, votefor, real)]
  ms <- CJ(i = seq_len(nrow(xs)), method = METHODS)
  ys <- merge(cbind(xs[ms$i], method = ms$method), s[on == TRUE, .(county, unit, method, C, W, SL, SG)], by = c("county", "unit", "method"), all.x = TRUE)
  ys[, n := fifelse(party == "DEM", C, fifelse(party == "REP", W, fifelse(party == "LIB", SL, SG)))]
  ys[is.na(n), n := 0]
  y <- rbind(y[, .(county, date, unit, gid, ctype, contest, choice, party, votefor, real, method, n)], ys[, .(county, date, unit, gid, ctype, contest, choice, party, votefor, real, method, n)])
  w <- dcast(y, county + date + unit + gid + ctype + contest + choice + party + votefor + real ~ method, value.var = "n")
  w[, Total := `Absentee by Mail` + `Early Voting` + `Election Day` + Provisional]
  out <- w[, .(County = county, `Election Date` = date, Precinct = unit, `Contest Group ID` = gid, `Contest Type` = ctype, `Contest Name` = contest, Choice = choice, `Choice Party` = party, `Vote For` = votefor,
               `Election Day` = `Election Day`, `Early Voting` = `Early Voting`, `Absentee by Mail` = `Absentee by Mail`, Provisional = Provisional, `Total Votes` = Total, `Real Precinct` = real)]
  if (write_zip) {
    txt <- file.path(FF, "results_pct_20261103.txt"); fwrite(out, txt, sep = "\t")
    owd <- setwd(FF); zip("results.zip.tmp.zip", "results_pct_20261103.txt", flags = "-q"); setwd(owd)
    file.rename(file.path(FF, "results.zip.tmp.zip"), file.path(FF, "results_pct_20261103.zip")); unlink(txt)
  }
  if (write_json) for (i in seq_len(nrow(ids))) {
    o <- out[County == ids$county[i]]
    j <- o[, .(lid = fifelse(`Contest Name` == CFG$senate_contest, CFG$senate_lid, CFG$contest_lid), aid = Precinct, anm = Precinct, sta = "Reported", bnm = Choice, vct = as.character(`Total Votes`), evc = as.character(`Election Day`),
               ovc = as.character(`Early Voting`), avc = as.character(`Absentee by Mail`), pvc = as.character(Provisional))]
    write_json(j, file.path(FF, sprintf("precinct_%d.txt", ids$id[i])))
  }
  meta <- list(last_modified_default = lm_json, last_modified = list(results_pct_20261103.zip = lm_zip))
  if (!is.null(status)) meta$status <- status
  write_json(meta, file.path(FF, "headers.json"), auto_unbox = TRUE)
}
ctys <- sort(unique(ce$county))
ed_mask <- function(share) { # share of real precincts in order of county, deterministic
  u <- ce[mi == 1, paste(county, unit)]
  r <- ce[mi == 1, .(k = paste(county, unit), cty = match(county, ctys), id = rowid(county))]
  setorder(r, cty, id)
  on <- r$k[seq_len(round(share * nrow(r)))]
  z <- as.numeric(ce[, paste(county, unit)] %in% on)
  z
}
run_step <- function(ts, expect = NULL, sign = NULL) {
  Sys.setenv(NEEDLE_NOW = ts)
  if (!is.null(sign)) { rc0 <- system2("Rscript", c(file.path(root, "R/sign_off.R"), sign), stdout = TRUE, stderr = TRUE); cat("  sign_off", sign, ":", tail(rc0, 2)[1], "\n") }
  rc <- system2(file.path(root, "run_loop.sh"), "--once")
  s <- fromJSON(file.path(LD, "state.json"), simplifyVector = FALSE)
  err <- validate_state(s)
  cc <- setNames(lapply(s$call$conditions, `[[`, "met"), sapply(s$call$conditions, `[[`, "id"))
  cat(sprintf("%s rc=%d status=%-8s src=%-4s counted=%6d pct=%5.1f exp=%6d P=%.3f mid=%+6.2f [%+.1f,%+.1f] call=%-9s ccu=%-5s fav=%-5s orig_hold=%-5s errs=%d\n", substr(ts, 12, 16), rc, s$status, s$feed$source, s$counted_votes, s$pct_counted_est, s$expected_total$mid, s$estimate$p_davis, s$estimate$margin_mid, s$estimate$margin_lo90, s$estimate$margin_hi90, s$call$status, cc$cannot_catch_up, cc$ccu_favourable_case, isTRUE(cc$orig_hold), length(err)))
  if (length(err)) stop("schema failure at ", ts, ": ", paste(err, collapse = "; "))
  if (!is.null(expect)) for (k in names(expect)) {
    got <- if (k == "status") s$status else if (k == "call") s$call$status else if (k == "source") s$feed$source else if (k == "rc") rc else stop(k)
    if (!identical(as.character(got), as.character(expect[[k]]))) stop(sprintf("at %s expected %s=%s, got %s", ts, k, expect[[k]], got))
  }
  invisible(s)
}
T <- function(h, m) sprintf("2026-11-03T%02d:%02d:00-05:00", h, m)
keep <- list()
all_e <- ctys; ev20 <- ctys[1:20]
# 1. Before polls close: zero-filled feed
wfeed(rep(0, nrow(ce)), character(), lm_zip = T(17, 0), lm_json = T(16, 50))
keep$pre <- run_step(T(18, 0), list(status = "pre", call = "none", source = "zip"))
# 2. Polls closed, nothing yet
run_step(T(19, 35), list(status = "pre"))
for (m in c(37, 39, 41, 43)) run_step(T(19, m), list(status = "pre"))   # more pre updates, so the history fixtures are long
# 3. First drop: early and mail only, 20 counties
wfeed(rep(0, nrow(ce)), ev20, lm_zip = T(19, 44), lm_json = T(19, 42))
run_step(T(19, 45), list(status = "counting", source = "zip"))
# Refusal: sign-off while the test is not met
r <- suppressWarnings(system2("Rscript", c(file.path(root, "R/sign_off.R"), "Davis"), stdout = TRUE, stderr = TRUE))
cat("  early sign-off refused:", any(grepl("refused", r)), "\n"); stopifnot(any(grepl("refused", r)), !file.exists(file.path(LD, "call_signoff.json")))
# 3b. JSON-only drops every two minutes (zip unchanged since 19:44): more early counties, then Election Day precincts.
# They make the history long enough for the page's band chart; the zip stays stale for step 4.
for (k in 1:6) { m <- 45 + 2 * k
  wfeed(ed_mask(.02 * (k - 1)), ctys[1:min(25, 20 + k)], lm_zip = T(19, 44), lm_json = T(19, m - 1), write_zip = FALSE)
  run_step(T(19, m), list(status = "counting")) }
# 4. Zip goes stale while the JSON moves: must switch source
wfeed(ed_mask(.12), all_e, lm_zip = T(19, 44), lm_json = T(19, 58), write_zip = FALSE)
run_step(T(20, 0), list(source = "json"))
for (k in 1:9) { m <- 2 * k   # JSON-only, ED rising from 13% to 23%, zip still stale
  wfeed(ed_mask(.12 + .0125 * k), all_e, lm_zip = T(19, 44), lm_json = T(20, m - 1), write_zip = FALSE)
  run_step(T(20, m), list(source = "json")) }
# 5. Zip catches up with partial Election Day
wfeed(ed_mask(.25), all_e, lm_zip = T(20, 19), lm_json = T(20, 10), write_json = FALSE)
keep$counting <- run_step(T(20, 20), list(source = "zip", status = "counting"))
# 6. HTTP 429: backoff, no state change, rc 3
wfeed(ed_mask(.25), all_e, lm_zip = T(20, 19), lm_json = T(20, 10), status = 429L, write_zip = FALSE, write_json = FALSE)
s6 <- run_step(T(20, 25), list(rc = 3))
wfeed(ed_mask(.25), all_e, lm_zip = T(20, 19), lm_json = T(20, 10), write_zip = FALSE, write_json = FALSE)
run_step(T(20, 35), list(rc = 3))                # still inside the 15-minute backoff
# 7. No change for 30 minutes after the backoff lifts: stale
run_step(T(20, 50), list(status = "stale"))
# 8. Counting resumes, most precincts in
wfeed(ed_mask(.80), all_e, lm_zip = T(21, 9), lm_json = T(21, 9))
run_step(T(21, 10), list(status = "counting"))
# 9. Test passes, first update
wfeed(ed_mask(.90), all_e, lm_zip = T(21, 19), lm_json = T(21, 19))
s9 <- run_step(T(21, 20), list(call = "none"))
# 11. Held 10 minutes: condition true, still no call without sign-off
wfeed(ed_mask(.92), all_e, lm_zip = T(21, 31), lm_json = T(21, 31))
s11 <- run_step(T(21, 32), list(call = "none"))
ids11 <- setNames(sapply(s11$call$conditions, `[[`, "met"), sapply(s11$call$conditions, `[[`, "id")); stopifnot(ids11[["cannot_catch_up"]], !ids11[["signoff"]])
# 12. Automatic call never happens: another update, still none
wfeed(ed_mask(.93), all_e, lm_zip = T(21, 37), lm_json = T(21, 37)); run_step(T(21, 38), list(call = "none"))
# Refusal: sign-off naming the trailing candidate
r <- suppressWarnings(system2("Rscript", c(file.path(root, "R/sign_off.R"), "Buckhout"), stdout = TRUE, stderr = TRUE))
cat("  wrong-candidate sign-off refused:", any(grepl("refused", r)), "\n"); stopifnot(any(grepl("refused", r)))
# 13. ZH signs off; the next update publishes
Sys.setenv(NEEDLE_NOW = T(21, 40))
r <- system2("Rscript", c(file.path(root, "R/sign_off.R"), "Davis"), stdout = TRUE, stderr = TRUE); cat(" ", tail(r, 2)[1], "\n")
wfeed(ed_mask(.95), all_e, lm_zip = T(21, 43), lm_json = T(21, 43))
keep$called <- run_step(T(21, 44), list(call = "Davis"))
# 14. Bad correction: Davis votes collapse, the test fails, so the call is withdrawn with a timestamp
wfeed(ed_mask(.96), all_e, extra_flip = .25, lm_zip = T(21, 49), lm_json = T(21, 49))
s14 <- run_step(T(21, 50), list(call = "withdrawn")); stopifnot(!is.null(s14$call$withdrawn_at))
cat("  withdrawn_at", s14$call$withdrawn_at, " called_at", s14$call$called_at, "\n")
# 15. Corrected back; old sign-off predates the withdrawal, so no call without a new one
wfeed(ed_mask(.97), all_e, lm_zip = T(21, 59), lm_json = T(21, 59)); run_step(T(22, 0), list(call = "withdrawn"))
wfeed(ed_mask(.98), all_e, lm_zip = T(22, 11), lm_json = T(22, 11)); run_step(T(22, 12), list(call = "withdrawn"))
# 16. New sign-off restores the call
Sys.setenv(NEEDLE_NOW = T(22, 14))
r <- system2("Rscript", c(file.path(root, "R/sign_off.R"), "Davis"), stdout = TRUE, stderr = TRUE); cat(" ", tail(r, 2)[1], "\n")
wfeed(ed_mask(1), all_e, lm_zip = T(22, 19), lm_json = T(22, 19))
s16 <- run_step(T(22, 20), list(call = "Davis", status = "final"))
# 17. Original automatic rule, logged only
lg <- fread(file.path(LD, "rule_log.csv")); print(lg[, .(t = substr(t, 12, 16), status, source, counted, p_davis, orig_pass, orig_called, ccu_fav, ccu_held, call)])
# 18. Repeated errors stop the loop: a corrupt zip and no JSON, in the loop proper
unlink(file.path(RW, "live"), recursive = TRUE); unlink(file.path(LD, "fetch_state.json"))
Sys.setenv(NEEDLE_NOW = T(22, 30))
writeLines("not a zip", file.path(FF, "results_pct_20261103.zip")); unlink(list.files(FF, "^precinct_", full.names = TRUE))
write_json(list(last_modified_default = T(22, 29)), file.path(FF, "headers.json"), auto_unbox = TRUE)
rc <- system2(file.path(root, "run_loop.sh"), c("--interval", "1", "--max-errors", "5"), stdout = TRUE, stderr = TRUE)
st <- attr(rc, "status"); cat("  loop exit on repeated errors:", st, "\n"); stopifnot(identical(as.integer(st), 1L))
cat(tail(readLines(file.path(LD, "loop.log")), 3), sep = "\n")
stopifnot(!dir.exists(file.path(LD, ".loop.lock")))
# Fixtures for the page. The seven counties outside the 2024 House race must be null; the rest non-null once votes are in.
NOHOUSE <- c("BEAUFORT", "CARTERET", "CRAVEN", "DARE", "HYDE", "ONSLOW", "PAMLICO")
for (k in c("counting", "called")) {
  sw <- setNames(lapply(keep[[k]]$by_county, `[[`, "swing_house"), sapply(keep[[k]]$by_county, `[[`, "county"))
  nn <- names(sw)[!sapply(sw, is.null)]
  cat(sprintf("  fixture %s: swing_house non-null in %d, null in %d\n", k, length(nn), 25 - length(nn)))
  stopifnot(setequal(setdiff(names(sw), nn), NOHOUSE))
}
for (k in names(keep)) { a <- keep[[k]]; dv <- a$davis_v_cooper$district; ol <- a$redistricting$old_lines; nl <- a$redistricting$new_lines; tn <- a$trailing_needs
  cat(sprintf("  fixture %s additions: Davis-Cooper %s on %s cells; Senate old %s (share %s) new %s (share %s); trailer needs %s, expected %s\n", k,
      format(dv$davis_minus_cooper %||% NA), format(dv$cells %||% NA), format(ol$senate_margin %||% NA), format(ol$counted_share %||% NA),
      format(nl$senate_margin %||% NA), format(nl$counted_share %||% NA), format(tn$needed_margin %||% NA), format(tn$expected_margin_mid %||% NA)))
  if (k != "pre") stopifnot(isTRUE(a$davis_v_cooper$available), isTRUE(a$redistricting$available), isTRUE(a$trailing_needs$available), abs(dv$davis_minus_cooper - 3) < 1)
  else stopifnot(!isTRUE(a$trailing_needs$available)) }
for (k in names(keep)) { s <- keep[[k]]; st <- sapply(s$groups, `[[`, "status"); vb <- unlist(lapply(s$by_county, `[[`, "vs_benchmark"))
  cat(sprintf("  fixture %s: history %d; sources %.3f/%.3f/%.3f; group status %s; counties ahead of benchmark Davis %d, Buckhout %d\n", k, length(s$history),
      s$sources$prior, s$sources$model, s$sources$actual, paste(names(table(st)), table(st), collapse = ", "), sum(vb > 0), sum(vb < 0))) }
fx <- file.path(root, "tests/fixtures")
fixed <- list(pre = "state_pre.json", counting = "state_counting.json", called = "state_called.json")
for (k in names(fixed)) write_atomic_json(keep[[k]], file.path(fx, fixed[[k]]))
for (k in names(fixed)) { e <- validate_state(fromJSON(file.path(fx, fixed[[k]]), simplifyVector = FALSE)); stopifnot(!length(e)) }
cat("Fixtures rewritten from rehearsal output, all valid. REHEARSAL PASSED\n")
unlink(tmp, recursive = TRUE)
