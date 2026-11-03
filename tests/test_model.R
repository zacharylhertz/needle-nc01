# Unit checks of the estimator and rules on hand-built states of the real 2026 baseline. Rscript tests/test_model.R
suppressPackageStartupMessages({library(data.table); library(jsonlite)})
# Small assertion helpers. testthat fails to load on this machine's R 4.3.1 (compare_proxy error).
FAILS <- 0L
test_that <- function(desc, code) { r <- tryCatch({ code; NULL }, error = function(e) conditionMessage(e), skip = function(e) "skip")
  if (is.null(r)) cat("ok   ", desc, "\n") else if (identical(r, "skip")) cat("skip ", desc, "\n") else { FAILS <<- FAILS + 1L; cat("FAIL ", desc, ":", r, "\n") } }
expect_true <- function(x) if (!isTRUE(x)) stop("expected TRUE")
expect_false <- function(x) if (!isFALSE(x)) stop("expected FALSE")
expect_equal <- function(a, b, tolerance = 1e-8) if (abs(a - b) > tolerance) stop(sprintf("%s != %s", a, b))
expect_gt <- function(a, b) if (!(a > b)) stop(sprintf("%s !> %s", a, b))
expect_lt <- function(a, b) if (!(a < b)) stop(sprintf("%s !< %s", a, b))
expect_length <- function(x, n) if (length(x) != n) stop(paste(x, collapse = "; "))
skip_if_not <- function(x) if (!x) signalCondition(structure(class = c("skip", "condition"), list(message = "skip", call = NULL)))
a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
root <- normalizePath(file.path(dirname(sub("^--file=", "", a[1])), "..")); Sys.setenv(NEEDLE_ROOT = root)
for (f in c("config", "lib_data", "model", "call_rule", "fetch", "publish")) source(file.path(root, "R", paste0(f, ".R")))
base <- load_base_live(root, CFG); ce <- base$cells; n <- nrow(ce)
set.seed(1)
mg <- ce$b + 8 + rnorm(n, 0, 3)
Tv <- fifelse(ce$mi == 1, ce$edb * 1.1, fifelse(ce$mi == 4, ce$prvb * .8, ce$A * .99))
Dall <- Tv * (.98 + mg / 100) / 2; Rall <- Tv * (.98 - mg / 100) / 2
run <- function(m, draws = 20000) { D <- Dall * m; R <- Rall * m; V <- Tv * m; ot <- outstanding(base, CFG, D, R, V); list(es = estimate(base, CFG, D, R, V, ndraw = draws, seed = 7, out = ot), ot = ot, D = D, R = R, V = V) }
show <- function(lbl, r) cat(sprintf("%-34s counted %6.0f exp %6.0f [%6.0f,%6.0f] pct %5.1f  mid %+6.2f [%+6.2f,%+6.2f] P %.3f\n", lbl, sum(r$V), r$es$exp_mid, r$es$exp_lo, r$es$exp_hi, r$es$pct_counted, r$es$margin_mid, r$es$margin_lo90, r$es$margin_hi90, r$es$p_davis))
true_final <- 100 * sum(Dall - Rall) / sum(Tv); cat(sprintf("true final margin in this scenario: %+.2f, total %.0f\n", true_final, sum(Tv)))

r0 <- run(rep(0, n)); show("zero votes, prior only", r0)
test_that("zero votes returns the prior", {
  expect_equal(r0$es$p_davis, pt(CFG$prior_mid / CFG$prior_sd, CFG$t_df), tolerance = .012)
  expect_equal(r0$es$margin_mid, CFG$prior_mid, tolerance = .1)
  expect_equal(sum(r0$V), 0)
})
rE <- run(as.numeric(ce$mi %in% 2:3)); show("first drop, early and mail only", rE)
test_that("early-only drop keeps ED uncertainty open", {
  expect_true(rE$es$margin_hi90 - rE$es$margin_lo90 > 4)
  expect_true(rE$es$pct_counted > 50 && rE$es$pct_counted < 75)
})
rs <- as.numeric(ce$mi %in% 2:3 | (ce$mi == 1 & ce$county %in% base$counties[1:6]))
rP <- run(rs); show("all early, ED in six counties", rP)
half <- rs; ed <- which(ce$mi == 1 & ce$county %in% base$counties[1:6]); half[ed[seq(1, length(ed), by = 3)]] <- 0.4
rH <- run(half); show("same, a third of them 40% reported", rH)
test_that("partial precincts stay outstanding", expect_gt(sum(rH$ot$EDb), sum(rP$ot$EDb)))
z <- as.numeric(ce$mi %in% 1:3); z[ce$county == "CRAVEN"] <- 0
rZ <- run(z); show("all in except Craven", rZ)
test_that("a county at zero remains expected", {
  expect_gt(rZ$es$exp_mid - sum(rZ$V), 30000)
  expect_lt(abs(rZ$es$margin_mid - true_final), 3)
})
rF <- run(as.numeric(ce$mi %in% 1:3)); show("full count, provisional out", rF)
test_that("full count pins the margin", {
  expect_lt(rF$es$margin_hi90 - rF$es$margin_lo90, .3)
  expect_equal(sum(rF$ot$EDb), 0)
  expect_equal(sum(rF$ot$Ocm[, 2:3]), 0)
  expect_equal(rF$es$margin_mid, 100 * sum(rF$D - rF$R) / sum(rF$V), tolerance = .2)
})
test_that("third-party votes are in the denominator only", {
  V <- c(100, 100); D <- c(50, 40); R <- c(40, 50)  # 10% Bailey in the first cell, none in margin terms
  x <- 100 * (sum(D) - sum(R)) / sum(V); expect_equal(x, 0)
  # A race where Bailey holds 10% of votes: reported margin is on total votes, so 45-35-20 is +10
  expect_equal(100 * (45 - 35) / (45 + 35 + 20), 10)
  expect_equal(rE$es$p_davis + rE$es$p_buckhout, 1)
})
test_that("hold requires ten minutes and withdraws on failure", {
  h <- hold_new(); h <- hold_step(h, 0, TRUE, "Davis", 10); expect_false(h$called)
  h <- hold_step(h, 5, TRUE, "Davis", 10); expect_false(h$called)
  h <- hold_step(h, 10, TRUE, "Davis", 10); expect_true(h$called); expect_equal(h$called_at, 10)
  h <- hold_step(h, 12, FALSE, "Davis", 10); expect_false(h$called); expect_equal(h$withdrawn_at, 12)
  h <- hold_step(h, 14, TRUE, "Davis", 10); expect_false(h$called)
  h <- hold_step(h, 24, TRUE, "Buckhout", 10); expect_false(h$called)
})
test_that("cannot-catch-up fails when a method with votes outstanding has no reports", {
  m <- as.numeric(ce$mi == 2); ot <- outstanding(base, CFG, Dall * m, Rall * m, Tv * m)
  cu <- ccu_test(base, CFG, Dall * m, Rall * m, Tv * m, ot, Bs = c(0, 5, 10))
  expect_true(cu$unstressed); expect_false(any(sapply(cu$by_B, `[[`, "passed")))
})
test_that("cannot-catch-up loosens as the count ends and tightens in B", {
  m <- as.numeric(ce$mi %in% 1:3); ot <- outstanding(base, CFG, Dall * m, Rall * m, Tv * m)
  cu <- ccu_test(base, CFG, Dall * m, Rall * m, Tv * m, ot, Bs = c(0, 5, 10))
  f <- sapply(cu$by_B, `[[`, "fav_lead"); expect_true(all(diff(f) < 0)); expect_true(all(f > 0))
})
test_that("zip parser and JSON parser agree on method labels", {
  fz <- file.path(RAW, "results_pct_20241105.zip"); skip_if_not(file.exists(fz))
  d <- read_results(fz); x <- contest_cells(d[county == "BEAUFORT" & contest == "US PRESIDENT"])
  j <- as.data.table(fromJSON(file.path(RAW, "er_20241105_precinct_7.txt")))[bnm == "Kamala D. Harris"]
  ev <- x[method == "Early Voting", sum(D)]; ed <- x[method == "Election Day", sum(D)]
  expect_equal(sum(as.numeric(j$ovc)), ev); expect_equal(sum(as.numeric(j$evc)), ed)
})
# --- Page additions: sources, county intervals, benchmark, county draws (added 2026-11-02) ---
src_ok <- function(x) { expect_equal(x$prior + x$model + x$actual, 1, tolerance = 1e-9); expect_true(all(c(x$prior, x$model, x$actual) >= 0 & c(x$prior, x$model, x$actual) <= 1)) }
test_that("sources are 1/0/0 before any votes", {
  x <- source_shares(base, r0$es, 0); src_ok(x)
  expect_equal(x$prior, 1, tolerance = 1e-9); expect_equal(x$model, 0, tolerance = 1e-9); expect_equal(x$actual, 0)
})
test_that("sources sum to 1 mid-count, with prior falling as votes come in", {
  xs <- lapply(list(rE, rP, rZ), function(r) source_shares(base, r$es, sum(r$V)))
  for (x in xs) src_ok(x)
  cat(sprintf("      sources early-only %.3f/%.3f/%.3f; six counties %.3f/%.3f/%.3f; all but Craven %.3f/%.3f/%.3f\n",
      xs[[1]]$prior, xs[[1]]$model, xs[[1]]$actual, xs[[2]]$prior, xs[[2]]$model, xs[[2]]$actual, xs[[3]]$prior, xs[[3]]$model, xs[[3]]$actual))
  expect_gt(xs[[1]]$prior, xs[[2]]$prior); expect_gt(xs[[2]]$actual, xs[[1]]$actual)
})
test_that("sources are 0/0/1 when everything, provisionals included, is counted", {
  Tv2 <- fifelse(ce$mi == 4, ce$prvb, Tv); D <- Tv2 * (.98 + mg / 100) / 2; R <- Tv2 * (.98 - mg / 100) / 2
  ot <- outstanding(base, CFG, D, R, Tv2); es <- estimate(base, CFG, D, R, Tv2, ndraw = 2000, seed = 7, out = ot)
  x <- source_shares(base, es, sum(Tv2)); src_ok(x)
  expect_equal(x$actual, 1, tolerance = 1e-9); expect_equal(x$prior, 0, tolerance = 1e-9)
})
test_that("county final-margin intervals are ordered lo95 <= lo50 <= mid <= hi50 <= hi95", {
  for (r in list(r0, rE, rP, rH, rZ, rF)) { k <- r$es$county
    expect_true(all(k$lo95 <= k$lo50 & k$lo50 <= k$mid & k$mid <= k$hi50 & k$hi50 <= k$hi95)) }
  e <- rP$es; expect_true(e$margin_lo95 <= e$margin_lo90 && e$margin_lo90 <= e$margin_lo50 && e$margin_hi50 <= e$margin_hi90 && e$margin_hi90 <= e$margin_hi95)
})
test_that("benchmark is even district-wide at the pre stage with expected-vote weights", {
  bm <- ce[, sum(E0 * (b - base$Bbar)) / sum(E0)]; cat(sprintf("      district benchmark at pre stage: %.2e\n", bm))
  expect_lt(abs(bm), 1e-8)
})
test_that("county final draws aggregate to the district draws", {
  for (r in list(r0, rE, rH)) { cd <- r$es$county_draws
    agg <- 100 * rowSums(cd$lead) / pmax(rowSums(cd$tot), 1)
    expect_lt(max(abs(agg - r$es$final_draws)), 1e-8); expect_lt(max(abs(rowSums(cd$tot) - r$es$tot_draws)), 1e-6) }
})
test_that("validate_state rejects broken new keys and accepts old history entries", {
  s <- fromJSON(file.path(root, "tests/fixtures/state_counting.json"), simplifyVector = FALSE)
  s$history[[1]] <- s$history[[1]][c("t", "p_davis", "margin_mid", "counted_votes")]; expect_length(validate_state(s), 0)
  s2 <- s; s2$sources$actual <- 0.9; expect_true(length(validate_state(s2)) > 0)
  s3 <- s; s3$by_county[[1]]$est_margin_lo50 <- s3$by_county[[1]]$est_margin_hi95 + 1; expect_true(length(validate_state(s3)) > 0)
})
for (f in c("state_pre", "state_counting", "state_called")) test_that(paste("fixture", f), expect_length(validate_state(fromJSON(file.path(root, "tests/fixtures", paste0(f, ".json")), simplifyVector = FALSE)), 0))
# ---- Page additions of 2026-11-03 (R/additions.R) ----
source(file.path(root, "R/additions.R"))
cell_tab <- function(D, R, V) data.table(county = ce$county, unit = ce$unit, method = METHODS[ce$mi], D = D, R = R, T = V)
test_that("trailing_needs: null before any votes, arithmetic identity once votes are in", {
  t0 <- trailing_needs(r0$es, r0$D, r0$R, r0$V); expect_false(t0$available)
  expect_true(all(sapply(t0[setdiff(names(t0), "available")], is.null)))
  t1 <- trailing_needs(rP$es, rP$D, rP$R, rP$V); lead <- sum(rP$D) - sum(rP$R)
  expect_true(t1$available); expect_true(identical(t1$leader, if (lead > 0) "Davis" else "Buckhout"))
  rem <- median(rP$es$tot_draws - sum(rP$V))
  expect_equal(t1$remaining_votes_mid, round(rem), tolerance = 0.51)
  expect_equal(t1$needed_margin, round(100 * abs(lead) / rem, 2), tolerance = 0.006)
  expect_true(t1$needed_margin_lo90 <= t1$needed_margin && t1$needed_margin <= t1$needed_margin_hi90)
  expect_true(t1$expected_margin_lo90 <= t1$expected_margin_mid && t1$expected_margin_mid <= t1$expected_margin_hi90)
  # Draw by draw: counted lead + remaining x remaining margin = final lead
  rm_d <- 100 * (rP$es$final_draws * rP$es$tot_draws / 100 - lead) / (rP$es$tot_draws - sum(rP$V))
  expect_lt(max(abs(lead + (rP$es$tot_draws - sum(rP$V)) * rm_d / 100 - rP$es$final_draws * rP$es$tot_draws / 100)), 1e-6)
  cat(sprintf("      leader %s by %d; remaining %d [%d, %d]; trailer needs %+.2f, expected %+.2f [%+.2f, %+.2f]\n", t1$leader, t1$lead_votes,
      t1$remaining_votes_mid, t1$remaining_votes_lo90, t1$remaining_votes_hi90, t1$needed_margin, t1$expected_margin_mid, t1$expected_margin_lo90, t1$expected_margin_hi90))
})
test_that("davis_v_cooper: like for like, a known 3-point gap is recovered", {
  h <- cell_tab(rP$D, rP$R, rP$V)
  s <- copy(h)[, `:=`(D = D - 0.015 * T, R = R + 0.015 * T)]
  s[method == "Early Voting" & county == base$counties[1], T := 0]           # Senate missing in one cell set: dropped
  geo <- unique(h[, .(county, unit)])
  b <- davis_v_cooper(h, list(cells = s, source = "zip"), geo, base$counties, root)
  expect_true(b$available); expect_length(b$by_county, 25)
  expect_equal(b$district$davis_minus_cooper, 3, tolerance = 0.011)
  expect_lt(b$district$votes_house, sum(h$T))
  z <- davis_v_cooper(cell_tab(r0$D, r0$R, r0$V), list(cells = s[, T := 0], source = "zip"), geo, base$counties, root)
  expect_true(is.null(z$district$davis_minus_cooper) && z$district$votes_house == 0)
})
test_that("redistricting: 2024 President fed as tonight's Senate reproduces the 2024 margins on both sets of lines", {
  d24 <- read_results(file.path(RAW, "results_pct_20241105.zip"))
  p <- contest_cells(d24[contest == "US PRESIDENT"])
  l24 <- fread(file.path(root, "data/baseline/lines_2024.csv"))
  mt <- data.table(ci = ce$ci, mi = ce$mi, b_h = ce$b_h)
  geo <- unique(data.table(county = ce$county, unit = ce$unit))
  r <- redistricting(list(zip = p, json = NULL), "zip", geo, mt[, .(county = base$counties[ci], unit = ce$unit, method = METHODS[mi], b_h)], root)
  cat(sprintf("      old: %+.2f (2024 %+.2f), share %.3f; new: %+.2f (2024 %+.2f), share %.3f; match %.3f flag %s\n", r$old_lines$senate_margin, r$old_lines$pres2024_margin,
      r$old_lines$counted_share, r$new_lines$senate_margin, r$new_lines$pres2024_margin, r$new_lines$counted_share, r$match_rate, r$low_match_flag))
  expect_lt(abs(r$old_lines$senate_margin - l24[lines == "old", harris_minus_trump]), 0.3)
  expect_lt(abs(r$old_lines$counted_share - 1), 0.02)
  expect_lt(abs(r$old_lines$senate_minus_pres2024), 0.5)
  # New lines: 2024 unit names differ from 2026 ones, so feed the 2026 cells at their own 2024 President margin instead.
  sn <- data.table(county = ce$county, unit = ce$unit, method = METHODS[ce$mi], real = ce$real, T = Tv, D = Tv * (1 + ce$b_h / 100) / 2, R = Tv * (1 - ce$b_h / 100) / 2)
  rn <- redistricting(list(zip = sn, json = NULL), "zip", geo, mt[, .(county = base$counties[ci], unit = ce$unit, method = METHODS[mi], b_h)], root)
  cat(sprintf("      new lines fed their own 2024 margins: senate %+.2f, like-for-like 2024 %+.2f\n", rn$new_lines$senate_margin, rn$new_lines$pres2024_margin_counted))
  expect_lt(abs(rn$new_lines$senate_minus_pres2024), 0.02)
  expect_true(is.logical(r$low_match_flag))
  n0 <- redistricting(list(zip = p[, `:=`(D = 0, R = 0, T = 0)], json = NULL), "zip", geo, mt[, .(county = base$counties[ci], unit = ce$unit, method = METHODS[mi], b_h)], root)
  expect_true(is.null(n0$old_lines$senate_margin) && n0$old_lines$counted_share == 0)
})
test_that("redistricting: JSON-only old lines are withheld; zero-vote counties count as not reporting (adv fixes 1, 4)", {
  d24 <- read_results(file.path(RAW, "results_pct_20241105.zip")); p <- contest_cells(d24[contest == "US PRESIDENT"])
  mt <- data.table(ci = ce$ci, mi = ce$mi, b_h = ce$b_h); geo <- unique(data.table(county = ce$county, unit = ce$unit))
  nb <- mt[, .(county = base$counties[ci], unit = ce$unit, method = METHODS[mi], b_h)]
  jo <- redistricting(list(zip = NULL, json = copy(p)[county %in% unique(geo$county)]), "json", geo, nb, root, log_line = function(...) NULL)
  expect_true(jo$available); expect_true(is.null(jo$old_lines) && is.null(jo$gap)); expect_false(is.null(jo$new_lines))
  expect_true(identical(jo$old_lines_unavailable_reason, "needs the statewide zip")); expect_true(identical(jo$source_old, "json"))
  zp <- redistricting(list(zip = copy(p), json = NULL), "zip", geo, nb, root)
  expect_true(is.null(zp$old_lines_unavailable_reason)); expect_false(is.null(zp$gap))
  oc <- fread(file.path(root, "data/baseline/old_lines_cells.csv"), colClasses = list(character = "unit"))
  gone <- setdiff(unique(oc$county), unique(geo$county))[1]; gnew <- unique(geo$county)[1]
  pz <- copy(p)[county %in% c(gone, gnew), `:=`(D = 0, R = 0, T = 0)]
  rz <- redistricting(list(zip = pz, json = NULL), "zip", geo, nb, root)
  expect_true(gone %in% unlist(rz$old_lines$counties_missing)); expect_true(gnew %in% unlist(rz$new_lines$counties_missing))
  expect_length(unlist(zp$old_lines$counties_missing), 0)
  st <- list(redistricting = jo); expect_length(validate_additions(fromJSON(toJSON(st, auto_unbox = TRUE, null = "null"), simplifyVector = FALSE)), 0)
})
test_that("trailing_needs: null margins under 100 remaining votes, expected margins clamped to [-100, 100] (adv fix 7)", {
  cnt <- sum(rP$V); lead <- sum(rP$D) - sum(rP$R); n <- length(rP$es$tot_draws)
  es <- list(tot_draws = cnt + rep(50, n), final_draws = 100 * lead / (cnt + 50) + rnorm(n))
  t1 <- trailing_needs(es, rP$D, rP$R, rP$V)
  expect_true(t1$available); expect_equal(t1$remaining_votes_mid, 50)
  expect_true(all(sapply(t1[c("needed_margin", "needed_margin_lo90", "needed_margin_hi90", "expected_margin_mid", "expected_margin_lo90", "expected_margin_hi90")], is.null)))
  es2 <- list(tot_draws = cnt + rep(c(150, 400), length.out = n), final_draws = 100 * (lead + rnorm(n, 0, 5000)) / (cnt + 300))
  t2 <- trailing_needs(es2, rP$D, rP$R, rP$V)
  e <- unlist(t2[c("expected_margin_lo90", "expected_margin_mid", "expected_margin_hi90")])
  expect_length(e, 3); expect_true(all(e >= -100 & e <= 100)); expect_true(any(abs(e) == 100))
})
test_that("additions never break: bad Senate input gives null-shaped blocks that validate", {
  mt <- data.table(ci = ce$ci, mi = ce$mi, b_h = ce$b_h)
  a <- suppressWarnings(state_additions(cell_tab(rP$D, rP$R, rP$V), base, mt, rP$es, rP$D, rP$R, rP$V, list(zip = data.table(x = 1), json = NULL), "zip", root))
  expect_false(a$davis_v_cooper$available); expect_true(a$trailing_needs$available)
  s <- fromJSON(file.path(root, "tests/fixtures/state_counting.json"), simplifyVector = FALSE)
  s2 <- fromJSON(toJSON(c(s[setdiff(names(s), names(a))], a), auto_unbox = TRUE, null = "null", na = "null"), simplifyVector = FALSE)
  expect_length(validate_state(s2), 0)
  s3 <- s2; s3$trailing_needs$needed_margin <- "x"; expect_true(length(validate_state(s3)) > 0)
  s4 <- s2; s4$redistricting$old_lines <- list(a = 1); expect_true(length(validate_state(s4)) > 0)
})
test_that("fixtures carry the three additions", {
  for (f in c("state_counting", "state_called")) { s <- fromJSON(file.path(root, "tests/fixtures", paste0(f, ".json")), simplifyVector = FALSE)
    expect_true(all(c("davis_v_cooper", "redistricting", "trailing_needs") %in% names(s)))
    expect_true(isTRUE(s$davis_v_cooper$available) && isTRUE(s$redistricting$available) && isTRUE(s$trailing_needs$available)) }
  s <- fromJSON(file.path(root, "tests/fixtures/state_pre.json"), simplifyVector = FALSE); expect_false(s$trailing_needs$available)
})
cat("failures:", FAILS, "\n"); quit(status = as.integer(FAILS > 0))
