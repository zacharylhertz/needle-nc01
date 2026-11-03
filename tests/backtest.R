# Replays past contests on the 2026 NC-1 lines through the real estimator and both calling rules.
# Rscript tests/backtest.R [orderings] [cores]. Writes tests/backtest_results/*.csv.
suppressPackageStartupMessages({library(data.table); library(parallel)})
a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
root <- normalizePath(file.path(dirname(sub("^--file=", "", a[1])), "..")); Sys.setenv(NEEDLE_ROOT = root)
source(file.path(root, "R/config.R")); source(file.path(root, "R/lib_data.R")); source(file.path(root, "R/model.R"))
source(file.path(root, "R/call_rule.R")); source(file.path(root, "tests/backtest_prep.R"))
args <- commandArgs(TRUE)
NORD <- if (length(args) >= 1) as.integer(args[1]) else 200L
CORES <- if (length(args) >= 2) as.integer(args[2]) else 8L
OUT <- file.path(root, "tests/backtest_results"); dir.create(OUT, showWarnings = FALSE)
past <- readRDS(file.path(RAW, "past_results_geo.rds"))
fac_tbl <- fread(file.path(root, "data/baseline/split_factors.csv"))[, .(county, unit, fact)]
A_cm <- fread(file.path(root, "data/baseline/accepted_past_by_county.csv"))
Bs <- c(0, 5, 10); Ps <- c(.99, .995, .999)
TAG <- Sys.getenv("BT_TAG", ""); if (nzchar(TAG)) OUT <- file.path(OUT, TAG); dir.create(OUT, showWarnings = FALSE)
if (nzchar(Sys.getenv("BT_D_PRIOR"))) CFG$d_prior_mean <- as.numeric(strsplit(Sys.getenv("BT_D_PRIOR"), ",")[[1]])
ONLY <- Sys.getenv("BT_ONLY", "")

specs <- list(
  list(id = "Senate 2022", ct = "US SENATE", yk = "d22", y = 2022),
  list(id = "Governor 2024", ct = "NC GOVERNOR", yk = "d24", y = 2024),
  list(id = "AG 2024", ct = "NC ATTORNEY GENERAL", yk = "d24", y = 2024),
  list(id = "President 2024", ct = "US PRESIDENT", yk = "d24", y = 2024))
contests <- lapply(specs, function(s) { o <- prep_contest(s$ct, s$yk, s$y, CFG, past, fac_tbl, A_cm); o$id <- s$id; o })
# Reverse-time stress: 2022 Senate against a 2024 Harris/Stein baseline by cell. It breaks the "earlier election only"
# rule on purpose, because it is the only presidential-to-midterm pair without COVID voting, which is tonight's situation.
rev_stress <- {
  o <- contests[[1]]; x <- copy(o$cells)
  h <- contest_cells(past$d24[contest == "US PRESIDENT"]); s <- contest_cells(past$d24[contest == "NC GOVERNOR"])
  mk <- function(z) { list(cell = z[T > 0, .(county, unit, method, m = 100 * (D - R) / T, T0 = T)], cm = z[, .(m_cm = 100 * sum(D - R) / max(sum(T), 1)), by = .(county, method)]) }
  bl <- function(x, Z) { z <- mk(Z); r <- merge(x[, .(county, unit, method)], z$cell, by = c("county", "unit", "method"), all.x = TRUE); r <- merge(r, z$cm, by = c("county", "method"), all.x = TRUE)
    r[is.na(T0), `:=`(T0 = 0, m = 0)]; r[, val := (T0 * m + CFG$baseline_shrink_k * m_cm) / (T0 + CFG$baseline_shrink_k)]; r[, .(county, unit, method, val)] }
  b1 <- bl(x, h); b2 <- bl(x, s)
  x[, b := NULL]
  x <- merge(x, b1, by = c("county", "unit", "method")); setnames(x, "val", "b1"); x <- merge(x, b2, by = c("county", "unit", "method")); setnames(x, "val", "b2")
  x[, b := CFG$blend_harris * b1 + (1 - CFG$blend_harris) * b2]; x[, c("b1", "b2") := NULL]
  ed <- h[method == "Election Day" & real == "Y", .(county, unit, T)]
  x[, edb := NULL]
  x[method == "Election Day", edb := ed$T[match(paste(county, unit), paste(ed$county, ed$unit))]]
  for (c0 in x[method == "Election Day" & is.na(edb), unique(county)]) {
    left <- ed[county == c0, sum(T)] - x[county == c0 & method == "Election Day", sum(edb, na.rm = TRUE)]
    x[county == c0 & method == "Election Day" & is.na(edb), edb := max(left, 0) / .N]
  }
  list(contest = "US SENATE", year = 2022, cells = x, id = "Senate 2022 on 2024 baseline (reverse-time stress)")
}
contests[[5]] <- rev_stress

shift_cells <- function(x, target) {
  x <- copy(x)
  if (is.na(target)) return(x)
  s <- target - x[, 100 * sum(D - R) / sum(T)]
  x[, `:=`(D = pmax(D + T * s / 200, 0), R = pmax(R - T * s / 200, 0))]
  # a floor can leave the total slightly off target; nudge again once
  s2 <- target - x[, 100 * sum(D - R) / sum(T)]
  x[, `:=`(D = pmax(D + T * s2 / 200, 0), R = pmax(R - T * s2 / 200, 0))]
  x[]
}

# Simulated reporting order, minutes after polls close.
order_times <- function(x, seed) {
  set.seed(seed); cty <- unique(x$county); tt <- rep(Inf, nrow(x))
  for (c0 in cty) {
    late <- runif(1) < .15                         # 15% of counties hold early votes back until ED counts
    ed_start <- runif(1, 30, 150); ed_dur <- runif(1, 20, 90)
    ev_t <- if (late) ed_start + runif(1, 0, ed_dur) else runif(1, 0, 30)
    i <- which(x$county == c0)
    for (j in i) tt[j] <- switch(x$method[j],
      "Election Day" = ed_start + runif(1, 0, ed_dur),
      "Early Voting" = ev_t + runif(1, 0, 10),
      "Absentee by Mail" = ev_t + runif(1, 0, 10),
      "Provisional" = Inf)
  }
  tt
}

replay <- function(x, base, cfg, run_seed) {
  # x and base$cells are row-aligned
  tt <- order_times(x, run_seed)
  fin <- x[, 100 * sum(D - R) / sum(T)]; winner <- if (fin > 0) "Davis" else "Buckhout"
  grid <- seq(10, ceiling(max(tt[is.finite(tt)]) / 10) * 10 + 10, by = 10)
  Tall <- sum(x$T)
  H_o <- lapply(Ps, function(p) hold_new()); H_c <- lapply(Bs, function(b) hold_new())
  rows <- vector("list", length(grid))
  ever_withdraw_o <- rep(0L, length(Ps)); ever_withdraw_c <- rep(0L, length(Bs))
  first_o <- rep(NA_real_, length(Ps)); lead_o <- rep(NA_character_, length(Ps))
  first_c <- rep(NA_real_, length(Bs)); lead_c <- rep(NA_character_, length(Bs))
  first_est_c <- rep(NA_real_, length(Bs)); wrong_o <- rep(FALSE, length(Ps)); wrong_c <- rep(FALSE, length(Bs))
  for (k in seq_along(grid)) {
    tk <- grid[k]; m <- tt <= tk
    D <- x$D * m; R <- x$R * m; V <- x$T * m
    ot <- outstanding(base, cfg, D, R, V)
    es <- estimate(base, cfg, D, R, V, ndraw = cfg$draws_backtest, seed = run_seed * 1000L + k, out = ot)
    lead <- sum(D) - sum(R); cnt <- sum(V); pct_true <- 100 * cnt / Tall
    ed_c <- x[, uniqueN(county[method == "Election Day" & m])]
    pv <- sum(ot$Ocm[, 4])
    for (i in seq_along(Ps)) {
      e <- orig_eval(es, cfg, cnt, lead, ed_c, pv, p_min = Ps[i])
      was <- H_o[[i]]$called
      H_o[[i]] <- hold_step(H_o[[i]], tk, e$pass, e$leader, cfg$orig$hold_minutes)
      if (was && !H_o[[i]]$called) ever_withdraw_o[i] <- ever_withdraw_o[i] + 1L
      if (!was && H_o[[i]]$called) {
        if (is.na(first_o[i])) { first_o[i] <- pct_true; lead_o[i] <- H_o[[i]]$called_leader }
        if (!identical(H_o[[i]]$called_leader, winner)) wrong_o[i] <- TRUE
      }
    }
    cu <- ccu_test(base, cfg, D, R, V, ot, Bs = Bs)
    for (i in seq_along(Bs)) {
      e <- ccu_eval(cu, cu$by_B[[i]])
      was <- H_c[[i]]$called
      H_c[[i]] <- hold_step(H_c[[i]], tk, e$pass, e$leader, cfg$ccu$hold_minutes)
      if (was && !H_c[[i]]$called) ever_withdraw_c[i] <- ever_withdraw_c[i] + 1L
      if (!was && H_c[[i]]$called) {
        if (is.na(first_c[i])) { first_c[i] <- pct_true; lead_c[i] <- H_c[[i]]$called_leader; first_est_c[i] <- es$pct_counted }
        if (!identical(H_c[[i]]$called_leader, winner)) wrong_c[i] <- TRUE
      }
    }
    rows[[k]] <- list(t = tk, pct_true = pct_true, pct_est = es$pct_counted, p = es$p_davis, mid = es$margin_mid, lo = es$margin_lo90,
                      hi = es$margin_hi90, exp_mid = es$exp_mid)
  }
  list(path = rbindlist(rows), fin = fin, winner = winner,
       rules = rbind(data.table(rule = "orig", par = Ps, first_pct = first_o, leader = lead_o, wrong = wrong_o, withdrawals = ever_withdraw_o),
                     data.table(rule = "ccu", par = Bs, first_pct = first_c, leader = lead_c, wrong = wrong_c, withdrawals = ever_withdraw_c)))
}

targets <- c(NA, -3, -1, 1, 3)
if (nzchar(ONLY)) contests <- contests[as.integer(strsplit(ONLY, ",")[[1]])]
jobs <- CJ(ci = seq_along(contests), ti = seq_along(targets), r = seq_len(NORD))
t0 <- Sys.time()
do_job <- function(k) {
  j <- jobs[k]; o <- contests[[j$ci]]
  x <- shift_cells(o$cells, targets[j$ti])
  base <- make_base(x[, .(county, unit, method, real, b, A, edb, prvb)], CFG)
  # make_base keeps row order of its input
  res <- replay(x, base, CFG, run_seed = 100000L * j$ci + 1000L * j$ti + j$r)
  res$path[, `:=`(contest = o$id, target = targets[j$ti], run = j$r, fin = res$fin)]
  res$rules[, `:=`(contest = o$id, target = targets[j$ti], run = j$r, fin = res$fin, winner = res$winner)]
  res[c("path", "rules")]
}
res <- mclapply(seq_len(nrow(jobs)), do_job, mc.cores = CORES, mc.preschedule = TRUE)
err <- sum(sapply(res, inherits, "try-error"))
cat("jobs", nrow(jobs), "errors", err, "minutes", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), "\n")
if (err) print(res[[which(sapply(res, inherits, "try-error"))[1]]])
ok <- res[!sapply(res, inherits, "try-error")]
path <- rbindlist(lapply(ok, `[[`, "path")); rules <- rbindlist(lapply(ok, `[[`, "rules"))
saveRDS(list(path = path, rules = rules), file.path(RAW, paste0("backtest_runs", TAG, ".rds")))
source(file.path(root, "tests/backtest_report.R"))
