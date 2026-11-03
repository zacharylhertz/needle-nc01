# The estimator. Works on a prepared `base` (from make_base) and vote vectors aligned with base$cells.
# Margins are D minus R in points of total votes. Bailey's votes are in the denominator and nowhere else:
# Davis wins when D > R, and Bailey cannot reach plurality in any plausible draw.
suppressPackageStartupMessages(library(data.table))

# cells: county, unit, method, b (baseline margin), A (accepted ballots, early and mail cells),
# edb (ED votes in the base year, ED cells), prvb (expected provisional votes, provisional cells).
make_base <- function(cells, cfg, ed_ratio_prior = cfg$ed_ratio_prior,
                      rolloff = c(ev = cfg$rolloff_ev, mail = cfg$rolloff_mail),
                      prior_mid = cfg$prior_mid, prior_sd = cfg$prior_sd) {
  cells <- as.data.table(copy(cells))
  # Composition adjustment, off by default. Only the live 2026 table carries b_h, so backtest cells are never touched.
  if (isTRUE(cfg$early_comp_adjust) && "b_h" %in% names(cells)) cells <- comp_adjust(cells)
  counties <- sort(unique(cells$county))
  cells[, ci := match(county, counties)]
  cells[, mi := match(method, METHODS)]
  for (v in c("A", "edb", "prvb")) {
    set(cells, j = v, value = as.numeric(cells[[v]]))
    cells[is.na(get(v)), (v) := 0]
  }
  Am <- matrix(0, length(counties), 4)
  for (k in 2:3) {
    a <- cells[mi == k, .(s = sum(A)), by = ci]
    Am[a$ci, k] <- a$s * if (k == 2) rolloff[["ev"]] else rolloff[["mail"]] * (1 + cfg$late_mail_frac)
  }
  pv <- cells[mi == 4, .(s = sum(prvb)), by = ci]
  Am[pv$ci, 4] <- pv$s
  # prior expected votes by cell, used only to translate the prior on the final margin to a prior on swing
  cells[, E0 := fifelse(mi == 1, edb * ed_ratio_prior,
                 fifelse(mi == 2, A * rolloff[["ev"]],
                  fifelse(mi == 3, A * rolloff[["mail"]] * (1 + cfg$late_mail_frac), prvb)))]
  tot <- cells[, sum(E0)]
  b <- list(cells = cells, counties = counties, Em = Am, ed_ratio_prior = ed_ratio_prior, rolloff = rolloff)
  b$E0_tot <- tot
  b$Bbar <- cells[, sum(E0 * b) / sum(E0)]
  wm <- numeric(4); w1 <- cells[, sum(E0), by = mi]; wm[w1$mi] <- w1$V1 / tot
  wc <- numeric(length(counties)); w2 <- cells[, sum(E0), by = ci]; wc[w2$ci] <- w2$V1 / tot
  wcm2 <- cells[, sum(E0), by = .(ci, mi)][, sum((V1 / tot)^2)]
  # Method deviations are contrasts: their expected-vote-weighted mean is held near zero, so tau_d moves votes between
  # methods without widening the prior on the final margin. tau_out does widen it, so it is taken out of g's variance.
  extra <- sum(wc^2) * cfg$tau_c^2 + wcm2 * cfg$tau_cm^2 + sum(wm^2) * cfg$tau_out^2
  b$prior_g_var <- max(prior_sd^2 - extra, 1)
  b$mu0 <- prior_mid - b$Bbar
  # linear posterior set-up: g, d[1:4], a[1:nc]
  nc <- length(counties)
  b$P0 <- diag(c(1 / b$prior_g_var, rep(1 / cfg$tau_d^2, 4), rep(1 / cfg$tau_c^2, nc)))
  b$P0[2:5, 2:5] <- b$P0[2:5, 2:5] + tcrossprod(wm) / 0.5^2
  dm <- cfg$d_prior_mean; dm <- dm - sum(wm * dm)     # keep the prior mean on the final margin where it is
  b$m0 <- c(b$mu0, dm, rep(0, nc))
  b$nc <- nc
  b
}

# Early/mail baseline + adj from data/baseline/comp_cells.csv (R/build_comp.R: beta x (2026 - 2024 composition) of accepted
# ballots). Election Day and provisional cells are unchanged. The unadjusted baseline is kept in b_noadj. Because mu0 is set on
# the final margin (prior_mid - Bbar), this moves the expected early-versus-Election-Day split, not the prior on the final margin.
comp_adjust <- function(cells, path = file.path(needle_root(), "data/baseline/comp_cells.csv")) {
  if (!file.exists(path)) stop("early_comp_adjust is on but comp_cells.csv is missing; run R/build_comp.R")
  cc <- fread(path, colClasses = list(character = "unit"))[unit != "(county)" & !is.na(adj)]
  a <- fcoalesce(cc$adj[match(paste(cells$county, cells$unit, cells$method), paste(cc$county, cc$unit, cc$method))], 0)
  a[!cells$method %in% c("Early Voting", "Absentee by Mail")] <- 0
  cells[, b_noadj := b][, b := b + a][]
}

# Align a live cell table to base$cells; cells never seen before are appended with county x method baseline.
align_live <- function(live, base, cfg) {
  cells <- base$cells
  key <- paste(cells$county, cells$unit, cells$method, sep = "|")
  lk <- paste(live$county, live$unit, live$method, sep = "|")
  new <- which(!(lk %in% key) & live$T > 0)
  if (length(new)) {
    cm <- cells[, .(bb = if (sum(E0) > 0) sum(E0 * b) / sum(E0) else mean(b)), by = .(county, method)]
    add <- live[new, .(county, unit, method)]
    add <- merge(add, cm, by = c("county", "method"), all.x = TRUE)
    add[is.na(bb), bb := 0]
    add <- add[county %in% base$counties]
    add[, `:=`(b = bb, bb = NULL, A = 0, edb = 0, prvb = 0, E0 = 0,
               ci = match(county, base$counties), mi = match(method, METHODS), real = "N")]
    base$cells <- rbind(cells, add, fill = TRUE)
    key <- paste(base$cells$county, base$cells$unit, base$cells$method, sep = "|")
  }
  i <- match(key, lk)
  list(base = base, D = fcoalesce(live$D[i], 0), R = fcoalesce(live$R[i], 0), V = fcoalesce(live$T[i], 0),
       new_cells = length(new))
}

# Linear Gaussian posterior. X is sparse-ish: index columns for g, d, a. Returns mean and chol of precision.
.posterior <- function(base, ci, mi, y, v) {
  nc <- base$nc; p <- 5 + nc
  P <- base$P0; rhs <- as.numeric(base$P0 %*% base$m0)
  for (k in seq_along(y)) {
    j <- c(1L, 1L + mi[k], 5L + ci[k])
    P[j, j] <- P[j, j] + 1 / v[k]
    rhs[j] <- rhs[j] + y[k] / v[k]
  }
  L <- chol(P)
  list(m = backsolve(L, forwardsolve(t(L), rhs)), L = L)
}

# Election Day turnout-ratio posterior on the log scale: district + county effect.
.ed_ratio <- function(base, cfg, reported, V, edb, ci) {
  nc <- base$nc
  a <- data.table(ci = ci, V = V, edb = edb, r = reported)[r == TRUE & edb > 0,
        .(v = sum(V), e = sum(edb), n = .N), by = ci]
  P <- diag(c(1 / cfg$ed_ratio_sd^2, rep(1 / cfg$ed_ratio_county_sd^2, nc)))
  m0 <- c(log(base$ed_ratio_prior), rep(0, nc))
  rhs <- as.numeric(P %*% m0)
  if (nrow(a)) for (k in seq_len(nrow(a))) {
    j <- c(1L, 1L + a$ci[k]); w <- a$n[k] / cfg$ed_ratio_noise^2
    y <- log(max(a$v[k], 1) / a$e[k])
    P[j, j] <- P[j, j] + w; rhs[j] <- rhs[j] + w * y
  }
  L <- chol(P)
  m <- backsolve(L, forwardsolve(t(L), rhs))
  list(m = m, L = L, county_mid = exp(m[1] + m[-1]),
       county_sd = sapply(seq_len(nc), function(i) {
         x <- numeric(nc + 1); x[c(1, i + 1)] <- 1
         sqrt(sum(forwardsolve(t(L), x)^2))
       }))
}

# Outstanding-vote accounting, shared by the win probability and the cannot-catch-up test.
outstanding <- function(base, cfg, D, R, V) {
  ce <- base$cells; nc <- base$nc
  ci <- ce$ci; mi <- ce$mi; b <- ce$b
  isED <- mi == 1
  reported <- V > 0
  # Partial precincts: reported Election Day votes well under what the other reporting precincts imply. They are left
  # out of the turnout ratio and their remainder stays outstanding. The zip has no reporting flag, so a county where
  # every precinct is half in looks like low turnout until the rest arrive.
  rr <- which(isED & reported & ce$edb >= 20)
  med <- if (length(rr)) median(V[rr] / ce$edb[rr]) else base$ed_ratio_prior
  thr <- cfg$ed_partial_frac * max(med, .75 * base$ed_ratio_prior)
  partial <- isED & reported & ce$edb >= 20 & V < thr * ce$edb
  er <- .ed_ratio(base, cfg, reported & isED & !partial, V, ce$edb, ci)
  rmid <- er$county_mid[ci]
  ed_out <- numeric(length(V))
  ed_out[isED & !reported] <- ce$edb[isED & !reported]
  ed_out[partial] <- pmax(ce$edb[partial] - V[partial] / rmid[partial], 0)
  Vcm <- matrix(0, nc, 4); x <- data.table(ci, mi, V)[, .(s = sum(V)), by = .(ci, mi)]
  Vcm[cbind(x$ci, x$mi)] <- x$s
  Ocm <- pmax(base$Em - Vcm, 0)
  # A county x method within closed_frac of its expected count is closed.
  closed <- Vcm >= cfg$closed_frac * base$Em & Vcm > 0
  Ocm[closed] <- 0
  # Once every Election Day precinct in a county has reported, its early and mail units have too.
  ed_left <- tapply(ed_out[isED], factor(ci[isED], levels = seq_len(nc)), sum); ed_left[is.na(ed_left)] <- 0
  has_v <- Vcm[, 1] > 0
  Ocm[ed_left == 0 & has_v & Vcm[, 2] > 0, 2] <- 0
  Ocm[ed_left == 0 & has_v & Vcm[, 3] > 0, 3] <- 0
  # cell-level remainder, for the baseline of what is outstanding
  cell_out <- numeric(length(V))
  ev <- which(mi %in% 2:3)
  roll <- ifelse(mi[ev] == 2, base$rolloff[["ev"]], base$rolloff[["mail"]])
  cell_out[ev] <- pmax(ce$A[ev] * roll - V[ev], 0)
  # Out baseline by county x method
  dt <- data.table(ci, mi, b, ed_out, cell_out, prv_out = pmax(ce$prvb - V, 0))
  dt[, w := fifelse(mi == 1, ed_out, fifelse(mi == 4, prv_out, cell_out))]
  bb <- dt[, .(bb = if (sum(w) > 0) sum(w * b) / sum(w) else NA_real_, ws = sum(w)), by = .(ci, mi)]
  fb <- dt[, .(fb = mean(b)), by = .(ci, mi)]
  bb <- merge(bb, fb, by = c("ci", "mi")); bb[is.na(bb), bb := fb]
  BB <- matrix(0, nc, 4); BB[cbind(bb$ci, bb$mi)] <- bb$bb
  EDb <- numeric(nc); e <- dt[mi == 1, .(s = sum(ed_out)), by = ci]; EDb[e$ci] <- e$s
  list(er = er, EDb = EDb, Ocm = Ocm, BB = BB, Vcm = Vcm, ed_out = ed_out, cell_out = cell_out)
}

estimate <- function(base, cfg, D, R, V, ndraw = cfg$draws_live, seed = 1L, out = NULL) {
  set.seed(seed)
  ce <- base$cells; nc <- base$nc; ci <- ce$ci; mi <- ce$mi
  if (is.null(out)) out <- outstanding(base, cfg, D, R, V)
  counted <- sum(V); lead <- sum(D) - sum(R)
  # Swing observations collapsed to county x method with precision weights.
  r <- which(V > 0)
  if (length(r)) {
    w <- 1 / (cfg$tau_e^2 + cfg$sig2 / V[r])
    y <- 100 * (D[r] - R[r]) / V[r] - ce$b[r]
    o <- data.table(ci = ci[r], mi = mi[r], w = w, wy = w * y)[, .(w = sum(w), wy = sum(wy)), by = .(ci, mi)]
    post <- .posterior(base, o$ci, o$mi, o$wy / o$w, 1 / o$w + cfg$tau_cm^2)
  } else post <- .posterior(base, integer(), integer(), numeric(), numeric())
  p <- length(post$m)
  mix <- sqrt(cfg$t_df / rchisq(ndraw, cfg$t_df))
  th <- matrix(rnorm(ndraw * p), ndraw, p)
  th <- t(backsolve(post$L, t(th))) * mix
  th <- sweep(th, 2, post$m, "+")
  # Turnout draws.
  er <- out$er
  z <- matrix(rnorm(ndraw * (nc + 1)), ndraw, nc + 1)
  lr <- sweep(t(backsolve(er$L, t(z))), 2, er$m, "+")
  rc <- exp(lr[, 1] + lr[, -1, drop = FALSE])          # ndraw x nc
  ev_n <- exp(rnorm(ndraw, 0, cfg$ev_noise))
  pv_n <- exp(rnorm(ndraw, 0, cfg$prov_noise))
  Ocm <- out$Ocm; BB <- out$BB
  # Shared error of what is still out, by method: unreported places differ from reported places in the same direction
  # (the backtest's undercoverage). It fades only as the outstanding share falls.
  dout <- matrix(rnorm(ndraw * 4, 0, cfg$tau_out), ndraw, 4) * mix
  add_v <- numeric(ndraw); add_l <- numeric(ndraw)
  tot_by_m <- matrix(0, ndraw, 4)
  # County-level accumulators (reporting only; they use the same draws, so counties sum to the district).
  cv_c <- matrix(0, ndraw, nc); cl_c <- matrix(0, ndraw, nc)
  for (cc in seq_len(nc)) {
    for (m in 1:4) {
      n0 <- if (m == 1) out$EDb[cc] else Ocm[cc, m]
      if (n0 <= 0) next
      vol <- if (m == 1) n0 * rc[, cc] else if (m == 4) n0 * pv_n else n0 * ev_n
      e <- rnorm(ndraw, 0, cfg$tau_cm) * mix
      sw <- BB[cc, m] + th[, 1] + th[, 1 + m] + th[, 5 + cc] + e + dout[, m]
      sw <- pmin(pmax(sw, -100), 100)
      add_v <- add_v + vol; add_l <- add_l + vol * sw / 100
      tot_by_m[, m] <- tot_by_m[, m] + vol
      cv_c[, cc] <- cv_c[, cc] + vol; cl_c[, cc] <- cl_c[, cc] + vol * sw / 100
    }
  }
  tot <- counted + add_v
  final <- 100 * (lead + add_l) / pmax(tot, 1)
  q <- quantile(final, c(.05, .5, .95), names = FALSE)
  qt <- quantile(tot, c(.05, .5, .95), names = FALSE)
  q2 <- quantile(final, c(.025, .25, .75, .975), names = FALSE)
  # County final margins: counted plus projected outstanding, draw by draw.
  Vc <- tapply(V, factor(ci, levels = seq_len(nc)), sum); Vc[is.na(Vc)] <- 0
  Lc <- tapply(D - R, factor(ci, levels = seq_len(nc)), sum); Lc[is.na(Lc)] <- 0
  ctot <- sweep(cv_c, 2, Vc, "+"); clead <- sweep(cl_c, 2, Lc, "+")
  cfin <- 100 * clead / pmax(ctot, 1)
  cq <- apply(cfin, 2, quantile, probs = c(.025, .25, .5, .75, .975), names = FALSE)   # 5 x nc
  county <- data.table(ci = seq_len(nc), exp_mid = apply(ctot, 2, median), counted = as.numeric(Vc),
                       lo95 = cq[1, ], lo50 = cq[2, ], mid = cq[3, ], hi50 = cq[4, ], hi95 = cq[5, ])
  list(margin_mid = q[2], margin_lo90 = q[1], margin_hi90 = q[3],
       margin_lo95 = q2[1], margin_lo50 = q2[2], margin_hi50 = q2[3], margin_hi95 = q2[4],
       county = county, county_draws = list(tot = ctot, lead = clead), final_draws = final, tot_draws = tot,
       p_davis = mean(final > 0), p_buckhout = 1 - mean(final > 0),
       exp_mid = qt[2], exp_lo = qt[1], exp_hi = qt[3],
       pct_counted = if (qt[2] > 0) 100 * counted / qt[2] else 0,
       method_out_mid = apply(tot_by_m, 2, median), out = out, post = post)
}

# Where the final-margin estimate comes from, for the page's "sources" bar. Reporting only; nothing here feeds the model.
# actual = votes counted / median estimated final total. The rest (1 - actual) is split between prior and model by W, the
# shrinkage weight on the prior in the swing estimate of the outstanding cells: for county c and method m the projected swing
# is s = g + d_m + a_c, and W_cm = Var_post(s) / Var_prior(s), the weight a Gaussian posterior mean puts on the prior mean
# (1 with no votes, falling toward 0 as reported results pin s down). W is the average of W_cm weighted by each county x
# method's expected outstanding votes (Election Day: 2024 votes still out x the county turnout-ratio median; early, mail and
# provisional: the outstanding count). prior = (1 - actual) W, model = (1 - actual) (1 - W).
source_shares <- function(base, es, counted) {
  out <- es$out; nc <- base$nc
  vol <- cbind(out$EDb * out$er$county_mid, out$Ocm[, 2:4, drop = FALSE])    # nc x 4
  actual <- if (es$exp_mid > 0) min(counted / es$exp_mid, 1) else 0
  if (sum(vol) <= 0 || actual >= 1) return(list(prior = 0, model = 0, actual = 1, W = NA_real_))
  P0i <- chol2inv(chol(base$P0)); L <- es$post$L
  num <- 0
  for (cc in seq_len(nc)) for (m in 1:4) if (vol[cc, m] > 0) {
    x <- numeric(5 + nc); x[c(1, 1 + m, 5 + cc)] <- 1
    vpost <- sum(forwardsolve(t(L), x)^2); vpri <- drop(crossprod(x, P0i %*% x))
    num <- num + vol[cc, m] * min(vpost / vpri, 1)
  }
  W <- min(max(num / sum(vol), 0), 1)
  list(prior = (1 - actual) * W, model = (1 - actual) * (1 - W), actual = actual, W = W)
}

# Favourable-case numbers for the cannot-catch-up test ------------------------------------------
# For the trailing candidate: baseline + largest county swing to them observed tonight in the method + B.
ccu_test <- function(base, cfg, D, R, V, out, Bs = cfg$ccu$buffer_B) {
  ce <- base$cells; nc <- base$nc; ci <- ce$ci; mi <- ce$mi
  ld <- sum(D) - sum(R); cnt <- sum(V)
  pts <- if (cnt > 0) 100 * ld / cnt else 0
  res <- list(lead_pts = pts, leader = if (ld > 0) "Davis" else if (ld < 0) "Buckhout" else NA_character_)
  sgn <- if (ld >= 0) 1 else -1   # +1: leader is Davis, the trailing candidate gains by lowering the margin
  # Reported county x method swing against the reported-weighted baseline.
  r <- which(V > 0)
  sw <- NULL
  if (length(r)) sw <- data.table(ci = ci[r], mi = mi[r], V = V[r], L = D[r] - R[r], bv = ce$b[r] * V[r])[
    , .(swing = 100 * sum(L) / sum(V) - sum(bv) / sum(V)), by = .(ci, mi)]
  best <- rep(NA_real_, 4)
  if (!is.null(sw)) for (m in 1:4) {
    s <- sw[mi == m, swing]
    if (length(s)) best[m] <- max(-sgn * s)        # largest swing toward the trailing candidate
  }
  # High-end volumes. ED: upper 90% turnout bound by county. Early and mail: accepted minus reported, by cell,
  # capped at the county x method figure so a relabelled pseudo-precinct does not leave phantom ballots.
  rq95 <- exp(log(out$er$county_mid) + qnorm(.95) * out$er$county_sd)
  hi <- numeric(length(V))
  hi[mi == 1] <- out$ed_out[mi == 1] * rq95[ci[mi == 1]]
  cell_raw <- pmax(ce$A - V, 0)
  for (m in 2:3) {
    idx <- which(mi == m)
    ot <- data.table(i = idx, ci = ci[idx], o = cell_raw[idx], A = ce$A[idx], V = V[idx])
    ot[, cap := pmax(sum(A) * (1 + if (m == 3) cfg$late_mail_frac else 0) - sum(V), 0), by = ci]
    ot[, s := sum(o), by = ci]
    ot[, o2 := fifelse(s > cap & s > 0, o * cap / s, o)]
    hi[ot$i] <- ot$o2
  }
  idx <- which(mi == 4)
  hi[idx] <- pmax(ce$prvb[idx] - V[idx], 0) * exp(qnorm(.95) * cfg$prov_noise)
  # Which methods cannot be stress-tested. Provisional ballots are not counted on election night, so that
  # method can never have reports; it uses the largest swing seen in any other method instead. (Stated
  # deviation: the literal rule would fail in every run.)
  out_m <- tapply(hi, factor(mi, levels = 1:4), sum); out_m[is.na(out_m)] <- 0
  unstressed <- any(out_m[1:3] > 0 & is.na(best[1:3]))
  res$unstressed <- unstressed
  best4 <- if (is.na(best[4])) suppressWarnings(max(best[1:3], na.rm = TRUE)) else best[4]
  if (!is.finite(best4)) best4 <- 0
  best[4] <- best4
  by_B <- lapply(Bs, function(B) {
    fav <- rep(0, length(V))
    use <- hi > 0
    s <- best[mi[use]]; s[is.na(s)] <- 0
    # margin in the favourable case, from the leader's side: baseline-as-leader minus swing minus buffer
    f <- sgn * ce$b[use] - s - B
    f <- pmax(pmin(f, 100), -100)
    fav_lead <- abs(ld) + sum(hi[use] * f / 100)
    list(B = B, fav_lead = fav_lead, passed = !unstressed && fav_lead > 0 && cnt > 0,
         lead_ok = abs(pts) > cfg$ccu$lead_min_pts)
  })
  res$by_B <- by_B
  res$out_hi_total <- sum(hi)
  res
}
