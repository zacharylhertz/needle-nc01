# Past contests on the 2026 NC-1 lines as replayable objects. Baseline: 2020 President only (earlier election).
# Denominators: that year's accepted ballots by county x method; the split among units inside a county uses final votes.
prep_contest <- function(ct, yr_key, year, cfg, past, fac_tbl, A_cm, CTY) {
  d <- past[[yr_key]]; base20 <- past$d20
  fac <- function(x) { x <- merge(x, fac_tbl, by = c("county", "unit"), all.x = TRUE); x[is.na(fact), fact := 1]
    x[, `:=`(D = D * fact, R = R * fact, T = T * fact, fact = NULL)]; x[] }
  cc <- fac(contest_cells(d[contest == ct]))[T > 0]
  b20 <- fac(contest_cells(base20[contest == "US PRESIDENT"]))
  cm <- b20[, .(m_cm = 100 * sum(D - R) / max(sum(T), 1)), by = .(county, method)]
  own <- b20[T > 0, .(county, unit, method, m = 100 * (D - R) / T, T0 = T)]
  x <- merge(cc, own, by = c("county", "unit", "method"), all.x = TRUE)
  x <- merge(x, cm, by = c("county", "method"), all.x = TRUE)
  x[is.na(T0), `:=`(T0 = 0, m = 0)]
  k <- cfg$baseline_shrink_k
  x[, b := (T0 * m + k * m_cm) / (T0 + k)]
  # ED base
  ed <- b20[method == "Election Day" & real == "Y", .(county, unit, T)]
  x[method == "Election Day", edb := ed$T[match(paste(county, unit), paste(ed$county, ed$unit))]]
  for (c0 in x[method == "Election Day" & is.na(edb), unique(county)]) {
    left <- ed[county == c0, sum(T)] - x[county == c0 & method == "Election Day", sum(edb, na.rm = TRUE)]
    x[county == c0 & method == "Election Day" & is.na(edb), edb := max(left, 0) / .N]
  }
  # Accepted ballots: actual county x method figure, cell share from final votes. Provisional base: 2020 by county.
  yy <- year; a <- A_cm[year == yy]
  x <- merge(x, a[, .(county, method, A_c = A)], by = c("county", "method"), all.x = TRUE)
  x[method %in% c("Early Voting", "Absentee by Mail"), A := A_c * T / sum(T), by = .(county, method)]
  pv <- b20[method == "Provisional", .(p = sum(T)), by = county]
  x[method == "Provisional", prvb := pv$p[match(county, pv$county)] * T / sum(T), by = county]
  list(contest = ct, year = year, cells = x[, .(county, unit, real, method, D, R, T, b, A, edb, prvb)])
}
