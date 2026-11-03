# Builds everything in data/baseline from public precinct files and the voter-file aggregates.
# Run: Rscript R/build_baseline.R. Prints the recheck of the briefed figures and writes
# data/baseline/recheck.txt, blend_regression.csv, cells_2026.csv, precinct_xwalk.csv.
source(file.path({a <- grep("^--file=", commandArgs(FALSE), value = TRUE); dirname(sub("^--file=", "", a[1]))}, "config.R"))
root <- needle_root(); source(file.path(root, "R/lib_data.R"))
options(width = 150, datatable.print.nrows = 100)
BD <- file.path(root, "data/baseline")
sink(file.path(BD, "recheck.txt"), split = TRUE)

ids <- fread(file.path(BD, "county_ids.csv")); CTY <- ids$county
d26 <- read_results(file.path(RAW, "results_pct_20261103.zip"))
geo <- unique(d26[startsWith(contest, CFG$contest_prefix), .(county, unit, real)])
SPLIT <- c("GRANVILLE", "ONSLOW")
fwrite(geo, file.path(BD, "geo_2026.csv"))
cat(sprintf("2026 units %d, real %d, counties %d\n", nrow(geo), geo[real == "Y", .N], uniqueN(geo$county)))
print(geo[county %in% SPLIT, .N, by = county])
# Real precinct totals in the split counties, from the county-wide contest rows of the 2026 file
print(d26[county %in% SPLIT & real == "Y" & grepl("^US SENATE", contest), .(all_real = uniqueN(unit)), by = county])

in_geo <- function(d) d[county %in% CTY & (!county %in% SPLIT | unit %in% geo[county %in% SPLIT, unit])]
load_y <- function(y) in_geo(read_results(file.path(RAW, sprintf("results_pct_%s.zip", y))))
d24 <- load_y("20241105"); d22 <- load_y("20221108"); d20 <- load_y("20201103")
saveRDS(list(d24 = d24, d22 = d22, d20 = d20, geo = geo), file.path(RAW, "past_results_geo.rds"))

topline <- function(d, ct) {
  x <- contest_cells(d[contest == ct])
  x[, .(D = sum(D), R = sum(R), T = sum(T))][, .(contest = ct, T, margin = round(100 * (D - R) / T, 2))]
}
cat("\nMargins on 2026 NC-1 lines\n")
print(rbindlist(list(topline(d24, "US PRESIDENT"), topline(d24, "NC GOVERNOR"), topline(d24, "NC ATTORNEY GENERAL"))))
cat("(2022 and 2020)\n"); print(rbindlist(list(topline(d22, "US SENATE"), topline(d20, "US PRESIDENT"), topline(d20, "NC GOVERNOR"))))
x <- contest_cells(d24[contest == "US PRESIDENT"])
cat("\n2024 President votes by method on lines, whole counties flag\n")
print(x[, .(T = sum(T)), by = method][, share := round(100 * T / sum(T), 1)][])
w <- x[!county %in% SPLIT]
cat(sprintf("Whole counties: early share %.1f%% of votes; share of early in pseudo-precincts %.1f%%\n",
    100 * w[method == "Early Voting", sum(T)] / w[, sum(T)],
    100 * w[method == "Early Voting" & real == "N", sum(T)] / w[method == "Early Voting", sum(T)]))
cat("Split-county pseudo-precinct votes in 2024 President rows:", x[county %in% SPLIT & real == "N", sum(T)], "\n")
cat("Provisional 2024 votes on lines:", x[method == "Provisional", sum(T)], "\n")
for (nm in c("d22", "d20")) {
  ct <- if (nm == "d22") "US SENATE" else "US PRESIDENT"
  xx <- contest_cells(get(nm)[contest == ct])
  cat(nm, ct, "\n"); print(xx[, .(T = sum(T)), by = method][, share := round(100 * T / sum(T), 1)][])
}
ms <- merge(geo[real == "Y"], unique(d24[real == "Y", .(county, unit)])[, m := 1], by = c("county", "unit"), all.x = TRUE)
cat(sprintf("Real 2026 precincts matched to 2024 by exact name: %d of %d\n", ms[m == 1, .N], nrow(ms)))
print(ms[is.na(m), .(county, unit)])

# ---- Blend weights ---------------------------------------------------------------------------------
wide <- function(d, ct) contest_cells(d[contest == ct])[T > 0, .(county, unit, method, m = 100 * (D - R) / T, T)]
fit_w <- function(tgt, a, b, lab) {
  z <- merge(merge(tgt, a, by = c("county", "unit", "method"), suffixes = c("", ".a")), b, by = c("county", "unit", "method"), suffixes = c("", ".b"))
  z <- z[T >= 30]
  z[, `:=`(y = m - m.b, x = m.a - m.b)]
  f <- lm(y ~ x, z, weights = T)
  sf <- summary(f)$coef
  # Same regression within county x method, because the model absorbs those levels through swing terms
  z[, `:=`(yd = y - weighted.mean(y, T), xd = x - weighted.mean(x, T)), by = .(county, method)]
  f2 <- lm(yd ~ xd - 1, z, weights = T); s2 <- summary(f2)$coef
  rmse <- function(w) z[, sqrt(weighted.mean((m - (w * m.a + (1 - w) * m.b) - weighted.mean(m - (w * m.a + (1 - w) * m.b), T))^2, T))]
  data.table(target = lab, n = nrow(z), w_a = sf["x", 1], se = sf["x", 2], p = sf["x", 4], w_a_within = s2["xd", 1], se_within = s2["xd", 2],
             p_within = s2["xd", 4], rmse_w0 = rmse(0), rmse_w5 = rmse(.5), rmse_w1 = rmse(1))
}
davis24 <- wide(d24[county %in% CTY], "US HOUSE OF REPRESENTATIVES DISTRICT 01")
reg <- rbind(
  fit_w(davis24, wide(d24, "US PRESIDENT"), wide(d24, "NC GOVERNOR"), "Davis 2024 on old NC-1 overlap ~ w Harris + (1-w) Stein"),
  fit_w(wide(d22, "US SENATE"), wide(d20, "US PRESIDENT"), wide(d20, "NC GOVERNOR"), "Beasley 2022 ~ w Biden + (1-w) Cooper"))
print(reg); fwrite(reg, file.path(BD, "blend_regression.csv"))
ol <- contest_cells(d24[contest == "US HOUSE OF REPRESENTATIVES DISTRICT 01"])
cat(sprintf("Davis 2024 on the overlap of old NC-1 with new lines: %d votes, margin %.2f\n", ol[, sum(T)], ol[, 100 * sum(D - R) / sum(T)]))
pp <- contest_cells(d24[contest == "US PRESIDENT"])[paste(county, unit) %in% ol[T > 0, paste(county, unit)]]
cat(sprintf("Harris on the same units %.2f; House votes per President vote %.4f\n", pp[, 100 * sum(D - R) / sum(T)], ol[, sum(T)] / pp[, sum(T)]))
sink()

# ---- Split-county geography corrections, from the 2026 voter file's district labels ---------------------
agg26 <- fread(file.path(RAW, "agg_20261103.csv"))
gr <- fread(cmd = sprintf("awk -F'\\t' 'NR==1 || $1==\"GRANVILLE\"' %s", file.path(RAW, "ext_20261103.tsv")),
            sep = "\t", quote = "", na.strings = "", colClasses = "character")
gr <- gr[grepl("^ACCEPTED", ballot_rtn_status) & !is.na(precinct_desc)]
sal <- gr[precinct_desc == "SALEM", mean(cong_dist_desc == "CONGRESSIONAL DISTRICT 1")]
# unit codes for the names: ANTI, CRDL, EAOX, SALM, SOOX in NC-1. WOEL (West Oxford Elem.) appears in the NC-1 contest
# but none of its accepted ballots carry the district 1 label, so it counts as zero expected votes.
FACT <- data.table(county = "GRANVILLE", unit = c("SALM", "WOEL"), fact = c(sal, 0))
fwrite(FACT, file.path(BD, "split_factors.csv"))
cat(sprintf("\nGranville: SALEM share of accepted ballots in CD1 %.3f; WOEL share in CD1 0 of %d\n", sal, gr[precinct_desc == "WEST OXFORD ELEM.", .N]))
fac <- function(x) { x <- merge(x, FACT, by = c("county", "unit"), all.x = TRUE); x[is.na(fact), fact := 1]; x[, `:=`(D = D * fact, R = R * fact, T = T * fact, fact = NULL)]; x[] }
tl2 <- function(d, ct) fac(contest_cells(d[contest == ct]))[, .(contest = ct, T = sum(T), margin = 100 * sum(D - R) / sum(T))]
cat("Margins after the Granville correction\n"); print(rbind(tl2(d24, "US PRESIDENT"), tl2(d24, "NC GOVERNOR"), tl2(d24, "NC ATTORNEY GENERAL")))

# ---- Crosswalks ---------------------------------------------------------------------------------------
sx <- fread(file.path(BD, "site_xwalk.csv"))
agg24 <- fread(file.path(RAW, "agg_20241105.csv"))
pres24 <- fac(contest_cells(d24[contest == "US PRESIDENT"]))
ev_real <- pres24[method == "Early Voting" & real == "Y", .(ev = sum(T)), by = county][ev > 0, county]
pseudo_ev_26 <- geo[real == "N" & !grepl("ABSENTEE|PROVISIONAL|TRANSFER|SPARE", unit), unique(county)]
real_ev_26 <- setdiff(CTY, pseudo_ev_26)
cat("\nCounties reporting early vote in real precincts per 2026 template:", paste(real_ev_26, collapse = " "), "\n")
# Precinct name -> code by matching 2024 accepted ballots to 2024 early + mail votes, greedily on closest counts.
xw <- list()
for (cc in setdiff(intersect(real_ev_26, ev_real), SPLIT)) {
  v <- pres24[county == cc & method %in% c("Early Voting", "Absentee by Mail") & real == "Y", .(n = sum(T)), by = unit][unit %in% geo[county == cc, unit]]
  a <- agg24[county_desc == cc & !is.na(precinct_desc) & nzchar(precinct_desc), .(a = sum(N)), by = precinct_desc]
  g <- CJ(unit = v$unit, precinct_desc = a$precinct_desc)
  g <- merge(merge(g, v, by = "unit"), a, by = "precinct_desc"); g[, d := abs(n - a)]
  setorder(g, d); used_u <- character(); used_p <- character(); res <- list()
  for (k in seq_len(nrow(g))) if (!(g$unit[k] %in% used_u) && !(g$precinct_desc[k] %in% used_p)) {
    res[[length(res) + 1]] <- g[k]; used_u <- c(used_u, g$unit[k]); used_p <- c(used_p, g$precinct_desc[k]) }
  xw[[cc]] <- rbindlist(res)[, county := cc]
}
known <- data.table(county = c(rep("GRANVILLE", 6), "ONSLOW"), unit = c("ANTI", "CRDL", "EAOX", "SALM", "SOOX", "WOEL", "SW19"),
                    precinct_desc = c("ANTIOCH", "CREDLE", "EAST OXFORD", "SALEM", "SOUTH OXFORD", "WEST OXFORD ELEM.", "SW19"))
kv <- contest_cells(d24[contest == "US PRESIDENT" & county %in% SPLIT])[method %in% c("Early Voting", "Absentee by Mail"), .(n = sum(T)), by = .(county, unit)]
ka <- agg24[county_desc %in% SPLIT, .(a = sum(N)), by = .(county = county_desc, precinct_desc)]
known <- merge(merge(known, kv, by = c("county", "unit")), ka, by = c("county", "precinct_desc")); known[, d := abs(n - a)]
xw <- rbindlist(c(xw, list(known)), fill = TRUE)
xw[, rel := d / pmax(a, 1)]
cat(sprintf("Precinct name to code matches %d; ballots %.0f, votes %.0f; median |diff| %.1f%%, max %.1f%%\n", nrow(xw), xw[, sum(a)], xw[, sum(n)], 100 * median(xw$rel), 100 * max(xw$rel)))
print(xw[, .(n = .N, med_rel = round(100 * median(rel), 1), max_rel = round(100 * max(rel), 1)), by = county])
# 2026 names for Vance's merged precincts, which have no 2024 counts to match on. Same source as precinct_predecessors.csv.
xw <- rbind(xw, data.table(county = "VANCE", unit = c("CH", "SH", "TWN"), precinct_desc = c("CENTRAL HENDERSON", "SOUTH HENDERSON", "TOWNSVILLE")), fill = TRUE)
# Granville names are known directly; SW19 is its own name.
fwrite(xw[, .(county, unit, precinct_desc, votes_2024 = n, ballots_2024 = a)], file.path(BD, "precinct_xwalk.csv"))

# ---- Baseline cells --------------------------------------------------------------------------------------
w_h <- CFG$blend_harris
# Predecessors: 2024 units renamed or merged by 2026, from data/baseline/precinct_predecessors.csv. `weight` is the share of
# the old unit's votes each new unit takes, so one old unit can split among several and several can pool into one.
pred <- fread(file.path(BD, "precinct_predecessors.csv"), colClasses = list(character = c("unit_2026", "unit_2024")))
remap <- function(x) {
  z <- merge(x, pred[, .(county, unit = unit_2024, unit_2026, weight)], by = c("county", "unit"), all.x = TRUE, allow.cartesian = TRUE)
  z[!is.na(unit_2026), `:=`(unit = unit_2026, D = D * weight, R = R * weight, T = T * weight)]
  z[, .(D = sum(D), R = sum(R), T = sum(T)), by = .(county, unit, real, method)]
}
pres24_raw <- pres24; pres24 <- remap(pres24)
alias <- sx[nzchar(alias_2024), .(county, unit, alias = alias_2024)]
alias <- rbind(alias, data.table(county = "DARE", unit = "ABSENTEE", alias = "ABSENTEE BY MAIL"))
marg <- function(ct) {
  x <- remap(fac(contest_cells(d24[contest == ct])))
  cm <- x[, .(m_cm = 100 * sum(D - R) / sum(T), T_cm = sum(T)), by = .(county, method)]
  list(cell = x[T > 0, .(county, unit, method, m = 100 * (D - R) / T, T)], cm = cm)
}
H <- marg("US PRESIDENT"); S <- marg("NC GOVERNOR")
mk <- function(g, m) {  # per method cell list
  r <- CJ(i = seq_len(nrow(g)), method = m); r[, `:=`(county = g$county[i], unit = g$unit[i], real = g$real[i])]; r[, i := NULL]; r[]
}
is_abs <- grepl("ABSENTEE", geo$unit); is_prv <- geo$unit == "PROVISIONAL"; is_ev_u <- geo$real == "N" & !is_abs & !is_prv & !grepl("TRANSFER|SPARE", geo$unit)
cnt_abs <- unique(geo$county[is_abs]); cnt_prv <- unique(geo$county[is_prv])
cells <- rbind(
  mk(geo[real == "Y"], "Election Day"),
  mk(geo[real == "Y" & county %in% real_ev_26], "Early Voting"), mk(geo[is_ev_u], "Early Voting"),
  mk(geo[real == "Y" & !county %in% cnt_abs], "Absentee by Mail"), mk(geo[is_abs & !grepl("SUP", unit)], "Absentee by Mail"),
  mk(geo[real == "Y" & !county %in% cnt_prv], "Provisional"), mk(geo[is_prv], "Provisional"))
cells <- merge(cells, alias, by = c("county", "unit"), all.x = TRUE); cells[is.na(alias), alias := unit]
blend1 <- function(cells, Mx, nm) {
  z <- merge(cells, Mx$cell, by.x = c("county", "alias", "method"), by.y = c("county", "unit", "method"), all.x = TRUE)
  z <- merge(z, Mx$cm[, .(county, method, m_cm)], by = c("county", "method"), all.x = TRUE)
  z[is.na(T), `:=`(T = 0, m = 0)]
  z[is.na(m_cm), m_cm := 0]
  z[, val := (T * m + CFG$baseline_shrink_k * m_cm) / (T + CFG$baseline_shrink_k)]
  z[, .(county, unit, method, val, T)]
}
bh <- blend1(cells, H); bs <- blend1(cells, S)
cells <- merge(cells, bh[, .(county, unit, method, b_h = val, T_base = T)], by = c("county", "unit", "method"))
cells <- merge(cells, bs[, .(county, unit, method, b_s = val)], by = c("county", "unit", "method"))
cells[, b := w_h * b_h + (1 - w_h) * b_s]
# 2024 House race (Davis, Buckhout, Bailey), for the map's swing only; the model never sees it. DISTRICT 01 rows carry only
# in-district votes in split precincts, so the Granville factor does not apply. NA where the county was not in that race
# (old NC-3 had no Democrat) and where a precinct has under 10 votes in the contest (Granville WOEL has 1).
HOUSE24 <- "US HOUSE OF REPRESENTATIVES DISTRICT 01"
hx <- remap(contest_cells(d24[contest == HOUSE24]))
Hh <- list(cell = hx[T > 0, .(county, unit, method, m = 100 * (D - R) / T, T)],
           cm = hx[, .(m_cm = if (sum(T) > 0) 100 * sum(D - R) / sum(T) else NA_real_), by = .(county, method)])
bo <- blend1(cells, Hh)
cells <- merge(cells, bo[, .(county, unit, method, b_house24 = val, T_house24 = T)], by = c("county", "unit", "method"))
in_race <- hx[T > 0, unique(county)]
cells[!county %in% in_race, b_house24 := NA_real_]
small <- hx[county == "GRANVILLE", .(T = sum(T)), by = unit][T < 10, unit]
cells[county == "GRANVILLE" & unit %in% small, b_house24 := NA_real_]
cat(sprintf("\n2024 House race: %d counties with a baseline, %d without (%s); Granville precincts under 10 votes: %s\n",
            length(in_race), length(setdiff(CTY, in_race)), paste(setdiff(CTY, in_race), collapse = ", "), paste(small, collapse = ", ")))
# Orphan scan: 2024 real units with votes that nothing in 2026 inherits, and 2026 real units with no own Election Day base.
orph <- pres24[real == "Y" & T > 0, .(T = sum(T)), by = .(county, unit)][!paste(county, unit) %in% geo[, paste(county, unit)]]
zero <- cells[method == "Election Day" & T_base == 0, .(county, unit)]
cat("\nOrphaned 2024 real units after predecessors:", nrow(orph), "\n"); print(orph)
cat("2026 real precincts with zero own Election Day base:\n"); print(zero)
# ED base votes: own 2024 votes where the precinct matches, else equal share of the county's unmatched votes.
ed <- pres24[method == "Election Day" & real == "Y", .(county, unit, T)]
cells[method == "Election Day", edb := ed$T[match(paste(county, unit), paste(ed$county, ed$unit))]]
cty_ed <- ed[, .(tot = sum(T)), by = county]
for (cc in unique(cells[method == "Election Day" & is.na(edb), county])) {
  left <- cty_ed[county == cc, tot] - cells[county == cc & method == "Election Day", sum(edb, na.rm = TRUE)]
  n <- cells[county == cc & method == "Election Day" & is.na(edb), .N]
  cells[county == cc & method == "Election Day" & is.na(edb), edb := max(left, 0) / n]
}
# Provisional: 2024 count, on the county's 2026 provisional cells.
pv <- pres24[method == "Provisional", .(tot = sum(T)), by = county]
cells[, prvb := NA_real_]
own <- pres24[method == "Provisional" & T > 0, .(county, unit, T)]
cells[method == "Provisional", prvb := own$T[match(paste(county, alias), paste(own$county, own$unit))]]
cells[method == "Provisional" & is.na(prvb), prvb := 0]
for (cc in pv$county) {
  n <- cells[county == cc & method == "Provisional", sum(prvb)]; tt <- pv[county == cc, tot]
  if (n <= 0) cells[county == cc & method == "Provisional", prvb := tt / .N] else cells[county == cc & method == "Provisional", prvb := prvb * tt / n]
}
fwrite(cells[, .(county, unit, method, real, alias, b_h = round(b_h, 3), b_s = round(b_s, 3), b = round(b, 3), b_house24 = round(b_house24, 3), T_base, edb = round(edb, 2), prvb = round(prvb, 3))],
       file.path(BD, "cells_2026.csv"))
cat(sprintf("\nCells %d; ED base votes %.0f; provisional %.0f\n", nrow(cells), cells[, sum(edb, na.rm = TRUE)], cells[, sum(prvb, na.rm = TRUE)]))
print(cells[, .(cells = .N, own_base = sum(T_base > 0), b_mean = round(weighted.mean(b, pmax(T_base, 1)), 2)), by = method])

# ---- Rolloff: contest votes per accepted ballot on the lines, and House votes per top-of-ticket vote ---------
ro <- function(d, ct, ag) {
  x <- fac(contest_cells(d[contest == ct]))[, .(v = sum(T)), by = method]
  a <- ag[!county_desc %in% c("GRANVILLE", "ONSLOW"), .(a = sum(N)), by = m]
  xw <- fac(contest_cells(d[contest == ct & !county %in% SPLIT]))[, .(v = sum(T)), by = method]
  merge(xw, a, by.x = "method", by.y = "m")[, .(contest = ct, method, v, a, ratio = round(v / a, 4))]
}
agg22 <- fread(file.path(RAW, "agg_20221108.csv"))
agg22[m == "ONE-STOP", m := "Early Voting"]
rr <- rbind(ro(d24, "US PRESIDENT", agg24), ro(d22, "US SENATE", agg22)); print(rr)
hh <- function(d, top) {
  h <- contest_cells(d[contest == "US HOUSE OF REPRESENTATIVES DISTRICT 01"])[, .(h = sum(T)), by = .(county, unit, method)]
  t <- contest_cells(d[contest == top])[, .(t = sum(T)), by = .(county, unit, method)]
  merge(h[h > 0], t, by = c("county", "unit", "method"))[, .(house_per_top = round(sum(h) / sum(t), 4)), by = method]
}
print(hh(d24, "US PRESIDENT")); print(hh(d22, "US SENATE"))

# ---- County check: Harris minus Trump baseline, each method weighted by that county's 2024 votes -----------------
raw <- pres24_raw[, .(raw_T = sum(T), raw_margin = 100 * sum(D - R) / sum(T)), by = county]
mt <- pres24_raw[, .(T_m = sum(T)), by = .(county, method)]
wm <- cells[, .(bm = if (sum(T_base) > 0) weighted.mean(b_h, T_base) else mean(b_h)), by = .(county, method)]
ck <- merge(mt[T_m > 0], wm, by = c("county", "method"), all.x = TRUE)
na_m <- ck[is.na(bm), sum(T_m)]
ck <- ck[!is.na(bm), .(baseline = sum(T_m * bm) / sum(T_m)), by = county]
ck <- merge(raw, ck, by = "county"); ck[, diff := baseline - raw_margin]
ee <- fread(file.path(BD, "early_expected.csv"), colClasses = list(character = "unit"))
e0 <- merge(cells, ee[, .(county, unit, method, A)], by = c("county", "unit", "method"), all.x = TRUE)
e0[is.na(A), A := 0]; e0[is.na(edb), edb := 0]; e0[is.na(prvb), prvb := 0]
e0[, E0 := fcase(method == "Election Day", edb * CFG$ed_ratio_prior, method == "Early Voting", A * CFG$rolloff_ev,
                 method == "Absentee by Mail", A * CFG$rolloff_mail * (1 + CFG$late_mail_frac), default = prvb)]
lines <- c("", "County check: Harris minus Trump baseline by 2024 method totals, against raw 2024 (split counties on NC-1 precincts)",
  capture.output(print(ck[order(-abs(diff)), .(county, votes = round(raw_T), raw = round(raw_margin, 2), baseline = round(baseline, 2), diff = round(diff, 2))], nrows = 30)),
  sprintf("Votes in a county x method with no 2026 cell: %.0f", na_m),
  sprintf("District raw %.2f; baseline b_h by 2024 method totals %.2f; by own 2024 votes %.2f; by expected votes (E0) %.2f; blend b by E0 %.2f",
          pres24_raw[, 100 * sum(D - R) / sum(T)], ck[, sum(raw_T * baseline) / sum(raw_T)], cells[, weighted.mean(b_h, T_base)], e0[, weighted.mean(b_h, E0)], e0[, weighted.mean(b, E0)]),
  sprintf("Counties off by more than 1 point: %s", if (ck[abs(diff) > 1, .N]) paste(ck[abs(diff) > 1, county], collapse = ", ") else "none"))
# Same check for the 2024 House baseline, against raw DISTRICT 01 rows.
hr <- hx[, .(raw_T = sum(T), raw_margin = 100 * sum(D - R) / sum(T)), by = county][raw_T > 0]
hm <- hx[T > 0, .(T_m = sum(T)), by = .(county, method)]
hw <- cells[!is.na(b_house24), .(bm = if (sum(T_house24) > 0) weighted.mean(b_house24, T_house24) else mean(b_house24)), by = .(county, method)]
hc <- merge(hm, hw, by = c("county", "method"), all.x = TRUE)[!is.na(bm), .(baseline = sum(T_m * bm) / sum(T_m)), by = county]
hc <- merge(hr, hc, by = "county", all = TRUE); hc[, diff := baseline - raw_margin]
lines <- c(lines, "", "County check: 2024 House baseline by 2024 method totals, against raw DISTRICT 01 rows",
  capture.output(print(hc[order(county), .(county, votes = round(raw_T), raw = round(raw_margin, 2), baseline = round(baseline, 2), diff = round(diff, 2))], nrows = 30)),
  sprintf("Counties with a House baseline: %d; without: %d; off by more than 1 point: %s", cells[!is.na(b_house24), uniqueN(county)], cells[, uniqueN(county)] - cells[!is.na(b_house24), uniqueN(county)],
          if (hc[abs(diff) > 1, .N]) paste(hc[abs(diff) > 1, county], collapse = ", ") else "none"))
cat(lines, sep = "\n"); cat(lines, sep = "\n", file = file.path(BD, "recheck.txt"), append = TRUE)
