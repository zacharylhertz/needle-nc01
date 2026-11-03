# Composition of accepted early and mail ballots by reporting cell, and the early/mail baseline adjustment it implies.
# Run: Rscript R/build_comp.R. Reads the voter-file extracts in ~/needle_nc01_raw (2022, 2024, 2026) and writes aggregates only:
#   data/baseline/comp_cells.csv       shares registered DEM, REP, UNA, Black and 65+ by 2026 early/mail cell and year, cells with
#                                      at least 10 ballots (smaller cells are blank and use their county x method row, unit "(county)");
#                                      plus `adj`, the points added to the cell's baseline when CFG$early_comp_adjust is TRUE.
#   data/baseline/comp_regression.csv  cell margin on composition, 2024 (used) and 2022 (backtest), effect, SE, p, R2.
# Adjustment: adj = beta x (2026 composition - 2024 composition), beta from 2024 early/mail cells: Harris minus Trump margin on
# the registered D minus R share, weighted by votes. Election Day cells are never adjusted (no composition is known for them).
source(file.path({a <- grep("^--file=", commandArgs(FALSE), value = TRUE); dirname(sub("^--file=", "", a[1]))}, "config.R"))
root <- needle_root(); source(file.path(root, "R/lib_data.R"))
BD <- file.path(root, "data/baseline")
CTY <- fread(file.path(BD, "county_ids.csv"))$county; SPLIT <- c("GRANVILLE", "ONSLOW")
EM <- c("Early Voting", "Absentee by Mail")
SPEC <- "dr"     # which fit sets adj: "dr" (D minus R share only) or "full" (D, R, Black, 65+)
K <- CFG$baseline_shrink_k

ee <- fread(file.path(BD, "early_expected.csv"), colClasses = list(character = "unit"))
cells <- fread(file.path(BD, "cells_2026.csv"), colClasses = list(character = c("unit", "alias")))[method %in% EM]
sx <- fread(file.path(BD, "site_xwalk.csv"), colClasses = list(character = "unit"))
pw <- fread(file.path(BD, "precinct_xwalk.csv"), colClasses = list(character = "unit"))
sal <- fread(file.path(BD, "split_factors.csv"))
srcmap <- ee[, .(src = if (any(src == "site")) "site" else if (any(src == "county")) "county" else "precinct",
                 one = unit[1]), by = .(county, method)]

# ---- Voter file -> cell composition, one year at a time. Voter rows stay in memory and are dropped after aggregation. ----
ctl <- paste(sprintf("$1==\"%s\"", CTY), collapse = " || ")
comp_year <- function(y) {
  e <- fread(cmd = sprintf("awk -F'\\t' 'NR==1 || %s' %s", ctl, shQuote(file.path(RAW, sprintf("ext_%s.tsv", y)))),
             sep = "\t", quote = "", na.strings = "", colClasses = "character")
  e <- e[grepl("^ACCEPTED", ballot_rtn_status)]
  e[, method := fifelse(grepl("^EARLY|ONE", ballot_req_type), "Early Voting", "Absentee by Mail")]
  if (y == "20261103") {      # 2026 lines by the file's own district label, as in R/refresh_early.R
    e <- e[cong_dist_desc == "CONGRESSIONAL DISTRICT 1" | (is.na(cong_dist_desc) & !county_desc %in% SPLIT)]
    e[, w := 1]
  } else {                    # earlier files: 2026 lines by precinct name in the split counties, Salem by its CD1 share
    e <- e[!county_desc %in% SPLIT | precinct_desc %in% c("ANTIOCH", "CREDLE", "EAST OXFORD", "SALEM", "SOUTH OXFORD", "SW19")]
    e[, w := fifelse(county_desc == "GRANVILLE" & precinct_desc == "SALEM", sal[unit == "SALM", fact], 1)]
  }
  e[, `:=`(county = county_desc, dem = voter_party_code == "DEM", rep = voter_party_code == "REP", una = voter_party_code == "UNA",
           blk = grepl("^BLACK", race), a65 = suppressWarnings(as.integer(age)) >= 65)]
  e <- merge(e, srcmap, by = c("county", "method"))
  e[, unit := fcase(src == "site", sx$unit[match(paste(county, site_name), paste(sx$county, sx$site_name))],
                    src == "precinct", pw$unit[match(paste(county, precinct_desc), paste(pw$county, pw$precinct_desc))],
                    default = one)]
  e[!paste(county, unit, method) %in% ee[, paste(county, unit, method)], unit := NA_character_]
  ag <- function(x, by) x[, .(n = sum(w), dem = 100 * sum(w * dem) / sum(w), rep = 100 * sum(w * rep) / sum(w), una = 100 * sum(w * una) / sum(w),
                             blk = 100 * sum(w * blk) / sum(w), a65 = 100 * sum(w * a65, na.rm = TRUE) / sum(w)), by = by]
  out <- rbind(ag(e[!is.na(unit)], c("county", "unit", "method")), ag(e, c("county", "method"))[, unit := "(county)"], use.names = TRUE)
  out[, year := as.integer(substr(y, 1, 4))]
  cat(sprintf("%s: %d accepted ballots on the lines, %.1f%% placed in a 2026 cell\n", y, round(sum(e$w)), 100 * e[!is.na(unit), sum(w)] / sum(e$w)))
  rm(e); gc(FALSE); out
}
comp <- rbindlist(lapply(c("20221108", "20241105", "20261103"), comp_year))
comp[, dr := dem - rep]
saveRDS(comp, file.path(RAW, "comp_all.rds"))   # includes cells under 10 ballots, so it stays outside Dropbox

# ---- Fit: early/mail cell margin on composition, same cells, weighted by votes ------------------------------
past <- readRDS(file.path(RAW, "past_results_geo.rds"))
cell_m <- function(d, ct) contest_cells(d[contest == ct])[method %in% EM & T > 0, .(county, alias = unit, method, m = 100 * (D - R) / T, T)]
fit_year <- function(yr, d, ct) {
  z <- merge(cells[, .(county, unit, alias, method)], cell_m(d, ct), by = c("county", "alias", "method"))
  z <- merge(z, comp[year == yr & unit != "(county)" & n >= 10], by = c("county", "unit", "method"))[T >= 30]
  hc <- function(f) { X <- model.matrix(f); w <- weights(f); r <- resid(f); B <- solve(crossprod(X * sqrt(w)))
    sqrt(diag(B %*% crossprod(X * (w * r)) %*% B) * nrow(X) / (nrow(X) - ncol(X))) }
  one <- function(f, spec, within) { s <- summary(f)$coef; s <- s[rownames(s) != "(Intercept)", , drop = FALSE]
    data.table(year = yr, contest = ct, spec = spec, within = within, term = rownames(s), est = s[, 1], se = s[, 2], p = s[, 4],
               se_hc1 = hc(f)[rownames(s)], n = nrow(z), r2 = summary(f)$r.squared) }
  zw <- copy(z)[, (c("m", "dem", "rep", "blk", "a65", "dr")) := lapply(.SD, function(v) v - weighted.mean(v, T)),
                by = .(county, method), .SDcols = c("m", "dem", "rep", "blk", "a65", "dr")]
  rbind(one(lm(m ~ dr, z, weights = T), "dr", FALSE), one(lm(m ~ dem + rep + blk + a65, z, weights = T), "full", FALSE),
        one(lm(m ~ dr - 1, zw, weights = T), "dr", TRUE), one(lm(m ~ dem + rep + blk + a65 - 1, zw, weights = T), "full", TRUE))
}
reg <- rbind(fit_year(2024L, past$d24, "US PRESIDENT"), fit_year(2022L, past$d22, "US SENATE"))
options(width = 160); print(reg[, lapply(.SD, function(v) if (is.numeric(v)) signif(v, 3) else v)])
fwrite(reg, file.path(BD, "comp_regression.csv"))

# ---- Adjustment on the 2026 cells ---------------------------------------------------------------------------
V <- c("dem", "rep", "blk", "a65", "dr")
delta_between <- function(y_to, y_from, beta) {
  wide <- function(yr) comp[year == yr, c("county", "unit", "method", "n", V), with = FALSE]
  a <- merge(wide(y_to), wide(y_from), by = c("county", "unit", "method"), suffixes = c("_to", "_fr"), all = TRUE)
  cm <- a[unit == "(county)"]; ce <- merge(ee[method %in% EM, .(county, unit, method)], a[unit != "(county)"], by = c("county", "unit", "method"), all.x = TRUE)
  for (v in V) {
    cmv <- cm[[paste0(v, "_to")]] - cm[[paste0(v, "_fr")]]
    dcm <- cmv[match(paste(ce$county, ce$method), paste(cm$county, cm$method))]
    dc <- ce[[paste0(v, "_to")]] - ce[[paste0(v, "_fr")]]
    nn <- pmin(ce$n_to, ce$n_fr); ok <- !is.na(nn) & nn >= 10
    set(ce, j = paste0("d_", v), value = fifelse(ok, (nn * dc + K * dcm) / (nn + K), dcm))   # shrunk toward county x method
  }
  ce[, adj := if (SPEC == "dr") beta[["dr"]] * d_dr else beta[["dem"]] * d_dem + beta[["rep"]] * d_rep + beta[["blk"]] * d_blk + beta[["a65"]] * d_a65]
  ce[]
}
b24 <- reg[year == 2024 & spec == SPEC & within == FALSE, setNames(est, term)]
adj <- delta_between(2026L, 2024L, b24)

# ---- comp_cells.csv: cells with at least 10 ballots, county x method rows for the rest -------------------------
w <- dcast(comp[n >= 10], county + unit + method ~ year, value.var = c("n", "dem", "rep", "una", "blk", "a65"))
w <- merge(rbind(ee[method %in% EM, .(county, unit, method)], unique(comp[unit == "(county)", .(county, unit, method)])), w,
           by = c("county", "unit", "method"), all.x = TRUE)
w <- merge(w, adj[, .(county, unit, method, d_dr = round(d_dr, 2), adj = round(adj, 2))], by = c("county", "unit", "method"), all.x = TRUE)
num <- setdiff(names(w), c("county", "unit", "method")); w[, (num) := lapply(.SD, round, 2), .SDcols = num]
setorder(w, county, method, unit); fwrite(w, file.path(BD, "comp_cells.csv"))
E <- merge(adj, ee, by = c("county", "unit", "method"))
cat(sprintf("\nSpec %s, beta(2024) = %s. Cells %d; adj mean by accepted ballots %+.2f, range %+.2f to %+.2f; Early %+.2f, Mail %+.2f\n",
    SPEC, paste(round(b24, 3), collapse = ", "), nrow(E), E[, weighted.mean(adj, A)], min(E$adj), max(E$adj),
    E[method == "Early Voting", weighted.mean(adj, A)], E[method != "Early Voting", weighted.mean(adj, A)]))
cat("District composition, accepted early and mail ballots on the lines (county rows, ballot-weighted):\n")
print(comp[unit == "(county)", .(n = round(sum(n)), dem = round(weighted.mean(dem, n), 1), rep = round(weighted.mean(rep, n), 1), una = round(weighted.mean(una, n), 1),
       blk = round(weighted.mean(blk, n), 1), a65 = round(weighted.mean(a65, n), 1)), by = .(year, method)][order(method, year)])
