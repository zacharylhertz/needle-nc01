# Old NC-1 lines (the 2024 map) mapped onto 2026 reporting units, for the page's redistricting comparison.
# Reporting only: nothing here feeds the estimate. Run: Rscript R/build_old_lines.R
# Writes data/baseline/old_lines_units.csv, old_lines_cells.csv, old_lines_match.csv, lines_2024.csv.
#
# Old NC-1 = the reporting units in the 2024 contest "US HOUSE OF REPRESENTATIVES DISTRICT 01" (statewide 2024 precinct
# file). Counties with every 2024 real precinct in that contest are whole: every 2026 unit in them is on the old lines,
# weight 1, including early-voting sites, absentee and provisional pseudo-precincts whatever they are called tonight.
# A county with only some precincts in the contest is split (in 2024 only Granville). There, a 2026 unit takes the weight
# of its 2024 namesake (or predecessor): 2024 House votes / 2024 President votes on that unit, 1 for a precinct wholly in
# old NC-1. Pseudo-precincts in a split county cannot be placed, so they are allocated pro rata, as build_baseline.R does for
# split-county pseudo-precincts: weight = the share of that county's 2024 President votes in the same vote method cast on
# old-NC-1 units. The 2024 Harris minus Trump margin of each 2026 cell comes from its matched 2024 cell (exact name, then
# data/baseline/precinct_predecessors.csv) when that cell had at least 10 votes, otherwise from its county x method figure.
source(file.path({a <- grep("^--file=", commandArgs(FALSE), value = TRUE); dirname(sub("^--file=", "", a[1]))}, "config.R"))
root <- needle_root(); source(file.path(root, "R/lib_data.R"))
options(width = 150)
BD <- file.path(root, "data/baseline")
d24 <- read_results(file.path(RAW, "results_pct_20241105.zip"))
d26 <- read_results(file.path(RAW, "results_pct_20261103.zip"))
pres <- contest_cells(d24[contest == "US PRESIDENT"])
h24 <- contest_cells(d24[contest == "US HOUSE OF REPRESENTATIVES DISTRICT 01"])
OLD <- sort(unique(h24$county))
new_cty <- fread(file.path(BD, "county_ids.csv"))$county
pres <- pres[county %in% OLD]
# Unit weights on the old lines: House votes / President votes per 2024 unit (all methods).
uw <- merge(h24[, .(H = sum(T)), by = .(county, unit, real)], pres[, .(P = sum(T)), by = .(county, unit)], by = c("county", "unit"), all = TRUE)
uw[is.na(H), H := 0]; uw[is.na(P), P := 0]
real_all <- pres[real == "Y", .(n_all = uniqueN(unit)), by = county]
real_old <- uw[H > 0 & real == "Y", .(n_old = uniqueN(unit)), by = county]
cs <- merge(real_all, real_old, by = "county", all.x = TRUE); cs[is.na(n_old), n_old := 0L]
SPLIT_OLD <- cs[n_old < n_all, county]
cat("Old NC-1 counties:", length(OLD), "; split by the old line:", paste(SPLIT_OLD, collapse = " "), "\n")
cat("Old NC-1 counties not in the 2026 district:", paste(setdiff(OLD, new_cty), collapse = " "), "\n")
uw[, w := fifelse(!county %in% SPLIT_OLD, 1, fifelse(P > 0, pmin(H / P, 1), 0))]
uw[county %in% SPLIT_OLD & w > 0.97, w := 1]       # rounding and roll-off, not a split
uw[county %in% SPLIT_OLD & H == 0, w := 0]
pw <- merge(pres, uw[, .(county, unit, w)], by = c("county", "unit"), all.x = TRUE)
pw[is.na(w), w := fifelse(county %in% SPLIT_OLD, 0, 1)]
# Pro rata method weights for pseudo-precincts in split counties.
pr_m <- pw[county %in% SPLIT_OLD, .(w_m = if (sum(T) > 0) sum(T * w) / sum(T) else 0), by = .(county, method)]
old24 <- pw[w > 0, .(county, unit, real, method, D = D * w, R = R * w, T = T * w)]
# Remap renamed or merged 2024 units to their 2026 names.
pred <- fread(file.path(BD, "precinct_predecessors.csv"), colClasses = list(character = c("unit_2026", "unit_2024")))
u26 <- unique(d26[contest == "US SENATE" & county %in% OLD, .(county, unit, real)])
z <- copy(old24)
z[, unit_2024 := unit]
z <- merge(z, pred[, .(county, unit = unit_2024, unit_2026, pw = weight)], by = c("county", "unit"), all.x = TRUE, allow.cartesian = TRUE)
z[, match := fifelse(paste(county, unit) %in% u26[, paste(county, unit)], "exact", fifelse(!is.na(unit_2026), "predecessor", "none"))]
z[match == "predecessor", `:=`(unit = unit_2026, D = D * pw, R = R * pw, T = T * pw)]
z[match == "predecessor" & !paste(county, unit) %in% u26[, paste(county, unit)], match := "none"]
# Match rate by county: share of 2024 old-NC-1 President votes whose unit reaches a 2026 unit.
mr <- z[, .(votes_2024_old = round(sum(T)), exact = round(sum(T[match == "exact"])), predecessor = round(sum(T[match == "predecessor"])),
            unmatched = round(sum(T[match == "none"])), unmatched_real = round(sum(T[match == "none" & real == "Y"])), unmatched_units = paste(unique(unit_2024[match == "none" & T > 0]), collapse = "|")), by = county]
mr[, rate := round((exact + predecessor) / pmax(votes_2024_old, 1), 4)]
# Coverage: unmatched pseudo-precincts in whole counties are still on the old lines tonight (every unit of a whole county
# counts) and take their county x method baseline, so only unmatched real precincts are a gap.
mr[, coverage := round(1 - unmatched_real / pmax(votes_2024_old, 1), 4)]
mr[, `:=`(in_2026_district = county %in% new_cty, split_by_old_line = county %in% SPLIT_OLD)]
tot <- mr[, .(county = "ALL", votes_2024_old = sum(votes_2024_old), exact = sum(exact), predecessor = sum(predecessor), unmatched = sum(unmatched),
              unmatched_real = sum(unmatched_real), unmatched_units = "", rate = round((sum(exact) + sum(predecessor)) / sum(votes_2024_old), 4),
              coverage = round(1 - sum(unmatched_real) / sum(votes_2024_old), 4), in_2026_district = NA, split_by_old_line = NA)]
mr <- rbind(mr, tot); print(mr[, !"unmatched_units"]); fwrite(mr, file.path(BD, "old_lines_match.csv"))
# Cells: 2024 President margin for each 2026 unit x method on the old lines.
cm <- old24[, .(m_cm = if (sum(T) > 0) 100 * sum(D - R) / sum(T) else NA_real_), by = .(county, method)]
zc <- z[match != "none", .(D = sum(D), R = sum(R), T24 = sum(T)), by = .(county, unit, method)]
cells <- CJ(k = seq_len(nrow(u26)), method = METHODS)
cells <- cbind(u26[cells$k], method = cells$method)
cells <- merge(cells, zc, by = c("county", "unit", "method"), all.x = TRUE)
cells <- merge(cells, cm, by = c("county", "method"), all.x = TRUE)
cells[, b_old := fifelse(!is.na(T24) & T24 >= 10, 100 * (D - R) / T24, m_cm)]
# Weight of each 2026 cell on the old lines.
uw26 <- merge(cells[, .(county, unit, real)], unique(z[match != "none", .(county, unit, wu = 0)]), by = c("county", "unit"), all.x = TRUE) |> unique()
wsp <- unique(z[match != "none" & county %in% SPLIT_OLD, .(county, unit, unit_2024)])
wsp <- merge(wsp, uw[, .(county, unit_2024 = unit, w)], by = c("county", "unit_2024"))[, .(w_u = max(w)), by = .(county, unit)]
cells <- merge(cells, wsp, by = c("county", "unit"), all.x = TRUE)
cells <- merge(cells, pr_m, by = c("county", "method"), all.x = TRUE)
cells[, w := fifelse(!county %in% SPLIT_OLD, 1, fifelse(real == "Y", fcoalesce(w_u, 0), fcoalesce(w_m, 0)))]
cells[, match := fifelse(!is.na(T24), "matched", fifelse(real == "N", "county_method", "none"))]
# County x method fallback rows (unit "*") for units that appear only tonight; in split counties they take weight 0 if real.
fb <- cm[, .(county, unit = "*", real = NA_character_, method, b_old = m_cm, w = fifelse(county %in% SPLIT_OLD, NA_real_, 1), T24 = NA_real_, match = "fallback")]
fb <- merge(fb, pr_m, by = c("county", "method"), all.x = TRUE)[, w_pseudo := fifelse(county %in% SPLIT_OLD, w_m, 1)][, w_m := NULL]
out <- rbind(cells[, .(county, unit, real, method, w = round(w, 4), w_pseudo = NA_real_, b_old = round(b_old, 3), T24 = round(T24, 1), match)],
             fb[, .(county, unit, real, method, w, w_pseudo = round(w_pseudo, 4), b_old = round(b_old, 3), T24, match)])
fwrite(out, file.path(BD, "old_lines_cells.csv"))
units <- cells[, .(weight = round(max(w), 4), match = if (any(match == "matched")) "matched" else match[1]), by = .(county, unit, real)]
um <- unique(z[match != "none", .(county, unit, how = match, unit_2024)])[, .(how = paste(unique(how), collapse = "+"), unit_2024 = paste(unique(unit_2024), collapse = ";")), by = .(county, unit)]
units <- merge(units, um, by = c("county", "unit"), all.x = TRUE)
units[is.na(how), how := fifelse(real == "N" & !county %in% SPLIT_OLD, "pseudo, whole county", fifelse(real == "N", "pseudo, pro rata", "none"))]
units[, match := NULL]
fwrite(units[weight > 0 | real == "N"], file.path(BD, "old_lines_units.csv"))
# District 2024 President margins on both sets of lines.
past <- readRDS(file.path(RAW, "past_results_geo.rds"))
sf <- fread(file.path(BD, "split_factors.csv"))
pn <- merge(contest_cells(past$d24[contest == "US PRESIDENT"]), sf, by = c("county", "unit"), all.x = TRUE)[is.na(fact), fact := 1]
l24 <- data.table(lines = c("old", "new"), votes_2024_pres = c(round(old24[, sum(T)]), round(pn[, sum(T * fact)])),
                  harris_minus_trump = c(round(old24[, 100 * sum(D - R) / sum(T)], 3), round(pn[, 100 * sum((D - R) * fact) / sum(T * fact)], 3)),
                  match_rate = c(tot$rate, NA), coverage = c(tot$coverage, NA))
print(l24); fwrite(l24, file.path(BD, "lines_2024.csv"))
cat(sprintf("2026 units on old lines: %d (real %d); weight-0 Granville units dropped\n", units[weight > 0, .N], units[weight > 0 & real == "Y", .N]))
