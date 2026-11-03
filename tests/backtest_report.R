# Summaries from the runs. Sourced by backtest.R, or alone: Rscript tests/backtest_report.R
if (!exists("path")) {
  suppressPackageStartupMessages(library(data.table))
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE); root <- normalizePath(file.path(dirname(sub("^--file=", "", a[1])), ".."))
  x <- readRDS(file.path(path.expand("~/needle_nc01_raw"), "backtest_runs.rds")); path <- x$path; rules <- x$rules
  OUT <- file.path(root, "tests/backtest_results")
}
options(width = 170, datatable.print.nrows = 200)
rules[, target := fifelse(is.na(target), "as run", sprintf("%+d", as.integer(target)))]
path[, target := fifelse(is.na(target), "as run", sprintf("%+d", as.integer(target)))]
rt <- rules[, .(runs = .N, fin = round(mean(fin), 2),
                called_pct = round(100 * mean(!is.na(first_pct)), 1),
                false_pct = round(100 * mean(wrong), 1),
                med_first = round(median(first_pct, na.rm = TRUE), 1),
                p10_first = round(quantile(first_pct, .1, na.rm = TRUE), 1),
                withdrawn_runs_pct = round(100 * mean(withdrawals > 0), 1)),
            by = .(rule, par, contest, target)]
setorder(rt, rule, par, contest, target)
fwrite(rt, file.path(OUT, "rule_table.csv"))
print(rt)
near <- rules[target %in% c("-3", "-1", "+1", "+3")]
pool <- near[, .(false_pct = round(100 * mean(wrong), 2), called_pct = round(100 * mean(!is.na(first_pct)), 1), med_first = round(median(first_pct, na.rm = TRUE), 1),
                 p10_first = round(quantile(first_pct, .1, na.rm = TRUE), 1), withdrawn_pct = round(100 * mean(withdrawals > 0), 1), over95 = round(100 * mean(first_pct >= 95, na.rm = TRUE), 1)), by = .(rule, par, near_zero = fifelse(target %in% c("-1", "+1"), "+/-1", "+/-3"))]
setorder(pool, rule, par, near_zero); fwrite(pool, file.path(OUT, "rule_pool_near_zero.csv")); print(pool)
path[, win := fin > 0]
path[, dec := pmin(floor(p * 10), 9)]
cal <- path[, .(n = .N, mean_p = round(mean(p), 3), won = round(mean(win), 3), contest_targets = uniqueN(paste(contest, target))), by = dec][order(dec)]
fwrite(cal, file.path(OUT, "calibration.csv")); print(cal)
path[, tail := fifelse(p < .01, "p<.01", fifelse(p > .99, "p>.99", "mid"))]
print(path[tail != "mid", .(n = .N, won = round(mean(win), 4), wrong_share = round(mean(win != (p > .5)), 4)), by = tail])
cp <- rbindlist(lapply(c(25, 50, 75, 90), function(q) {
  path[pct_true >= q, .SD[1], by = .(contest, target, run)][, .(at = q, n = .N, abs_err = round(mean(abs(mid - fin)), 2), med_abs = round(median(abs(mid - fin)), 2), bias = round(mean(mid - fin), 2), cover90 = round(mean(lo <= fin & hi >= fin), 3)), by = contest]
}))
fwrite(cp, file.path(OUT, "error_by_pct.csv")); print(cp)
cp2 <- rbindlist(lapply(c(25, 50, 75, 90), function(q) path[pct_true >= q, .SD[1], by = .(contest, target, run)][, .(at = q, abs_err = round(mean(abs(mid - fin)), 2), cover90 = round(mean(lo <= fin & hi >= fin), 3))])); print(cp2)
print(path[, .(max_pct_est = round(max(pct_est), 1), max_true = round(max(pct_true), 1)), by = contest])
