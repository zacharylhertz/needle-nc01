# County groups for the page's "which places are ahead of benchmark" panel. Rscript R/build_groups.R
# Writes data/baseline/county_groups.csv: one row per county, with aggregate shares and cut points only.
# The voter-file extract (~/needle_nc01_raw/ext_20261103.tsv) is read into memory and only county totals leave it.
#   pres_lean: 2024 Harris minus Trump county margin on the new lines (raw, from the county check in recheck.txt):
#              at least +15 "Strong for Harris", at most -15 "Strong for Trump", otherwise "More competitive".
#   area:      Beaufort, Carteret, Craven, Dare, Hyde, Pamlico, Onslow are "Counties new to NC-1".
#   race, age: share Black and share 65+ among 2026 accepted early and mail ballots in NC-1 (same filter as
#              R/refresh_early.R), split at the median county; a county at the median goes to "More".
source(file.path({a <- grep("^--file=", commandArgs(FALSE), value = TRUE); dirname(sub("^--file=", "", a[1]))}, "config.R"))
root <- needle_root(); source(file.path(root, "R/lib_data.R"))
BD <- file.path(root, "data/baseline")
ids <- fread(file.path(BD, "county_ids.csv")); CTY <- ids$county; SPLIT <- c("GRANVILLE", "ONSLOW")
NEW <- c("BEAUFORT", "CARTERET", "CRAVEN", "DARE", "HYDE", "PAMLICO", "ONSLOW")

ext <- file.path(RAW, "ext_20261103.tsv")
ctl <- paste(sprintf("$1==\"%s\"", CTY), collapse = " || ")
e <- fread(cmd = sprintf("awk -F'\\t' 'NR==1 || %s' %s", ctl, shQuote(ext)), sep = "\t", quote = "", na.strings = "",
           colClasses = "character", select = c("county_desc", "race", "age", "cong_dist_desc", "ballot_rtn_status"))
e <- e[grepl("^ACCEPTED", ballot_rtn_status)]
e <- e[cong_dist_desc == "CONGRESSIONAL DISTRICT 1" | (is.na(cong_dist_desc) & !county_desc %in% SPLIT)]
a <- suppressWarnings(as.integer(e$age))
sh <- data.table(county = e$county_desc, black = grepl("^BLACK", e$race), old = !is.na(a) & a >= 65)[
  , .(ballots = .N, black_share = mean(black), age65_share = mean(old)), by = county]
rm(e, a)

# Raw 2024 county margins, from the county check written by R/build_baseline.R
rc <- readLines(file.path(BD, "recheck.txt"))
i0 <- grep("^County check: Harris minus Trump", rc)[1]; i1 <- grep("^Votes in a county x method", rc)[1]
tb <- regmatches(rc[i0:i1], regexec("^\\s*[0-9]+:\\s+([A-Z]+)\\s+([0-9.]+)\\s+(-?[0-9.]+)\\s+(-?[0-9.]+)\\s+(-?[0-9.]+)\\s*$", rc[i0:i1]))
tb <- rbindlist(lapply(Filter(length, tb), function(x) data.table(county = x[2], pres2024_margin = as.numeric(x[4]))))
stopifnot(setequal(tb$county, CTY), setequal(sh$county, CTY))

g <- merge(tb, sh, by = "county")
g[, area := fifelse(county %in% NEW, "Counties new to NC-1", "Counties from the old NC-1")]
g[, pres_lean := fifelse(pres2024_margin >= 15, "Strong for Harris", fifelse(pres2024_margin <= -15, "Strong for Trump", "More competitive"))]
g[, `:=`(black_cut = median(black_share), age65_cut = median(age65_share))]
g[, race := fifelse(black_share >= black_cut, "More Black voters", "Fewer Black voters")]
g[, age := fifelse(age65_share >= age65_cut, "More older voters", "More younger voters")]
for (v in c("black_share", "age65_share", "black_cut", "age65_cut")) set(g, j = v, value = round(g[[v]], 4))
setcolorder(g, c("county", "pres2024_margin", "pres_lean", "area", "ballots", "black_share", "black_cut", "race", "age65_share", "age65_cut", "age"))
fwrite(g[order(county)], file.path(BD, "county_groups.csv"))
print(g[order(county)])
for (k in c("pres_lean", "area", "race", "age")) print(g[, .N, by = k])
