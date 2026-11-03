# Morning refresh of the voter-file denominators and the early electorate panel.
# Usage: Rscript R/refresh_early.R [--no-download]. Downloads only if the zip's ETag changed.
# Writes aggregates only: data/baseline/early_expected.csv (ballots per reporting cell, no demographics)
# and data/live/early_electorate.json (demographic groups, cells under 10 merged into Other or dropped).
source(file.path({a <- grep("^--file=", commandArgs(FALSE), value = TRUE); dirname(sub("^--file=", "", a[1]))}, "config.R"))
root <- needle_root(); source(file.path(root, "R/lib_data.R"))
BD <- file.path(root, "data/baseline"); LD <- file.path(root, "data/live")
Y <- "20261103"; URL <- sprintf("https://s3.amazonaws.com/dl.ncsbe.gov/ENRS/2026_11_03/absentee_%s.zip", Y)
zipf <- file.path(RAW, sprintf("absentee_%s.zip", Y)); hdr <- paste0(zipf, ".hdr")
ids <- fread(file.path(BD, "county_ids.csv")); CTY <- ids$county; SPLIT <- c("GRANVILLE", "ONSLOW")
args <- commandArgs(TRUE)

header_val <- function(f, name) {
  if (!file.exists(f)) return(NA_character_)
  l <- grep(paste0("^", name, ":"), readLines(f, warn = FALSE), ignore.case = TRUE, value = TRUE)
  if (!length(l)) NA_character_ else trimws(sub("^[^:]*:", "", l[length(l)]))
}
if (!"--no-download" %in% args) {
  etag <- header_val(hdr, "etag")
  tmp <- paste0(zipf, ".part"); th <- paste0(hdr, ".part")
  cmd <- c("-sS", "-m", "900", "-A", shQuote(CFG$user_agent), "-D", shQuote(th), "-o", shQuote(tmp), "-w", shQuote("%{http_code}"))
  if (!is.na(etag)) cmd <- c(cmd, "-H", shQuote(paste("If-None-Match:", etag)))
  code <- system2("curl", c(cmd, URL), stdout = TRUE)
  message("voter file HTTP ", code)
  if (code == "200") { file.rename(tmp, zipf); file.rename(th, hdr) } else if (code != "304") stop("voter file fetch failed: HTTP ", code)
  unlink(c(tmp, th))
}
as_of_h <- parse_http_date(header_val(hdr, "last-modified"))
ext <- file.path(RAW, sprintf("ext_%s.tsv", Y))
stamp <- paste0(ext, ".from"); cur <- paste(file.mtime(zipf))
if (!file.exists(ext) || !file.exists(stamp) || readLines(stamp) != cur) {
  message("extracting voter file")
  inner <- unzip(zipf, list = TRUE)$Name[1]
  system(sprintf("unzip -p %s %s | perl %s > %s", shQuote(zipf), shQuote(inner), shQuote(file.path(root, "R/extract_voter_file.pl")), shQuote(ext)))
  writeLines(cur, stamp)
}
ctl <- paste(sprintf("$1==\"%s\"", CTY), collapse = " || ")
rd <- function(f) {
  e <- fread(cmd = sprintf("awk -F'\\t' 'NR==1 || %s' %s", ctl, shQuote(f)), sep = "\t", quote = "", na.strings = "", colClasses = "character")
  e <- e[grepl("^ACCEPTED", ballot_rtn_status)]
  e[, m := fifelse(grepl("^EARLY|ONE", ballot_req_type), "Early Voting", "Absentee by Mail")]
  e[]
}
e26 <- rd(ext)
# NC-1 on the 2026 file's own district label, with blank-label rows kept in whole counties.
e26 <- e26[cong_dist_desc == "CONGRESSIONAL DISTRICT 1" | (is.na(cong_dist_desc) & !county_desc %in% SPLIT)]
cat(sprintf("Accepted ballots in NC-1: %d (Early %d, Mail %d); status exactly ACCEPTED %d\n", nrow(e26), e26[m == "Early Voting", .N], e26[m != "Early Voting", .N], e26[ballot_rtn_status == "ACCEPTED", .N]))
print(e26[, .N, by = ballot_rtn_status])

# ---- Expected ballots per reporting cell ------------------------------------------------------------------
cells <- fread(file.path(BD, "cells_2026.csv"), colClasses = list(character = c("unit", "alias")))
sx <- fread(file.path(BD, "site_xwalk.csv"), colClasses = list(character = "unit"))
pw <- fread(file.path(BD, "precinct_xwalk.csv"), colClasses = list(character = "unit"))
out <- list()
for (cc in CTY) for (mt in c("Early Voting", "Absentee by Mail")) {
  cl <- cells[county == cc & method == mt]
  ee <- e26[county_desc == cc & m == mt]
  tot <- nrow(ee)
  r <- cl[, .(county, unit, method, A = 0.0, src = "")]
  if (cl[, all(real == "N")] && mt == "Early Voting") {
    s <- ee[, .N, by = site_name]
    s <- merge(s, sx[county == cc], by = "site_name", all.x = TRUE)
    if (s[is.na(unit), sum(N)] > 0) message("unmapped early sites in ", cc, ": ", s[is.na(unit), sum(N)], " ballots")
    r[, A := as.numeric(s$N[match(unit, s$unit)])]; r[is.na(A), A := 0]; r[, src := "site"]
  } else if (cl[, all(real == "N")]) {           # one mail pseudo-precinct
    r[, A := tot / .N]; r[, src := "county"]
  } else {
    p <- ee[, .N, by = precinct_desc]
    p <- merge(p, pw[county == cc], by = "precinct_desc", all.x = TRUE)
    r[, A := as.numeric(p$N[match(unit, p$unit)])]; r[, src := "precinct"]
    miss <- r[is.na(A), .N]; left <- tot - r[, sum(A, na.rm = TRUE)]
    if (miss) {   # precincts without a name crosswalk share the remainder by 2024 ED votes
      ed <- cells[county == cc & method == "Election Day", .(unit, edb)]
      r[is.na(A), `:=`(src = "alloc_by_ed", A = max(left, 0) * ed$edb[match(unit, ed$unit)] / max(sum(ed$edb[match(unit, ed$unit)]), 1))]
    }
    gap <- tot - r[, sum(A)]
    if (abs(gap) > 0) r[, A := A + gap * A / max(sum(A), 1)]  # unmatched names: keep the county total exact
  }
  out[[paste(cc, mt)]] <- r
}
out <- rbindlist(out)
out[, A := round(A, 2)]
fwrite(out, file.path(BD, "early_expected.csv"))
print(out[, .(A = round(sum(A))), by = .(method, src)])
cat("county totals check, cells minus voter file:", out[, sum(A)] - nrow(e26), "\n")

# ---- Early electorate panel --------------------------------------------------------------------------------
groups <- function(e) {
  a <- suppressWarnings(as.integer(e$age))
  list(by_party = fifelse(e$voter_party_code %in% c("DEM", "REP", "UNA"), e$voter_party_code, "Other"),
       by_race = fcase(e$race == "WHITE", "White", grepl("^BLACK", e$race), "Black", default = "Other or undesignated"),
       by_age = fcase(a < 30, "18-29", a < 45, "30-44", a < 65, "45-64", a >= 65, "65+", default = "Unknown"),
       by_gender = fcase(e$gender == "F", "F", e$gender == "M", "M", default = "U"))
}
tab <- function(g, w = NULL) { if (is.null(w)) w <- rep(1, length(g)); r <- data.table(group = g, w = w)[, .(n = sum(w)), by = group]; r[, share := n / sum(n)]; r[] }
cache <- file.path(BD, "early_2024_shares.csv")
if (!file.exists(cache)) {
  e24 <- rd(file.path(RAW, "ext_20241105.tsv"))
  sal <- fread(file.path(BD, "split_factors.csv"))
  names_split <- c("ANTIOCH", "CREDLE", "EAST OXFORD", "SALEM", "SOUTH OXFORD", "SW19")
  e24 <- e24[!county_desc %in% SPLIT | precinct_desc %in% names_split]
  w24 <- fifelse(e24$county_desc == "GRANVILLE" & e24$precinct_desc == "SALEM", sal[unit == "SALM", fact], 1)
  g24 <- groups(e24)
  s24 <- rbindlist(lapply(names(g24), function(k) tab(g24[[k]], w24)[, .(panel = k, group, n = round(n), share)]))
  fwrite(s24, cache)
  cat("2024 accepted ballots on the 2026 lines:", round(sum(w24)), "\n")
}
s24 <- fread(cache)
g26 <- groups(e26); ee <- list(as_of = iso_now(as_of_h), total = nrow(e26))
for (k in names(g26)) {
  t <- tab(g26[[k]])
  small <- t[n < 10]
  if (nrow(small)) {  # merge suppressed cells into the panel's residual group, or drop if none remains
    t <- t[n >= 10]
    message("suppressed ", nrow(small), " cell(s) under 10 in ", k)
  }
  t <- merge(t, s24[panel == k, .(group, share_2024 = share)], by = "group", all.x = TRUE)
  setorder(t, -n)
  ee[[k]] <- lapply(seq_len(nrow(t)), function(i) list(group = t$group[i], n = t$n[i], share = round(t$share[i], 4),
              share_2024 = if (is.na(t$share_2024[i])) NULL else round(t$share_2024[i], 4)))
}
dir.create(LD, showWarnings = FALSE, recursive = TRUE)
tmp <- tempfile(tmpdir = LD); write_json(ee, tmp, auto_unbox = TRUE, pretty = TRUE, null = "null"); file.rename(tmp, file.path(LD, "early_electorate.json"))
for (k in names(g26)) { cat(k, "\n"); print(merge(tab(g26[[k]]), s24[panel == k, .(group, share_2024 = share)], by = "group", all = TRUE)[order(-n)]) }
# County groups for the page (Black and 65+ shares of the same ballots, median splits): data/baseline/county_groups.csv
system2("Rscript", file.path(root, "R/build_groups.R"), stdout = FALSE)
cat("county groups rebuilt:", file.path(BD, "county_groups.csv"), "\n")
