# Operations stress test for the election-night loop. Runs only in temp dirs, never touches the network,
# data/live/ or data/baseline/ in this folder, and runs git only inside temp copies (checked with rev-parse).
# Fake feeds reuse tests/rehearse.R's construction (2022 Senate relabelled, on the 2026 template); nothing voter-level is written.
# Usage: Rscript tests/stress.R [report.csv]
suppressPackageStartupMessages({library(data.table); library(jsonlite)})
a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
root <- normalizePath(file.path(dirname(sub("^--file=", "", a[1])), "..")); Sys.setenv(NEEDLE_ROOT = root)
for (f in c("config", "lib_data", "model", "call_rule", "fetch", "publish")) source(file.path(root, "R", paste0(f, ".R")))
realraw <- RAW
out_csv <- commandArgs(TRUE)[1]
TMP <- file.path(Sys.getenv("STRESS_TMP", dirname(tempdir())), paste0("stress_", format(Sys.time(), "%H%M%S"))); dir.create(TMP, recursive = TRUE); TMP <- normalizePath(TMP)
cat("temp dir:", TMP, "\n")

# ---- fake feed, copied from rehearse.R ----
d26 <- read_results(file.path(realraw, "results_pct_20261103.zip"))
tpl <- d26[startsWith(contest, CFG$contest_prefix)]
ids <- fread(file.path(root, "data/baseline/county_ids.csv"))
base <- load_base_live(root, CFG); ce <- base$cells
set.seed(2026)
ce[, Tv := fifelse(mi == 1, edb * 1.1, fifelse(mi == 4, 0, A * .99))]
cg <- fread(file.path(root, "data/baseline/county_groups.csv"))
ce[, ceff := cg[match(ce$county, cg$county), fifelse(area == "Counties new to NC-1", -10, fifelse(race == "More Black voters", 4, 1))]]
ce[, mg := pmin(pmax(b + 13 + fifelse(mi == 1, -4, 2) + ceff + rnorm(.N, 0, 4), -95), 95)]
ce[, `:=`(D = round(Tv * (.98 + mg / 100) / 2), R = round(Tv * (.98 - mg / 100) / 2), L = round(Tv * .02))]
ctys <- sort(unique(ce$county))
ed_mask <- function(share) {
  r <- ce[mi == 1, .(k = paste(county, unit), cty = match(county, ctys), id = rowid(county))]
  setorder(r, cty, id)
  on <- r$k[seq_len(round(share * nrow(r)))]
  as.numeric(ce[, paste(county, unit)] %in% on)
}
# Extra hooks: omut(out) edits the results table before both writes; zmut(out) only the zip; jmut(j, i) each county JSON.
wfeed <- function(frac_ed, ev_counties, lm_zip, lm_json, write_zip = TRUE, write_json = TRUE, status = NULL,
                  omut = identity, zmut = identity, jmut = function(j, i) j) {
  v <- copy(ce)
  v[, on := fifelse(mi %in% 2:3, county %in% ev_counties, fifelse(mi == 1, frac_ed[match(paste(county, unit), paste(county, unit))] > 0, FALSE))]
  x <- tpl[, .(county, date, unit, gid, ctype, contest, choice, party, votefor, real)]
  m <- CJ(i = seq_len(nrow(x)), method = METHODS)
  x2 <- cbind(x[m$i], method = m$method)
  y <- merge(x2, v[on == TRUE, .(county, unit, method, D, R, L)], by = c("county", "unit", "method"), all.x = TRUE)
  y[, n := fifelse(party == "DEM", D, fifelse(party == "REP", R, L))]
  y[is.na(n), n := 0]
  w <- dcast(y, county + date + unit + gid + ctype + contest + choice + party + votefor + real ~ method, value.var = "n")
  w[, Total := `Absentee by Mail` + `Early Voting` + `Election Day` + Provisional]
  out <- w[, .(County = county, `Election Date` = date, Precinct = unit, `Contest Group ID` = gid, `Contest Type` = ctype, `Contest Name` = contest, Choice = choice, `Choice Party` = party, `Vote For` = votefor,
               `Election Day` = `Election Day`, `Early Voting` = `Early Voting`, `Absentee by Mail` = `Absentee by Mail`, Provisional = Provisional, `Total Votes` = Total, `Real Precinct` = real)]
  out <- omut(out)
  if (write_zip) {
    txt <- file.path(FF, "results_pct_20261103.txt"); fwrite(zmut(copy(out)), txt, sep = "\t")
    owd <- setwd(FF); zip("results.zip.tmp.zip", "results_pct_20261103.txt", flags = "-q"); setwd(owd)
    file.rename(file.path(FF, "results.zip.tmp.zip"), file.path(FF, "results_pct_20261103.zip")); unlink(txt)
  }
  if (write_json) for (i in seq_len(nrow(ids))) {
    o <- out[County == ids$county[i]]
    j <- o[, .(lid = CFG$contest_lid, aid = Precinct, anm = Precinct, sta = "Reported", bnm = Choice, vct = as.character(`Total Votes`), evc = as.character(`Election Day`),
               ovc = as.character(`Early Voting`), avc = as.character(`Absentee by Mail`), pvc = as.character(Provisional))]
    write_json(jmut(j, i), file.path(FF, sprintf("precinct_%d.txt", ids$id[i])))
  }
  meta <- list(last_modified_default = lm_json, last_modified = list(results_pct_20261103.zip = lm_zip))
  if (!is.null(status)) meta$status <- status
  write_json(meta, file.path(FF, "headers.json"), auto_unbox = TRUE)
}
T <- function(h, m) sprintf("2026-11-03T%02d:%02d:00-05:00", h, m)

# ---- harness ----
newdirs <- function(tag) {
  d <- file.path(TMP, tag); dir.create(d)
  LD <<- file.path(d, "live"); FF <<- file.path(d, "feed"); RW <<- file.path(d, "raw")
  for (x in c(LD, FF, RW)) dir.create(x)
  file.copy(file.path(root, "data/live/early_electorate.json"), file.path(LD, "early_electorate.json"))
  Sys.setenv(NEEDLE_LIVE_DIR = LD, NEEDLE_FAKE_FEED = FF, NEEDLE_RAW = RW)
}
md5 <- function(p) if (file.exists(p)) unname(tools::md5sum(p)) else NA_character_
step1 <- function(ts, timeit = FALSE) {
  Sys.setenv(NEEDLE_NOW = ts)
  sp <- file.path(LD, "state.json"); m0 <- md5(sp)
  t0 <- Sys.time()
  o <- suppressWarnings(if (timeit) system2("/usr/bin/time", c("-l", "Rscript", file.path(root, "R/step.R")), stdout = TRUE, stderr = TRUE)
                        else system2("Rscript", file.path(root, "R/step.R"), stdout = TRUE, stderr = TRUE))
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  rc <- attr(o, "status"); if (is.null(rc)) rc <- 0L
  s <- if (file.exists(sp)) tryCatch(fromJSON(sp, simplifyVector = FALSE), error = function(e) NULL) else NULL
  valid <- !is.null(s) && !length(validate_state(s))
  rss <- grep("maximum resident set size", o, value = TRUE)
  rss_mb <- if (length(rss)) as.numeric(sub("^\\s*([0-9]+).*", "\\1", rss[1])) / 2^20 else NA
  logl <- grep("^20[0-9]{2}-|Error|error", o, value = TRUE)
  davis <- if (!is.null(s)) s$candidates[[1]]$votes else NA
  list(rc = as.integer(rc), secs = secs, changed = !identical(m0, md5(sp)), valid = valid, s = s, log = logl, rss_mb = rss_mb,
       counted = if (!is.null(s)) s$counted_votes else NA, src = if (!is.null(s)) s$feed$source else NA, davis = davis,
       hist = if (!is.null(s)) length(s$history) else NA)
}
RES <- list()
rep1 <- function(item, check, pass, detail) {
  RES[[length(RES) + 1]] <<- data.table(item = item, check = check, result = if (isTRUE(pass)) "PASS" else "FAIL", detail = detail)
  cat(sprintf("[%s] %s %s: %s\n", if (isTRUE(pass)) "PASS" else "FAIL", item, check, detail))
}
short <- function(r) sprintf("rc=%d valid=%s rewritten=%s src=%s counted=%s davis=%s | log: %s", r$rc, r$valid, r$changed, r$src, r$counted, r$davis,
                             paste(substr(tail(r$log, 3), 1, 160), collapse = " // "))
all_e <- ctys
good <- function(share = .25, h = 19, m = 59) wfeed(ed_mask(share), all_e, lm_zip = T(h, m), lm_json = T(h, m))

# ---- 1. runtime ----
newdirs("runtime")
for (sh in list(c(0, 0), c(.5, 12), c(1, 25))) {
  wfeed(ed_mask(sh[1]), ctys[seq_len(sh[2])], lm_zip = T(20, 0), lm_json = T(20, 0))
  k <- length(RES)
  r <- step1(T(20, 3 * (length(RES) + 1)), timeit = TRUE)
  rep1("1 runtime", sprintf("pct_counted_est=%.1f", if (is.null(r$s)) NA else r$s$pct_counted_est), r$rc == 0 && r$secs < 60,
       sprintf("%.1f s wall, peak RSS %.0f MB, rc=%d", r$secs, r$rss_mb, r$rc))
}

# ---- 2. feed failures ----
newdirs("zip_missing"); wfeed(ed_mask(.25), all_e, T(19, 59), T(19, 59), write_zip = FALSE)
r <- step1(T(20, 0)); rep1("2 feed", "zip missing, JSON present", r$rc == 0 && r$valid && r$src == "json" && any(grepl("HTTP 404", r$log)), short(r))
newdirs("json_missing"); wfeed(ed_mask(.25), all_e, T(19, 59), T(19, 59), write_json = FALSE)
r <- step1(T(20, 0)); rep1("2 feed", "JSON missing, zip present", r$rc == 0 && r$valid && r$src == "zip" && any(grepl("HTTP 404", r$log)), short(r))

# Scenario after one good update at 20:00; the bad feed lands at 20:03.
scen <- function(tag, bad, ok, label, share0 = .25) {
  newdirs(tag); good(share0); r0 <- step1(T(20, 0))
  if (r0$rc != 0) { rep1("2 feed", label, FALSE, paste("good first step failed:", short(r0))); return(invisible(NULL)) }
  bad(); r <- step1(T(20, 3)); r$r0 <- r0
  rep1("2 feed", label, ok(r), paste0(short(r), sprintf(" | before: counted=%s davis=%s", r0$counted, r0$davis)))
  invisible(r)
}
trunc_file <- function(p) { b <- readBin(p, "raw", file.size(p)); writeBin(b[seq_len(length(b) %/% 2)], p) }
scen("corrupt_zip", function() { good(.30, 20, 2); trunc_file(file.path(FF, "results_pct_20261103.zip")) },
     function(r) r$rc == 0 && r$valid && r$src == "json" && any(grepl("zip unreadable", r$log)), "truncated zip, JSON fine")
for (code in c(403L, 429L)) {
  r <- scen(paste0("http", code), function() { good(.30, 20, 2); m <- fromJSON(file.path(FF, "headers.json")); m$status <- code; write_json(m, file.path(FF, "headers.json"), auto_unbox = TRUE) },
       function(r) r$rc == 3 && !r$changed && any(grepl(paste("HTTP", code), r$log)), sprintf("HTTP %d", code))
  r2 <- step1(T(20, 10))
  rep1("2 feed", sprintf("HTTP %d, next cycle inside backoff", code), r2$rc == 3 && !r2$changed && any(grepl("backing off until", r2$log)), short(r2))
  fs <- fromJSON(file.path(LD, "fetch_state.json")); rep1("2 feed", sprintf("HTTP %d backoff length", code), (fs$backoff_until - fs$last_attempt) == CFG$backoff_seconds,
       sprintf("backoff_until - last_attempt = %d s", as.integer(fs$backoff_until - fs$last_attempt)))
}
scen("http500", function() { good(.30, 20, 2); m <- fromJSON(file.path(FF, "headers.json")); m$status <- 500L; write_json(m, file.path(FF, "headers.json"), auto_unbox = TRUE) },
     function(r) r$rc == 3 && r$valid && any(grepl("HTTP 500", r$log)) && any(grepl("all sources failed", r$log)), "HTTP 500 on everything (exit 3: loop waits, does not stop)")
scen("empty_zip", function() { good(.30, 20, 2); file.create(file.path(FF, "results_pct_20261103.zip")) },
     function(r) r$rc == 0 && r$valid && r$src == "json" && any(grepl("zip unreadable", r$log)), "empty (0-byte) zip, JSON fine")
scen("empty_json", function() { good(.30, 20, 2); file.create(file.path(FF, sprintf("precinct_%d.txt", ids$id[1]))) },
     function(r) r$rc == 0 && r$valid && r$src == "zip" && any(grepl("json unreadable", r$log)), "empty (0-byte) JSON for one county, zip fine")
# A county's JSON comes back as [] while the zip is stale (304) and the JSON Last-Modified is 20 minutes newer
scen("json_county_empty", function() wfeed(ed_mask(.30), all_e, lm_zip = T(19, 59), lm_json = T(20, 19), write_zip = FALSE, jmut = function(j, i) if (i == 1) j[0] else j),
     function(r) r$valid && (r$rc != 0 || any(grepl(tolower(ids$county[1]), tolower(r$log))) || r$src == "zip"), sprintf("JSON for %s is [] (zip unchanged)", ids$county[1]))
for (nm in list(c("Don  Davis", "double space"), c("DON DAVIS", "all caps"))) {
  newdirs(paste0("name_", gsub(" ", "", nm[2])))
  wfeed(ed_mask(.25), all_e, T(19, 59), T(19, 59), write_zip = FALSE, jmut = function(j, i) j[bnm == CFG$candidates$D, bnm := nm[1]][])
  r <- step1(T(20, 0))
  rep1("2 feed", sprintf("JSON-only, candidate spelled '%s' (%s)", nm[1], nm[2]), (r$valid && !is.na(r$davis) && r$davis > 0) || (r$rc != 0 && any(grepl("do not match", r$log))), short(r))
}
newdirs("name_zip"); wfeed(ed_mask(.25), all_e, T(19, 59), T(19, 59), write_json = FALSE, zmut = function(o) o[Choice == CFG$candidates$D, Choice := "DON  DAVIS"][])
r <- step1(T(20, 0)); rep1("2 feed", "zip-only, Choice spelled 'DON  DAVIS'", r$rc == 0 && r$valid && r$davis > 0, short(r))
newdirs("method_json"); wfeed(ed_mask(.25), all_e, T(19, 59), T(19, 59), write_zip = FALSE, jmut = function(j, i) setnames(copy(j), "evc", "edv"))
r <- step1(T(20, 0)); rep1("2 feed", "JSON-only, field evc renamed edv", r$rc != 0 && any(grepl("json unreadable|no readable", r$log)) || (r$rc == 0 && r$valid && r$counted > 0), short(r))
newdirs("method_zip"); wfeed(ed_mask(.25), all_e, T(19, 59), T(19, 59), write_json = FALSE, zmut = function(o) setnames(o, c("Election Day", "Early Voting"), c("Election Day Votes", "One Stop")))
r <- step1(T(20, 0)); rep1("2 feed", "zip-only, method headers renamed (Election Day Votes, One Stop)", r$rc == 0 && r$valid && r$counted > 0, short(r))
r <- scen("votes_down", function() good(.25, 20, 2), function(r) r$rc == 0 && r$valid, "counts go DOWN (ED 50% -> 25% of precincts)", share0 = .5)
if (!is.null(r)) rep1("2 feed", "counts go DOWN: logged?", any(grepl("decreas|down|fell|lower|correction", r$log, ignore.case = TRUE)),
                      sprintf("counted %s -> %s; log lines mentioning a decrease: %d", r$r0$counted, r$counted, sum(grepl("decreas|down|fell|lower|correction", r$log, ignore.case = TRUE))))
scen("new_precinct", function() wfeed(ed_mask(.30), all_e, T(20, 2), T(20, 2), omut = function(o) { u <- o[County == ctys[1] & `Real Precinct` == "Y", Precinct][1]; o[County == ctys[1] & Precinct == u, Precinct := "ZZ STRESS NEW"][] }),
     function(r) r$rc == 0 && r$valid && any(grepl("not in the baseline", r$log)), "precinct name not in baseline")

# ---- 3. stale data ----
newdirs("stale"); good(.25); r0 <- step1(T(20, 0)); r1 <- step1(T(20, 3))
rep1("3 stale", "same files and Last-Modified twice", r1$rc == 0 && r1$valid,
     sprintf("rc=%d; state.json rewritten=%s; history %d -> %d; updated_at %s -> %s; fetch log: %s", r1$rc, r1$changed, r0$hist, r1$hist, r0$s$updated_at, r1$s$updated_at, paste(substr(r1$log, 1, 100), collapse = " // ")))

# ---- 4. loop mechanics ----
newdirs("lock"); good(.25); dir.create(file.path(LD, ".loop.lock")); writeLines("99999", file.path(LD, ".loop.lock/pid"))
o <- suppressWarnings(system2(file.path(root, "run_loop.sh"), "--once", stdout = TRUE, stderr = TRUE)); st <- attr(o, "status")
rep1("4 loop", "stale lock from dead pid 99999", identical(as.integer(st), 4L) && any(grepl("Remove the folder", o)), sprintf("exit %s; message: %s", st, paste(o, collapse = " ")))
newdirs("maxerr")   # feed not posted yet: every URL 404s, e.g. a loop started before NCSBE puts files up
write_json(list(last_modified_default = T(19, 0)), file.path(FF, "headers.json"), auto_unbox = TRUE); Sys.setenv(NEEDLE_NOW = T(19, 0))
t0 <- Sys.time(); o <- suppressWarnings(system2(file.path(root, "run_loop.sh"), c("--interval", "1", "--max-errors", "5"), stdout = TRUE, stderr = TRUE)); st <- attr(o, "status")
lg <- readLines(file.path(LD, "loop.log"))
rep1("4 loop", "MAXERR stop (all files 404)", identical(as.integer(st), 1L) && sum(grepl("update failed", lg)) == 5 && !dir.exists(file.path(LD, ".loop.lock")),
     sprintf("exit %s after %.0f s; 'update failed' lines %d; STOPPING line %s; lock removed %s; first reason: %s", st, as.numeric(difftime(Sys.time(), t0, units = "secs")),
             sum(grepl("update failed", lg)), any(grepl("STOPPING", lg)), !dir.exists(file.path(LD, ".loop.lock")), grep("Error|no readable", lg, value = TRUE)[1]))
# Recovery: 4 errors, a success, 4 more errors, a success; the loop must still be running.
newdirs("recover"); good(.25); r0 <- step1(T(20, 0)); Sys.setenv(NEEDLE_NOW = T(20, 1))
ee <- file.path(LD, "early_electorate.json"); ee_bak <- file.path(TMP, "ee.json"); file.copy(ee, ee_bak); unlink(ee)
lp <- file.path(LD, "loop.log")
system2(file.path(root, "run_loop.sh"), c("--interval", "1"), stdout = file.path(TMP, "recover.out"), stderr = file.path(TMP, "recover.out"), wait = FALSE)
waitfor <- function(pat, n, secs = 240) { t0 <- Sys.time(); repeat { l <- if (file.exists(lp)) readLines(lp, warn = FALSE) else character(); if (sum(grepl(pat, l)) >= n) return(TRUE)
  if (difftime(Sys.time(), t0, units = "secs") > secs) return(FALSE); Sys.sleep(.5) } }
ok1 <- waitfor("\\(4 in a row\\)", 1); file.copy(ee_bak, ee); ok2 <- waitfor("status=", 2); unlink(ee)
ok3 <- waitfor("\\(4 in a row\\)", 2); file.copy(ee_bak, ee); ok4 <- waitfor("status=", 3)
pid <- suppressWarnings(as.integer(readLines(file.path(LD, ".loop.lock/pid"))))
alive <- !is.na(pid) && tools::pskill(pid, 0L)
lg <- readLines(lp)
rep1("4 loop", "recovery: 4 errors, success, 4 errors, success", ok1 && ok2 && ok3 && ok4 && alive && !any(grepl("STOPPING", lg)),
     sprintf("reached 4-in-a-row twice=%s/%s, successes after=%s/%s, loop alive=%s, STOPPING lines=%d, max streak in log=%s", ok1, ok3, ok2, ok4, alive, sum(grepl("STOPPING", lg)),
             max(as.integer(sub(".*\\(([0-9]+) in a row.*", "\\1", grep("in a row", lg, value = TRUE))))))
if (alive) { tools::pskill(pid, tools::SIGTERM); Sys.sleep(4) }
rep1("4 loop", "SIGTERM removes lock", !dir.exists(file.path(LD, ".loop.lock")), sprintf("lock present after TERM: %s", dir.exists(file.path(LD, ".loop.lock"))))

# ---- 6 then 5. git, temp copies only ----
G <- function(dir, ...) {
  stopifnot(startsWith(normalizePath(dir), TMP))
  o <- suppressWarnings(system2("git", c("-C", shQuote(dir), ...), stdout = TRUE, stderr = TRUE))
  attr(o, "rc") <- attr(o, "status") %||% 0L; o
}
gd <- file.path(TMP, "git"); dir.create(gd); file.copy(root, gd, recursive = TRUE); cp <- normalizePath(file.path(gd, basename(root)))
unlink(file.path(cp, ".git"), recursive = TRUE)
G(cp, "init", "-q"); top <- normalizePath(G(cp, "rev-parse", "--show-toplevel")[1])
stopifnot(identical(top, cp))
G(cp, "config", "user.name", "stress"); G(cp, "config", "user.email", "stress@example.invalid"); G(cp, "config", "commit.gpgsign", "false")
G(cp, "add", "-A"); files <- G(cp, "ls-files")
sz <- file.size(file.path(cp, files))
fl <- data.table(file = files, kb = round(sz / 1024, 1))
pii_cols <- "last_name|first_name|res_street|mail_addr|voter_reg_num|ncid|birth|full_name|phone"
fl[, pii := sapply(file.path(cp, file), function(p) if (grepl("\\.(csv|tsv|txt)$", p)) any(grepl(pii_cols, readLines(p, n = 2, warn = FALSE), ignore.case = TRUE)) else FALSE)]
fl[, flag := fifelse(kb > 5 * 1024, "over 5 MB", fifelse(grepl("\\.log$|raw|\\.rds$|\\.DS_Store|\\.Rhistory|rule_log|fetch_state|signoff", file), "should not be public?", fifelse(pii, "voter-identifying column names", "")))]
print(system2("git", c("-C", shQuote(cp), "check-ignore", "-v", "--no-index", "data/live/loop.log", "data/live/fetch_state.json"), stdout = TRUE)); cat("\n6. files `git add -A` would commit in a fresh repo:\n"); print(fl, nrows = 500)
fwrite(fl, file.path(TMP, "git_add_files.csv"))
rep1("6 gitignore", "files staged by git add -A", !any(nzchar(fl$flag)), sprintf("%d files, %.0f KB total; flagged: %s", nrow(fl), sum(fl$kb), paste(fl[nzchar(flag), paste0(file, " (", flag, ")")], collapse = ", ")))

# 5. push to a local bare repo
G(cp, "commit", "-q", "-m", "init"); bare <- file.path(TMP, "bare.git"); system2("git", c("init", "-q", "--bare", shQuote(bare)))
G(cp, "remote", "add", "origin", bare); br <- G(cp, "symbolic-ref", "--short", "HEAD")[1]; G(cp, "push", "-q", "-u", "origin", br)
nb <- function() as.integer(system2("git", c("-C", shQuote(bare), "rev-list", "--count", br), stdout = TRUE))
n0 <- nb()
cl <- file.path(cp, "data/live"); unlink(file.path(cl, c("fetch_state.json", "rule_state.rds", "rule_log.csv", "loop.log")))
LD <- cl; FF <- file.path(TMP, "pushfeed"); dir.create(FF); RW <- file.path(TMP, "pushraw"); dir.create(RW)
Sys.setenv(NEEDLE_LIVE_DIR = cl, NEEDLE_FAKE_FEED = FF, NEEDLE_RAW = RW, NEEDLE_NOW = T(20, 0)); good(.25)
o <- suppressWarnings(system2(file.path(cp, "run_loop.sh"), c("--push", "--once"), stdout = TRUE, stderr = TRUE)); rc1 <- attr(o, "status") %||% 0L
n1 <- nb(); touched <- system2("git", c("-C", shQuote(bare), "show", "--name-only", "--format=", br), stdout = TRUE); touched <- touched[nzchar(touched)]
rep1("5 push", "first --push --once", rc1 == 0 && n1 - n0 == 1 && identical(touched, "data/live/state.json"), sprintf("rc=%d; commits in bare %d -> %d; files in new commit: %s", rc1, n0, n1, paste(touched, collapse = ",")))
o <- suppressWarnings(system2(file.path(cp, "run_loop.sh"), c("--push", "--once"), stdout = TRUE, stderr = TRUE)); rc2 <- attr(o, "status") %||% 0L
n2 <- nb(); st_dirty <- G(cp, "status", "--porcelain")
empties <- sum(sapply(system2("git", c("-C", shQuote(bare), "rev-list", br), stdout = TRUE), function(h) length(system2("git", c("-C", shQuote(bare), "diff-tree", "--root", "--no-commit-id", "--name-only", "-r", h), stdout = TRUE)) == 0))
rep1("5 push", "second run, same clock and feed", rc2 == 0 && empties == 0, sprintf("rc=%d; commits %d -> %d (%s); empty commits in bare: %d; dirty after: %s", rc2, n1, n2,
     if (n2 == n1) "state unchanged, no commit" else "state.json changed, so a real commit", empties, paste(st_dirty, collapse = ";")))
G(cp, "remote", "set-url", "origin", file.path(TMP, "nonexistent.git")); Sys.setenv(NEEDLE_NOW = T(20, 3)); good(.40, 20, 2)
lp <- file.path(cl, "loop.log")
system2(file.path(cp, "run_loop.sh"), c("--push", "--interval", "1"), stdout = file.path(TMP, "pushfail.out"), stderr = file.path(TMP, "pushfail.out"), wait = FALSE)
okp <- waitfor("push failed", 2, 180)
pid <- suppressWarnings(as.integer(readLines(file.path(cl, ".loop.lock/pid")))); alive <- !is.na(pid) && tools::pskill(pid, 0L)
ahead <- G(cp, "rev-list", "--count", paste0("origin/", br, "..HEAD"))[1]
rep1("5 push", "push failure (origin = nonexistent path)", okp && alive, sprintf("'push failed' lines=%d; loop alive=%s; local commits ahead of origin=%s; bare unchanged=%s",
     sum(grepl("push failed", readLines(lp))), alive, ahead, nb() == n2))
if (alive) { tools::pskill(pid, tools::SIGTERM); Sys.sleep(4) }

R <- rbindlist(RES); cat("\n"); print(R[, .(item, check, result)])
if (!is.na(out_csv)) fwrite(R, out_csv)
cat(sprintf("\n%d PASS, %d FAIL. Temp dir kept for inspection: %s\n", sum(R$result == "PASS"), sum(R$result == "FAIL"), TMP))
