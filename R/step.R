# One update: fetch, model, publish. Called by run_loop.sh. Exits non-zero on an error it cannot recover from.
suppressPackageStartupMessages({library(data.table); library(jsonlite)})
a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
root <- normalizePath(file.path(dirname(sub("^--file=", "", a[1])), "..")); Sys.setenv(NEEDLE_ROOT = root)
for (f in c("config", "lib_data", "model", "call_rule", "fetch", "additions", "publish")) source(file.path(root, "R", paste0(f, ".R")))
LD <- Sys.getenv("NEEDLE_LIVE_DIR", file.path(root, "data/live")); dir.create(LD, showWarnings = FALSE, recursive = TRUE)
log_line <- function(...) cat(format(needle_now(), "%Y-%m-%d %H:%M:%S %Z", tz = CFG$tz), paste0(...), "\n", file = stderr(), sep = " ")
state_path <- file.path(LD, "state.json")
rs_path <- file.path(LD, "rule_state.rds")

ids <- fread(file.path(root, "data/baseline/county_ids.csv"))
ft <- fetch_feeds(LD, ids, log_line)
if (ft$status %in% c("blocked", "backoff")) quit(status = 3)    # the shell loop waits; this is not an error
fs <- ft$fs
raw <- file.path(RAW, "live")
geo <- fread(file.path(root, "data/baseline/geo_2026.csv"), colClasses = list(character = "unit"))

sum_votes <- function(x) if (is.null(x)) -1 else sum(x$T)
zf <- file.path(raw, "results.zip")
zl <- if (file.exists(zf)) tryCatch(live_from_zip(zf, CFG), error = function(e) { log_line("zip unreadable: ", conditionMessage(e)); NULL })
jf <- file.path(raw, "json", sprintf("precinct_%d.txt", ids$id))
jl <- if (all(file.exists(jf))) tryCatch(live_from_json(jf, ids, CFG, geo), error = function(e) { log_line("json unreadable: ", conditionMessage(e)); NULL })
if (is.null(zl) && is.null(jl)) stop("no readable feed")
# Senate contest for the page additions (R/additions.R); failures log and leave NULL, never stop the update.
sen <- tryCatch(read_senate(zf, jf, ids, CFG, geo, log_line), error = function(e) { log_line("senate feed failed (page additions only): ", conditionMessage(e)); NULL })
pick <- choose_source(fs, sum_votes(zl), sum_votes(jl))
src <- pick$source
if (src == "zip" && is.null(zl)) src <- "json"
if (src == "json" && is.null(jl)) src <- "zip"
live <- if (src == "zip") zl else jl
prev <- if (file.exists(state_path)) fromJSON(state_path, simplifyVector = FALSE) else NULL
if (!is.null(prev) && is.numeric(prev$counted_votes) && sum(live$T) < prev$counted_votes)
  log_line("counted votes fell from ", prev$counted_votes, " to ", sum(live$T), " (source ", src, ")")
if (!is.null(prev) && !identical(prev$feed$source, src)) log_line("source switched from ", prev$feed$source, " to ", src,
    " (zip Last-Modified ", format(pick$zip_last_modified), ", json Last-Modified ", format(pick$json_last_modified), ")")

now <- needle_now()
# last change: when this pipeline first saw the chosen source's data differ
sig <- digest::digest(live[order(county, unit, method), .(county, unit, method, D, R, T)])
rs <- if (file.exists(rs_path)) {
  r0 <- readRDS(rs_path)
  r0
} else list(orig = hold_new(), ccu = hold_new(), sig = NA_character_, last_change = NA_real_)
rs$live_dir <- LD
fp <- paste0(sig)
vote_sig_changed <- !identical(rs$sig, fp)
if (vote_sig_changed) {
  rs$sig <- fp
  rs$last_change <- if (is.na(rs$last_change)) {
    lm0 <- if (src == "zip") pick$zip_last_modified else pick$json_last_modified
    if (is.na(lm0)) as.numeric(now) else min(as.numeric(lm0), as.numeric(now))
  } else as.numeric(now)
}
lm <- if (src == "zip") pick$zip_last_modified else pick$json_last_modified
feed <- list(source = src, last_modified = if (is.na(lm)) now else lm, last_change_num = rs$last_change)

ee_path <- file.path(LD, "early_electorate.json")
early <- if (file.exists(ee_path)) fromJSON(ee_path, simplifyVector = FALSE) else stop("run R/refresh_early.R first")
base <- load_base_live(root, CFG)
cv <- live[, .(D = sum(D), R = sum(R), O = sum(T) - sum(D) - sum(R))]
cand <- data.table(name = c(CFG$candidates$D, CFG$candidates$R, CFG$candidates$L), party = c("DEM", "REP", "LIB"), votes = c(cv$D, cv$R, cv$O))

out <- build_state(live, base, CFG, prev, rs, now, feed, early, cand, senate = sen)
if (out$new_cells > 0) log_line(out$new_cells, " reporting cells not in the baseline; using county x method baseline")
err <- validate_state(fromJSON(toJSON(out$state, auto_unbox = TRUE, null = "null", na = "null", digits = 6), simplifyVector = FALSE))
if (length(err)) stop("state failed validation: ", paste(err, collapse = "; "))
write_atomic_json(out$state, state_path)
out$rs$live_dir <- NULL
tmp <- tempfile(tmpdir = LD); saveRDS(out$rs, tmp); file.rename(tmp, rs_path)
s <- out$state
fwrite(data.table(t = s$updated_at, status = s$status, source = src, counted = s$counted_votes, pct = s$pct_counted_est, p_davis = s$estimate$p_davis, mid = s$estimate$margin_mid,
                  orig_pass = out$orig$pass, orig_called = out$rs$orig$called, ccu_fav = out$ccu$fav, ccu_lead = out$ccu$lead_ok, ccu_held = isTRUE(out$rs$ccu$held),
                  fav_lead = round(out$ccu$fav_lead), call = s$call$status),
       file.path(LD, "rule_log.csv"), append = file.exists(file.path(LD, "rule_log.csv")))
log_line(sprintf("status=%s source=%s counted=%d pct=%.1f P(Davis)=%.3f mid=%+.2f call=%s ccu=%s orig=%s", s$status, src, s$counted_votes, s$pct_counted_est, s$estimate$p_davis, s$estimate$margin_mid, s$call$status, isTRUE(out$rs$ccu$held), out$rs$orig$called))
# Every source failing: publish what is on disk so "stale" can show, then exit 3 so the shell loop keeps waiting
# instead of stopping for the night after a 10-minute outage. Exit 1 stays reserved for real errors.
if (identical(ft$status, "errors") && ft$errs >= 1L + nrow(ids)) { log_line("all sources failed this cycle; will retry"); quit(status = 3) }
