# ZH's hand on the call. The pipeline never publishes a call by itself under the cannot-catch-up rule.
# Usage:
#   Rscript R/sign_off.R status           show the current test state
#   Rscript R/sign_off.R Davis            sign off for Davis (refused unless the test is currently met for Davis)
#   Rscript R/sign_off.R Buckhout         likewise
#   Rscript R/sign_off.R clear            remove the sign-off (a published call becomes withdrawn at the next update)
suppressPackageStartupMessages(library(jsonlite))
a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
root <- normalizePath(file.path(dirname(sub("^--file=", "", a[1])), "..")); Sys.setenv(NEEDLE_ROOT = root)
source(file.path(root, "R/config.R"))
LD <- Sys.getenv("NEEDLE_LIVE_DIR", file.path(root, "data/live"))
sp <- file.path(LD, "call_signoff.json"); st <- file.path(LD, "state.json")
arg <- commandArgs(TRUE)
if (!length(arg)) arg <- "status"
s <- if (file.exists(st)) fromJSON(st, simplifyVector = FALSE) else NULL
show <- function() {
  if (is.null(s)) { cat("no state.json yet\n"); return(invisible()) }
  cat(sprintf("updated %s  status %s  counted %d (%.1f%%)  P(Davis) %.3f  call %s\n", s$updated_at, s$status, s$counted_votes, s$pct_counted_est, s$estimate$p_davis, s$call$status))
  for (cd in s$call$conditions) cat(sprintf("  %-22s %-5s value %s threshold %s\n", cd$id, cd$met, cd$value, cd$threshold))
  cat("sign-off file:", if (file.exists(sp)) paste(readLines(sp), collapse = " ") else "none", "\n")
}
if (arg[1] == "status") { show(); quit(status = 0) }
if (arg[1] == "clear") { unlink(sp); cat("sign-off removed\n"); quit(status = 0) }
if (!arg[1] %in% c("Davis", "Buckhout")) stop("use Davis, Buckhout, status or clear")
if (is.null(s)) stop("no state.json; nothing to sign off on")
cc <- Filter(function(x) x$id == "cannot_catch_up", s$call$conditions)[[1]]
if (!isTRUE(cc$met)) { show(); stop("cannot_catch_up is not met; sign-off refused") }
lead <- s$candidates[[1]]$votes - s$candidates[[2]]$votes
who <- if (lead > 0) "Davis" else "Buckhout"
if (who != arg[1]) { show(); stop(sprintf("the leader is %s, not %s; sign-off refused", who, arg[1])) }
now <- Sys.getenv("NEEDLE_NOW", "")
stamp <- if (nzchar(now)) now else { x <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = CFG$tz); sub("([+-][0-9]{2})([0-9]{2})$", "\\1:\\2", x) }
tmp <- tempfile(tmpdir = LD)
write_json(list(candidate = arg[1], signed_at = stamp, signed_by = "ZH"), tmp, auto_unbox = TRUE, pretty = TRUE)
file.rename(tmp, sp)
cat("signed off:", arg[1], "at", stamp, "\nThe call publishes at the next update if the test still holds.\n")
