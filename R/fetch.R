# Feed download. One process, at most once every CFG$min_fetch_seconds, conditional GET on every request.
# NEEDLE_FAKE_FEED (rehearsal only) replaces the network with a local directory holding the same files plus
# a headers.json of status codes and Last-Modified values. NEEDLE_NOW replaces the clock.
ZIP_URL <- "https://s3.amazonaws.com/dl.ncsbe.gov/ENRS/2026_11_03/results_pct_20261103.zip"
JSON_URL <- "https://er.ncsbe.gov/enr/20261103/data/precinct_%d.txt"

needle_now <- function() {
  s <- Sys.getenv("NEEDLE_NOW", "")
  if (nzchar(s)) parse_iso(s) else Sys.time()
}

.http_get <- function(url, dest, etag = NA, lastmod = NA, ua = CFG$user_agent) {
  fake <- Sys.getenv("NEEDLE_FAKE_FEED", "")
  if (nzchar(fake)) {
    meta <- fromJSON(file.path(fake, "headers.json"))
    nm <- basename(url)
    code <- if (!is.null(meta$status)) as.integer(meta$status) else 200L
    if (code %in% c(403L, 429L, 500L)) return(list(code = code))
    src <- file.path(fake, nm)
    if (!file.exists(src)) return(list(code = 404L))
    lm <- if (nm %in% names(meta$last_modified)) meta$last_modified[[nm]] else meta$last_modified_default
    tag <- substr(tools::md5sum(src), 1, 16)
    if (!is.na(etag) && identical(unname(etag), unname(tag))) return(list(code = 304L))
    file.copy(src, dest, overwrite = TRUE)
    return(list(code = 200L, etag = unname(tag), last_modified = lm))
  }
  h <- curl::new_handle(useragent = ua, timeout = 90, connecttimeout = 20, failonerror = FALSE)
  hdr <- character()
  if (!is.na(etag)) hdr <- c(hdr, `If-None-Match` = etag)
  if (!is.na(lastmod)) hdr <- c(hdr, `If-Modified-Since` = lastmod)
  if (length(hdr)) do.call(curl::handle_setheaders, c(list(h), as.list(hdr)))
  tmp <- paste0(dest, ".part")
  r <- tryCatch(curl::curl_fetch_disk(url, tmp, handle = h), error = function(e) list(status_code = 0L, err = conditionMessage(e)))
  code <- as.integer(r$status_code)
  if (code == 200L) {
    file.rename(tmp, dest)
    hh <- curl::parse_headers_list(r$headers)
    return(list(code = 200L, etag = hh$etag %||% NA_character_, last_modified = hh$`last-modified` %||% NA_character_))
  }
  unlink(tmp)
  list(code = code, err = r$err %||% NA_character_)
}
`%||%` <- function(a, b) if (is.null(a)) b else a

fetch_state_path <- function(LD) file.path(LD, "fetch_state.json")

# Returns list(status, zip_changed, json_changed, fs). status: ok | throttled | backoff | blocked.
fetch_feeds <- function(LD, ids, log) {
  fsp <- fetch_state_path(LD)
  fs <- if (file.exists(fsp)) fromJSON(fsp, simplifyVector = FALSE) else list(files = list())
  now <- needle_now()
  nown <- as.numeric(now)
  if (!is.null(fs$backoff_until) && nown < fs$backoff_until) { log("backing off until ", iso_now(as.POSIXct(fs$backoff_until, origin = "1970-01-01"))); return(list(status = "backoff", fs = fs)) }
  if (!is.null(fs$last_attempt) && nown - fs$last_attempt < CFG$min_fetch_seconds) { log("skipping fetch: last attempt ", round(nown - fs$last_attempt), " s ago"); return(list(status = "throttled", fs = fs)) }
  fs$last_attempt <- nown
  raw <- file.path(RAW, "live"); dir.create(raw, showWarnings = FALSE, recursive = TRUE); dir.create(file.path(raw, "json"), showWarnings = FALSE)
  blocked <- FALSE; errs <- 0L

  get1 <- function(key, url, dest) {
    prev <- fs$files[[key]]
    r <- .http_get(url, dest, etag = prev$etag %||% NA, lastmod = prev$last_modified %||% NA)
    if (r$code %in% c(403L, 429L)) { log("HTTP ", r$code, " on ", url, ": backing off and not working around it"); blocked <<- TRUE; return(FALSE) }
    if (r$code == 304L) return(FALSE)
    if (r$code != 200L) { log("HTTP ", r$code, " on ", url, " ", r$err %||% ""); errs <<- errs + 1L; return(FALSE) }
    md <- unname(tools::md5sum(dest))
    changed <- !identical(prev$md5 %||% "", md)
    fs$files[[key]] <<- list(etag = r$etag %||% NA, last_modified = r$last_modified %||% NA, md5 = md,
                             changed_at = if (changed) nown else (prev$changed_at %||% nown))
    changed
  }
  zip_ch <- get1("zip", ZIP_URL, file.path(raw, "results.zip"))
  json_ch <- FALSE
  if (!blocked) for (i in seq_len(nrow(ids))) {
    id <- ids$id[i]
    ch <- get1(paste0("json", id), sprintf(JSON_URL, id), file.path(raw, "json", sprintf("precinct_%d.txt", id)))
    json_ch <- json_ch || ch
    if (blocked) break
    if (!nzchar(Sys.getenv("NEEDLE_FAKE_FEED", ""))) Sys.sleep(CFG$json_request_spacing)
  }
  if (blocked) { fs$backoff_until <- nown + CFG$backoff_seconds }
  write_json(fs, fsp, auto_unbox = TRUE, null = "null", na = "null")
  list(status = if (blocked) "blocked" else if (errs > 0L) "errors" else "ok", errs = errs, zip_changed = zip_ch, json_changed = json_ch, fs = fs)
}

# The fresher source: later Last-Modified wins; a tie goes to more counted votes, then to the zip.
choose_source <- function(fs, zip_votes, json_votes) {
  lm <- function(k) { v <- fs$files[[k]]$last_modified; if (is.null(v) || is.na(v)) as.POSIXct(NA) else { p <- parse_http_date(v); if (is.na(p)) parse_iso(v) else p } }
  zlm <- lm("zip")
  jk <- grep("^json", names(fs$files), value = TRUE)
  jlm <- if (length(jk)) { x <- do.call(c, lapply(jk, lm)); if (all(is.na(x))) as.POSIXct(NA) else max(x, na.rm = TRUE) } else as.POSIXct(NA)
  # Later Last-Modified wins, unless the other source holds more votes and the newer header leads by under 10 minutes,
  # which is one county's file moving ahead of the others rather than a correction. A tie goes to the zip.
  src <- if (is.na(jlm) || json_votes < 0) "zip" else if (is.na(zlm) || zip_votes < 0) "json" else {
    newer <- if (zlm > jlm) "zip" else if (jlm > zlm) "json" else "tie"
    gap <- abs(as.numeric(difftime(zlm, jlm, units = "mins")))
    if (newer == "tie") { if (json_votes > zip_votes) "json" else "zip" }
    else if (newer == "zip") { if (zip_votes < json_votes && gap < 10) "json" else "zip" }
    else { if (json_votes < zip_votes && gap < 10) "zip" else "json" }
  }
  list(source = src, zip_last_modified = zlm, json_last_modified = jlm)
}
