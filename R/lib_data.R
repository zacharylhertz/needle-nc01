# Reading NCSBE results and voter-file extracts into a common long form.
suppressPackageStartupMessages({library(data.table); library(jsonlite)})

METHODS <- c("Election Day", "Early Voting", "Absentee by Mail", "Provisional")
RAW <- path.expand(Sys.getenv("NEEDLE_RAW", "~/needle_nc01_raw"))

# Precinct results file (zip or txt). Older files say "One Stop" for early voting and have a trailing tab.
read_results <- function(path) {
  con <- if (grepl("\\.zip$", path)) {
    inner <- unzip(path, list = TRUE)$Name[1]
    td <- tempfile("res"); dir.create(td); unzip(path, inner, exdir = td); file.path(td, inner)
  } else path
  d <- fread(con, quote = "", sep = "\t", fill = TRUE, select = 1:15,
             colClasses = list(character = 3))
  setnames(d, c("county", "date", "unit", "gid", "ctype", "contest", "choice", "party", "votefor",
                "ed", "ev", "mail", "prov", "tot", "real"))
  d[, unit := trimws(unit)]
  # Strip thousands separators: "1,234" would otherwise become NA and then count as 0.
  for (v in c("ed", "ev", "mail", "prov", "tot")) set(d, j = v, value = as.numeric(gsub(",", "", as.character(d[[v]]))))
  d[]
}

# Two-way rows of one contest -> cell table: county, unit, real, method, D, R, O, T.
contest_cells <- function(d, keep_party = TRUE) {
  x <- melt(d, id.vars = c("county", "unit", "real", "party", "choice"),
            measure.vars = c("ed", "ev", "mail", "prov"), variable.name = "m", value.name = "v")
  x[, method := METHODS[match(m, c("ed", "ev", "mail", "prov"))]]
  # A blank vote field reads as NA; count it as 0 so one blank cannot turn every total into NA.
  x[, .(D = sum(v[party == "DEM"], na.rm = TRUE), R = sum(v[party == "REP"], na.rm = TRUE), T = sum(v, na.rm = TRUE)),
    by = .(county, unit, real, method)]
}

# Live NC-1 rows, zip source. Returns the same cell table plus L and the three-candidate totals.
live_from_zip <- function(path, cfg) {
  d <- read_results(path)
  d <- d[startsWith(contest, cfg$contest_prefix)]
  if (!nrow(d)) stop("no rows for contest in zip: ", cfg$contest_prefix)
  if (!all(c("DEM", "REP") %in% d$party)) stop("zip contest rows lack a DEM or REP party code: ", paste(unique(d$party), collapse = ", "))
  c26 <- contest_cells(d)
  c26[]
}

# Backup JSON. Field meanings, verified against the 2024 zip for Beaufort (Harris rows match exactly):
# evc = Election Day, ovc = Early Voting (one-stop), avc = Absentee by Mail, pvc = Provisional, vct = total.
# Note evc is NOT early vote.
live_from_json <- function(files, cty_map, cfg, geo = NULL) {
  out <- list()
  for (f in files) {
    id <- as.integer(sub(".*precinct_([0-9]+)\\.txt$", "\\1", f))
    j <- as.data.table(fromJSON(f))
    # An empty county file would silently drop that county's votes; refuse it so the zip is used instead.
    if (!nrow(j)) stop("empty JSON for ", basename(f))
    miss <- setdiff(c("lid", "aid", "bnm", "evc", "ovc", "avc", "pvc"), names(j))
    if (length(miss)) stop("json fields missing in ", basename(f), ": ", toString(miss))
    j <- j[lid == cfg$contest_lid]
    if (!nrow(j)) stop("contest id ", cfg$contest_lid, " absent from ", basename(f))
    j[, county := cty_map$county[match(id, cty_map$id)]]
    out[[f]] <- j
  }
  j <- rbindlist(out, fill = TRUE)
  # The JSON has no party field: a respelled candidate name would silently zero that candidate, so refuse the file.
  if (!all(c(cfg$candidates$D, cfg$candidates$R) %in% j$bnm)) stop("candidate names in json do not match CFG: ", paste(unique(j$bnm), collapse = ", "))
  j[, party := fifelse(bnm == cfg$candidates$D, "DEM", fifelse(bnm == cfg$candidates$R, "REP", "OTH"))]
  for (v in c("evc", "ovc", "avc", "pvc")) set(j, j = v, value = as.numeric(gsub(",", "", j[[v]])))
  x <- melt(j, id.vars = c("county", "aid", "party"), measure.vars = c("evc", "ovc", "avc", "pvc"),
            variable.name = "m", value.name = "v")
  x[, method := METHODS[match(m, c("evc", "ovc", "avc", "pvc"))]]
  r <- x[, .(D = sum(v[party == "DEM"], na.rm = TRUE), R = sum(v[party == "REP"], na.rm = TRUE), T = sum(v, na.rm = TRUE)),
         by = .(county, unit = trimws(aid), method)]
  if (!is.null(geo)) r <- merge(r, geo[, .(county, unit, real)], by = c("county", "unit"), all.x = TRUE)
  else r[, real := NA_character_]
  r[is.na(real), real := "N"]
  r[]
}

# Voter-file extract written by R/extract_voter_file.pl, kept outside Dropbox.
read_extract <- function(y) {
  f <- file.path(RAW, sprintf("ext_%s.tsv", y))
  e <- fread(f, sep = "\t", quote = "", na.strings = "", colClasses = "character")
  e[, accepted := grepl("^ACCEPTED", ballot_rtn_status)]
  e[, m := fifelse(grepl("^EARLY|ONE", ballot_req_type), "Early Voting", "Absentee by Mail")]
  e[]
}

iso_now <- function(now = NULL, tz = "America/New_York") {
  if (is.null(now)) now <- Sys.time()
  s <- format(now, "%Y-%m-%dT%H:%M:%S%z", tz = tz)
  sub("([+-][0-9]{2})([0-9]{2})$", "\\1:\\2", s)
}
parse_iso <- function(s) {
  if (is.null(s) || !length(s) || is.na(s) || !nzchar(s)) return(as.POSIXct(NA))
  s2 <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", s)
  as.POSIXct(s2, format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
}
parse_http_date <- function(s) {
  if (is.null(s) || !length(s) || is.na(s)) return(as.POSIXct(NA))
  old <- Sys.getlocale("LC_TIME"); on.exit(Sys.setlocale("LC_TIME", old)); Sys.setlocale("LC_TIME", "C")
  as.POSIXct(s, format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")
}
