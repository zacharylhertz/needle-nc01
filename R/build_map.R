# Builds the page's county map as one inline SVG fragment, data/baseline/map_svg.html, for needle/index.qmd.
# Sources: Census cartographic boundary files (counties, states, places, 2024, via tigris) and the NC State Board of
# Elections shapefile for the current congressional map, SL 2025-95 (Census 119th CD files are the old map).
# Rscript R/build_map.R [path to folder holding "SL 2025-95.shp"; downloaded to a temp folder if omitted]
suppressPackageStartupMessages({library(sf); library(tigris); library(rmapshaper); library(data.table)})
options(tigris_use_cache = TRUE, tigris_class = "sf"); sf_use_s2(FALSE)
a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
root <- normalizePath(file.path(dirname(sub("^--file=", "", a[1])), ".."))
OFFICIAL <- "https://s3.amazonaws.com/dl.ncsbe.gov/ShapeFiles/USCongress/SL%202025-95%20-%20Shapefile.zip"
args <- commandArgs(TRUE)
shp_dir <- if (length(args)) args[1] else {
  d <- tempfile("sl2025"); dir.create(d); z <- file.path(d, "s.zip"); download.file(OFFICIAL, z, mode = "wb", quiet = TRUE); unzip(z, exdir = d); d }
CRS <- 32119   # NC State Plane, metres
W <- 800       # SVG width in map units
SPLIT <- c("GRANVILLE", "ONSLOW")

keys <- sort(unique(fread(file.path(root, "data/baseline/geo_2026.csv"))$county))
stopifnot(length(keys) == 25)
dist <- st_read(list.files(shp_dir, "SL 2025-95\\.shp$", full.names = TRUE), quiet = TRUE)
cat("District file fields:", names(dist), "\n"); print(st_drop_geometry(dist)[, 1:min(3, ncol(dist) - 1)])
dcol <- intersect(c("DISTRICT", "District", "district", "CD", "DISTRICT_N"), names(dist))[1]
d1 <- st_make_valid(st_transform(dist[as.integer(as.character(dist[[dcol]])) == 1, ], CRS))
stopifnot(nrow(d1) == 1)

cty <- st_transform(counties("NC", cb = TRUE, year = 2024, progress_bar = FALSE), CRS)
cty$key <- toupper(cty$NAME)
# Check the official district against the 25-county list
ov <- suppressWarnings(st_intersection(st_make_valid(cty[, "key"]), st_geometry(d1)))
ov$a <- as.numeric(st_area(ov))
share <- merge(data.table(k = cty$key, total = as.numeric(st_area(cty))), data.table(k = ov$key, a = ov$a)[, .(a = sum(a)), by = k], by = "k")
share[, pct := round(100 * a / total, 2)]
cat("\nCounties with part of the district, share of county land inside (top and split rows):\n")
print(share[order(pct)][pct > 0.5][1:min(8, .N)])
inside <- share[pct > 1, k]
cat("Clipped-district county list equals geo_2026 list:", setequal(inside, keys), "\n")
if (!setequal(inside, keys)) { print(setdiff(inside, keys)); print(setdiff(keys, inside)); stop("district and county list disagree") }
stopifnot(all(share[k %in% setdiff(keys, SPLIT), pct] > 97), all(share[k %in% SPLIT, pct] < 97))

# One layer, so simplification keeps shared borders: whole counties, plus inner and outer parts of the split two.
parts <- cty[!cty$key %in% SPLIT, "key"]; parts$role <- ifelse(parts$key %in% keys, "in", "ctx")
for (k in SPLIT) {
  g <- st_make_valid(st_geometry(cty[cty$key == k, ]))
  inner <- st_collection_extract(st_intersection(g, st_geometry(d1)), "POLYGON")
  outer <- st_collection_extract(st_difference(g, st_geometry(d1)), "POLYGON")
  parts <- rbind(parts, st_sf(key = k, role = "in", geometry = st_union(inner)), st_sf(key = paste0(k, "_OUT"), role = "ctx", geometry = st_union(outer)))
}
KEEP <- as.numeric(Sys.getenv("MAP_KEEP", "0.22"))
simp <- ms_simplify(parts, keep = KEEP, keep_shapes = TRUE, explode = FALSE, weighting = 0.7)
simp <- st_make_valid(simp)
nc1 <- simp[simp$role == "in", ]
stopifnot(setequal(nc1$key, keys), nrow(nc1) == 25)

b <- st_bbox(nc1); dx <- b[["xmax"]] - b[["xmin"]]; dy <- b[["ymax"]] - b[["ymin"]]
pad <- 0.035 * dx
x0 <- b[["xmin"]] - pad; x1 <- b[["xmax"]] + pad; y0 <- b[["ymin"]] - pad; y1 <- b[["ymax"]] + pad
sc <- W / (x1 - x0); H <- round((y1 - y0) * sc)
frame <- st_as_sfc(st_bbox(c(xmin = x0, ymin = y0, xmax = x1, ymax = y1), crs = st_crs(CRS)))
px <- function(x) round((x - x0) * sc, 1); py <- function(y) round((y1 - y) * sc, 1)
num <- function(v) sub("^(-?)0\\.", "\\1.", sub("\\.0$", "", sprintf("%.1f", v)))
ring_d <- function(m, fx = px, fy = py) {
  xs <- fx(m[, 1]); ys <- fy(m[, 2]); keep <- c(TRUE, xs[-1] != xs[-length(xs)] | ys[-1] != ys[-length(ys)])
  xs <- xs[keep]; ys <- ys[keep]; if (length(xs) < 4) return("")
  xs <- xs[-length(xs)]; ys <- ys[-length(ys)]
  paste0("M", paste(num(xs), num(ys), collapse = " ", sep = " "), "Z")
}
geom_d <- function(g, fx = px, fy = py) {
  g <- st_cast(st_sfc(g, crs = st_crs(CRS)), "MULTIPOLYGON")[[1]]
  paste(unlist(lapply(g, function(pl) vapply(pl, ring_d, "", fx = fx, fy = fy))), collapse = "")
}
clip_d <- function(sfc) { i <- suppressWarnings(st_intersection(st_make_valid(sfc), frame)); i <- st_collection_extract(i, "POLYGON"); if (!length(i)) return(""); geom_d(st_union(i)) }

# Neighbouring land
st <- st_transform(states(cb = TRUE, year = 2024, progress_bar = FALSE), CRS)
va <- st[st$STUSPS == "VA", ]
va_s <- st_make_valid(ms_simplify(va, keep = 0.15, keep_shapes = TRUE))
land_d <- clip_d(st_geometry(va_s))
ctx <- simp[simp$role == "ctx", ]
ctx_d <- unlist(lapply(seq_len(nrow(ctx)), function(i) clip_d(st_geometry(ctx)[i])))
ctx_d <- ctx_d[nzchar(ctx_d)]

# County centres: centroid of the largest piece when that falls inside it, otherwise a point on its surface
centre <- function(g) {
  pl <- st_cast(st_sfc(g, crs = st_crs(CRS)), "POLYGON"); big <- pl[which.max(as.numeric(st_area(pl)))]
  ct <- suppressWarnings(st_centroid(big)); if (!as.logical(st_within(ct, big, sparse = FALSE))) ct <- suppressWarnings(st_point_on_surface(big))
  st_coordinates(ct)[1, ]
}
title <- function(k) tools::toTitleCase(tolower(k))
nc1 <- nc1[order(nc1$key), ]
paths <- vapply(seq_len(nrow(nc1)), function(i) {
  ct <- centre(st_geometry(nc1)[[i]])
  sprintf('<path class="cty" data-c="%s" data-n="%s" data-x="%s" data-y="%s" tabindex="0" role="button" d="%s"/>', nc1$key[i], title(nc1$key[i]), num(px(ct[1])), num(py(ct[2])), geom_d(st_geometry(nc1)[[i]]))
}, "")

# Orientation places, checked against the county each should fall in
pl <- st_transform(places("NC", cb = TRUE, year = 2024, progress_bar = FALSE), CRS)
want <- data.table(name = c("Raleigh", "Rocky Mount", "Greenville", "Elizabeth City", "New Bern", "Jacksonville"),
                   county = c("WAKE", "NASH|EDGECOMBE", "PITT", "PASQUOTANK|CAMDEN", "CRAVEN", "ONSLOW"),
                   anchor = c("start", "start", "start", "end", "start", "start"), dx = c(6, 6, 6, -6, 6, 6), dy = c(4, 4, 4, 4, 4, 4))
lab <- character()
for (i in seq_len(nrow(want))) {
  m <- pl[pl$NAME == want$name[i], ]; stopifnot(nrow(m) == 1)
  ct <- suppressWarnings(st_centroid(st_geometry(m)))
  inc <- cty$key[as.logical(st_within(ct, cty, sparse = FALSE))]
  ok <- grepl(paste0("^(", want$county[i], ")$"), inc)
  xy <- st_coordinates(ct)[1, ]
  infr <- xy[1] > x0 && xy[1] < x1 && xy[2] > y0 && xy[2] < y1
  cat(sprintf("Place %-15s lon/lat %.3f, %.3f in county %s, expected %s: %s; in frame: %s\n", want$name[i], st_coordinates(st_transform(ct, 4326))[1], st_coordinates(st_transform(ct, 4326))[2], inc, want$county[i], ok, infr))
  stopifnot(ok)
  if (infr) lab <- c(lab, sprintf('<g class="place" data-p="%s"><circle cx="%s" cy="%s"/><text x="%s" y="%s" text-anchor="%s">%s</text></g>', want$name[i], num(px(xy[1])), num(py(xy[2])), num(px(xy[1])), num(py(xy[2])), want$anchor[i], want$name[i]))
}

# Locator inset: whole state, district marked
nc <- st_make_valid(st[st$STUSPS == "NC", ])
nc_s <- st_make_valid(ms_simplify(nc, keep = 0.012, keep_shapes = TRUE))
d_s <- st_make_valid(ms_simplify(st_union(st_geometry(nc1)) |> st_sf(), keep = 0.05, keep_shapes = TRUE))
bn <- st_bbox(nc_s); IW <- 170; isc <- IW / (bn[["xmax"]] - bn[["xmin"]]); IH <- (bn[["ymax"]] - bn[["ymin"]]) * isc
ifx <- function(x) round((x - bn[["xmin"]]) * isc, 1); ify <- function(y) round((bn[["ymax"]] - y) * isc, 1)
ipoly <- function(g) { g <- st_cast(g, "MULTIPOLYGON"); pcs <- lapply(g[[1]], function(pp) vapply(pp, ring_d, "", fx = ifx, fy = ify)); paste(unlist(pcs), collapse = "") }

svg <- sprintf('<svg class="cmap" id="cmap" viewBox="0 0 %d %d" role="group" aria-describedby="cmap-desc">
<rect width="%d" height="%d" class="water"/>
<path class="land" d="%s"/>
<g class="ctx">%s</g>
<g id="cmap-counties">%s</g>
<g id="cmap-overlay"></g>
<g id="cmap-places">%s</g>
<g class="inset" id="cmap-inset" transform="translate(%d %.0f)"><path class="inset-nc" d="%s"/><path class="inset-d" d="%s"/></g>
</svg>', W, H, W, H, land_d,
  paste(sprintf('<path d="%s"/>', ctx_d), collapse = ""), paste(paths, collapse = "\n"), paste(lab, collapse = ""),
  W - IW - 8, H - IH - 8, ipoly(st_geometry(nc_s)), ipoly(st_geometry(d_s)))
out <- file.path(root, "data/baseline/map_svg.html")
writeLines(svg, out)
cat(sprintf("\nSVG %d x %d, %.1f KB, 25 county keys joined to geo_2026.csv: %s\n", W, H, file.size(out) / 1024, setequal(nc1$key, keys)))
