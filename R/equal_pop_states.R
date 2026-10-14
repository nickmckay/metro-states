# 48 equal-population states for the contiguous US.
#
# Method: a capacity-constrained centroidal power diagram. Each state has a centre c_k and
# a weight w_k; every census tract joins the state minimising |x - c_k|^2 - w_k. Weights are
# tuned until each state holds 1/48 of the population, and centres are repeatedly moved to
# their population's centre of mass. Power-diagram cells are convex, so states come out
# compact rather than long and skinny.
#
# Run from the repo root: Rscript R/equal_pop_states.R

suppressPackageStartupMessages({
  library(sf); library(terra); library(ggplot2); library(maps); library(ggrepel)
})
sf_use_s2(FALSE)
set.seed(48)
K <- 48
crs <- 5070

# ---- tract populations (2020 Census centres of population) ---------------------------
raw <- "data-raw/CenPop2020_Mean_TR.txt"
if (!file.exists(raw)) {
  dir.create("data-raw", showWarnings = FALSE)
  download.file("https://www2.census.gov/geo/docs/reference/cenpop2020/tract/CenPop2020_Mean_TR.txt", raw)
}
tr <- read.csv(raw, colClasses = c("character", "character", "character", "numeric", "numeric", "numeric"),
               fileEncoding = "UTF-8-BOM")
tr <- tr[!tr$STATEFP %in% c("02", "15", "72") & tr$POPULATION > 0, ]
xy <- st_coordinates(st_transform(st_as_sf(tr, coords = c("LONGITUDE", "LATITUDE"), crs = 4326), crs)) / 1000
p <- tr$POPULATION
target <- sum(p) / K
cat(sprintf("%d tracts, %s people, target %s per state\n", nrow(tr), format(sum(p), big.mark = ","),
            format(round(target), big.mark = ",")))

# ---- helpers ----------------------------------------------------------------------------
sqdist <- function(X, C) outer(rowSums(X^2), rowSums(C^2), "+") - 2 * X %*% t(C)
assign_power <- function(X, C, w) max.col(-(sqdist(X, C) - rep(w, each = nrow(X))), ties.method = "first")
wmean <- function(v, a) as.vector(tapply(v * p, factor(a, 1:K), sum) / tapply(p, factor(a, 1:K), sum))
popk <- function(a) { s <- tapply(p, factor(a, 1:K), sum); s[is.na(s)] <- 0; as.vector(s) }

# ---- initialise: population-weighted k-means++ then a few Lloyd steps --------------------
C <- xy[sample(nrow(xy), 1, prob = p), , drop = FALSE]
for (k in 2:K) {
  d <- pmax(apply(sqdist(xy, C), 1, min), 0)
  C <- rbind(C, xy[sample(nrow(xy), 1, prob = p * d), ])
}
w <- rep(0, K)
for (i in 1:30) { a <- assign_power(xy, C, w); C <- cbind(wmean(xy[, 1], a), wmean(xy[, 2], a)) }

# ---- balance + centroid iterations -------------------------------------------------------
# Rprop-style weight tuning: each state has its own step, which grows while its population
# error keeps the same sign and halves when it flips. Robust across tiny city cells and
# huge rural ones.
cell_scale <- function(C, a) {
  d2 <- rowSums((xy - C[a, ])^2)
  sc <- tapply(d2 * p, factor(a, 1:K), sum) / pmax(popk(a), 1)
  sc[is.na(sc) | sc == 0] <- median(sc, na.rm = TRUE)
  as.vector(sc)
}
balance <- function(C, w, step, iters, tol) {
  last <- rep(0, K)
  for (i in seq_len(iters)) {
    a <- assign_power(xy, C, w)
    dev <- (popk(a) - target) / target
    if (max(abs(dev)) < tol) break
    sg <- sign(dev)
    step <- ifelse(last == 0, step, ifelse(sg == last, step * 1.2, step * 0.5))
    w <- w - sg * step
    last <- sg
  }
  list(w = w, a = a, dev = dev, step = step)
}
fit_file <- "data-raw/equal_pop_fit.rds"
if (file.exists(fit_file) && Sys.getenv("REFIT") != "1") {
  fit <- readRDS(fit_file); C <- fit$C; w <- fit$w  # reuse saved centres (set REFIT=1 to redo the search)
} else {
  a <- assign_power(xy, C, w)
  step <- 0.05 * cell_scale(C, a)
  for (outer in 1:60) {
    b <- balance(C, w, step, 80, 0.01)
    w <- b$w
    step <- pmax(b$step, 0.01 * cell_scale(C, b$a))  # don't let steps collapse between centre moves
    newC <- cbind(wmean(xy[, 1], b$a), wmean(xy[, 2], b$a))
    moved <- max(sqrt(rowSums((newC - C)^2)))
    C <- C + 0.7 * (newC - C)  # damped move toward each state's centre of population
    if (outer %% 5 == 0) cat(sprintf("outer %d: max centre move %.1f km, max pop error %.2f%%\n",
                                     outer, moved, 100 * max(abs(b$dev))))
    if (moved < 1 && max(abs(b$dev)) < 0.01) break
  }
}
# Final balance: tract-level repair. The power diagram gets every state within a few percent;
# discrete tracts make exact weights hard to find, so finish like redistricting tools do. The state
# furthest from its target trades its cheapest border tract with a neighbour, always moving people
# from the more over-populated state to the less, so the process can't cycle.
repair <- function(C, w, tol = 0.004, max_moves = 20000) {
  D <- sqdist(xy, C) - rep(w, each = nrow(xy))
  a <- max.col(-D, ties.method = "first")
  top <- t(apply(D, 1, order)[1:2, ])        # each tract's best and runner-up states
  P <- popk(a)
  # one move: returns TRUE if state k could trade a tract with a suitable neighbour
  try_move <- function(k, dev) {
    if (dev[k] > 0) {                          # give a tract away
      # only border tracts: the receiving state must be the tract's runner-up (or its power owner,
      # for a tract moved earlier), so trades never create exclaves
      mine <- which(a == k)
      nb <- setdiff(unique(c(top[mine, 1], top[mine, 2])), k)
      nb <- nb[dev[nb] < dev[k]]
      if (!length(nb)) return(FALSE)
      j <- nb[which.min(dev[nb])]
      # a move only helps if the tract is smaller than the gap between the two states
      mine <- mine[(top[mine, 1] == j | top[mine, 2] == j) & p[mine] < P[k] - P[j] & movable[mine]]
      if (!length(mine)) return(FALSE)
      i <- mine[which.min((D[mine, j] - D[mine, k]) / p[mine])]   # least distortion per person moved
      from <- k; to <- j
    } else {                                   # take a tract from a neighbour
      cand <- which(a != k & (top[, 1] == k | top[, 2] == k))
      nb <- unique(a[cand]); nb <- nb[dev[nb] > dev[k]]
      if (!length(nb)) return(FALSE)
      j <- nb[which.max(dev[nb])]
      theirs <- cand[a[cand] == j & p[cand] < P[j] - P[k] & movable[cand]]
      if (!length(theirs)) return(FALSE)
      i <- theirs[which.min((D[theirs, k] - D[theirs, j]) / p[theirs])]
      from <- j; to <- k
    }
    a[i] <<- to; P[from] <<- P[from] - p[i]; P[to] <<- P[to] + p[i]
    TRUE
  }
  for (m in seq_len(max_moves)) {
    dev <- (P - target) / target
    if (max(abs(dev)) < tol) break
    # work on the worst state that can actually trade; stop when none can
    ok <- FALSE
    for (k in order(-abs(dev))) if (abs(dev[k]) >= tol && try_move(k, dev)) { ok <- TRUE; break }
    if (!ok) break
  }
  power_a <- max.col(-D, ties.method = "first")
  cat(sprintf("repair: moved %d tracts (%s people)\n", sum(a != power_a), format(sum(p[a != power_a]), big.mark = ",")))
  list(a = a, power_a = power_a, dev = (popk(a) - target) / target)
}
# Each tract's Voronoi catchment: used to keep trades to small, dense tracts, and to redraw moved ones
states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) |> st_make_valid() |> st_transform(crs)
conus <- st_union(states) |> st_make_valid()
tpts <- st_as_sf(data.frame(x = xy[, 1] * 1000, y = xy[, 2] * 1000, id = seq_len(nrow(xy))), coords = c("x", "y"), crs = crs)
vor <- st_collection_extract(st_voronoi(st_union(tpts), envelope = st_as_sfc(st_bbox(conus))), "POLYGON")
vor <- st_sf(geometry = vor)
vor$tract <- unlist(st_intersects(vor, tpts))
vor_area <- numeric(nrow(xy)); vor_area[vor$tract] <- as.numeric(st_area(vor)) / 1e6
movable <- vor_area < 300   # km²; moving bigger rural catchments leaves visible jagged chunks
b <- repair(C, w)
a <- b$a
cat(sprintf("final max population error: %.2f%%\n", 100 * max(abs(b$dev))))

# ---- polygons: assign a 2 km land grid with the same power rule ---------------------------
r <- rast(ext(vect(conus)), resolution = 2000, crs = st_crs(crs)$wkt)
gxy <- xyFromCell(r, 1:ncell(r)) / 1000
lab <- integer(nrow(gxy))
for (s in split(seq_len(nrow(gxy)), ceiling(seq_len(nrow(gxy)) / 2e5))) lab[s] <- assign_power(gxy[s, , drop = FALSE], C, w)
values(r) <- lab
r <- mask(r, vect(conus))
# Moved tracts: reassign the part of each one's Voronoi catchment that lies in its old state
mv <- which(a != b$power_a)
if (length(mv)) {
  vor <- vor[vor$tract %in% mv, ]  # catchments computed before the repair
  newlab <- rasterize(vect(vor), r, field = a[vor$tract], touches = FALSE)
  oldlab <- rasterize(vect(vor), r, field = b$power_a[vor$tract], touches = FALSE)
  swap <- which(!is.na(values(newlab)) & values(r) == values(oldlab))
  r[swap] <- values(newlab)[swap]
}
regions <- st_as_sf(as.polygons(r, dissolve = TRUE)) |> setNames(c("id", "geometry"))
regions <- st_intersection(st_make_valid(regions), conus)
regions$pop <- popk(a)[regions$id]
regions$area_km2 <- as.numeric(st_area(regions)) / 1e6

# ---- name each state after the largest city inside it --------------------------------------
data(us.cities, package = "maps")
uc <- us.cities[!us.cities$country.etc %in% c("AK", "HI"), ]
uc_sf <- st_transform(st_as_sf(uc, coords = c("long", "lat"), crs = 4326), crs)
hit <- st_intersects(regions, uc_sf)
regions$name <- sapply(hit, function(h) {
  if (!length(h)) return(NA)
  top <- h[which.max(uc$pop[h])]
  nm <- sub(" [A-Z]{2}$", "", uc$name[top])
  if (nm == toupper(nm)) nm <- tools::toTitleCase(tolower(nm))  # capitals are upper-cased in us.cities
  sub("^Saint ", "St. ", nm)
})
regions$city_lon <- sapply(hit, function(h) uc$long[h[which.max(uc$pop[h])]])
regions$city_lat <- sapply(hit, function(h) uc$lat[h[which.max(uc$pop[h])]])

# Compactness: Polsby-Popper score 4*pi*A/P^2 (1 = circle)
regions$polsby_popper <- round(4 * pi * as.numeric(st_area(regions)) /
                                 as.numeric(st_length(st_cast(regions, "MULTILINESTRING")))^2, 3)

# ---- colours + plot ---------------------------------------------------------------------
adj <- st_intersects(st_buffer(regions, 3000))
adj <- lapply(seq_along(adj), function(i) setdiff(adj[[i]], i))
pal <- c("#8dd3c7", "#ffed6f", "#bebada", "#fb8072", "#80b1d3",
         "#fdb462", "#b3de69", "#fccde5", "#bc80bd", "#ccebc5")
col <- integer(nrow(regions))
for (i in order(-lengths(adj))) {
  ok <- setdiff(seq_along(pal), col[adj[[i]]])
  col[i] <- ok[which.min(tabulate(col, length(pal))[ok])]
}
regions$fill <- pal[col]

pts <- st_transform(st_as_sf(st_drop_geometry(regions), coords = c("city_lon", "city_lat"), crs = 4326), crs)
pts <- cbind(pts, st_coordinates(pts))
pts$label <- pts$name

map_layers <- function() list(
  geom_sf(data = regions, aes(fill = fill), colour = "grey20", linewidth = 0.35),
  geom_sf(data = states, fill = NA, colour = "grey45", linewidth = 0.15, linetype = "dashed"),
  scale_fill_identity()
)
p_main <- ggplot() + map_layers() +
  geom_sf(data = pts, size = 1.2) +
  geom_text_repel(data = pts, aes(X, Y, label = label), size = 2.9, fontface = "bold",
                  bg.color = "white", bg.r = 0.12, seed = 4, max.overlaps = Inf,
                  min.segment.length = 0.2, box.padding = 0.25) +
  coord_sf(crs = crs, datum = NA) +
  labs(title = "48 States, Equal Population",
       subtitle = sprintf("Each state holds 1/48 of the contiguous US population (about %s people), drawn as compactly as possible",
                          format(round(target, -3), big.mark = ",")),
       caption = "Population: 2020 Census tracts. States named for their largest city. Dashed lines: current state borders.") +
  theme_void(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, colour = "grey30"),
        plot.caption = element_text(colour = "grey40"),
        plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(10, 10, 10, 10))
bbi <- st_bbox(st_transform(st_as_sfc(st_bbox(c(xmin = -77.8, xmax = -71.0, ymin = 38.6, ymax = 42.4), crs = 4326)), crs))
ins_pts <- pts[st_coordinates(pts)[, 1] > bbi[1] & st_coordinates(pts)[, 1] < bbi[3] &
               st_coordinates(pts)[, 2] > bbi[2] & st_coordinates(pts)[, 2] < bbi[4], ]
inset <- ggplot() + map_layers() +
  geom_sf(data = ins_pts, size = 1.2) +
  geom_text_repel(data = ins_pts, aes(X, Y, label = label), size = 2.6, fontface = "bold",
                  bg.color = "white", bg.r = 0.12, seed = 1, max.overlaps = Inf) +
  coord_sf(crs = crs, datum = NA, xlim = bbi[c(1, 3)], ylim = bbi[c(2, 4)], expand = FALSE) +
  labs(title = "New York - Washington detail") +
  theme_void(base_size = 9) +
  theme(panel.border = element_rect(fill = NA, colour = "grey30"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = "white", colour = NA))
full <- st_bbox(regions)
wd <- full[3] - full[1]; ht <- full[4] - full[2]
ih <- 0.36 * ht; iw <- ih * (bbi[3] - bbi[1]) / (bbi[4] - bbi[2]) * 1.02
ix0 <- full[1] - 0.10 * wd; iy0 <- full[2] - 0.05 * ht
p_main <- p_main + annotation_custom(ggplotGrob(inset), xmin = ix0, xmax = ix0 + iw, ymin = iy0, ymax = iy0 + ih) +
  coord_sf(crs = crs, datum = NA, xlim = c(ix0, full[3]), ylim = c(iy0, full[4]), expand = TRUE)
ggsave("img/equal_pop_states.png", p_main, width = 16, height = 10.5, dpi = 200)

out <- st_drop_geometry(regions)[, c("name", "pop", "area_km2", "polsby_popper")]
out$area_km2 <- round(out$area_km2)
out <- out[order(-out$area_km2), ]
write.csv(out, "data/equal_pop_states.csv", row.names = FALSE)
saveRDS(list(C = C, w = w, regions = regions), "data-raw/equal_pop_fit.rds")
print(out, row.names = FALSE)
