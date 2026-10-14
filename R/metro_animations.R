# Two GIFs for the metro-region map:
#   metro_bubbles.gif    - bubbles grow out of each metro until they collide
#   states_to_metros.gif - today's states morph into the metro regions
# Reuses the city table, CONUS outline and region polygons from metro_states.R.

src <- readLines("R/metro_states.R")
eval(parse(text = src[1:(grep("^# Greedy graph", src) - 1)]))
suppressPackageStartupMessages(library(gifski))

# ---- colours -----------------------------------------------------------------
# Greedy colouring, but pick the least-used allowed colour so all hues get used
colour_graph <- function(polys, pal) {
  adj <- st_intersects(st_buffer(polys, 3000))
  adj <- lapply(seq_along(adj), function(i) setdiff(adj[[i]], i))
  col <- integer(nrow(polys))
  for (i in order(-lengths(adj))) {
    ok <- setdiff(seq_along(pal), col[adj[[i]]])
    col[i] <- ok[which.min(tabulate(col, length(pal))[ok])]
  }
  pal[col]
}
metro_pal <- c("#8dd3c7", "#ffed6f", "#bebada", "#fb8072", "#80b1d3",
               "#fdb462", "#b3de69", "#fccde5", "#bc80bd", "#ccebc5")
state_pal <- c("#d9cbb0", "#b9c9a7", "#c9b3b8", "#a9bfcc", "#e0d8a6", "#c4b6d6")
regions$fill <- colour_graph(regions, metro_pal)
states$fill <- colour_graph(states, state_pal)

# ---- animation grid (4 km) ------------------------------------------------------
g <- rast(ext(vect(conus)), resolution = 4000, crs = st_crs(crs)$wkt)
xy <- xyFromCell(g, 1:ncell(g))
ll <- st_coordinates(st_transform(st_as_sf(data.frame(xy), coords = c("x", "y"), crs = crs), 4326))
lat1 <- ll[, 2] * rad; lon1 <- ll[, 1] * rad
dist <- rep(Inf, nrow(ll)); near <- integer(nrow(ll))
for (i in seq_len(nrow(cities))) {
  lat2 <- cities$lat[i] * rad; lon2 <- cities$lon[i] * rad
  a <- sin((lat2 - lat1) / 2)^2 + cos(lat1) * cos(lat2) * sin((lon2 - lon1) / 2)^2
  d <- 6371 * 2 * asin(pmin(1, sqrt(a)))
  upd <- d < dist; dist[upd] <- d[upd]; near[upd] <- i
}
land <- !is.na(values(rasterize(vect(conus), g, touches = TRUE))[, 1])
state_id <- values(rasterize(vect(states), g, field = seq_len(nrow(states)), touches = TRUE))[, 1]
if (any(land & is.na(state_id))) state_id[land & is.na(state_id)] <- nearest(vect(xy[land & is.na(state_id), , drop = FALSE], crs = crs(g)), vect(states))$to_id
metro_col <- regions$fill[match(near, regions$id)]
state_col <- states$fill[state_id]
gext <- as.vector(ext(g))  # xmin, xmax, ymin, ymax

to_rgb <- function(cols) t(col2rgb(cols)) / 255
metro_rgb <- to_rgb(ifelse(land, metro_col, "white"))
state_rgb <- to_rgb(ifelse(land, state_col, "white"))

as_image <- function(cols) matrix(cols, nrow = nrow(g), ncol = ncol(g), byrow = TRUE)

# ---- shared plotting pieces -------------------------------------------------------
pts <- st_transform(st_as_sf(cities, coords = c("lon", "lat"), crs = 4326), crs)
pts <- cbind(pts, st_coordinates(pts))
lab_states <- st_point_on_surface(states)
lab_states$abb <- state.abb[match(tools::toTitleCase(lab_states$ID), state.name)]
lab_states$abb[lab_states$ID == "district of columbia"] <- NA
lab_states <- cbind(lab_states, st_coordinates(lab_states))
lab_states <- lab_states[!is.na(lab_states$abb), ]

bb <- st_bbox(conus)
base_plot <- function(img, title, subtitle) {
  ggplot() +
    annotation_raster(img, gext[1], gext[2], gext[3], gext[4], interpolate = FALSE) +
    coord_sf(crs = crs, datum = NA, xlim = bb[c(1, 3)], ylim = bb[c(2, 4)], expand = TRUE) +
    labs(title = title, subtitle = subtitle) +
    theme_void(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, colour = "grey30", size = 12),
          plot.background = element_rect(fill = "white", colour = NA),
          plot.margin = margin(8, 8, 8, 8))
}
metro_labels <- function(alpha = 1) {
  list(geom_point(data = pts, aes(X, Y), size = 1.2, alpha = alpha),
       geom_text_repel(data = pts, aes(X, Y, label = name), size = 2.7, fontface = "bold",
                       bg.color = "white", bg.r = 0.12, seed = 4, max.overlaps = Inf,
                       min.segment.length = 0.2, box.padding = 0.2, alpha = alpha))
}
ease <- function(t) ifelse(t < 0.5, 4 * t^3, 1 - (-2 * t + 2)^3 / 2)  # cubic in-out

W <- 11; H <- 7.4; DPI <- 100
render <- function(plots, file, delay) {
  dir <- tempfile("frames"); dir.create(dir)
  files <- sprintf("%s/f%04d.png", dir, seq_along(plots))
  for (i in seq_along(plots)) {
    ggsave(files[i], plots[[i]], width = W, height = H, dpi = DPI)
    if (i %% 20 == 0) message(file, ": frame ", i, "/", length(plots))
  }
  gifski(files, file, width = W * DPI, height = H * DPI, delay = delay, progress = FALSE)
}

if (Sys.getenv("SKIP_BUBBLES") != "1") {
# ---- GIF 1: growing bubbles --------------------------------------------------------
maxd <- max(dist[land])
unclaimed <- "#ececec"; front_w <- 25  # km-wide darker rim on each growing bubble
rim_rgb <- metro_rgb * 0.72
n_grow <- 70; n_hold <- 30
frames1 <- list()
for (k in seq_len(n_grow)) {
  # pace by land area claimed rather than radius, so the action is spread evenly
  r <- if (k == n_grow) maxd * 1.001 else quantile(dist[land], (k / n_grow)^1.15, names = FALSE)
  claimed <- land & dist <= r
  rim <- claimed & dist > r - front_w & k < n_grow
  rgbm <- metro_rgb; rgbm[rim, ] <- rim_rgb[rim, ]
  cols <- ifelse(land, unclaimed, "white")
  cols[claimed] <- rgb(rgbm[claimed, , drop = FALSE])
  frames1[[k]] <- base_plot(as_image(cols), "The United States of Nearest Big Metro",
                            sprintf("Every point joins the first of the 48 largest metros to reach it  -  radius %s km",
                                    format(round(r), big.mark = ","))) +
    geom_sf(data = states, fill = NA, colour = "grey55", linewidth = 0.15, linetype = "dashed") +
    metro_labels()
}
final1 <- base_plot(as_image(rgb(metro_rgb)), "The United States of Nearest Big Metro",
                    "Each region is the land closer to its metro than to any other of the 48 largest in the contiguous US") +
  geom_sf(data = regions, fill = NA, colour = "grey20", linewidth = 0.35) +
  geom_sf(data = states, fill = NA, colour = "grey55", linewidth = 0.15, linetype = "dashed") +
  metro_labels()
frames1 <- c(frames1, rep(list(final1), n_hold))
render(frames1, "img/metro_bubbles.gif", delay = 0.14)  # ~7 fps
}

# ---- GIF 2: each state morphs into a metro region ----------------------------------
# Pair the 48 states one-to-one with the 48 metro regions, favouring pairs that share
# land and sit close together, then tween every state's outline into its partner's.
library(transformr)
st48 <- states[states$ID != "district of columbia", ]
st48 <- st48[order(st48$ID), ]
reg <- regions[order(regions$id), ]
stopifnot(nrow(st48) == 48, nrow(reg) == 48)

overlap <- matrix(0, 48, 48)
hits <- st_intersects(st48, reg)
for (i in 1:48) for (j in hits[[i]])
  overlap[i, j] <- as.numeric(st_area(st_intersection(st_geometry(st48)[i], st_geometry(reg)[j]))) / 1e6
cdist <- as.matrix(dist(rbind(st_coordinates(st_centroid(st_geometry(st48))),
                              st_coordinates(st_centroid(st_geometry(reg))))))[1:48, 49:96] / 1000
# Squared distance favours many short shifts over one cross-country leap; shared land earns a bonus
cost <- (cdist / 100)^2 - overlap / 5000

# Exact minimum-cost assignment (Hungarian algorithm, O(n^3))
hungarian <- function(C) {
  n <- nrow(C); u <- v <- numeric(n + 1); p <- way <- integer(n + 1)
  for (i in 1:n) {
    p[1] <- i; j0 <- 0; minv <- rep(Inf, n + 1); used <- rep(FALSE, n + 1)
    repeat {
      used[j0 + 1] <- TRUE; i0 <- p[j0 + 1]; delta <- Inf; j1 <- 0
      for (j in 1:n) if (!used[j + 1]) {
        cur <- C[i0, j] - u[i0 + 1] - v[j + 1]
        if (cur < minv[j + 1]) { minv[j + 1] <- cur; way[j + 1] <- j0 }
        if (minv[j + 1] < delta) { delta <- minv[j + 1]; j1 <- j }
      }
      for (j in 0:n) if (used[j + 1]) { u[p[j + 1] + 1] <- u[p[j + 1] + 1] + delta; v[j + 1] <- v[j + 1] - delta } else minv[j + 1] <- minv[j + 1] - delta
      j0 <- j1
      if (p[j0 + 1] == 0) break
    }
    repeat { j1 <- way[j0 + 1]; p[j0 + 1] <- p[j1 + 1]; j0 <- j1; if (j0 == 0) break }
  }
  assign <- integer(n); for (j in 1:n) assign[p[j + 1]] <- j
  assign
}
match <- hungarian(cost)
stopifnot(!anyDuplicated(match))
pairs <- data.frame(state = st48$ID, metro = reg$name[match])
write.csv(pairs, "data/state_metro_pairs.csv", row.names = FALSE)

# Drop small islands (< 10% of a shape's main body) so they don't fly around mid-morph
drop_islands <- function(g) {
  parts <- st_cast(st_sfc(g, crs = crs), "POLYGON")
  a <- as.numeric(st_area(parts))
  st_union(parts[a >= 0.1 * max(a)])[[1]]
}
prep <- function(x) {
  g <- st_simplify(st_geometry(x), dTolerance = 4000, preserveTopology = TRUE)
  st_cast(st_sfc(lapply(g, drop_islands), crs = crs), "MULTIPOLYGON")
}
from <- st_sf(pair = 1:48, geometry = prep(st48))
to <- st_sf(pair = 1:48, geometry = prep(reg[match, ]))

n_states_hold <- 14; n_morph <- 60; n_end <- 24
tw <- tween_sf(from, to, ease = "cubic-in-out", nframes = n_morph, id = pair)
tw <- st_set_crs(st_as_sf(tw), crs)

from_fill <- st48$fill; to_fill <- reg$fill[match]
mix_cols <- function(a, b, p) rgb((1 - p) * t(col2rgb(a)) / 255 + p * t(col2rgb(b)) / 255)

start_xy <- st_coordinates(st_point_on_surface(st_geometry(st48)))
end_xy <- as.matrix(st_drop_geometry(pts)[match(reg$name[match], pts$name), c("X", "Y")])
abb <- state.abb[match(tools::toTitleCase(st48$ID), state.name)]

frame_plot <- function(geom, fills, title, subtitle) {
  ggplot() +
    geom_sf(data = conus, fill = "#f2f2f2", colour = NA) +
    geom_sf(data = geom, fill = fills, colour = "grey20", linewidth = 0.35, alpha = 0.92) +
    coord_sf(crs = crs, datum = NA, xlim = bb[c(1, 3)], ylim = bb[c(2, 4)], expand = TRUE) +
    labs(title = title, subtitle = subtitle) +
    theme_void(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, colour = "grey30", size = 12),
          plot.background = element_rect(fill = "white", colour = NA),
          plot.margin = margin(8, 8, 8, 8))
}

start <- frame_plot(st_geometry(st48), from_fill, "From 48 States...",
                    "Each state is about to become one of the 48 metro regions") +
  annotate("text", start_xy[, 1], start_xy[, 2], label = abb, size = 3, fontface = "bold", colour = "grey20")
frames2 <- rep(list(start), n_states_hold)
for (k in seq_len(n_morph)) {
  t <- (k - 1) / (n_morph - 1)
  te <- ease(t)
  g <- st_geometry(tw[tw$.frame == k, ][order(tw$pair[tw$.frame == k]), ])
  xy <- start_xy * (1 - te) + end_xy * te
  lab <- if (te < 0.5) abb else reg$name[match]
  frames2[[length(frames2) + 1]] <-
    frame_plot(g, mix_cols(from_fill, to_fill, te), if (te < 0.5) "From 48 States..." else "...to 48 Metro Regions",
               "Every state slides and reshapes into the metro region it overlaps most") +
    annotate("text", xy[, 1], xy[, 2], label = lab, size = if (te < 0.5) 3 else 2.5, fontface = "bold",
             colour = "grey15", alpha = abs(2 * te - 1))
}
end <- frame_plot(st_geometry(reg), reg$fill, "...to 48 Metro Regions",
                  "Each region is the land closer to its metro than to any other of the 48 largest") +
  geom_sf(data = states, fill = NA, colour = "grey45", linewidth = 0.15, linetype = "dashed") +
  metro_labels()
frames2 <- c(frames2, rep(list(end), n_end))
render(frames2, "img/states_to_metros.gif", delay = 0.14)  # ~7 fps
