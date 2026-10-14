suppressPackageStartupMessages({
  library(sf); library(terra); library(ggplot2); library(maps); library(ggrepel)
})
sf_use_s2(FALSE)

# 48 most populous metropolitan statistical areas in the contiguous US (2020 Census).
# Each metro is placed at its first-named principal city. Honolulu (~#55) falls below the cut.
cities <- read.csv(text = "
name,pop,lat,lon
New York,20140470,40.7128,-74.0060
Los Angeles,13200998,34.0522,-118.2437
Chicago,9618502,41.8781,-87.6298
Dallas-Fort Worth,7637387,32.7767,-96.7970
Houston,7122240,29.7604,-95.3698
Washington,6385162,38.9072,-77.0369
Philadelphia,6245051,39.9526,-75.1652
Miami,6138333,25.7617,-80.1918
Atlanta,6089815,33.7490,-84.3880
Boston,4941632,42.3601,-71.0589
Phoenix,4845832,33.4484,-112.0740
San Francisco,4749008,37.7749,-122.4194
Riverside,4599839,33.9806,-117.3755
Detroit,4392041,42.3314,-83.0458
Seattle,4018762,47.6062,-122.3321
Minneapolis,3690261,44.9778,-93.2650
San Diego,3298634,32.7157,-117.1611
Tampa,3175275,27.9506,-82.4572
Denver,2963821,39.7392,-104.9903
Baltimore,2844510,39.2904,-76.6122
St. Louis,2820253,38.6270,-90.1994
Orlando,2673376,28.5383,-81.3792
Charlotte,2660329,35.2271,-80.8431
San Antonio,2558143,29.4241,-98.4936
Portland,2512859,45.5152,-122.6784
Sacramento,2397382,38.5816,-121.4944
Pittsburgh,2370930,40.4406,-79.9959
Austin,2283371,30.2672,-97.7431
Las Vegas,2265461,36.1699,-115.1398
Cincinnati,2256884,39.1031,-84.5120
Kansas City,2192035,39.0997,-94.5786
Columbus,2138926,39.9612,-82.9988
Indianapolis,2111040,39.7684,-86.1581
Cleveland,2088251,41.4993,-81.6944
San Jose,2000468,37.3382,-121.8863
Nashville,1989519,36.1627,-86.7816
Virginia Beach,1799674,36.8529,-75.9780
Providence,1676579,41.8240,-71.4128
Jacksonville,1605848,30.3322,-81.6557
Milwaukee,1574731,43.0389,-87.9065
Oklahoma City,1425695,35.4676,-97.5164
Raleigh,1413982,35.7796,-78.6382
Memphis,1337779,35.1495,-90.0490
Richmond,1314434,37.5407,-77.4360
Louisville,1285439,38.2527,-85.7585
New Orleans,1271845,29.9511,-90.0715
Salt Lake City,1257936,40.7608,-111.8910
Hartford,1213531,41.7658,-72.6734
", strip.white = TRUE)
stopifnot(nrow(cities) == 48)

crs <- 5070  # CONUS Albers equal-area
states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) |>
  st_make_valid() |> st_transform(crs)
conus <- st_union(states) |> st_make_valid()

# Raster at ~2 km; assign each cell to the nearest city by true great-circle distance
r <- rast(ext(vect(conus)), resolution = 2000, crs = st_crs(crs)$wkt)
xy <- xyFromCell(r, 1:ncell(r))
ll <- st_coordinates(st_transform(st_as_sf(data.frame(xy), coords = c("x", "y"), crs = crs), 4326))
rad <- pi / 180
lat1 <- ll[, 2] * rad; lon1 <- ll[, 1] * rad
best <- rep(Inf, nrow(ll)); idx <- integer(nrow(ll))
for (i in seq_len(nrow(cities))) {
  lat2 <- cities$lat[i] * rad; lon2 <- cities$lon[i] * rad
  a <- sin((lat2 - lat1) / 2)^2 + cos(lat1) * cos(lat2) * sin((lon2 - lon1) / 2)^2
  d <- 2 * asin(pmin(1, sqrt(a)))
  upd <- d < best; best[upd] <- d[upd]; idx[upd] <- i
}
values(r) <- idx
r <- mask(r, vect(conus))

regions <- st_as_sf(as.polygons(r, dissolve = TRUE)) |> setNames(c("id", "geometry"))
regions <- st_intersection(st_make_valid(regions), conus)  # smooth coastline edges
regions$name <- cities$name[regions$id]
regions$area_km2 <- as.numeric(st_area(regions)) / 1e6

# Greedy graph colouring so neighbouring regions never share a colour
adj <- st_intersects(st_buffer(regions, 3000))
adj <- lapply(seq_along(adj), function(i) setdiff(adj[[i]], i))
pal <- c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3",
         "#fdb462", "#b3de69", "#fccde5", "#d9d9d9", "#ccebc5")
col <- integer(nrow(regions))
for (i in order(-lengths(adj))) {
  used <- col[adj[[i]]]
  col[i] <- which(!seq_along(pal) %in% used)[1]
}
regions$fill <- pal[col]

pts <- st_transform(st_as_sf(cities, coords = c("lon", "lat"), crs = 4326), crs)
pts <- cbind(pts, st_coordinates(pts))

p <- ggplot() +
  geom_sf(data = regions, aes(fill = fill), colour = "grey25", linewidth = 0.35) +
  geom_sf(data = states, fill = NA, colour = "grey45", linewidth = 0.15, linetype = "dashed") +
  geom_sf(data = pts, size = 1.4, colour = "black") +
  geom_text_repel(data = pts, aes(X, Y, label = name), size = 2.9, fontface = "bold",
                  bg.color = "white", bg.r = 0.12, min.segment.length = 0.2,
                  max.overlaps = Inf, seed = 4, box.padding = 0.25, point.padding = 0.1) +
  scale_fill_identity() +
  coord_sf(crs = crs, datum = NA) +
  labs(title = "The United States of Nearest Big Metro",
       subtitle = "Each region is the area closer (great-circle distance) to its city than to any other of the 48 most populous metro areas in the contiguous US",
       caption = "Metro populations: 2020 Census MSAs; each metro anchored at its principal city. Dashed lines: current state borders. Albers equal-area projection.") +
  theme_void(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, colour = "grey30"),
        plot.caption = element_text(colour = "grey40"),
        plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(10, 10, 10, 10))

# Inset: California's crowded coast (SF/Oakland/San Jose and LA/Long Beach)
bb <- st_bbox(st_transform(st_as_sfc(st_bbox(c(xmin = -80.8, xmax = -70.5, ymin = 36.4, ymax = 43.2), crs = 4326)), crs))
ins_pts <- pts[pts$name %in% c("New York","Philadelphia","Baltimore","Washington","Richmond","Virginia Beach","Hartford","Providence","Boston","Pittsburgh"), ]
inset <- ggplot() +
  geom_sf(data = regions, aes(fill = fill), colour = "grey25", linewidth = 0.35) +
  geom_sf(data = states, fill = NA, colour = "grey45", linewidth = 0.15, linetype = "dashed") +
  geom_sf(data = ins_pts, size = 1.2) +
  geom_text_repel(data = ins_pts, aes(X, Y, label = name), size = 2.6, fontface = "bold",
                  bg.color = "white", bg.r = 0.12, seed = 1, max.overlaps = Inf) +
  scale_fill_identity() +
  coord_sf(crs = crs, datum = NA, xlim = bb[c(1, 3)], ylim = bb[c(2, 4)], expand = FALSE) +
  labs(title = "Northeast detail") +
  theme_void(base_size = 9) +
  theme(panel.border = element_rect(fill = NA, colour = "grey30"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = "white", colour = NA))
full <- st_bbox(regions)
w <- full[3] - full[1]; h <- full[4] - full[2]
ih <- 0.36 * h; iw <- ih * (bb[3] - bb[1]) / (bb[4] - bb[2]) * 1.02
ix0 <- full[1] - 0.10 * w; iy0 <- full[2] - 0.05 * h
p <- p + annotation_custom(ggplotGrob(inset), xmin = ix0, xmax = ix0 + iw, ymin = iy0, ymax = iy0 + ih) +
  coord_sf(crs = crs, datum = NA, xlim = c(ix0, full[3]), ylim = c(iy0, full[4]), expand = TRUE)
ggsave("img/metro_states.png", p, width = 16, height = 10.5, dpi = 200)

out <- st_drop_geometry(regions)[, c("name", "area_km2")]
out <- out[order(-out$area_km2), ]
out$area_km2 <- round(out$area_km2)
write.csv(out, "data/metro_states_areas.csv", row.names = FALSE)
print(head(out, 8)); print(tail(out, 6))
