suppressPackageStartupMessages({
  library(sf); library(terra); library(ggplot2); library(maps); library(ggrepel)
})
sf_use_s2(FALSE)

# 48 most populous cities in the contiguous US (2020 Census, city proper).
# Honolulu and Anchorage both fall below #48, so no exclusions were needed.
cities <- read.csv(text = "
name,pop,lat,lon
New York,8804190,40.7128,-74.0060
Los Angeles,3898747,34.0522,-118.2437
Chicago,2746388,41.8781,-87.6298
Houston,2304580,29.7604,-95.3698
Phoenix,1608139,33.4484,-112.0740
Philadelphia,1603797,39.9526,-75.1652
San Antonio,1434625,29.4241,-98.4936
San Diego,1386932,32.7157,-117.1611
Dallas,1304379,32.7767,-96.7970
San Jose,1013240,37.3382,-121.8863
Austin,961855,30.2672,-97.7431
Jacksonville,949611,30.3322,-81.6557
Fort Worth,918915,32.7555,-97.3308
Columbus,905748,39.9612,-82.9988
Indianapolis,887642,39.7684,-86.1581
Charlotte,874579,35.2271,-80.8431
San Francisco,873965,37.7749,-122.4194
Seattle,737015,47.6062,-122.3321
Denver,715522,39.7392,-104.9903
Washington,689545,38.9072,-77.0369
Nashville,689447,36.1627,-86.7816
Oklahoma City,681054,35.4676,-97.5164
El Paso,678815,31.7619,-106.4850
Boston,675647,42.3601,-71.0589
Portland,652503,45.5152,-122.6784
Las Vegas,641903,36.1699,-115.1398
Detroit,639111,42.3314,-83.0458
Memphis,633104,35.1495,-90.0490
Louisville,633045,38.2527,-85.7585
Baltimore,585708,39.2904,-76.6122
Milwaukee,577222,43.0389,-87.9065
Albuquerque,564559,35.0844,-106.6504
Tucson,542629,32.2226,-110.9747
Fresno,542107,36.7378,-119.7871
Sacramento,524943,38.5816,-121.4944
Kansas City,508090,39.0997,-94.5786
Mesa,504258,33.4152,-111.8315
Atlanta,498715,33.7490,-84.3880
Omaha,486051,41.2565,-95.9345
Colorado Springs,478961,38.8339,-104.8214
Raleigh,467665,35.7796,-78.6382
Long Beach,466742,33.7701,-118.1937
Virginia Beach,459470,36.8529,-75.9780
Miami,442241,25.7617,-80.1918
Oakland,440646,37.8044,-122.2712
Minneapolis,429954,44.9778,-93.2650
Tulsa,413066,36.1540,-95.9928
Bakersfield,403455,35.3733,-119.0187
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
  labs(title = "The United States of Nearest Big City",
       subtitle = "Each region is the area closer (great-circle distance) to its city than to any other of the 48 most populous cities in the contiguous US",
       caption = "City populations: 2020 Census, city proper. Dashed lines: current state borders. Albers equal-area projection.") +
  theme_void(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, colour = "grey30"),
        plot.caption = element_text(colour = "grey40"),
        plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(10, 10, 10, 10))

# Inset: California's crowded coast (SF/Oakland/San Jose and LA/Long Beach)
bb <- st_bbox(st_transform(st_as_sfc(st_bbox(c(xmin = -123.2, xmax = -117.6, ymin = 33.4, ymax = 38.6), crs = 4326)), crs))
ins_pts <- pts[pts$name %in% c("San Francisco","Oakland","San Jose","Sacramento","Fresno","Bakersfield","Los Angeles","Long Beach"), ]
inset <- ggplot() +
  geom_sf(data = regions, aes(fill = fill), colour = "grey25", linewidth = 0.35) +
  geom_sf(data = states, fill = NA, colour = "grey45", linewidth = 0.15, linetype = "dashed") +
  geom_sf(data = ins_pts, size = 1.2) +
  geom_text_repel(data = ins_pts, aes(X, Y, label = name), size = 2.6, fontface = "bold",
                  bg.color = "white", bg.r = 0.12, seed = 1, max.overlaps = Inf) +
  scale_fill_identity() +
  coord_sf(crs = crs, datum = NA, xlim = bb[c(1, 3)], ylim = bb[c(2, 4)], expand = FALSE) +
  labs(title = "California detail") +
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
ggsave("img/city_states.png", p, width = 16, height = 10.5, dpi = 200)

out <- st_drop_geometry(regions)[, c("name", "area_km2")]
out <- out[order(-out$area_km2), ]
out$area_km2 <- round(out$area_km2)
write.csv(out, "data/city_states_areas.csv", row.names = FALSE)
print(head(out, 8)); print(tail(out, 6))
