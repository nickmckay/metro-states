suppressPackageStartupMessages({
  library(sf); library(terra); library(ggplot2); library(maps); library(ggrepel)
})
sf_use_s2(FALSE)

# The largest city (2020 Census, city proper) in each of the 48 contiguous states.
cities <- read.csv(text = "
name,state,pop,lat,lon
Huntsville,AL,215006,34.7304,-86.5861
Phoenix,AZ,1608139,33.4484,-112.0740
Little Rock,AR,202591,34.7465,-92.2896
Los Angeles,CA,3898747,34.0522,-118.2437
Denver,CO,715522,39.7392,-104.9903
Bridgeport,CT,148654,41.1792,-73.1894
Wilmington,DE,70898,39.7391,-75.5398
Jacksonville,FL,949611,30.3322,-81.6557
Atlanta,GA,498715,33.7490,-84.3880
Boise,ID,235684,43.6150,-116.2023
Chicago,IL,2746388,41.8781,-87.6298
Indianapolis,IN,887642,39.7684,-86.1581
Des Moines,IA,214133,41.5868,-93.6250
Wichita,KS,397532,37.6872,-97.3301
Louisville,KY,633045,38.2527,-85.7585
New Orleans,LA,383997,29.9511,-90.0715
Portland,ME,68408,43.6591,-70.2568
Baltimore,MD,585708,39.2904,-76.6122
Boston,MA,675647,42.3601,-71.0589
Detroit,MI,639111,42.3314,-83.0458
Minneapolis,MN,429954,44.9778,-93.2650
Jackson,MS,153701,32.2988,-90.1848
Kansas City,MO,508090,39.0997,-94.5786
Billings,MT,117116,45.7833,-108.5007
Omaha,NE,486051,41.2565,-95.9345
Las Vegas,NV,641903,36.1699,-115.1398
Manchester,NH,115644,42.9956,-71.4548
Newark,NJ,311549,40.7357,-74.1724
Albuquerque,NM,564559,35.0844,-106.6504
New York,NY,8804190,40.7128,-74.0060
Charlotte,NC,874579,35.2271,-80.8431
Fargo,ND,125990,46.8772,-96.7898
Columbus,OH,905748,39.9612,-82.9988
Oklahoma City,OK,681054,35.4676,-97.5164
Portland,OR,652503,45.5152,-122.6784
Philadelphia,PA,1603797,39.9526,-75.1652
Providence,RI,190934,41.8240,-71.4128
Charleston,SC,150227,32.7765,-79.9311
Sioux Falls,SD,192517,43.5446,-96.7311
Nashville,TN,689447,36.1627,-86.7816
Houston,TX,2304580,29.7604,-95.3698
Salt Lake City,UT,199723,40.7608,-111.8910
Burlington,VT,44743,44.4759,-73.2121
Virginia Beach,VA,459470,36.8529,-75.9780
Seattle,WA,737015,47.6062,-122.3321
Charleston,WV,48864,38.3498,-81.6326
Milwaukee,WI,577222,43.0389,-87.9065
Cheyenne,WY,65132,41.1400,-104.8202
", strip.white = TRUE)
cities$city <- cities$name
cities$name <- paste0(cities$city, ", ", cities$state)
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
  labs(title = "The United States of One City Per State",
       subtitle = "Each region is the area closer (great-circle distance) to its city than to the largest city of any other contiguous state",
       caption = "Each state's largest city by 2020 Census population (city proper). Dashed lines: current state borders. Albers equal-area projection.") +
  theme_void(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, colour = "grey30"),
        plot.caption = element_text(colour = "grey40"),
        plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(10, 10, 10, 10))

# Inset: California's crowded coast (SF/Oakland/San Jose and LA/Long Beach)
bb <- st_bbox(st_transform(st_as_sfc(st_bbox(c(xmin = -80.8, xmax = -69.5, ymin = 37.0, ymax = 45.3), crs = 4326)), crs))
ins_pts <- pts[pts$state %in% c("NY","NJ","CT","RI","MA","NH","VT","ME","PA","DE","MD","WV"), ]
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
ggsave("img/state_cities.png", p, width = 16, height = 10.5, dpi = 200)

out <- st_drop_geometry(regions)[, c("name", "area_km2")]
out <- out[order(-out$area_km2), ]
out$area_km2 <- round(out$area_km2)
write.csv(out, "data/state_cities_areas.csv", row.names = FALSE)
print(head(out, 8)); print(tail(out, 6))

# How much of each state stays with its own largest city?
st_abb <- state.abb[match(tools::toTitleCase(states$ID), state.name)]
home <- do.call(rbind, lapply(seq_len(nrow(cities)), function(i) {
  s <- states[which(st_abb == cities$state[i]), ]
  r <- regions[regions$id == i, ]
  keep <- as.numeric(sum(st_area(st_intersection(st_geometry(s), st_geometry(r)))))
  data.frame(state = cities$state[i], city = cities$city[i],
             state_km2 = round(as.numeric(sum(st_area(s))) / 1e6),
             kept_pct = round(100 * keep / as.numeric(sum(st_area(s))), 1))
}))
home <- home[order(home$kept_pct), ]
write.csv(home, "data/state_cities_home_share.csv", row.names = FALSE)
print(head(home, 10)); print(tail(home, 6))
