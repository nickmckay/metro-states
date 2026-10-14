# Export simplified region polygons (lon/lat GeoJSON) for the interactive site.
# Run from the repo root: Rscript R/export_geojson.R

# Same least-used greedy colouring and palette as the animations
colour_graph <- function(polys, pal) {
  adj <- sf::st_intersects(sf::st_buffer(polys, 3000))
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

export <- function(script, out, kind) {
  src <- readLines(script)
  env <- new.env()
  eval(parse(text = src[1:(grep("^# Greedy graph", src) - 1)]), envir = env)
  env$colour_graph <- colour_graph; env$metro_pal <- metro_pal
  with(env, {
    regions$pop <- cities$pop[regions$id]
    regions$lat <- cities$lat[regions$id]
    regions$lon <- cities$lon[regions$id]
    regions$rank <- regions$id
    regions$area_km2 <- round(regions$area_km2)
    regions$kind <- kind
    regions$fill <- colour_graph(regions, metro_pal)
    g <- st_simplify(regions, dTolerance = 1500, preserveTopology = TRUE)
    g <- st_transform(g[, c("name", "rank", "pop", "lat", "lon", "area_km2", "kind", "fill")], 4326)
    unlink(out)
    st_write(g, out, driver = "GeoJSON", layer_options = "COORDINATE_PRECISION=3", quiet = TRUE)
    s <- st_transform(st_simplify(states, dTolerance = 1500, preserveTopology = TRUE), 4326)
    unlink("data/states.geojson")
    st_write(s[, "ID"], "data/states.geojson", driver = "GeoJSON",
             layer_options = "COORDINATE_PRECISION=3", quiet = TRUE)
  })
}
export("R/metro_states.R", "data/metro_regions.geojson", "metro")
export("R/city_states.R", "data/city_regions.geojson", "city")
export("R/state_cities.R", "data/state_cities.geojson", "state")
