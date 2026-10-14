# The United States of Nearest Big Metro

What if the lower 48 were redrawn so every place belonged to whichever of the 48 largest
metro areas is closest? This repo builds the maps, animations and a small static website.

## Website
`index.html`, `style.css`, `app.js` – a static site (D3 from cdnjs) meant for GitHub Pages
(Settings → Pages → deploy from `main`, root). Preview locally with `python3 -m http.server`.

## Rebuilding (run from the repo root)
| Script | Output |
|---|---|
| `Rscript R/metro_states.R` | `img/metro_states.png`, `data/metro_states_areas.csv` |
| `Rscript R/city_states.R` | `img/city_states.png`, `data/city_states_areas.csv` (city-limits version) |
| `Rscript R/state_cities.R` | `img/state_cities.png`, `data/state_cities_areas.csv`, `data/state_cities_home_share.csv` (each state's largest city) |
| `Rscript R/metro_animations.R` | `img/metro_bubbles.gif`, `img/states_to_metros.gif`, `data/state_metro_pairs.csv` (set `SKIP_BUBBLES=1` to only redo the morph) |
| `Rscript R/export_geojson.R` | `data/*.geojson` for the interactive map |

R packages: sf, terra, ggplot2, ggrepel, maps, gifski, transformr.

## Method
- 48 most populous MSAs (2020 Census), each anchored at its first-named principal city.
- Each 2 km grid cell is assigned to the nearest metro by great-circle distance; cells are
  merged into polygons and clipped to the coastline (a spherical Voronoi diagram).
- The states→metros morph pairs each state with one region via an exact minimum-cost
  assignment (squared centroid distance, with a bonus for shared land), then tweens shapes.
