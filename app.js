// Interactive map + ledger for The United States of Nearest Big Metro

// Land area of each state (km², Census) for "about the size of" comparisons
const STATE_AREAS = {
  Texas: 676587, California: 403466, Montana: 376962, "New Mexico": 314161, Arizona: 294207,
  Nevada: 284332, Colorado: 268431, Wyoming: 251470, Oregon: 248608, Idaho: 214045, Utah: 212818,
  Kansas: 211754, Minnesota: 206232, Nebraska: 198974, "South Dakota": 196350, "North Dakota": 178711,
  Missouri: 178040, Oklahoma: 177660, Washington: 172119, Georgia: 148959, Michigan: 146435,
  Iowa: 144669, Illinois: 143793, Wisconsin: 140268, Florida: 138887, Arkansas: 134771,
  Alabama: 131171, "North Carolina": 125920, "New York": 122057, Mississippi: 121531,
  Pennsylvania: 115883, Louisiana: 111898, Tennessee: 106798, Ohio: 105829, Virginia: 102279,
  Kentucky: 102269, Indiana: 92789, Maine: 79883, "South Carolina": 77857, "West Virginia": 62259,
  Maryland: 25142, Vermont: 23871, "New Hampshire": 23187, Massachusetts: 20202, "New Jersey": 19047,
  Connecticut: 12542, Delaware: 5047, "Rhode Island": 2678,
};

const fmt = d3.format(",");
const fmtPct = d3.format(".1%");
const fmtPop = (n) => (n >= 1e6 ? d3.format(".2~f")(n / 1e6) + " million" : fmt(n));

function compareToState(area) {
  let best = null;
  for (const [name, a] of Object.entries(STATE_AREAS)) {
    const score = Math.abs(Math.log(area / a));
    if (!best || score < best.score) best = { name, a, score };
  }
  const r = area / best.a;
  if (r > 0.9 && r < 1.1) return { short: best.name, long: `about the size of ${best.name}` };
  const x = r >= 1 ? d3.format(".1f")(r) : d3.format(".2f")(r);
  return { short: `${x}× ${best.name}`, long: `about ${x} times the size of ${best.name}` };
}

// GDAL writes counter-clockwise outer rings; d3 expects clockwise, so flip anything inside-out
function rewind(fc) {
  for (const f of fc.features) {
    const g = f.geometry;
    const polys = g.type === "Polygon" ? [g.coordinates] : g.coordinates;
    for (const poly of polys) {
      const test = { type: "Polygon", coordinates: [poly[0]] };
      if (d3.geoArea(test) > 2 * Math.PI) poly.forEach((ring) => ring.reverse());
    }
  }
  return fc;
}

const W = 960, H = 600;
const svg = d3.select("#map").attr("viewBox", `0 0 ${W} ${H}`);
const gRegions = svg.append("g");
const gStates = svg.append("g");
const gCities = svg.append("g");
const tip = document.getElementById("tip");

const MODE_TEXT = {
  metro: { kicker: "Metro region", pop: "Metro population", resid: "metro", ledger: "Showing the metro-area version." },
  city: { kicker: "City region", pop: "City population", resid: "city", ledger: "Showing the city-limits version." },
  state: { kicker: "State champion", pop: "City population", resid: "city", ledger: "Showing the biggest-city-per-state version." },
};

const state = { mode: "metro", pinned: null, data: {}, sortKey: "rank", sortDir: 1 };

Promise.all([
  d3.json("data/metro_regions.geojson"),
  d3.json("data/city_regions.geojson"),
  d3.json("data/states.geojson"),
  d3.json("data/state_cities.geojson"),
]).then(([metro, city, states, perState]) => {
  state.data.metro = rewind(metro);
  state.data.city = rewind(city);
  state.data.state = rewind(perState);
  state.states = rewind(states);
  for (const fc of [metro, city, perState]) {
    const total = d3.sum(fc.features, (f) => f.properties.area_km2);
    for (const f of fc.features) {
      const p = f.properties;
      p.share = p.area_km2 / total;
      p.density = p.pop / p.area_km2;
      p.cmp = compareToState(p.area_km2);
    }
  }
  // the per-state file is ordered by state; rank its cities by population instead
  d3.sort(perState.features, (f) => -f.properties.pop).forEach((f, i) => (f.properties.rank = i + 1));
  if (MODE_TEXT[location.hash.slice(1)]) setMode(location.hash.slice(1), false);
  state.projection = d3.geoAlbersUsa().fitExtent([[6, 6], [W - 6, H - 6]], state.data.metro);
  state.path = d3.geoPath(state.projection);
  gStates.selectAll("path").data(state.states.features).join("path")
    .attr("class", "state-line").attr("d", state.path);
  render();
});

function features() { return state.data[state.mode].features; }

function render() {
  const fs = features();
  const path = state.path, proj = state.projection;

  gRegions.selectAll("path").data(fs, (d) => d.properties.name).join(
    (enter) => enter.append("path").attr("class", "region").style("opacity", 0)
      .call((e) => e.transition().duration(450).style("opacity", 1)),
    (update) => update,
    (exit) => exit.transition().duration(250).style("opacity", 0).remove()
  )
    .attr("d", path)
    .attr("fill", (d) => d.properties.fill)
    .on("mousemove", (ev, d) => hover(ev, d))
    .on("mouseleave", () => { tip.hidden = true; if (!state.pinned) clearActive(); else showCard(state.pinned); })
    .on("click", (ev, d) => pin(d));

  // City dots for all; labels only where the region is roomy enough
  const cities = fs.map((d) => ({ d, xy: proj([d.properties.lon, d.properties.lat]) })).filter((c) => c.xy);
  gCities.selectAll("circle").data(cities, (c) => c.d.properties.name).join("circle")
    .attr("class", "city-dot").attr("r", 2.6)
    .attr("cx", (c) => c.xy[0]).attr("cy", (c) => c.xy[1]);
  gCities.selectAll("text").data(cities.filter((c) => c.d.properties.area_km2 > 95000), (c) => c.d.properties.name)
    .join("text").attr("class", "city-label")
    .attr("x", (c) => c.xy[0]).attr("y", (c) => c.xy[1] - 6)
    .attr("text-anchor", "middle").text((c) => c.d.properties.name);

  state.pinned = null;
  renderExtremes();
  renderLedger();
  showDefaultCard();
}

function hover(ev, d) {
  const box = document.querySelector(".map-wrap").getBoundingClientRect();
  tip.hidden = false;
  tip.innerHTML = `<b>${d.properties.name}</b><br><span>${fmt(d.properties.area_km2)} km² · ${d.properties.cmp.short}</span>`;
  tip.style.left = `${ev.clientX - box.left}px`;
  tip.style.top = `${ev.clientY - box.top}px`;
  setActive(d.properties.name);
  showCard(d);
}

function pin(d) {
  state.pinned = state.pinned === d ? null : d;
  gRegions.selectAll("path").classed("pinned", (x) => x === state.pinned);
  if (state.pinned) showCard(d);
}

function setActive(name) {
  gRegions.selectAll("path").classed("active", (x) => x.properties.name === name);
  d3.selectAll("#ledger tbody tr").classed("active", (x) => x.name === name);
}
function clearActive() { setActive(null); showDefaultCard(); }

function showCard(d) {
  const p = d.properties;
  document.getElementById("cardKicker").textContent = MODE_TEXT[state.mode].kicker;
  document.getElementById("cardName").innerHTML = `<span class="swatch" style="background:${p.fill}"></span>${p.name}`;
  document.getElementById("cardRank").textContent = `#${p.rank} of 48`;
  document.getElementById("cardPopLabel").textContent = MODE_TEXT[state.mode].pop;
  document.getElementById("cardPop").textContent = fmtPop(p.pop);
  document.getElementById("cardArea").textContent = `${fmt(p.area_km2)} km²`;
  document.getElementById("cardShare").textContent = fmtPct(p.share);
  document.getElementById("cardCompare").textContent = `${p.name} would govern a territory ${p.cmp.long}.`;
}

function showDefaultCard() {
  if (state.pinned) return showCard(state.pinned);
  document.getElementById("cardKicker").textContent = "Region";
  document.getElementById("cardName").textContent = "Pick a region";
  document.getElementById("cardPopLabel").textContent = MODE_TEXT[state.mode].pop;
  for (const id of ["cardRank", "cardPop", "cardArea", "cardShare"]) document.getElementById(id).textContent = "-";
  document.getElementById("cardCompare").textContent =
    "Each region is drawn around one city. Hover the map to see how much land it would govern.";
}

function renderExtremes() {
  const ps = features().map((f) => f.properties);
  const big = d3.greatest(ps, (p) => p.area_km2);
  const small = d3.least(ps, (p) => p.area_km2);
  const sparse = d3.least(ps, (p) => p.density);
  const spread = big.area_km2 / small.area_km2;
  document.getElementById("extremes").innerHTML = `
    <div><div class="label">Largest</div><div class="big">${big.name}</div><div class="sub">${fmt(big.area_km2)} km², ${big.cmp.short}</div></div>
    <div><div class="label">Smallest</div><div class="big">${small.name}</div><div class="sub">${fmt(small.area_km2)} km², ${small.cmp.short}</div></div>
    <div><div class="label">Largest ÷ smallest</div><div class="big">${Math.round(spread)}×</div><div class="sub">Real states: Texas is ${Math.round(676587 / 2678)}× Rhode Island</div></div>
    <div><div class="label">Emptiest per resident</div><div class="big">${sparse.name}</div><div class="sub">${d3.format(".1f")(sparse.density)} ${MODE_TEXT[state.mode].resid} residents per km²</div></div>`;
}

function renderLedger() {
  const rows = features().map((f) => ({ ...f.properties, compare: f.properties.cmp.short, feature: f }));
  const k = state.sortKey, dir = state.sortDir;
  rows.sort((a, b) => (a[k] < b[k] ? -1 : a[k] > b[k] ? 1 : 0) * dir);
  const maxArea = d3.max(rows, (r) => r.area_km2);

  d3.select("#ledger tbody").selectAll("tr").data(rows, (r) => r.name).join("tr")
    .html((r) => `
      <td class="num">${r.rank}</td>
      <td class="name"><i style="background:${r.fill}"></i>${r.name}</td>
      <td class="num">${fmt(r.pop)}</td>
      <td class="num bar-cell">${fmt(r.area_km2)}<span class="bar" style="width:${(70 * r.area_km2) / maxArea}px"></span></td>
      <td class="num">${d3.format(".1f")(r.density)}</td>
      <td>${r.compare}</td>`)
    .on("mouseenter", (ev, r) => { setActive(r.name); showCard(r.feature); })
    .on("mouseleave", () => clearActive())
    .on("click", (ev, r) => { pin(r.feature); document.querySelector(".atlas").scrollIntoView({ behavior: "smooth" }); });

  d3.selectAll("#ledger th").attr("aria-sort", function () {
    return this.dataset.key === k ? (dir === 1 ? "ascending" : "descending") : null;
  });
  document.getElementById("ledgerMode").textContent =
    MODE_TEXT[state.mode].ledger;
}

d3.selectAll("#ledger th").on("click", function () {
  const key = this.dataset.key;
  if (state.sortKey === key) state.sortDir *= -1;
  else { state.sortKey = key; state.sortDir = key === "name" || key === "rank" || key === "compare" ? 1 : -1; }
  renderLedger();
});

function setMode(mode, draw = true) {
  state.mode = mode;
  d3.selectAll(".toggle button").attr("aria-checked", function () { return this.dataset.mode === mode; });
  history.replaceState(null, "", `#${mode}`);
  if (draw) render();
}
d3.selectAll(".toggle button").on("click", function () {
  if (this.dataset.mode !== state.mode) setMode(this.dataset.mode);
});

document.getElementById("showStates").addEventListener("change", (e) => {
  gStates.style("opacity", e.target.checked ? 1 : 0);
});
