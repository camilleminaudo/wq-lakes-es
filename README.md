# Lake & reservoir water quality database — site selection along an alkalinity gradient

R scripts to merge three water-quality databases of Spanish lakes and reservoirs into one
harmonised database, summarise it, and select candidate sites for a field campaign
(up to 15 lakes covering an alkalinity gradient, not too oligotrophic nor too eutrophic,
deep and large enough, within a given distance from Barcelona).

## Data sources

| Source | Content | Period |
|---|---|---|
| **ACA** (Agència Catalana de l'Aigua) | Catalan reservoirs and lakes, long format (one row per value), station coordinates in UTM 31N, sampling depth | 2007–2026 |
| **CHE** (Confederación Hidrográfica del Ebro, [datossuperficiales.chebro.es](https://datossuperficiales.chebro.es)) | ~80 lakes, reservoirs and wetlands of the Ebro basin (Pyrenean lakes, steppe saline lakes, reservoirs), long format, coordinates in UTM 30N, depth profiles and photic-zone integrated samples | 2016–2026 |
| **M100** — Margalef et al. "100 embalses" survey | ~100 Spanish reservoirs, wide format, ions in µeq/L and nutrients in µmol/L, several layers per profile | 1970s |

Raw data are **not included** in this repository.

## Workflow

### `01_build_wq_database.R` — build the database

- Reads the three sources and harmonises variable names and units. Priority variables:
  - alkalinity (meq/L)
  - TP, TN, PO₄, NO₃, NH₄, SO₄, Ca, Mg (mg/L)
  - conductivity (µS/cm), pH, chlorophyll-a (µg/L), Secchi depth (m)
- Handles detection-limit values (`<0.01`), comma decimals, field vs lab measurements, mixed text encodings.
- Keeps surface samples for water-quality statistics:
  - ACA: samples ≤ 5 m;
  - CHE: point samples ≤ 5 m and photic-zone integrated samples (whole-column and near-bottom samples left out);
  - M100: epilimnion / mixed layer.
- Matches the same lake across sources:
  - by name (e.g. M100 "Boadella" = ACA "Darnius-Boadella");
  - CHE near-shore points ("(orilla)") are pooled with their lake;
  - sites of different sources less than 1 km apart but with different names are listed in
    `cross_source_close_sites.csv`, to add to `lake_crosswalk` if they are the same lake.
- **Coordinates**:
  - from the ACA and CHE files (UTM X/Y), from a manual file, or looked up in OpenStreetMap;
  - lookups are cached and reused, and runs can be interrupted and resumed;
  - flags missing coordinates and different lakes of the same source sharing the same location.
- **Lake depth**: deepest sample in any database (a lower bound unless a vertical profile was sampled).
- **Lake area**: OpenStreetMap water polygons. Lakes < 1 ha are flagged.
- Depth and area can be overridden by hand in `lake_attributes_manual.csv`.
- Writes summary tables:
  - n, median, IQR, mean, SD and period per source × site × variable;
  - one row per lake with all priority variables, distance to Barcelona, depth, area and OECD trophic class.

### `02_overview_maps_selection.R` — explore and select

- Data-availability heatmap and one overview figure per variable (values, SD, number of observations, by source).
- Map of all lakes coloured by alkalinity.
- **Site selection**: sequential filters, each logged with the number of lakes left:
  1. excluded names / prefixes (wetlands, coastal lagoons, canals…)
  2. distance from Barcelona (`max_dist_km`)
  3. available alkalinity data, from the chosen sources (`alk_sources`)
  4. conductivity below `max_cond` (leaves out saline lakes)
  5. trophic state (`trophic_ok`)
  6. max depth ≥ `min_depth_m`
  7. area ≥ `min_area_ha`
- Lakes in `must_include` pass every filter; lakes in `exclude` never do.
- The remaining lakes are spread evenly along the (log) alkalinity gradient.
- Selection map, alkalinity vs trophic-state plot and optional interactive map (leaflet).

All thresholds are settings at the top of each script.

## Usage

1. Set `base_dir` and the input file paths (`aca_file`, `che_file`, `m100_file`, `m100_names_file`)
   at the top of both scripts. Set `che_file <- NULL` to run without the CHE data.
2. Run `01_build_wq_database.R`, then check the console messages:
   - unit conversions;
   - unmatched variables;
   - missing or suspicious coordinates;
   - sites of different sources that may be the same lake;
   - lakes < 1 ha.
3. Fix coordinates or names in `coordinates_manual.csv`, and optionally depth and area in
   `lake_attributes_manual.csv`, then run script 01 again.
   Both files can be edited in Excel (`;` and decimal commas are fine).
4. Adjust the selection settings and run `02_overview_maps_selection.R`.

## Outputs (`<base_dir>/WQ_database/`)

| File | Content |
|---|---|
| `wq_database_long.csv/.rds` | All measurements, harmonised (one row per value) |
| `summary_by_site_variable.csv` | Statistics per source × site × variable |
| `lake_summary_priority.csv` | One row per lake: priority variables, coordinates, depth, area, trophic class |
| `sites.csv` | Sampling sites with coordinates and coordinate checks |
| `variable_dictionary.csv`, `unit_conversions.csv` | Variable names, units and conversion factors applied |
| `cross_source_check.csv` | Lakes present in several sources: medians compared (unit sanity check) |
| `cross_source_close_sites.csv` | Sites of different sources < 1 km apart with different names (same lake?) |
| `coordinates_manual.csv`, `lake_attributes_manual.csv` | Files to complete / correct by hand |
| `selection_lakes.csv`, `selection_candidates.csv`, `selection_filter_log.csv` | Site selection |
| `figures/` | Availability, per-variable overviews, maps, selection |

## Requirements

- R ≥ 4.1 with `tidyverse`, `patchwork`, `maps`.
- Optional:
  - `sf` (UTM coordinates, lake areas)
  - `tidygeocoder` (coordinate lookup)
  - `osmdata` (lake areas)
  - `ggrepel`, `leaflet`

## Notes and caveats

- **Alkalinity units**: ACA alkalinity given in mg/L is assumed to be mg CaCO₃/L (setting `alk_mg_as`);
  CHE gives it explicitly as mg CO₃Ca/L.
- **CHE variables**:
  - the profile summaries (`PH_ZF`, `COND_ZF`, `COND_SUP`, `COND_FON`…) are kept under their own names, not merged with pH / conductivity;
  - no ammonium in the extract used;
  - several saline lakes (Chiprana, Gallocanta, Salinas de Añana…) reach > 50 000 µS/cm:
    use `max_cond` in script 02 to leave them out.
- **M100 variables**:
  - PO₄ is total reactive P (TRP);
  - pH is lab pH at 25 °C;
  - the reference temperature for conductivity is not stated.
- **M100 data are ~50 years old.** When a lake has data in several sources, values come from
  the first source in `source_priority` (default ACA, then CHE, then M100).
- **Trophic classes** use OECD (1982) boundaries applied to medians.
- **Distances** are straight-line distances from Barcelona.
- **Depth and area need checking.** Depth from sampling is only a lower bound, and areas from OpenStreetMap can be wrong; check both before final selection.
