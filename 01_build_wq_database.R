# =============================================================================
# 01_build_wq_database.R
#
# Builds ONE harmonised water-quality database from two sources:
#   ACA  : Catalan reservoirs & lakes, 2007–2026 (long format, one row = one value)
#   M100 : Margalef et al. "100 embalses" survey, 1970s (wide format, µeq/L & µmol/L)
#
# Outputs (in out_dir):
#   wq_database_long.csv / .rds      one row = one measurement, harmonised names & units
#   sites.csv                        one row = one sampling site, with coordinates
#   summary_by_site_variable.csv     n, median, IQR, mean, sd... per source x site x variable
#   lake_summary_priority.csv        one row = one lake, priority variables side by side
#   variable_dictionary.csv          harmonised variable names, units, original names
#   unit_conversions.csv             every (variable, original unit) -> factor applied
#   cross_source_check.csv           lakes present in both sources: medians compared
#   coordinates_manual.csv           fill in missing / wrong coordinates (and fix names)
#   coordinates_close_pairs.csv      different lakes with (almost) the same coordinates
#   lake_attributes_manual.csv       fill in max depth / area from other sources (optional)
#   lake_area_osm_cache.csv          lake areas from OpenStreetMap water polygons
#
# Then run 02_overview_maps_selection.R for figures, maps and lake selection.
# =============================================================================

library(tidyverse)

# ---- Settings ---------------------------------------------------------------

base_dir <- "C:/Users/Camille Minaudo/OneDrive - Universitat de Barcelona/Documentos/data"

aca_file        <- file.path(base_dir, "ACA/WQ_embassaments_estanys_2007-2026.csv")
m100_file       <- file.path(base_dir, "100embass/DAT_100embass_extract_des13_cerca llac.csv")       # <- adjust
m100_names_file <- file.path(base_dir, "100embass/DAT_100embass_extract_des13_cerca llac_NOMS.csv")  # <- adjust

out_dir <- file.path(base_dir, "ES_lakes_WQ_database")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ACA column names, written without accents, in lower case, non-letters as "."
# ("Codi Estació" -> "codi.estacio", "Massa d'aigua" -> "massa.d.aigua")
aca_cols <- c(code = "codi.estacio", name = "massa.d.aigua",
              variable = "variable", value = "valor", date = "data")

# Units ----
# NO3, NH4, PO4 expressed as the ion ("ion": mg NO3/L) or as N / P ("element": mg N-NO3/L)
nutrients_as <- "ion"

# How to read a unit given only as "mg/L" (no chemical tag):
alk_mg_as           <- "CaCO3"  # alkalinity in mg/L: "CaCO3" (÷50.04) or "HCO3" (÷61.02)
ions_unspecified_as <- "ion"    # NO3/NH4/PO4 in mg/L: "ion" or "element"

# If the ACA file has NO unit column, these units are assumed. CHECK THEM.
aca_assumed_units <- c(Alk = "mg/L", TP = "mg/L", TN = "mg/L", PO4 = "mg/L",
                       NO3 = "mg/L", NH4 = "mg/L", SO4 = "mg/L", Ca = "mg/L",
                       Mg = "mg/L", Cond = "uS/cm", CHLa = "ug/L", pH = "-",
                       Secchi = "m", Temp = "degC")

# Values below detection limit ("<0.01"): keep the limit ("limit") or half of it ("half")
censored_value <- "limit"

# Field ("camp") and lab ("laboratori") measurements of the same variable pooled?
pool_methods <- TRUE

# Depth ----
# M100 layers kept: E = epilimnion, M = mixed (unstratified), T = thermocline,
# H = hypolimnion. NULL = keep all.
m100_layers <- c("E", "M")
# ACA: if a depth column is found, keep samples down to this depth (m) for the water-quality
# statistics (rows without depth, "-", are kept). NULL = keep all depths.
# (The deepest sample of each lake is recorded before this filter, see "Lake depth & area".)
aca_max_depth_m <- 5
# ACA: keep only years >= this (NULL = all years)
aca_min_year <- NULL

# Coordinates ----
# Coordinates are reused from previous runs, in this order:
#   1. coordinates_manual.csv (your file; every site listed there is never looked up again,
#      even with empty lat/lon — fill them in by hand)
#   2. the ACA file (UTM X/Y)
#   3. coordinates_geocoded_cache.csv (earlier lookups, including failed ones)
# Only sites found in none of these are looked up online, and each result is saved at once,
# so an interrupted run loses nothing.
coords_manual_file <- file.path(out_dir, "coordinates_manual.csv")   # <- or the path of your own file
geocode_missing <- TRUE      # look up remaining sites in OpenStreetMap (needs tidygeocoder + internet)
aca_utm_crs     <- 25831     # CRS of ACA X/Y columns if any (ETRS89 / UTM 31N)
bcn             <- c(lat = 41.3874, lon = 2.1686)

# Lake depth & area ----
# Max depth = deepest sample in either database (a lower bound of the true max depth),
# unless given in lake_attributes_manual.csv.
min_n_depths_profile <- 3     # a lake counts as "profiled" if sampled at >= this many depths
# Area = OpenStreetMap water polygon containing (or nearest to) the lake point
get_osm_area     <- TRUE      # needs packages osmdata + sf, and internet
osm_snap_m       <- 300       # point outside any water polygon: take the nearest one within this distance
small_lake_ha    <- 1         # lakes smaller than this are flagged
close_coords_m   <- 500       # different lakes closer than this are flagged (duplicated coordinates?)

# Lakes present in both sources whose names don't match automatically.
# site_uid -> lake_key of the other source (lake_key = simplified name, see sites.csv)
lake_crosswalk <- tribble(
  ~site_uid,   ~lake_key
  # "M100_79", "darnius boadella"
)

# Variables to prioritise (order used in tables & figures)
priority_vars <- c("Alk", "TP", "TN", "PO4", "NO3", "NH4", "SO4",
                   "Ca", "Mg", "Cond", "pH", "CHLa", "Secchi")

# Preferred source when a lake has data in both (first = preferred)
source_priority <- c("ACA", "M100")


# ---- Reference tables ---------------------------------------------------------

molar_mass <- c(P = 30.974, N = 14.007, PO4 = 94.971, NO3 = 62.004, NH4 = 18.038,
                SO4 = 96.06, Ca = 40.078, Mg = 24.305, CaCO3 = 100.087, HCO3 = 61.017)
charge     <- c(PO4 = 3, NO3 = 1, NH4 = 1, SO4 = 2, Ca = 2, Mg = 2, CaCO3 = 2, HCO3 = 1)

target_unit <- c(Alk = "meq/L", TP = "mg P/L", TN = "mg N/L",
                 PO4 = "mg PO4/L", NO3 = "mg NO3/L", NH4 = "mg NH4/L",
                 SO4 = "mg SO4/L", Ca = "mg Ca/L", Mg = "mg Mg/L",
                 Cond = "µS/cm", pH = "-", CHLa = "µg/L", Secchi = "m", Temp = "°C")

var_description <- c(Alk = "Alkalinity", TP = "Total phosphorus", TN = "Total nitrogen",
                     PO4 = "Phosphate (soluble reactive P)", NO3 = "Nitrate",
                     NH4 = "Ammonium", SO4 = "Sulphate", Ca = "Calcium", Mg = "Magnesium",
                     Cond = "Conductivity", pH = "pH", CHLa = "Chlorophyll-a",
                     Secchi = "Secchi depth", Temp = "Water temperature")

# Plausible ranges after conversion: values outside are reported (unit problems)
plausible <- tribble(
  ~var,    ~lo,   ~hi,
  "Alk",    0,     20,
  "TP",     0,     5,
  "TN",     0,     50,
  "PO4",    0,     10,
  "NO3",    0,     200,
  "NH4",    0,     20,
  "SO4",    0,     3000,
  "Ca",     0,     800,
  "Mg",     0,     500,
  "Cond",   1,     20000,
  "pH",     3,     11,
  "CHLa",   0,     1000,
  "Secchi", 0,     40
)

# ACA variable names -> harmonised names (matched in lower case, extra spaces removed;
# the "(camp)" / "(laboratori)" suffix is handled separately)
aca_var_map <- c(
  "clorofil·la a" = "CHLa", "clorofil.la a" = "CHLa",
  "fòsfor total" = "TP", "nitrogen total" = "TN",
  "ph" = "pH",
  "alcalinitat" = "Alk", "alcalinitat (tac)" = "Alk",
  "calci" = "Ca", "magnesi" = "Mg", "sulfats" = "SO4",
  "nitrats" = "NO3", "amoni" = "NH4", "fosfats" = "PO4", "ortofosfats" = "PO4",
  "conductivitat a 20ºc" = "Cond", "conductivitat" = "Cond",
  "transparència (disc de secchi)" = "Secchi", "disc de secchi" = "Secchi",
  "transparència" = "Secchi",
  "temperatura" = "Temp", "temperatura de l'aigua" = "Temp"
)

# M100 columns -> harmonised names (other columns are kept under their own name)
m100_var_map <- c(
  "Alk ueq/L" = "Alk", "TP umol/L" = "TP", "TN umol/L" = "TN",
  "TRP umol/L" = "PO4",          # total reactive P ~ soluble reactive phosphate
  "NO3 ueq/L" = "NO3", "NH4 ueq/L" = "NH4", "SO4 ueq/L" = "SO4",
  "Ca ueq/L" = "Ca", "Mg ueq/L" = "Mg",
  "Cond uS cm" = "Cond",         # reference temperature not stated
  "PH25" = "pH",                 # lab pH at 25 °C (PHAQ is the other pH column)
  "Chla ug/L" = "CHLa", "Secchi m" = "Secchi", "TEMP" = "Temp"
)
m100_meta_cols <- c("IdMEU", "Any", "CodiEmbass", "Campanya", "Grup", "GrupSIMP",
                    "Grup2", "CAPA", "Fond m", "Fondval")
site_level_vars <- c("Secchi")   # one value per visit, repeated on every depth row in M100


# ---- Helpers ------------------------------------------------------------------

# Simplified name used to match the same lake across sources
lake_key_from_name <- function(x) {
  x %>%
    str_to_lower() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_replace_all("[^a-z0-9 ]", " ") %>%
    str_squish() %>%
    str_remove("^(embassament|embalse|pantano|panta|estany|estanys|encoro|lago|llac|lac)( (de les|de la|del|dels|de|d))?\\s+") %>%
    str_squish()
}

# Split a unit string into a scale ("mg", "ueq", ...) and a chemical tag ("caco3", "p", ...)
unit_parts <- function(u) {
  x <- u %>%
    str_to_lower() %>%
    str_replace_all("[\u00b5\u03bc]", "u") %>%
    str_remove_all("[\\s/\\-\\.\u00b7()]")
  rx    <- "^(meq|ueq|mmol|umol|mg|ug|ng|us|ms|g)"
  scale <- str_extract(x, rx)
  tag   <- x %>% str_remove(rx) %>% str_remove("^l") %>% str_remove("l$")
  list(scale = scale, tag = tag)
}

# Factor to convert a value of variable `var` given in `unit` into target_unit[var].
# Returns NA when the unit is not understood.
unit_factor <- function(var, unit) {
  if (var %in% c("pH")) return(1)
  if (!var %in% names(target_unit)) return(1)                # non-harmonised variable: unchanged
  if (var %in% c("Secchi", "Temp")) {
    if (!is.na(unit) && str_detect(str_to_lower(unit), "^cm")) return(0.01)
    return(1)
  }
  if (is.na(unit) || unit %in% c("", "-")) return(NA_real_)

  p     <- unit_parts(unit)
  mass  <- unname(c(mg = 1, ug = 1e-3, ng = 1e-6, g = 1e3)[p$scale])
  molar <- unname(c(mmol = 1, umol = 1e-3)[p$scale])
  equiv <- unname(c(meq = 1, ueq = 1e-3)[p$scale])
  tag   <- p$tag

  out <- switch(var,
                Alk = {
                  if (!is.na(equiv)) equiv
                  else if (!is.na(molar)) molar                           # mmol HCO3 = meq
                  else if (!is.na(mass)) {
                    as <- if (tag == "caco3") "CaCO3" else if (tag == "hco3") "HCO3" else alk_mg_as
                    mass / (molar_mass[[as]] / charge[[as]])
                  } else NA
                },
                TP = , TN = {
                  el  <- if (var == "TP") "P" else "N"
                  if (!is.na(molar)) molar * molar_mass[[el]]
                  else if (!is.na(mass)) {
                    if (var == "TP" && tag == "po4") mass * molar_mass[["P"]] / molar_mass[["PO4"]] else mass
                  } else NA
                },
                PO4 = , NO3 = , NH4 = {
                  el     <- if (var == "PO4") "P" else "N"
                  ion_lc <- str_to_lower(var); el_lc <- str_to_lower(el)
                  is_el  <- tag %in% c(el_lc, paste0(el_lc, ion_lc), paste0(ion_lc, el_lc))
                  if (!is.na(molar)) molar * molar_mass[[var]]
                  else if (!is.na(equiv)) equiv * molar_mass[[var]] / charge[[var]]
                  else if (!is.na(mass)) {
                    if (is_el || (tag == "" && ions_unspecified_as == "element"))
                      mass * molar_mass[[var]] / molar_mass[[el]]
                    else mass
                  } else NA
                },
                SO4 = , Ca = , Mg = {
                  if (!is.na(mass)) mass
                  else if (!is.na(molar)) molar * molar_mass[[var]]
                  else if (!is.na(equiv)) equiv * molar_mass[[var]] / charge[[var]]
                  else NA
                },
                Cond = {
                  if (identical(p$scale, "us")) 1 else if (identical(p$scale, "ms")) 1000 else NA
                },
                CHLa = {
                  if (identical(p$scale, "mg") && tag == "m3") 1          # mg/m3 = µg/L
                  else if (!is.na(mass)) mass * 1000
                  else NA
                },
                NA
  )
  as.numeric(out)
}

# "AGUILAR DE CAMPOO" -> "Aguilar de Campoo"
title_name <- function(x) {
  x <- str_to_title(x)
  str_replace_all(x, "(?<=\\s)(De|Del|La|Las|Los|El|Y|I|D'|Dels|Les)(?=\\s)", str_to_lower)
}

haversine_km <- function(lat1, lon1, lat2, lon2) {
  rad <- pi / 180
  a <- sin((lat2 - lat1) * rad / 2)^2 +
    cos(lat1 * rad) * cos(lat2 * rad) * sin((lon2 - lon1) * rad / 2)^2
  6371 * 2 * asin(pmin(1, sqrt(a)))
}

# Reads a text file whose lines are a mix of UTF-8 and Latin-1 / Windows-1252
read_lines_mixed_encoding <- function(f) {
  x  <- readLines(f, warn = FALSE)
  ok <- validUTF8(x)
  Encoding(x[ok]) <- "UTF-8"
  x[!ok] <- iconv(x[!ok], from = "CP1252", to = "UTF-8")
  x <- str_remove(x, "^\ufeff")
  str_remove(x, "\r$")
}

# ---- OpenStreetMap lake areas ----
# Water polygons (natural=water, landuse=reservoir) around a point, from the Overpass API
osm_water_polygons <- function(lat, lon, half_box_deg = 0.03) {
  bb  <- c(lon - half_box_deg, lat - half_box_deg, lon + half_box_deg, lat + half_box_deg)
  res <- osmdata::opq(bbox = bb, timeout = 120) %>%
    osmdata::add_osm_features(features = c('"natural"="water"', '"landuse"="reservoir"')) %>%
    osmdata::osmdata_sf()
  bind_osm_polygons(list(res$osm_polygons, res$osm_multipolygons))
}

# Polygons + multipolygons in one sf table with the tags we need
bind_osm_polygons <- function(parts) {
  tags  <- c("osm_id", "name", "natural", "landuse", "water")
  parts <- lapply(parts, function(p) {
    if (is.null(p) || nrow(p) == 0) return(NULL)
    for (tg in tags) if (!tg %in% names(p)) p[[tg]] <- NA_character_
    sf::st_sf(lapply(sf::st_drop_geometry(p)[, tags], as.character),
              geometry = sf::st_geometry(p))
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (length(parts) == 0) return(NULL)
  do.call(rbind, parts)
}

# Picks the water polygon containing the point (largest if several), else the nearest
# one within snap_m. Rivers and canals are ignored.
pick_water_polygon <- function(polys, lat, lon, snap_m) {
  none <- tibble(osm_id = NA_character_, osm_name = NA_character_, osm_tag = NA_character_,
                 area_ha = NA_real_, snap_dist_m = NA_real_, status = "no water polygon nearby")
  if (is.null(polys) || nrow(polys) == 0) return(none)
  polys <- polys[(polys$natural %in% "water" | polys$landuse %in% "reservoir") &
                   !(polys$water %in% c("river", "canal", "stream", "ditch", "drain",
                                        "wastewater", "lock", "fish_pass", "moat")), ]
  if (nrow(polys) == 0) return(none)
  polys  <- sf::st_make_valid(polys)
  pt     <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)
  area   <- as.numeric(sf::st_area(polys)) / 1e4
  dist   <- as.numeric(sf::st_distance(polys, pt))[seq_len(nrow(polys))]
  inside <- which(dist == 0)
  i <- if (length(inside) > 0) inside[which.max(area[inside])] else which.min(dist)
  if (dist[i] > snap_m) return(mutate(none, status = paste0("nearest water polygon > ", snap_m, " m")))
  tibble(osm_id   = polys$osm_id[i], osm_name = polys$name[i],
         osm_tag  = paste(na.omit(c(polys$natural[i], polys$landuse[i], polys$water[i])), collapse = "/"),
         area_ha  = round(area[i], 2), snap_dist_m = round(dist[i]),
         status   = if (dist[i] == 0) "point inside polygon" else "nearest polygon")
}

# Reads a csv that may have been re-saved by Excel (";" separator, decimal commas): all as text
csv_delim <- function(f) {
  first <- readLines(f, n = 1, warn = FALSE, encoding = "UTF-8")
  if (str_count(first, ";") > str_count(first, ",")) ";" else ","
}
# Writes a table of text columns in the same format as the user's file (";" + decimal comma, or ",")
write_user_csv <- function(df, f, delim = ",") {
  if (delim == ";") {
    df <- mutate(df, across(any_of(c("lat", "lon", "max_depth_m", "area_ha")),
                            ~ str_replace(as.character(.x), "\\.", ",")))
    write_excel_csv2(df, f, na = "")
  } else write_excel_csv(df, f, na = "")
}
read_user_csv <- function(f) {
  delim <- csv_delim(f)
  read_delim(f, delim = delim, col_types = cols(.default = "c"), na = c("", "NA"),
             locale = locale(encoding = "UTF-8"), trim_ws = TRUE)
}
parse_coord <- function(x) as.numeric(str_replace(str_squish(x), ",", "."))

find_col <- function(df, pattern) {
  hit <- names(df)[str_detect(str_to_lower(names(df)), pattern)]
  if (length(hit) == 0) NA_character_ else hit[1]
}


# =============================================================================
# 1. ACA
# =============================================================================

aca_enc <- coalesce(readr::guess_encoding(aca_file)$encoding[1], "UTF-8")
aca_raw <- read_csv(aca_file, col_types = cols(.default = col_character()),
                    locale = locale(encoding = aca_enc), na = c("", "NA"))
names(aca_raw) <- names(aca_raw) %>%
  stringi::stri_trans_general("Latin-ASCII") %>%
  str_to_lower() %>%
  str_replace_all("[^a-z0-9]+", ".") %>%
  str_remove_all("^\\.|\\.$")
cat("ACA file read with encoding", aca_enc, "—", nrow(aca_raw), "rows\n")
glimpse(aca_raw)

missing_cols <- setdiff(aca_cols, names(aca_raw))
if (length(missing_cols) > 0)
  stop("ACA columns not found: ", paste(missing_cols, collapse = ", "),
       "\nAvailable: ", paste(names(aca_raw), collapse = ", "))

# Optional columns, detected by name
aca_unit_col  <- find_col(aca_raw, "^unitat|^unit")
aca_depth_col <- find_col(aca_raw, "profund|fondaria|depth")
aca_x_col     <- find_col(aca_raw, "utm.?x|coord.*x|^x$")
aca_y_col     <- find_col(aca_raw, "utm.?y|coord.*y|^y$")
aca_lat_col   <- find_col(aca_raw, "^lat")
aca_lon_col   <- find_col(aca_raw, "^lon|^lng")
cat("\nOptional ACA columns found:\n")
print(c(unit = aca_unit_col, depth = aca_depth_col, x = aca_x_col, y = aca_y_col,
        lat = aca_lat_col, lon = aca_lon_col))

aca_raw <- filter(aca_raw, !is.na(.data[[aca_cols[["code"]]]]))     # drop empty rows

date_chr    <- aca_raw[[aca_cols[["date"]]]]
date_orders <- if (mean(str_detect(date_chr, "^\\d{1,2}[/.-]"), na.rm = TRUE) > 0.5)
  c("dmY", "dmY HMS", "dmy") else c("Ymd", "Ymd HMS")

aca <- aca_raw %>%
  transmute(
    source       = "ACA",
    site_code    = .data[[aca_cols[["code"]]]],
    site_name    = str_squish(.data[[aca_cols[["name"]]]]),
    var_original = str_squish(.data[[aca_cols[["variable"]]]]),
    value_chr    = str_squish(.data[[aca_cols[["value"]]]]),
    unit_original = if (!is.na(aca_unit_col)) str_squish(.data[[aca_unit_col]]) else NA_character_,
    depth_m      = if (!is.na(aca_depth_col))
      suppressWarnings(parse_number(.data[[aca_depth_col]], na = c("", "-", "NA")))
    else NA_real_,
    date         = as.Date(parse_date_time(.data[[aca_cols[["date"]]]], orders = date_orders,
                                           quiet = TRUE)),
    campaign     = NA_character_
  ) %>%
  mutate(
    var_key  = var_original %>% str_to_lower() %>% str_replace_all("°", "º") %>% str_squish(),
    method   = str_match(var_key, "\\((camp|laboratori)\\)\\s*$")[, 2],
    var_key  = str_squish(str_remove(var_key, "\\((camp|laboratori)\\)\\s*$")),
    var      = coalesce(unname(aca_var_map[var_key]), var_original),
    # units: from the file if there is a unit column, otherwise the assumed ones
    unit_original = coalesce(unit_original, unname(aca_assumed_units[var])),
    # values: "<0.01" = below detection limit; comma or point decimals
    censored = str_detect(value_chr, "^[<>]") %in% TRUE,
    value_raw = parse_number(str_replace(value_chr, "(\\d),(\\d)", "\\1.\\2")),
    value_raw = if_else(censored & str_detect(value_chr, "^<") & censored_value == "half",
                        value_raw / 2, value_raw)
  )

if (is.na(aca_unit_col))
  message("No unit column in the ACA file: units in 'aca_assumed_units' were used. CHECK them ",
          "in unit_conversions.csv and cross_source_check.csv.")

cat("\nACA dates: ", format(min(aca$date, na.rm = TRUE)), "to", format(max(aca$date, na.rm = TRUE)),
    "|", sum(is.na(aca$date)), "unreadable\n")

# Deepest sample per station (before the depth filter below)
aca_depth_info <- aca %>%
  filter(!is.na(depth_m)) %>%
  group_by(site_code) %>%
  summarise(max_depth_sampled_m = max(depth_m), n_depths = n_distinct(depth_m), .groups = "drop")

if (!is.null(aca_max_depth_m) && !is.na(aca_depth_col))
  aca <- filter(aca, is.na(depth_m) | depth_m <= aca_max_depth_m)
if (!is.null(aca_min_year))
  aca <- filter(aca, year(date) >= aca_min_year)

cat("\nACA variables not in aca_var_map (kept under their own name):\n")
print(sort(unique(aca$var_original[is.na(aca_var_map[aca$var_key])])))

cat("\nACA values that could not be read as numbers:\n")
print(count(filter(aca, is.na(value_raw), !is.na(value_chr)), value_chr, sort = TRUE))

# ACA site coordinates (if the file has them)
aca_coords <- NULL
if (!is.na(aca_lat_col) && !is.na(aca_lon_col)) {
  aca_coords <- aca_raw %>%
    transmute(site_code = .data[[aca_cols[["code"]]]],
              lat = parse_number(.data[[aca_lat_col]]),
              lon = parse_number(.data[[aca_lon_col]]))
} else if (!is.na(aca_x_col) && !is.na(aca_y_col)) {
  if (requireNamespace("sf", quietly = TRUE)) {
    xy <- aca_raw %>%
      transmute(site_code = .data[[aca_cols[["code"]]]],
                x = parse_number(.data[[aca_x_col]]),
                y = parse_number(.data[[aca_y_col]])) %>%
      filter(!is.na(x), !is.na(y)) %>%
      distinct(site_code, .keep_all = TRUE)
    ll <- sf::st_as_sf(xy, coords = c("x", "y"), crs = aca_utm_crs) %>%
      sf::st_transform(4326) %>% sf::st_coordinates()
    aca_coords <- tibble(site_code = xy$site_code, lat = ll[, "Y"], lon = ll[, "X"])
  } else {
    warning("ACA has X/Y columns but package 'sf' is not installed: install.packages('sf') to use them.")
  }
}
if (!is.null(aca_coords)) {
  aca_coords <- aca_coords %>%
    filter(!is.na(lat), !is.na(lon)) %>%
    group_by(site_code) %>%
    summarise(lat = mean(lat), lon = mean(lon), .groups = "drop") %>%
    mutate(coord_source = "ACA file")
}


# =============================================================================
# 2. M100 (Margalef et al., 1970s)
# =============================================================================

# Names: tab-separated, mixed encoding
m100_names <- read_lines_mixed_encoding(m100_names_file) %>%
  discard(~ .x == "") %>%
  tail(-1) %>%                                         # header "num nom"
  str_split_fixed("\t", 2) %>%
  as_tibble(.name_repair = ~ c("num", "nom")) %>%
  mutate(num = as.integer(num), nom = str_squish(nom)) %>%
  filter(!is.na(num))

m100_raw <- read_csv(m100_file, col_types = cols(.default = col_character()),
                     na = c("", "NA")) %>%
  filter(!is.na(CodiEmbass))
cat("\nM100:", nrow(m100_raw), "rows,", n_distinct(m100_raw$CodiEmbass), "reservoirs\n")
cat("Campaigns:", paste(sort(unique(m100_raw$Campanya)), collapse = ", "),
    "| Layers:", paste(sort(unique(m100_raw$CAPA)), collapse = ", "), "\n")

meas_cols <- setdiff(names(m100_raw), m100_meta_cols)

m100 <- m100_raw %>%
  mutate(CodiEmbass = as.integer(CodiEmbass)) %>%
  { if (is.null(m100_layers)) . else filter(., CAPA %in% m100_layers) } %>%
  pivot_longer(all_of(meas_cols), names_to = "column", values_to = "value_chr") %>%
  filter(!is.na(value_chr)) %>%
  left_join(m100_names, by = c("CodiEmbass" = "num")) %>%
  transmute(
    source        = "M100",
    site_code     = as.character(CodiEmbass),
    site_name     = title_name(coalesce(nom, paste("Embalse", CodiEmbass))),
    var_original  = column,
    unit_original = column %>% str_remove("^\\S+\\s*") %>% na_if("") %>%
      str_replace("^uS cm$", "uS/cm"),
    var           = coalesce(unname(m100_var_map[column]), word(column, 1)),
    method        = NA_character_,
    depth_m       = parse_number(`Fond m`),
    layer         = CAPA,
    date          = as.Date(NA),
    campaign      = Campanya,
    censored      = FALSE,
    value_raw     = parse_number(value_chr)
  ) %>%
  # Secchi is one value per visit, repeated on every depth row: keep it once
  group_by(site_code, campaign, var) %>%
  filter(!(var %in% site_level_vars) | row_number() == 1) %>%
  ungroup()

if (any(is.na(m100_names$nom[match(unique(m100$site_code), m100_names$num)])))
  warning("Some M100 reservoir codes have no name in the names file.")

m100_site_meta <- m100_raw %>%
  group_by(site_code = CodiEmbass) %>%
  summarise(m100_any = first(Any),          # looks like the dam construction year, not the sampling year
            m100_grup = first(Grup), m100_grupsimp = first(GrupSIMP),
            m100_grup2 = first(Grup2),
            # deepest sample, all layers (F = near the bottom)
            max_depth_sampled_m = suppressWarnings(max(parse_number(`Fond m`), na.rm = TRUE)),
            n_depths = n_distinct(`Fond m`), .groups = "drop") %>%
  mutate(max_depth_sampled_m = na_if(max_depth_sampled_m, -Inf))


# =============================================================================
# 3. Merge & harmonise units
# =============================================================================

db_raw <- bind_rows(aca, m100)

conversions <- db_raw %>%
  count(source, var, unit_original, name = "n_rows") %>%
  mutate(factor = map2_dbl(var, unit_original, unit_factor),
         unit   = if_else(var %in% names(target_unit),
                          unname(target_unit[var]), unit_original))

cat("\nUnit conversions applied to the harmonised variables (check!):\n")
conversions %>%
  filter(var %in% names(target_unit)) %>%
  arrange(var, source) %>%
  print(n = Inf)

bad_units <- filter(conversions, is.na(factor))
if (nrow(bad_units) > 0) {
  warning("Units not understood (values set to NA) — fix unit strings or aca_assumed_units:\n",
          paste(capture.output(print(bad_units)), collapse = "\n"))
}

db <- db_raw %>%
  left_join(select(conversions, source, var, unit_original, factor, unit),
            by = c("source", "var", "unit_original")) %>%
  mutate(value = value_raw * factor)

# Optional: N and P species as N / P
if (nutrients_as == "element") {
  el_fac  <- c(PO4 = molar_mass[["P"]] / molar_mass[["PO4"]],
               NO3 = molar_mass[["N"]] / molar_mass[["NO3"]],
               NH4 = molar_mass[["N"]] / molar_mass[["NH4"]])
  el_unit <- c(PO4 = "mg P-PO4/L", NO3 = "mg N-NO3/L", NH4 = "mg N-NH4/L")
  db <- db %>%
    mutate(value = if_else(var %in% names(el_fac), value * el_fac[var], value),
           unit  = if_else(var %in% names(el_unit), el_unit[var], unit))
  target_unit[names(el_unit)] <- el_unit
}

# Implausible values (usually a unit problem)
implausible <- db %>%
  inner_join(plausible, by = "var") %>%
  filter(value < lo | value > hi) %>%
  count(source, var, unit, name = "n_outside_range")
cat("\nValues outside plausible ranges (check units if many):\n")
print(implausible)

db <- db %>% filter(!is.na(value))

# Field vs lab measurements kept apart if requested ("CHLa_camp", "CHLa_laboratori")
if (!pool_methods)
  db <- mutate(db, var = if_else(is.na(method), var, paste(var, method, sep = "_")))


# =============================================================================
# 4. Sites, lake keys and coordinates
# =============================================================================

coords_cache_file  <- file.path(out_dir, "coordinates_geocoded_cache.csv")

manual_raw <- if (file.exists(coords_manual_file)) {
  read_user_csv(coords_manual_file)
} else tibble(site_uid = character(), site_name = character(), lat = character(),
              lon = character(), note = character())
if (nrow(manual_raw) > 0 && !all(c("site_uid", "lat", "lon") %in% names(manual_raw)))
  stop(coords_manual_file, " must have columns site_uid, lat, lon (found: ",
       paste(names(manual_raw), collapse = ", "), ")")
for (cc in c("site_name", "note")) if (!cc %in% names(manual_raw)) manual_raw[[cc]] <- NA_character_
manual_raw <- manual_raw %>% filter(!is.na(site_uid)) %>% distinct(site_uid, .keep_all = TRUE)
cat("\ncoordinates_manual.csv:", nrow(manual_raw), "sites,",
    sum(!is.na(parse_coord(manual_raw$lat))), "with coordinates\n")

# Names corrected in coordinates_manual.csv replace the original ones
name_fixes <- manual_raw %>%
  filter(!is.na(site_name), str_squish(site_name) != "") %>%
  distinct(site_uid, .keep_all = TRUE) %>%
  transmute(site_uid, site_name_manual = str_squish(site_name))

sites <- db %>%
  distinct(source, site_code, site_name) %>%
  mutate(site_uid = paste(source, site_code, sep = "_")) %>%
  group_by(site_uid) %>%
  slice(1) %>%                                              # one name per station code
  ungroup() %>%
  left_join(name_fixes, by = "site_uid") %>%
  mutate(name_original = site_name,
         site_name = coalesce(site_name_manual, site_name),
         lake_key  = lake_key_from_name(site_name)) %>%
  select(-site_name_manual)

renamed <- filter(sites, site_name != name_original)
if (nrow(renamed) > 0) {
  cat("\nNames corrected from coordinates_manual.csv:\n")
  print(select(renamed, site_uid, name_original, site_name), n = Inf)
}

dup_names <- db %>% distinct(source, site_code, site_name) %>% add_count(source, site_code) %>% filter(n > 1)
if (nrow(dup_names) > 0) {
  cat("\nStation codes with more than one name (first one kept):\n"); print(dup_names)
}

# Automatic matching across sources: M100 name contained in an ACA name
aca_keys <- unique(sites$lake_key[sites$source == "ACA"])
auto_match <- sites %>%
  filter(source == "M100", !lake_key %in% aca_keys) %>%
  mutate(match = map_chr(lake_key, function(k) {
    hit <- aca_keys[str_detect(aca_keys, paste0("\\b", k, "\\b"))]
    if (length(hit) == 1) hit else NA_character_
  })) %>%
  filter(!is.na(match))
if (nrow(auto_match) > 0) {
  cat("\nM100 reservoirs matched to ACA by name (check):\n")
  print(select(auto_match, site_uid, site_name, lake_key, matched_to = match))
  sites <- sites %>%
    rows_update(transmute(auto_match, site_uid, lake_key = match), by = "site_uid")
}
if (nrow(lake_crosswalk) > 0)
  sites <- rows_update(sites, lake_crosswalk, by = "site_uid", unmatched = "ignore")

cat("\nLakes present in both sources:\n")
print(sites %>% group_by(lake_key) %>% filter(n_distinct(source) > 1) %>%
        arrange(lake_key) %>% select(lake_key, source, site_uid, site_name), n = Inf)

# ---- Coordinates: ACA file > manual file > geocoding cache > new geocoding ----

coords_manual <- manual_raw %>%
  transmute(site_uid, lat = parse_coord(lat), lon = parse_coord(lon), coord_source = "manual") %>%
  filter(!is.na(lat), !is.na(lon))

coords_cache <- if (file.exists(coords_cache_file)) {
  read_user_csv(coords_cache_file) %>% mutate(lat = parse_coord(lat), lon = parse_coord(lon))
} else tibble(site_uid = character(), lat = double(), lon = double(), coord_source = character(),
              osm_class = character(), osm_type = character(), osm_name = character())

coords_aca <- if (is.null(aca_coords)) NULL else
  aca_coords %>% transmute(site_uid = paste("ACA", site_code, sep = "_"), lat, lon, coord_source)

coords_known <- bind_rows(coords_manual, coords_aca,
                          filter(coords_cache, !is.na(lat))) %>%
  distinct(site_uid, .keep_all = TRUE)                      # first source wins

to_geocode <- sites %>% filter(!site_uid %in% coords_known$site_uid,
                               !site_uid %in% manual_raw$site_uid,     # your file: never looked up again
                               !site_uid %in% coords_cache$site_uid,   # earlier failures: not retried
                               !str_detect(site_name, "^Embalse \\d+$"))  # no real name: nothing to search

cat("\nCoordinates reused: ",
    sum(sites$site_uid %in% coords_manual$site_uid), " from coordinates_manual.csv, ",
    sum(sites$site_uid %in% setdiff(coords_aca$site_uid, coords_manual$site_uid)), " from the ACA file, ",
    sum(sites$site_uid %in% setdiff(filter(coords_cache, !is.na(lat))$site_uid,
                                    c(coords_manual$site_uid, coords_aca$site_uid))), " from the cache",
    " | to look up now: ", nrow(to_geocode), "\n", sep = "")

if (geocode_missing && nrow(to_geocode) > 0) {
  if (!requireNamespace("tidygeocoder", quietly = TRUE)) {
    warning("Install 'tidygeocoder' to look up missing coordinates: install.packages('tidygeocoder')")
  } else {
    cat("Geocoding", nrow(to_geocode), "sites with OpenStreetMap (1 query/s);",
        "results are saved after each site, so you can stop and resume.\n")
    # Query variants, tried in order until one returns a result
    queries <- function(name, source) {
      if (source == "ACA") c(name, paste(name, "Catalunya"))
      else c(paste("Embalse de", name), paste("Embalse del", name), paste("Pantano de", name),
             paste("Encoro de", name), paste("Embassament de", name), paste(name, "embalse"))
    }
    geo_one <- function(q) {
      res <- tryCatch(
        tidygeocoder::geo(address = q, method = "osm", full_results = TRUE, limit = 1,
                          custom_query = list(countrycodes = "es"), quiet = TRUE),
        error = function(e) NULL)
      if (is.null(res) || is.na(res$lat[1])) return(NULL)
      tibble(lat = res$lat[1], lon = res$long[1],
             osm_class = if ("class" %in% names(res)) as.character(res$class[1]) else NA,
             osm_type  = if ("type" %in% names(res)) as.character(res$type[1]) else NA,
             osm_name  = if ("display_name" %in% names(res)) res$display_name[1] else NA,
             coord_source = paste0("OSM: ", q))
    }
    for (i in seq_len(nrow(to_geocode))) {
      uid <- to_geocode$site_uid[i]
      r <- NULL
      for (q in queries(to_geocode$site_name[i], to_geocode$source[i])) {
        r <- geo_one(q); if (!is.null(r)) break
      }
      if (is.null(r)) r <- tibble(lat = NA_real_, lon = NA_real_, coord_source = "OSM: not found")
      r <- mutate(r, site_uid = uid, .before = 1)
      cat(sprintf("  [%d/%d] %s: %s\n", i, nrow(to_geocode), to_geocode$site_name[i], r$coord_source))
      coords_cache <- bind_rows(filter(coords_cache, site_uid != uid), r)
      write_excel_csv(coords_cache, coords_cache_file, na = "")          # saved at every step
      if (!is.na(r$lat)) coords_known <- bind_rows(coords_known, r)
    }
    coords_known <- distinct(coords_known, site_uid, .keep_all = TRUE)
  }
}

water_classes <- c("water", "reservoir", "dam", "lake", "basin", "weir", "natural", "waterway")

sites <- sites %>%
  left_join(select(coords_known, site_uid, lat, lon, coord_source,
                   any_of(c("osm_class", "osm_type", "osm_name"))), by = "site_uid")
for (cc in c("osm_class", "osm_type", "osm_name"))
  if (!cc %in% names(sites)) sites[[cc]] <- NA_character_
sites <- sites %>%
  mutate(
    dist_bcn_km = round(haversine_km(bcn[["lat"]], bcn[["lon"]], lat, lon)),
    coord_check = case_when(
      is.na(lat) ~ "MISSING",
      str_starts(coord_source, "OSM") &
        !(coalesce(osm_type, "") %in% water_classes | coalesce(osm_class, "") %in% water_classes) ~
        "CHECK: geocoded result is not a water feature",
      str_starts(coord_source, "OSM") ~ "geocoded: check on map",
      TRUE ~ "ok")
  ) %>%
  left_join(mutate(m100_site_meta, site_uid = paste("M100", site_code, sep = "_")) %>%
              select(-site_code, -max_depth_sampled_m, -n_depths), by = "site_uid") %>%
  left_join(bind_rows(
    mutate(aca_depth_info, site_uid = paste("ACA", site_code, sep = "_")),
    transmute(m100_site_meta, site_uid = paste("M100", site_code, sep = "_"),
              max_depth_sampled_m, n_depths)) %>%
      select(site_uid, max_depth_sampled_m, n_depths),
    by = "site_uid")

# Different lakes with (almost) the same coordinates: usually a wrong geocode or copy-paste.
# Pairs where both points come from the ACA file are not checked (stations are surveyed).
pts <- sites %>% filter(!is.na(lat)) %>% select(site_uid, site_name, lake_key, lat, lon, coord_source)
close_pairs <- cross_join(pts, pts, suffix = c("_1", "_2")) %>%
  filter(site_uid_1 < site_uid_2, lake_key_1 != lake_key_2,
         !(coord_source_1 == "ACA file" & coord_source_2 == "ACA file")) %>%
  mutate(dist_m = round(1000 * haversine_km(lat_1, lon_1, lat_2, lon_2))) %>%
  filter(dist_m < close_coords_m) %>%
  select(site_uid_1, site_name_1, site_uid_2, site_name_2, dist_m, lat_1, lon_1, lat_2, lon_2)
write_excel_csv(close_pairs, file.path(out_dir, "coordinates_close_pairs.csv"), na = "")
if (nrow(close_pairs) > 0) {
  warning(nrow(close_pairs), " pairs of different lakes are less than ", close_coords_m,
          " m apart — probably wrong coordinates (see coordinates_close_pairs.csv)")
  cat("\nDifferent lakes with (almost) the same coordinates:\n")
  print(select(close_pairs, site_uid_1, site_name_1, site_uid_2, site_name_2, dist_m), n = Inf)
  flagged <- unique(c(close_pairs$site_uid_1, close_pairs$site_uid_2))
  sites <- mutate(sites, coord_check = if_else(site_uid %in% flagged,
                                               "CHECK: same place as another lake", coord_check))
}

n_missing <- sum(sites$coord_check == "MISSING")
if (n_missing > 0) {
  warning(n_missing, " sites have no coordinates — fill in lat/lon in ", coords_manual_file,
          " and run this script again")
  cat("\nSites without coordinates:\n")
  print(filter(sites, coord_check == "MISSING") %>% select(site_uid, site_name), n = Inf)
}
cat("\nCoordinate status:\n")
print(count(sites, source, coord_check))

# Template for manual coordinates: existing manual entries + sites missing / to check
manual_delim <- if (file.exists(coords_manual_file)) csv_delim(coords_manual_file) else ","
manual_template <- bind_rows(
  manual_raw,
  sites %>% filter(coord_check != "ok", !site_uid %in% manual_raw$site_uid) %>%
    transmute(site_uid, site_name, lat = as.character(lat), lon = as.character(lon),
              note = coord_check)
) %>% distinct(site_uid, .keep_all = TRUE)
write_user_csv(manual_template, coords_manual_file, manual_delim)


# =============================================================================
# 5. Final long database
# =============================================================================

db <- db %>%
  mutate(site_uid = paste(source, site_code, sep = "_")) %>%
  select(-site_name) %>%
  left_join(select(sites, site_uid, site_name, lake_key), by = "site_uid") %>%
  mutate(year     = year(date),
         event    = coalesce(as.character(date), campaign),   # a sampling occasion
         priority = var %in% priority_vars) %>%
  select(source, site_uid, site_code, site_name, lake_key, date, year, campaign, event,
         depth_m, layer, var, value, unit, censored, method, priority,
         var_original, value_raw, unit_original)

write_rds(db, file.path(out_dir, "wq_database_long.rds"))
write_excel_csv(db, file.path(out_dir, "wq_database_long.csv"), na = "")
write_excel_csv(sites, file.path(out_dir, "sites.csv"), na = "")
write_excel_csv(conversions, file.path(out_dir, "unit_conversions.csv"), na = "")

var_dict <- db %>%
  group_by(var, unit) %>%
  summarise(priority = first(priority),
            sources = paste(sort(unique(source)), collapse = "+"),
            original_names = paste(sort(unique(paste0(source, ": ", var_original))), collapse = " | "),
            n_obs = n(), .groups = "drop") %>%
  mutate(description = unname(var_description[var])) %>%
  arrange(desc(priority), match(var, priority_vars), var) %>%
  relocate(description, .after = unit)
write_excel_csv(var_dict, file.path(out_dir, "variable_dictionary.csv"), na = "")


# =============================================================================
# 6. Summary tables
# =============================================================================

summary_stats <- function(df) {
  df %>% summarise(
    n_obs      = n(),
    n_events   = n_distinct(event),
    n_censored = sum(censored),
    first_year = if (all(is.na(year))) NA_integer_ else as.integer(min(year, na.rm = TRUE)),
    last_year  = if (all(is.na(year))) NA_integer_ else as.integer(max(year, na.rm = TRUE)),
    median     = median(value),
    q25        = unname(quantile(value, 0.25)),
    q75        = unname(quantile(value, 0.75)),
    mean       = mean(value),
    sd         = sd(value),
    min        = min(value),
    max        = max(value),
    .groups    = "drop")
}

# 6a. Per source x site x variable (tidy, all variables)
summary_site <- db %>%
  group_by(source, site_uid, site_name, lake_key, priority, var, unit) %>%
  summary_stats() %>%
  mutate(period = if_else(source == "M100", "1970s",
                          paste(first_year, last_year, sep = "–"))) %>%
  left_join(select(sites, site_uid, lat, lon, dist_bcn_km, coord_check), by = "site_uid") %>%
  arrange(desc(priority), match(var, priority_vars), var, source, site_name)

write_excel_csv(summary_site, file.path(out_dir, "summary_by_site_variable.csv"), na = "")
write_rds(summary_site, file.path(out_dir, "summary_by_site_variable.rds"))

# 6b. Per lake (sources and stations pooled per source), priority variables in columns.
lake_stats <- db %>%
  filter(priority) %>%
  group_by(lake_key, source, var) %>%
  summary_stats()

lake_best <- lake_stats %>%
  mutate(src_rank = match(source, source_priority)) %>%
  group_by(lake_key, var) %>%
  slice_min(src_rank, n = 1, with_ties = FALSE) %>%
  ungroup()

lake_coords <- sites %>%
  mutate(src_rank = match(source, source_priority)) %>%
  filter(!is.na(lat)) %>%
  group_by(lake_key) %>%
  slice_min(src_rank, n = 1, with_ties = TRUE) %>%
  summarise(lat = mean(lat), lon = mean(lon), coord_source = first(coord_source),
            coord_check = first(coord_check), .groups = "drop")

lake_info <- sites %>%
  mutate(src_rank = match(source, source_priority)) %>%
  arrange(src_rank) %>%
  group_by(lake_key) %>%
  summarise(lake_name = first(site_name),
            sources   = paste(sort(unique(source)), collapse = "+"),
            site_uids = paste(site_uid, collapse = ";"),
            .groups = "drop")

# ---- 6b'. Lake depth & area ----------------------------------------------------

# Depth: deepest sample over all stations and both sources
lake_depth <- sites %>%
  group_by(lake_key) %>%
  summarise(
    max_depth_sampled_m = if (all(is.na(max_depth_sampled_m))) NA_real_ else max(max_depth_sampled_m, na.rm = TRUE),
    n_depths            = if (all(is.na(n_depths))) NA_integer_ else as.integer(max(n_depths, na.rm = TRUE)),
    .groups = "drop")

# Area: OpenStreetMap polygons, cached (a lake is looked up again only if its point moved)
osm_cache_file <- file.path(out_dir, "lake_area_osm_cache.csv")
osm_cache <- if (file.exists(osm_cache_file)) {
  read_user_csv(osm_cache_file) %>%
    mutate(across(any_of(c("lat", "lon", "area_ha", "snap_dist_m")), parse_coord))
} else tibble(lake_key = character(), lat = double(), lon = double(), osm_id = character(),
              osm_name = character(), osm_tag = character(), area_ha = double(),
              snap_dist_m = double(), status = character())

attr_manual_file <- file.path(out_dir, "lake_attributes_manual.csv")
has_manual_area <- if (file.exists(attr_manual_file)) {
  read_user_csv(attr_manual_file) %>% filter(!is.na(parse_coord(area_ha))) %>% pull(lake_key)
} else character()

to_query <- lake_coords %>%
  filter(!lake_key %in% has_manual_area) %>%                 # area already given by hand
  left_join(select(osm_cache, lake_key, lat_c = lat, lon_c = lon, status), by = "lake_key") %>%
  filter(is.na(status) | str_starts(status, "error") |
           abs(lat - lat_c) > 1e-4 | abs(lon - lon_c) > 1e-4) %>%
  select(lake_key, lat, lon)

if (get_osm_area && nrow(to_query) > 0) {
  if (!requireNamespace("osmdata", quietly = TRUE) || !requireNamespace("sf", quietly = TRUE)) {
    warning("Install 'osmdata' and 'sf' to get lake areas from OpenStreetMap: ",
            "install.packages(c('osmdata', 'sf'))")
  } else {
    cat("\nLake areas from OpenStreetMap for", nrow(to_query), "lakes (a few seconds each);",
        "saved after each lake, so you can stop and resume.\n")
    n_err <- 0
    for (i in seq_len(nrow(to_query))) {
      k <- to_query$lake_key[i]; la <- to_query$lat[i]; lo <- to_query$lon[i]
      Sys.sleep(1)                                     # be gentle with the Overpass server
      out <- tryCatch(pick_water_polygon(osm_water_polygons(la, lo), la, lo, osm_snap_m),
                      error = function(e) tibble(status = paste("error:", conditionMessage(e))))
      out <- bind_cols(tibble(lake_key = k, lat = la, lon = lo), out)
      n_err <- n_err + str_starts(out$status, "error")
      cat(sprintf("  [%d/%d] %s: %s\n", i, nrow(to_query), k,
                  if (is.na(out$area_ha %||% NA)) out$status else paste(out$area_ha, "ha")))
      osm_cache <- bind_rows(filter(osm_cache, lake_key != k), out)
      write_excel_csv(osm_cache, osm_cache_file, na = "")              # saved at every step
    }
    if (n_err > 0) warning(n_err, " OpenStreetMap queries failed (server busy?): run the script again to retry them.")
  }
}

# Manual values (e.g. from the dam inventory, bathymetric maps, papers) override the rest
attr_manual <- if (file.exists(attr_manual_file)) {
  read_user_csv(attr_manual_file) %>%
    transmute(lake_key, max_depth_manual_m = parse_coord(max_depth_m),
              area_manual_ha = parse_coord(area_ha), attr_note = note)
} else tibble(lake_key = character(), max_depth_manual_m = double(),
              area_manual_ha = double(), attr_note = character())

lake_attributes <- lake_info %>%
  select(lake_key) %>%
  left_join(lake_depth, by = "lake_key") %>%
  left_join(select(osm_cache, lake_key, osm_id, osm_name, osm_tag, area_osm_ha = area_ha,
                   osm_snap_dist_m = snap_dist_m, osm_status = status), by = "lake_key") %>%
  left_join(attr_manual, by = "lake_key") %>%
  mutate(
    max_depth_m  = coalesce(max_depth_manual_m, max_depth_sampled_m),
    depth_source = case_when(!is.na(max_depth_manual_m) ~ "manual",
                             !is.na(max_depth_sampled_m) ~ "deepest sample",
                             TRUE ~ NA_character_),
    # TRUE = trustworthy: manual, or sampled along a profile. FALSE = only a lower bound.
    depth_known  = !is.na(max_depth_manual_m) |
      (!is.na(n_depths) & n_depths >= min_n_depths_profile),
    area_ha      = coalesce(area_manual_ha, area_osm_ha),
    area_source  = case_when(!is.na(area_manual_ha) ~ "manual",
                             !is.na(area_osm_ha) ~ "OpenStreetMap",
                             TRUE ~ NA_character_),
    size_flag    = if_else(!is.na(area_ha) & area_ha < small_lake_ha,
                           paste0("< ", small_lake_ha, " ha"), NA_character_)
  )

# Template for manual attributes: keeps what is already filled in, adds new lakes
attr_template <- lake_info %>%
  select(lake_key, lake_name) %>%
  left_join(select(lake_attributes, lake_key, max_depth_sampled_m, n_depths, area_osm_ha), by = "lake_key") %>%
  left_join(if (file.exists(attr_manual_file))
    read_user_csv(attr_manual_file) %>%
      select(lake_key, any_of(c("max_depth_m", "area_ha", "note")))
    else tibble(lake_key = character(), max_depth_m = character(),
                area_ha = character(), note = character()),
    by = "lake_key") %>%
  relocate(max_depth_m, area_ha, note, .after = lake_name)
write_user_csv(mutate(attr_template, across(everything(), as.character)), attr_manual_file,
               if (file.exists(attr_manual_file)) csv_delim(attr_manual_file) else ",")

small <- filter(lake_attributes, !is.na(size_flag))
if (nrow(small) > 0) {
  warning(nrow(small), " lakes are smaller than ", small_lake_ha, " ha (column size_flag)")
  cat("\nLakes smaller than", small_lake_ha, "ha:\n")
  print(small %>% left_join(select(lake_info, lake_key, lake_name), by = "lake_key") %>%
          select(lake_name, area_ha, area_source, osm_name), n = Inf)
}
cat("\nDepth and area availability (lakes):\n")
print(lake_attributes %>% summarise(
  n_lakes = n(), depth_any = sum(!is.na(max_depth_m)), depth_profiled_or_manual = sum(depth_known),
  area = sum(!is.na(area_ha)), smaller_than_flag = nrow(small)))

unit_suffix <- function(v) {
  u <- target_unit[v]
  ifelse(is.na(u) | u == "-", v, paste0(v, " [", u, "]"))
}

lake_wide <- lake_best %>%
  select(lake_key, var, median, n_obs, source) %>%
  mutate(var = factor(var, levels = intersect(priority_vars, unique(var)))) %>%
  arrange(var) %>%
  pivot_wider(names_from = var, values_from = c(median, n_obs, source),
              names_glue = "{var}_{.value}", names_vary = "slowest")

oecd_class <- function(x, breaks) {
  cut(x, c(-Inf, breaks, Inf), labels = c("oligotrophic", "mesotrophic", "eutrophic", "hypertrophic"))
}

lake_summary <- lake_info %>%
  left_join(lake_coords, by = "lake_key") %>%
  mutate(dist_bcn_km = round(haversine_km(bcn[["lat"]], bcn[["lon"]], lat, lon))) %>%
  left_join(select(lake_attributes, lake_key, max_depth_m, depth_known, depth_source, n_depths,
                   area_ha, area_source, size_flag, osm_name, osm_tag, osm_snap_dist_m, osm_status),
            by = "lake_key") %>%
  left_join(lake_wide, by = "lake_key") %>%
  mutate(
    # OECD (1982) boundaries, applied here to medians
    trophic_CHLa = if ("CHLa_median" %in% names(.)) oecd_class(CHLa_median, c(2.5, 8, 25)) else NA,
    trophic_TP   = if ("TP_median" %in% names(.)) oecd_class(TP_median, c(0.010, 0.035, 0.100)) else NA,
    trophic      = coalesce(as.character(trophic_CHLa), as.character(trophic_TP))
  ) %>%
  arrange(dist_bcn_km)

# Column names with units for the saved file
lake_summary_out <- lake_summary %>%
  rename_with(~ paste0(unit_suffix(str_remove(.x, "_median$")), " median"), ends_with("_median"))
write_excel_csv(lake_summary_out, file.path(out_dir, "lake_summary_priority.csv"), na = "")
write_rds(lake_summary, file.path(out_dir, "lake_summary_priority.rds"))

# 6c. Lakes in both sources: are the units consistent? (ratio ACA / M100 far from ~1 = suspicious)
cross_check <- lake_stats %>%
  group_by(lake_key) %>% filter(n_distinct(source) > 1) %>% ungroup() %>%
  select(lake_key, source, var, median) %>%
  pivot_wider(names_from = source, values_from = median) %>%
  { if (all(c("ACA", "M100") %in% names(.))) mutate(., ratio_ACA_M100 = ACA / M100) else . }
write_excel_csv(cross_check, file.path(out_dir, "cross_source_check.csv"), na = "")
if (nrow(cross_check) > 0 && "ratio_ACA_M100" %in% names(cross_check)) {
  cat("\nMedian ratio ACA / M100 per variable, lakes in both sources",
      "(1970s vs today, so not exactly 1 — but a factor ~50 or ~1000 means a unit problem):\n")
  print(cross_check %>% group_by(var) %>%
          summarise(n_lakes = n(), median_ratio = median(ratio_ACA_M100, na.rm = TRUE)))
}

cat("\nDone. Files written to", out_dir, "\n")
cat(nrow(db), "measurements |", nrow(sites), "sites |", nrow(lake_summary), "lakes\n")
