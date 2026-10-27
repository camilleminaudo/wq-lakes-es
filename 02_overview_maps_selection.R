# =============================================================================
# 02_overview_maps_selection.R
#
# Uses the outputs of 01_build_wq_database.R to:
#   1. show data availability (lake x variable)
#   2. draw one overview figure per priority variable (values, SD, n, by source:
#      ACA, CHE, M100)
#   3. map all lakes, coloured by alkalinity, with a distance ring around Barcelona
#   4. select up to n_select lakes spanning the alkalinity gradient, within a
#      distance from Barcelona and a trophic window, and map / plot the selection
#
# Figures go to <out_dir>/figures, the selection to <out_dir>/selection_*.csv
# =============================================================================

library(tidyverse)
library(patchwork)

# ---- Settings ---------------------------------------------------------------

base_dir <- "C:/Users/Camille Minaudo/OneDrive - Universitat de Barcelona/Documentos/data"
out_dir  <- file.path(base_dir, "ES_lakes_WQ_database")
fig_dir  <- file.path(out_dir, "figures")
dir.create(fig_dir, showWarnings = FALSE)

# priority_vars <- c("Alk", "TP", "TN", "PO4", "NO3", "NH4", "SO4",
#                    "Ca", "Mg", "Cond", "pH", "CHLa", "Secchi")
priority_vars <- c("Alk", "TP", "TN", "CHLa")
# log_vars <- c("Alk", "TP", "TN", "PO4", "NO3", "NH4", "SO4", "CHLa", "Cond")
log_vars <- c("Alk", "TP", "TN")
bcn      <- c(lat = 41.3874, lon = 2.1686)

# Selection settings ----
n_select        <- 20
max_dist_km     <- 400                     # straight-line distance from Barcelona
trophic_ok      <- c("oligotrophic","mesotrophic", "eutrophic")   # OECD class (CHLa median, else TP median)
keep_unknown_trophic <- FALSE              # keep lakes with neither CHLa nor TP?
alk_sources     <- c("ACA", "CHE") # accept Alk from these sources (c("ACA", "CHE") = recent data only)  "ACA", "CHE", "M100"
min_obs_alk     <- 1                       # minimum number of Alk observations
alk_scale       <- "linear"                   # spread the selection evenly on a "log" or "linear" alk scale
max_cond        <- 3000                    # us/cm. max accepted to avoid saline ecosystems
dist_weight     <- 0.2                     # 0 = ignore distance; >0 = prefer closer lakes at equal alk
min_depth_m     <- 10                      # max depth (deepest sample or manual value)
keep_unknown_depth <- TRUE                 # keep lakes whose depth is unknown / only a lower bound < min_depth_m?
min_area_ha     <- 3                       # lake area (OpenStreetMap or manual value)
keep_unknown_area  <- TRUE                 # keep lakes with unknown area?
must_include    <- c("banyoles","montcortes","sau","sant ponc","redon")             # lake_key values always selected

# Redó, Lago la Cruz

exclude         <- c("susqueda","el pasteral")             # lake_key values never selected
# lake_key prefixes never selected (wetlands, coastal lagoons, small ponds...)
exclude_prefixes <- c("delta de llobregat", "delta del llobregat", "platja",
                      "desembocadura", "basses", "estanyols", "estanyets",
                      "aiguamolls", "herbassars", "delta de l ebre",
                      "canal", "complejo",                                      # not a lake
                      "11","23","27",                                           # not properly IDed
                      "gran de mainera","gran del pesso","ibon","fondo",       # too remote
                      "marbore","baiau","major de la gallina",                # too remote
                      "mariola","naut de","gerber","negre de boi","gemena de baix",       # too remote
                      "gran de tumeneia","montoliu",       # too remote
                      "sant marti de tous",                                     # too small
                      "l arispe"                                                # not a lake and too small
                      )                                   


# ---- Data -------------------------------------------------------------------

db           <- read_rds(file.path(out_dir, "wq_database_long.rds"))
summary_site <- read_rds(file.path(out_dir, "summary_by_site_variable.rds"))
lakes        <- read_rds(file.path(out_dir, "lake_summary_priority.rds"))

units <- db %>% distinct(var, unit) %>% deframe()
axis_label <- function(v) if (!is.na(units[v]) && units[v] != "-") paste0(v, " [", units[v], "]") else v

base_theme <- theme_bw(base_size = 9) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank())
no_y <- theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
fig_height <- function(n) min(49, 1.5 + 0.15 * n)                 # inches (ggsave max 50)
source_cols <- c(ACA = "#1b7e8a", CHE = "#6a4c93", M100 = "#c4622d")

# Unique label per site: the station code is added when several stations share a name
# (e.g. wetlands with several ACA stations)
site_labels <- summary_site %>%
  distinct(site_uid, source, site_name) %>%
  add_count(source, site_name, name = "n_same_name") %>%
  mutate(label = if_else(n_same_name > 1,
                         paste0(site_name, " (", str_remove(site_uid, "^[^_]+_"), ") [", source, "]"),
                         paste0(site_name, " [", source, "]"))) %>%
  select(site_uid, label)

summary_site <- left_join(summary_site, site_labels, by = "site_uid")
db           <- left_join(db, site_labels, by = "site_uid")

sites_with_coords <- lakes %>% filter(!is.na(lat))
cat(nrow(lakes), "lakes,", nrow(sites_with_coords), "with coordinates\n")
if (nrow(sites_with_coords) < nrow(lakes))
  warning(nrow(lakes) - nrow(sites_with_coords),
          " lakes have no coordinates and are left out of maps and selection ",
          "(see coordinates_manual.csv)")


# =============================================================================
# 1. Data availability
# =============================================================================

avail <- summary_site %>%
  filter(priority) %>%
  mutate(var = factor(var, levels = priority_vars))

p_avail <- ggplot(avail, aes(var, fct_reorder(label, n_obs, .fun = sum), fill = n_obs)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = n_obs), size = 2, colour = "white") +
  scale_fill_viridis_c(trans = "log10", name = "n obs", end = 0.85) +
  facet_grid(source ~ ., scales = "free_y", space = "free_y") +
  labs(x = NULL, y = NULL, title = "Observations per site and priority variable") +
  base_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(fig_dir, "00_availability.png"), p_avail,
       width = 7, height = fig_height(n_distinct(avail$label)), dpi = 200, limitsize = FALSE)


# =============================================================================
# 2. One overview figure per priority variable
#    values (points + boxplot) | SD | n obs (red = below detection limit)
#    sites ordered by median, coloured by source
# =============================================================================

plot_variable <- function(v) {
  s   <- filter(summary_site, var == v)
  lvl <- unique(s$label[order(s$median)])
  s   <- mutate(s, label = factor(label, levels = lvl))
  d   <- db %>% filter(var == v) %>% mutate(label = factor(label, levels = lvl))
  if (v %in% log_vars) d <- filter(d, value > 0)          # zeros can't be shown on a log axis
  same_y <- scale_y_discrete(drop = FALSE)                 # keeps the 3 panels aligned
  
  p_val <- ggplot(d, aes(value, label, colour = source)) +
    geom_jitter(height = 0.2, width = 0, alpha = 0.2, size = 0.5) +
    geom_boxplot(outlier.shape = NA, fill = NA, width = 0.6) +
    scale_colour_manual(values = source_cols) +
    labs(x = axis_label(v), y = NULL, title = v, colour = NULL) +
    same_y + base_theme
  if (v %in% log_vars) p_val <- p_val + scale_x_log10()
  
  s_sd <- filter(s, !is.na(sd))                            # sd is NA for single observations
  p_sd <- if (nrow(s_sd) == 0) {
    ggplot(s, aes(x = 0, y = label)) + geom_blank() +
      labs(x = "SD (n = 1 everywhere)", y = NULL) + same_y + base_theme + no_y
  } else {
    ggplot(s_sd, aes(sd, label)) +
      geom_segment(aes(x = 0, xend = sd, yend = label), colour = "grey70") +
      geom_point(aes(colour = source), size = 1.3, show.legend = FALSE) +
      scale_colour_manual(values = source_cols) +
      labs(x = "SD", y = NULL) + same_y + base_theme + no_y
  }
  
  p_n <- ggplot(s, aes(y = label)) +
    geom_col(aes(x = n_obs), fill = "grey60") +
    geom_col(aes(x = n_censored), fill = "firebrick") +
    labs(x = "n obs", y = NULL) + same_y + base_theme + no_y
  
  p_val + p_sd + p_n + plot_layout(widths = c(4, 1, 1), guides = "collect") &
    theme(legend.position = "top")
}

for (v in intersect(priority_vars, unique(summary_site$var))) {
  ns <- n_distinct(paste(summary_site$site_uid[summary_site$var == v]))
  ggsave(file.path(fig_dir, paste0("var_", v, ".png")), plot_variable(v),
         width = 9, height = fig_height(ns) + 0.5, dpi = 200, limitsize = FALSE)
}
# In RStudio: plot_variable("Alk")


# =============================================================================
# 3. Maps
# =============================================================================

circle_km <- function(lat0, lon0, r_km, n = 180) {
  b  <- seq(0, 2 * pi, length.out = n)
  d  <- r_km / 6371
  la <- lat0 * pi / 180; lo <- lon0 * pi / 180
  lat <- asin(sin(la) * cos(d) + cos(la) * sin(d) * cos(b))
  lon <- lo + atan2(sin(b) * sin(d) * cos(la), cos(d) - sin(la) * sin(lat))
  tibble(lat = lat * 180 / pi, lon = lon * 180 / pi)
}

basemap <- map_data("world", region = c("Spain", "Portugal", "France", "Andorra"))
ring    <- circle_km(bcn[["lat"]], bcn[["lon"]], max_dist_km)

map_base <- function(xlim, ylim) {
  ggplot() +
    geom_polygon(data = basemap, aes(long, lat, group = group),
                 fill = "grey95", colour = "grey70", linewidth = 0.2) +
    geom_path(data = ring, aes(lon, lat), linetype = "dashed", colour = "grey40") +
    annotate("point", x = bcn[["lon"]], y = bcn[["lat"]], shape = 8, size = 2.5) +
    coord_quickmap(xlim = xlim, ylim = ylim) +
    labs(x = NULL, y = NULL) +
    theme_bw(base_size = 9) +
    theme(panel.background = element_rect(fill = "aliceblue"), panel.grid = element_blank())
}

p_map_all <- map_base(xlim = c(-9.5, 3.5), ylim = c(35.8, 44)) +
  geom_point(data = filter(sites_with_coords, is.na(Alk_median)),
             aes(lon, lat), shape = 4, colour = "grey50", size = 1) +
  geom_point(data = filter(sites_with_coords, !is.na(Alk_median)),
             aes(lon, lat, fill = Alk_median, shape = Alk_source), size = 2.3, colour = "black", stroke = 0.3) +
  scale_fill_viridis_c(trans = "log10", name = axis_label("Alk")) +
  scale_shape_manual(values = c(ACA = 21, CHE = 22, M100 = 24), name = "Alk data") +
  labs(title = "All lakes and reservoirs — median alkalinity",
       subtitle = paste0("Dashed ring: ", max_dist_km, " km from Barcelona; × = no alkalinity data"))

ggsave(file.path(fig_dir, "map_all_alkalinity.png"), p_map_all, width = 9, height = 7, dpi = 200)


# =============================================================================
# 4. Selection along the alkalinity gradient
# =============================================================================

excluded_by_prefix <- sites_with_coords %>%
  filter(str_starts(lake_key, paste0("(", paste(str_squish(exclude_prefixes), collapse = "|"), ")")))
cat("\nLakes excluded by prefix:", nrow(excluded_by_prefix), "\n")
print(count(excluded_by_prefix, prefix = str_extract(lake_key, paste(str_squish(exclude_prefixes), collapse = "|"))))

# Filters applied one after the other; the log shows how many lakes each one removes.
# must_include lakes pass every filter.
filter_log <- tibble(step = "lakes with coordinates", n_left = nrow(sites_with_coords))
apply_step <- function(df, keep, label) {
  keep <- (keep %in% TRUE) | df$lake_key %in% must_include
  filter_log <<- add_row(filter_log, step = label, n_left = sum(keep))
  df[keep, ]
}

candidates <- sites_with_coords
candidates <- apply_step(candidates, !candidates$lake_key %in% c(exclude, excluded_by_prefix$lake_key),
                         "not excluded (names / prefixes)")
candidates <- apply_step(candidates, candidates$dist_bcn_km <= max_dist_km,
                         paste0("<= ", max_dist_km, " km from Barcelona"))
candidates <- apply_step(candidates, !is.na(candidates$Alk_median) & candidates$Alk_n_obs >= min_obs_alk &
                           candidates$Alk_source %in% alk_sources,
                         paste0("alkalinity data (>= ", min_obs_alk, " obs, from ",
                                paste(alk_sources, collapse = "/"), ")"))
candidates <- apply_step(candidates, !is.na(candidates$Cond_median) & candidates$Cond_median < max_cond,
                         paste0("conductivity data (< ", max_cond, " uS/cm)"))
candidates <- apply_step(candidates, candidates$trophic %in% trophic_ok |
                           (keep_unknown_trophic & is.na(candidates$trophic)),
                         paste0("trophic state: ", paste(trophic_ok, collapse = ", ")))
# Depth: a deepest sample from a lake without a vertical profile is only a lower bound,
# so it can confirm "deep enough" but not "too shallow".
depth_ok <- with(candidates,
                 (!is.na(max_depth_m) & max_depth_m >= min_depth_m) |
                   (keep_unknown_depth & (is.na(max_depth_m) | !depth_known)))
candidates <- apply_step(candidates, depth_ok,
                         paste0("max depth >= ", min_depth_m, " m",
                                if (keep_unknown_depth) " (or unknown)" else ""))
area_ok <- with(candidates,
                (!is.na(area_ha) & area_ha >= min_area_ha) | (keep_unknown_area & is.na(area_ha)))
candidates <- apply_step(candidates, area_ok,
                         paste0("area >= ", min_area_ha, " ha",
                                if (keep_unknown_area) " (or unknown)" else ""))

cat("\nSelection filters (lakes left after each step):\n")
print(filter_log)
write_excel_csv(filter_log, file.path(out_dir, "selection_filter_log.csv"))

# Greedy pick: n targets evenly spaced over the alkalinity range of the candidates;
# for each target, the closest candidate not yet chosen (ties broken by distance to BCN).
pick_gradient <- function(alk, dist, n, scale = "log", forced = rep(FALSE, length(alk)),
                          dist_weight = 0) {
  if (length(alk) <= n) return(seq_along(alk))
  x <- if (scale == "log") log10(alk) else alk
  x <- (x - min(x)) / diff(range(x))                       # 0–1 along the gradient
  d <- dist / max(dist)
  chosen  <- which(forced)
  targets <- seq(0, 1, length.out = n)
  for (i in chosen) targets <- targets[-which.min(abs(targets - x[i]))]
  for (t in targets) {
    avail  <- setdiff(seq_along(x), chosen)
    score  <- abs(x[avail] - t) + dist_weight * d[avail] / n
    chosen <- c(chosen, avail[which.min(score)])
  }
  chosen
}

if (nrow(candidates) == 0) {
  warning("No candidate lakes: relax max_dist_km, trophic_ok, alk_sources or min_obs_alk.")
} else {
  idx <- pick_gradient(candidates$Alk_median, candidates$dist_bcn_km, n_select,
                       scale = alk_scale, forced = candidates$lake_key %in% must_include,
                       dist_weight = dist_weight)
  
  candidates <- candidates %>%
    mutate(selected = row_number() %in% idx)
  selection <- candidates %>%
    filter(selected) %>%
    arrange(Alk_median) %>%
    mutate(rank_alk = row_number()) %>%
    select(rank_alk, lake_name, lake_key, sources, dist_bcn_km, lat, lon,
           max_depth_m, depth_known, area_ha, size_flag,
           ends_with("_median"), Alk_n_obs, Alk_source, trophic, coord_check, site_uids)
  
  cat("\nSelected lakes (ordered by alkalinity):\n")
  print(select(selection, rank_alk, lake_name, dist_bcn_km, max_depth_m, area_ha, Alk_median,
               any_of(c("TP_median", "CHLa_median")), trophic), n = Inf)
  if (any(is.na(selection$max_depth_m) | !selection$depth_known | is.na(selection$area_ha)))
    message("Some selected lakes have unknown depth or area: check them, or fill ",
            "lake_attributes_manual.csv and rerun both scripts.")
  
  write_excel_csv(selection, file.path(out_dir, "selection_lakes.csv"), na = "")
  write_excel_csv(select(candidates, selected, everything()),
                  file.path(out_dir, "selection_candidates.csv"), na = "")
  
  # ---- Map of the selection ----
  lab_layer <- if (requireNamespace("ggrepel", quietly = TRUE)) {
    ggrepel::geom_text_repel(data = filter(candidates, selected), aes(lon, lat, label = lake_name),
                             size = 2.6, min.segment.length = 0, max.overlaps = Inf)
  } else {
    geom_text(data = filter(candidates, selected), aes(lon, lat, label = lake_name),
              size = 2.6, vjust = -0.8)
  }
  pad  <- 0.3
  xlim <- range(c(ring$lon, candidates$lon)) + c(-pad, pad)
  ylim <- range(c(ring$lat, candidates$lat)) + c(-pad, pad)
  
  p_map_sel <- map_base(xlim, ylim) +
    geom_point(data = filter(sites_with_coords, !lake_key %in% candidates$lake_key),
               aes(lon, lat), colour = "grey60", size = 1) +
    geom_point(data = filter(candidates, !selected), aes(lon, lat, fill = Alk_median),
               shape = 21, size = 1.8, alpha = 0.6, stroke = 0.2) +
    geom_point(data = filter(candidates, selected), aes(lon, lat, fill = Alk_median),
               shape = 21, size = 3.5, stroke = 0.8) +
    lab_layer +
    scale_fill_viridis_c(trans = "log10", name = axis_label("Alk")) +
    labs(title = paste0("Selection: ", sum(candidates$selected), " lakes along the alkalinity gradient"),
         subtitle = paste0("≤ ", max_dist_km, " km from Barcelona; ",
                           paste(trophic_ok, collapse = "/"), "; depth ≥ ", min_depth_m,
                           " m; area ≥ ", min_area_ha, " ha",
                           if (keep_unknown_depth || keep_unknown_area) " (unknown kept)" else "",
                           "\nBig dots = selected, small = candidates, grey = filtered out"))
  
  ggsave(file.path(fig_dir, "map_selection.png"), p_map_sel, width = 9, height = 8, dpi = 200)
  
  # ---- Gradient plot: alkalinity vs trophic indicators ----
  troph_var <- if ("CHLa_median" %in% names(candidates) && any(!is.na(candidates$CHLa_median))) "CHLa" else "TP"
  troph_lines <- if (troph_var == "CHLa") c(2.5, 8, 25) else c(0.010, 0.035, 0.100)
  
  all_pts <- sites_with_coords %>%
    filter(!is.na(Alk_median), !is.na(.data[[paste0(troph_var, "_median")]])) %>%
    left_join(select(candidates, lake_key, selected), by = "lake_key") %>%
    mutate(status = case_when(selected ~ "selected", !is.na(selected) ~ "candidate",
                              TRUE ~ "not a candidate"))
  
  p_grad <- ggplot(all_pts, aes(Alk_median, .data[[paste0(troph_var, "_median")]])) +
    geom_hline(yintercept = troph_lines, linetype = "dotted", colour = "grey50") +
    geom_point(aes(colour = status, size = status)) +
    { if (requireNamespace("ggrepel", quietly = TRUE))
      ggrepel::geom_text_repel(data = filter(all_pts, status == "selected"),
                               aes(label = lake_name), size = 2.5, max.overlaps = Inf)
      else geom_text(data = filter(all_pts, status == "selected"),
                     aes(label = lake_name), size = 2.5, vjust = -0.8) } +
    scale_x_log10() + scale_y_log10() +
    scale_colour_manual(values = c(selected = "firebrick", candidate = "grey30",
                                   `not a candidate` = "grey80")) +
    scale_size_manual(values = c(selected = 2.8, candidate = 1.5, `not a candidate` = 1)) +
    labs(x = axis_label("Alk"), y = axis_label(troph_var), colour = NULL, size = NULL,
         title = "Alkalinity gradient vs trophic state",
         subtitle = "Dotted lines: OECD oligo / meso / eutro / hypertrophic boundaries") +
    base_theme + theme(panel.grid.major.y = element_line())
  
  ggsave(file.path(fig_dir, "selection_gradient.png"), p_grad, width = 8, height = 6, dpi = 200)
  
  # ---- Interactive map (optional, opens in the RStudio viewer) ----
  if (requireNamespace("leaflet", quietly = TRUE)) {
    library(leaflet)
    pal <- colorNumeric("viridis", domain = log10(sites_with_coords$Alk_median), na.color = "grey")
    m <- leaflet(sites_with_coords) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircles(lng = bcn[["lon"]], lat = bcn[["lat"]], radius = max_dist_km * 1000,
                 fill = FALSE, color = "grey", dashArray = "4") %>%
      addCircleMarkers(lng = ~lon, lat = ~lat, radius = ~ifelse(lake_key %in% selection$lake_key, 9, 5),
                       color = "black", weight = 1, fillOpacity = 0.9,
                       fillColor = ~pal(log10(Alk_median)),
                       label = ~lake_name,
                       popup = ~paste0("<b>", lake_name, "</b><br>",
                                       "Alk: ", signif(Alk_median, 3), " meq/L<br>",
                                       if ("TP_median" %in% names(sites_with_coords)) paste0("TP: ", signif(TP_median, 2), " mg/L<br>") else "",
                                       if ("CHLa_median" %in% names(sites_with_coords)) paste0("Chl-a: ", signif(CHLa_median, 2), " µg/L<br>") else "",
                                       "Trophic: ", trophic, "<br>",
                                       "Max depth: ", ifelse(is.na(max_depth_m), "?", paste0(max_depth_m, " m",
                                                                                             ifelse(depth_known, "", " (lower bound)"))), "<br>",
                                       "Area: ", ifelse(is.na(area_ha), "?", paste0(round(area_ha, 1), " ha")), "<br>",
                                       "Distance BCN: ", dist_bcn_km, " km<br>",
                                       "Source(s): ", sources))
    print(m)
    # htmlwidgets::saveWidget(m, file.path(fig_dir, "map_interactive.html"))
  }
}

# Tip: to compare the selected lakes in detail
if (exists("selection")) db %>% filter(lake_key %in% selection$lake_key, priority) %>%
  ggplot(aes(lake_key, value, colour = source)) + geom_boxplot() +
  facet_wrap(~ var, scales = "free") + coord_flip()


# =============================================================================
# 5. Overview of the candidate lakes (all lakes that passed every filter)
#    a. distribution of each main variable per lake (raw observations)
#    b. lake medians against alkalinity
#    c. summary table: medians, depth, area, distance (colour = rank among candidates)
#    Selected lakes are marked with * and shown in red.
# =============================================================================

overview_vars <- c("Alk", "TP", "TN", "CHLa")   # <- main variables to show
ov_log_vars   <- c("TP", "TN", "PO4", "NO3", "NH4", "SO4", "Ca", "Mg", "Cond","CHLa",
                   "area_ha")

if (exists("candidates") && nrow(candidates) > 0 && "selected" %in% names(candidates)) {
  
  ov_vars <- intersect(overview_vars, unique(db$var))
  status_cols <- c(selected = "firebrick", candidate = "grey35")
  
  # One label per lake, ordered by alkalinity; * = selected
  cand <- candidates %>%
    mutate(status = if_else(selected, "selected", "candidate"),
           label  = make.unique(paste0(lake_name, if_else(selected, " *", ""))),
           label  = fct_reorder(label, Alk_median))
  n_cand <- nrow(cand)
  
  # Lake medians that are mostly below the detection limit (e.g. CHE "<20 mg CaCO3/L" = 0.4 meq/L):
  # these lakes are not really ranked against each other on that variable
  cens <- db %>%
    filter(lake_key %in% cand$lake_key, var %in% overview_vars) %>%
    group_by(lake_key, col = paste0(var, "_median")) %>%
    summarise(below_lod = mean(censored) >= 0.5, .groups = "drop")
  n_lod_alk <- sum(cens$below_lod[cens$col == "Alk_median"])
  if (n_lod_alk > 0)
    message(n_lod_alk, " candidate lakes have a median alkalinity below the detection limit: ",
            "their position at the low end of the gradient is uncertain.")
  
  # ---- a. Distributions per lake --------------------------------------------
  d_ov <- db %>%
    filter(lake_key %in% cand$lake_key, var %in% ov_vars) %>%
    select(-any_of(c("label", "status"))) %>%                # site labels from section 1
    inner_join(select(cand, lake_key, label, status), by = "lake_key")
  
  panel_dist <- function(v, show_y) {
    d <- filter(d_ov, var == v)
    if (v %in% ov_log_vars) d <- filter(d, value > 0)
    p <- ggplot(d, aes(value, label, colour = status)) +
      geom_jitter(height = 0.2, width = 0, alpha = 0.15, size = 0.4) +
      geom_boxplot(outlier.shape = NA, fill = NA, width = 0.6, linewidth = 0.3) +
      scale_colour_manual(values = status_cols, drop = FALSE) +
      scale_y_discrete(drop = FALSE) +
      labs(x = axis_label(v), y = NULL, colour = NULL, title = v) +
      base_theme
    if (v %in% ov_log_vars) p <- p + scale_x_log10()
    if (!show_y) p <- p + no_y
    p
  }
  p_dist <- wrap_plots(map2(ov_vars, seq_along(ov_vars) == 1, panel_dist), nrow = 1) +
    plot_layout(guides = "collect") +
    plot_annotation(title = paste0("Candidate lakes (", n_cand, ") — distribution of the main variables"),
                    subtitle = "Lakes ordered by median alkalinity; * = selected") &
    theme(legend.position = "top")
  ggsave(file.path(fig_dir, "candidates_1_distributions.png"), p_dist,
         width = 3 + 2.2 * length(ov_vars), height = fig_height(n_cand) + 1, dpi = 200,
         limitsize = FALSE)
  
  # ---- b. Lake medians against alkalinity ------------------------------------
  y_vars <- c(paste0(setdiff(ov_vars, "Alk"), "_median"), "max_depth_m", "area_ha", "dist_bcn_km")
  y_vars <- intersect(y_vars, names(cand))
  y_vars <- y_vars[map_lgl(y_vars, ~ any(!is.na(cand[[.x]])))]   # skip empty ones (e.g. no areas yet)
  y_lab  <- function(col) {
    v <- str_remove(col, "_median$")
    switch(v, max_depth_m = "Max depth [m]", area_ha = "Area [ha]",
           dist_bcn_km = "Distance to Barcelona [km]", axis_label(v))
  }
  panel_vs_alk <- function(col) {
    d <- filter(cand, !is.na(.data[[col]]), !is.na(Alk_median)) %>%
      left_join(filter(cens, col == "Alk_median") %>% select(lake_key, alk_lod = below_lod),
                by = "lake_key") %>%
      left_join(filter(cens, col == !!col) %>% select(lake_key, y_lod = below_lod), by = "lake_key") %>%
      mutate(lod = if_else(coalesce(alk_lod, FALSE) | coalesce(y_lod, FALSE),
                           "below detection limit", "measured"))
    v <- str_remove(col, "_median$")
    if (v %in% ov_log_vars) d <- filter(d, .data[[col]] > 0)
    p <- ggplot(d, aes(Alk_median, .data[[col]], colour = status)) +
      geom_point(aes(size = status, shape = lod)) +
      scale_shape_manual(values = c(measured = 16, `below detection limit` = 1), drop = FALSE,
                         name = NULL) +
      scale_colour_manual(values = status_cols, drop = FALSE) +
      scale_size_manual(values = c(selected = 2.2, candidate = 1.2), drop = FALSE) +
      scale_x_log10() +
      labs(x = axis_label("Alk"), y = y_lab(col), colour = NULL, size = NULL) +
      base_theme + theme(panel.grid.major.y = element_line())
    if (v %in% ov_log_vars) p <- p + scale_y_log10()
    if (v == "CHLa") p <- p + geom_hline(yintercept = c(2.5, 8, 25), linetype = "dotted", colour = "grey50")
    if (v == "TP")   p <- p + geom_hline(yintercept = c(0.010, 0.035, 0.100), linetype = "dotted", colour = "grey50")
    if (v == "max_depth_m") p <- p + geom_hline(yintercept = min_depth_m, linetype = "dashed", colour = "grey50")
    p
  }
  p_vs_alk <- wrap_plots(map(y_vars, panel_vs_alk), ncol = 3) +
    plot_layout(guides = "collect") +
    plot_annotation(title = paste0("Candidate lakes (", n_cand, ") — lake medians along the alkalinity gradient"),
                    subtitle = paste("Dotted: OECD trophic boundaries; dashed: minimum depth;",
                                     "open symbols: median below the detection limit (x or y)")) &
    theme(legend.position = "top")
  ggsave(file.path(fig_dir, "candidates_2_vs_alkalinity.png"), p_vs_alk,
         width = 10, height = 2.8 * ceiling(length(y_vars) / 3) + 1, dpi = 200)
  
  # ---- c. Summary table ---------------------------------------------------------
  tab_cols <- intersect(c(paste0(ov_vars, "_median"), "max_depth_m", "area_ha", "dist_bcn_km"),
                        names(cand))
  tab_cols <- tab_cols[map_lgl(tab_cols, ~ any(!is.na(cand[[.x]])))]
  tab <- cand %>%
    select(label, status, all_of(tab_cols)) %>%
    pivot_longer(all_of(tab_cols), names_to = "col", values_to = "value") %>%
    left_join(cand %>% select(label, lake_key), by = "label") %>%
    left_join(cens, by = c("lake_key", "col")) %>%
    group_by(col) %>%
    mutate(rank = percent_rank(value)) %>%                 # 0 = lowest, 1 = highest among candidates
    ungroup() %>%
    mutate(col  = factor(col, levels = tab_cols,
                         labels = map_chr(tab_cols, y_lab) %>% str_remove(" to Barcelona")),
           text = case_when(is.na(value) ~ "–",
                            abs(value) >= 100 ~ format(round(value), big.mark = ""),
                            TRUE ~ as.character(signif(value, 2))),
           text = if_else(coalesce(below_lod, FALSE) & text != "–", paste0("<", text), text))
  p_tab <- ggplot(tab, aes(col, label, fill = rank)) +
    geom_tile(colour = "white") +
    geom_text(aes(label = text, fontface = if_else(status == "selected", "bold", "plain")),
              size = 2.3) +
    scale_fill_distiller(palette = "BrBG", direction = 1, na.value = "grey92",
                         name = "Rank among\ncandidates", breaks = c(0, 0.5, 1),
                         labels = c("lowest", "median", "highest")) +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = NULL,
         title = paste0("Candidate lakes (", n_cand, ") — medians, depth, area and distance"),
         subtitle = "Ordered by alkalinity; bold and * = selected; < = median below the detection limit") +
    base_theme +
    theme(axis.text.x.top = element_text(angle = 45, hjust = 0), panel.grid = element_blank())
  ggsave(file.path(fig_dir, "candidates_3_table.png"), p_tab,
         width = 2.5 + 0.9 * length(tab_cols), height = fig_height(n_cand) + 1.2, dpi = 200,
         limitsize = FALSE)
  
  cat("\nCandidate overview figures written to", fig_dir, "(candidates_1_..., candidates_2_..., candidates_3_...)\n")
}