
# ============================================================
# GUITARFISH ON THE GRID
# Script 12: Spatial evidence map
# Step 1: Mapping data validation
# ============================================================

library(sf)
library(dplyr)
library(ggplot2)
library(here)

# 1. Load validated NPF polygons
npf <- st_read(
  here("data", "processed", "npf_rfi_2024_fixed.gpkg"),
  quiet = TRUE
)

# 2. Load CSIRO expert distribution
csiro <- st_read(
  here(
    "data", "raw", "csiro_guitarfish",
    "CAAB_FISHMAPPolygon.shp"
  ),
  quiet = TRUE
)

# 3. Load screened OBIS records
obis <- readRDS(
  here(
    "data", "processed",
    "obis_northern_australia_screened.rds"
  )
)

# 4. Transform to a common mapping CRS
npf <- st_transform(npf, 3577)
csiro <- st_transform(csiro, 3577)
obis <- st_transform(obis, 3577)

# 5. Check datasets
cat("\n--- SPATIAL LAYER VALIDATION ---\n")

print(data.frame(
  layer = c("NPF", "CSIRO", "OBIS"),
  features = c(
    nrow(npf),
    nrow(csiro),
    nrow(obis)
  ),
  crs = c(
    st_crs(npf)$epsg,
    st_crs(csiro)$epsg,
    st_crs(obis)$epsg
  )
))

stopifnot(
  st_crs(npf) == st_crs(csiro),
  st_crs(npf) == st_crs(obis),
  all(st_is_valid(npf)),
  all(st_is_valid(csiro)),
  nrow(obis) == 105
)

# 6. Inspect recent NPF observations
recent_npf <- obis |>
  filter(
    !is.na(year_recovered),
    year_recovered >= 2020,
    inside_npf
  )

cat("\n--- RECENT RECORDS INSIDE NPF ---\n")

print(
  recent_npf |>
    st_drop_geometry() |>
    select(
      year_recovered,
      basisOfRecord,
      flags,
      dataset_id,
      longitude,
      latitude
    )
)

# 7. Check NPF fishing-intensity classes
cat("\n--- FISHING INTENSITY CLASSES ---\n")

print(
  npf |>
    st_drop_geometry() |>
    count(RelativeIntensity)
)

cat("\nMapping data validation completed.\n")


# ============================================================
# 8. PREPARE OBSERVATION SYMBOLS
# ============================================================

obis_map <- obis |>
  mutate(
    observation_period = case_when(
      is.na(year_recovered) ~ "Undated",
      year_recovered < 2020 ~ "Historical",
      TRUE ~ "Recent (2020-2026)"
    ),
    on_land_flag = grepl(
      "ON_LAND",
      coalesce(as.character(flags), "")
    )
  )

cat("\n--- MAP OBSERVATION COUNTS ---\n")
print(
  obis_map |>
    st_drop_geometry() |>
    count(observation_period, on_land_flag)
)

# ============================================================
# 9. PREPARE MAP EXTENT AND COASTLINE
# ============================================================

australia <- rnaturalearth::ne_countries(
  country = "Australia",
  scale = "medium",
  returnclass = "sf"
) |>
  st_transform(3577)

# Use a geographic bounding box around the NPF.
# This gives consistent map coverage in the projected CRS.
study_window <- st_as_sfc(
  st_bbox(
    c(
      xmin = 128,
      ymin = -19,
      xmax = 143,
      ymax = -9
    ),
    crs = st_crs(4326)
  )
) |>
  st_transform(3577)

# Crop CSIRO distribution to the mapping window
csiro_map <- suppressWarnings(
  st_intersection(
    csiro,
    st_make_valid(study_window)
  )
)


# ============================================================
# 10. FINAL SPATIAL EVIDENCE MAP
# ============================================================

library(scales)

# ------------------------------------------------------------
# 10.1 Observation categories
# ------------------------------------------------------------

period_colours <- c(
  "Recent (2020-2026)" = "#175676",
  "Historical" = "#737B83",
  "Undated" = "#A66B24"
)

obis_map <- obis_map |>
  mutate(
    quality_category = if_else(
      on_land_flag,
      "ON_LAND flagged",
      "Not ON_LAND flagged"
    ),
    observation_period = factor(
      observation_period,
      levels = c(
        "Recent (2020-2026)",
        "Historical",
        "Undated"
      )
    )
  )

# ------------------------------------------------------------
# 10.2 Geographic labels
# ------------------------------------------------------------

region_labels <- data.frame(
  name = c(
    "Arafura Sea",
    "Gulf of\nCarpentaria",
    "Northern Australia"
  ),
  longitude = c(134.0, 139.1, 134.1),
  latitude = c(-10.7, -14.0, -16.3),
  type = c("Ocean", "Ocean", "Land")
)

region_labels <- st_as_sf(
  region_labels,
  coords = c("longitude", "latitude"),
  crs = 4326
) |>
  st_transform(3577)

# ------------------------------------------------------------
# 10.3 Map extent and cartographic elements
# ------------------------------------------------------------

map_bbox <- st_bbox(study_window)

x_min <- as.numeric(map_bbox["xmin"])
x_max <- as.numeric(map_bbox["xmax"])
y_min <- as.numeric(map_bbox["ymin"])
y_max <- as.numeric(map_bbox["ymax"])

scale_x <- x_min + 100000
scale_y <- y_min + 90000

north_x <- x_max - 110000
north_y <- y_max - 120000

# ------------------------------------------------------------
# 10.4 Construct spatial evidence map
# ------------------------------------------------------------

p_spatial <- ggplot() +
  
  # Geographic background
  
  geom_sf(
    data = australia,
    fill = "#F1EFEA",
    colour = "#C5C9C7",
    linewidth = 0.25
  ) +
  
  # Muted fishing intensity background
  
  geom_sf(
    data = npf,
    aes(fill = RelativeIntensity),
    colour = NA,
    alpha = 0.65
  ) +
  
  # Expert distribution boundary
  
  geom_sf(
    data = csiro,
    fill = NA,
    colour = "#175676",
    linewidth = 0.45,
    linetype = "dashed"
  ) +
  
  # Geographic labels
  
  geom_sf_text(
    data = filter(region_labels, type == "Ocean"),
    aes(label = name),
    colour = "#527D91",
    fontface = "italic",
    size = 3.8
  ) +
  
  geom_sf_text(
    data = filter(region_labels, type == "Land"),
    aes(label = name),
    colour = "#858A85",
    fontface = "bold",
    size = 4
  ) +
  
  # Historical and undated observations
  
  geom_sf(
    data = filter(
      obis_map,
      observation_period != "Recent (2020-2026)"
    ),
    aes(
      colour = observation_period,
      shape = quality_category
    ),
    size = 2.8,
    stroke = 0.9,
    alpha = 0.85
  ) +
  
  # Recent observations plotted last
  # to improve visibility
  
  geom_sf(
    data = filter(
      obis_map,
      observation_period == "Recent (2020-2026)"
    ),
    aes(
      colour = observation_period,
      shape = quality_category
    ),
    size = 3.5,
    stroke = 1.2
  ) +
  
  # Scale bar
  
  annotate(
    "segment",
    x = scale_x,
    xend = scale_x + 100000,
    y = scale_y,
    yend = scale_y,
    linewidth = 2.8,
    colour = "#243746"
  ) +
  
  annotate(
    "segment",
    x = scale_x + 100000,
    xend = scale_x + 200000,
    y = scale_y,
    yend = scale_y,
    linewidth = 2.8,
    colour = "white"
  ) +
  
  annotate(
    "segment",
    x = scale_x,
    xend = scale_x + 200000,
    y = scale_y,
    yend = scale_y,
    linewidth = 0.3,
    colour = "#243746"
  ) +
  
  annotate(
    "text",
    x = scale_x,
    y = scale_y - 35000,
    label = "0",
    size = 3.2
  ) +
  
  annotate(
    "text",
    x = scale_x + 100000,
    y = scale_y - 35000,
    label = "100",
    size = 3.2
  ) +
  
  annotate(
    "text",
    x = scale_x + 200000,
    y = scale_y - 35000,
    label = "200 km",
    size = 3.2
  ) +
  
  # Grid north
  
  annotate(
    "segment",
    x = north_x,
    xend = north_x,
    y = north_y - 45000,
    yend = north_y,
    arrow = arrow(
      length = grid::unit(0.22, "cm")
    ),
    colour = "#243746",
    linewidth = 0.6
  ) +
  
  annotate(
    "text",
    x = north_x,
    y = north_y + 28000,
    label = "GN",
    fontface = "bold",
    size = 3.5
  ) +
  
  # Consistent fishing-intensity colours
  
  scale_fill_manual(
    name = "2024 fishing intensity",
    values = c(
      "Low" = "#66A68D",
      "Medium" = "#E4B253",
      "High" = "#C96962"
    ),
    breaks = c("Low", "Medium", "High")
  ) +
  
  scale_colour_manual(
    name = "Observation period",
    values = period_colours,
    drop = FALSE
  ) +
  
  scale_shape_manual(
    name = "OBIS spatial flag",
    values = c(
      "Not ON_LAND flagged" = 16,
      "ON_LAND flagged" = 4
    )
  ) +
  
  coord_sf(
    crs = st_crs(3577),
    datum = st_crs(4326),
    xlim = c(x_min, x_max),
    ylim = c(y_min, y_max),
    expand = FALSE
  ) +
  
  labs(
    title = "Giant guitarfish occurrence evidence",
    subtitle = paste(
      "OBIS records in relation to the CSIRO expert range",
      "and 2024 fishing intensity"
    ),
    x = "Longitude",
    y = "Latitude",
    caption = paste(
      "Dashed blue boundary: CSIRO expert distribution.",
      "Crosses indicate ON_LAND flags; absence of this flag does not establish record accuracy.",
      "Multiple observations may share coordinates or overlap visually.",
      "Occurrence records do not establish abundance, trawl encounters or bycatch.",
      "Sources: OBIS, CSIRO, ABARES and Natural Earth.",
      sep = "\n"
    )
  ) +
  
  guides(
    fill = guide_legend(
      order = 1,
      nrow = 1
    ),
    colour = guide_legend(
      order = 2,
      override.aes = list(
        shape = 16,
        size = 3.5,
        alpha = 1
      )
    ),
    shape = guide_legend(
      order = 3,
      override.aes = list(
        colour = "#243746",
        size = 3.5,
        alpha = 1
      )
    )
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 21,
      colour = "#173247"
    ),
    
    plot.subtitle = element_text(
      colour = "#536572",
      size = 11.5,
      margin = margin(b = 12)
    ),
    
    panel.background = element_rect(
      fill = "#EAF3F7",
      colour = NA
    ),
    
    panel.border = element_rect(
      colour = "#CDD8DF",
      fill = NA,
      linewidth = 0.5
    ),
    
    panel.grid.major = element_line(
      colour = "#D2E0E7",
      linewidth = 0.3
    ),
    
    panel.grid.minor = element_blank(),
    
    axis.text = element_text(
      colour = "#536572",
      size = 11
    ),
    
    axis.title = element_text(
      colour = "#243746",
      size = 12
    ),
    
    legend.position = "bottom",
    legend.box = "vertical",
    
    legend.title = element_text(
      face = "bold",
      size = 11
    ),
    
    legend.text = element_text(
      size = 10
    ),
    
    legend.key.height = grid::unit(0.4, "cm"),
    legend.spacing.y = grid::unit(0.05, "cm"),
    legend.margin = margin(0, 0, 0, 0),
    
    plot.caption = element_text(
      colour = "#647480",
      hjust = 0,
      size = 9,
      lineheight = 1.2,
      margin = margin(t = 10)
    ),
    
    plot.margin = margin(18, 22, 15, 18),
    
    plot.background = element_rect(
      fill = "white",
      colour = NA
    )
  )

print(p_spatial)

# ============================================================
# 11. EXPORT FINAL SPATIAL EVIDENCE MAP
# ============================================================

ggsave(
  here("figures", "12_spatial_evidence_refined.png"),
  plot = p_spatial,
  width = 13.33,
  height = 8.5,
  dpi = 300,
  bg = "white"
)

cat("\nFinal spatial evidence map saved.\n")
