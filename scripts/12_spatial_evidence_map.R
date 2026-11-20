
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
# 10. REFINED SPATIAL EVIDENCE MAP
# ============================================================

library(scales)

# Explicitly distinguish observation-period colours
period_colours <- c(
  "Historical" = "#747474",
  "Recent (2020-2026)" = "#174F91",
  "Undated" = "#B07A35"
)

# Define the ON_LAND flag as a separate mapping category
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

p_spatial <- ggplot() +
  
  geom_sf(
    data = australia,
    fill = "#EAE8E2",
    colour = "#9A9A94",
    linewidth = 0.25
  ) +
  
  geom_sf(
    data = npf,
    aes(fill = RelativeIntensity),
    colour = NA,
    alpha = 0.9
  ) +
  
  geom_sf(
    data = csiro_map,
    fill = NA,
    colour = "#385B7A",
    linewidth = 0.45,
    linetype = "longdash"
  ) +
  
  geom_sf(
    data = obis_map,
    aes(
      colour = observation_period,
      shape = quality_category
    ),
    size = 2.6,
    stroke = 0.9,
    alpha = 0.95
  ) +
  
  scale_fill_manual(
    name = "2024 fishing intensity",
    values = c(
      "Low" = "#B7DCD3",
      "Medium" = "#E6B86B",
      "High" = "#BF6C61"
    ),
    breaks = c("Low", "Medium", "High")
  ) +
  
  scale_colour_manual(
    name = "OBIS observation period",
    values = period_colours
  ) +
  
  scale_shape_manual(
    name = "OBIS spatial flag",
    values = c(
      "Not ON_LAND flagged" = 16,
      "ON_LAND flagged" = 4
    )
  ) +
  
  coord_sf(
    xlim = st_bbox(study_window)[c("xmin", "xmax")],
    ylim = st_bbox(study_window)[c("ymin", "ymax")],
    expand = FALSE
  ) +
  
  labs(
    title = "Giant guitarfish occurrence evidence and fishing activity",
    subtitle = "Northern Australia | 2024 NPF intensity, CSIRO expert range and OBIS records",
    x = "Longitude",
    y = "Latitude",
    caption = paste(
      "CSIRO expert distribution: dashed blue boundary.",
      "Crosses: OBIS records flagged ON_LAND; flags require further verification.",
      "Fishing-intensity overlap does not establish encounters or bycatch.",
      sep = "\n"
    )
  ) +
  
  guides(
    fill = guide_legend(order = 1),
    colour = guide_legend(order = 2),
    shape = guide_legend(order = 3)
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 17
    ),
    plot.subtitle = element_text(
      colour = "#555555",
      size = 10
    ),
    panel.grid.major = element_line(
      colour = "#E6E6E6",
      linewidth = 0.25
    ),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(
      colour = "#666666",
      hjust = 0,
      size = 9
    ),
    plot.margin = margin(12, 15, 12, 12)
  )

print(p_spatial)

# ============================================================
# 11. EXPORT REFINED DRAFT
# ============================================================

ggsave(
  here("figures", "12_spatial_evidence_refined.png"),
  plot = p_spatial,
  width = 13,
  height = 8,
  dpi = 300,
  bg = "white"
)

cat("\nRefined spatial evidence map saved.\n")
