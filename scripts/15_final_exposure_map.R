

# ============================================================
# GUITARFISH ON THE GRID
# Script 15: Final Spatial Exposure Map
#
# Species: Glaucostegus typus
# Region: Northern Prawn Fishery, Australia
#
# PURPOSE:
# Validate and partition mapped NPF fishing-intensity
# polygons by overlap with the CSIRO expert distribution.
#
# This is a spatial exposure screening analysis.
# It does NOT estimate bycatch, mortality or abundance.
# ============================================================


# ------------------------------------------------------------
# 1. LOAD PACKAGES
# ------------------------------------------------------------

library(sf)
library(dplyr)
library(readr)
library(here)

sf::sf_use_s2(FALSE)

cat("\n--- SCRIPT 15 STARTED ---\n")


# ------------------------------------------------------------
# 2. LOAD SPATIAL DATASETS
# ------------------------------------------------------------

npf <- st_read(
  here(
    "data", "processed",
    "npf_rfi_2024_fixed.gpkg"
  ),
  quiet = TRUE
)

csiro <- st_read(
  here(
    "data", "raw",
    "csiro_guitarfish",
    "CAAB_FISHMAPPolygon.shp"
  ),
  quiet = TRUE
)

original_overlap <- st_read(
  here(
    "data", "processed",
    "npf_guitarfish_overlap_2024.gpkg"
  ),
  quiet = TRUE
)

validated_results <- read_csv(
  here(
    "outputs",
    "exposure_area_summary.csv"
  ),
  show_col_types = FALSE
)


# ------------------------------------------------------------
# 3. TRANSFORM TO AUSTRALIAN ALBERS
# ------------------------------------------------------------

target_crs <- 3577

npf <- st_transform(npf, target_crs)
csiro <- st_transform(csiro, target_crs)

original_overlap <- st_transform(
  original_overlap,
  target_crs
)


# ------------------------------------------------------------
# 4. VALIDATE SPATIAL INPUTS
# ------------------------------------------------------------

stopifnot(
  nrow(npf) == 48,
  nrow(csiro) == 1,
  all(st_is_valid(npf)),
  all(st_is_valid(csiro)),
  !any(st_is_empty(npf)),
  !any(st_is_empty(csiro)),
  all(st_is_valid(original_overlap)),
  !any(st_is_empty(original_overlap))
)

stopifnot(
  "RelativeIntensity" %in% names(npf),
  "RelativeIntensity" %in% names(original_overlap)
)

cat("\n--- SPATIAL INPUT VALIDATION ---\n")

cat("NPF features:", nrow(npf), "\n")
cat("CSIRO features:", nrow(csiro), "\n")

cat(
  "Original overlap features:",
  nrow(original_overlap), "\n"
)

cat("Target CRS:", target_crs, "\n")

cat("\n--- NPF INTENSITY CLASSES ---\n")

print(
  npf |>
    st_drop_geometry() |>
    count(RelativeIntensity)
)


# ------------------------------------------------------------
# 5. CALCULATE NEW SPATIAL PARTITION
# ------------------------------------------------------------

csiro_union <- st_union(csiro)

npf_inside <- st_intersection(
  npf,
  csiro_union
)

npf_outside <- st_difference(
  npf,
  csiro_union
)

npf_inside <- npf_inside[
  !st_is_empty(npf_inside),
]

npf_outside <- npf_outside[
  !st_is_empty(npf_outside),
]

cat("\n--- SPATIAL PARTITION ---\n")

cat(
  "Inside features:",
  nrow(npf_inside), "\n"
)

cat(
  "Outside features:",
  nrow(npf_outside), "\n"
)

stopifnot(
  all(st_is_valid(npf_inside)),
  all(st_is_valid(npf_outside))
)


# ------------------------------------------------------------
# 6. HELPER FUNCTION: AREA BY INTENSITY
# ------------------------------------------------------------

calculate_class_area <- function(
    spatial_data,
    output_name
) {
  
  spatial_data |>
    mutate(
      area_km2 =
        as.numeric(st_area(spatial_data)) / 1e6
    ) |>
    st_drop_geometry() |>
    group_by(RelativeIntensity) |>
    summarise(
      area_km2 = sum(area_km2),
      .groups = "drop"
    ) |>
    rename(
      !!output_name := area_km2
    )
}


# ------------------------------------------------------------
# 7. CALCULATE AREA SUMMARIES
# ------------------------------------------------------------

original_npf_area <- calculate_class_area(
  npf,
  "original_km2"
)

inside_area <- calculate_class_area(
  npf_inside,
  "inside_km2"
)

outside_area <- calculate_class_area(
  npf_outside,
  "outside_km2"
)

saved_overlap_area <- calculate_class_area(
  original_overlap,
  "saved_overlap_km2"
)


# ------------------------------------------------------------
# 8. VALIDATE INSIDE + OUTSIDE PARTITION
# ------------------------------------------------------------

partition_check <- original_npf_area |>
  left_join(
    inside_area,
    by = "RelativeIntensity"
  ) |>
  left_join(
    outside_area,
    by = "RelativeIntensity"
  ) |>
  mutate(
    inside_km2 = coalesce(inside_km2, 0),
    outside_km2 = coalesce(outside_km2, 0),
    
    reconstructed_km2 =
      inside_km2 + outside_km2,
    
    difference_km2 =
      reconstructed_km2 - original_km2,
    
    inside_percent =
      100 * inside_km2 / original_km2
  )

cat("\n--- PARTITION VALIDATION ---\n")

print(
  partition_check,
  n = Inf,
  width = Inf
)

# Exact partition should reconstruct original polygons
# within a small numerical tolerance.

stopifnot(
  all(
    abs(partition_check$difference_km2) < 0.01
  )
)

cat("\nSpatial partition validation PASSED.\n")


# ------------------------------------------------------------
# 9. THREE-WAY BASELINE COMPARISON
# ------------------------------------------------------------

three_way_check <- partition_check |>
  select(
    RelativeIntensity,
    inside_km2
  ) |>
  left_join(
    saved_overlap_area,
    by = "RelativeIntensity"
  ) |>
  left_join(
    validated_results |>
      select(
        intensity,
        baseline_overlap_km2
      ),
    by = c(
      "RelativeIntensity" = "intensity"
    )
  ) |>
  mutate(
    new_minus_saved_km2 =
      inside_km2 - saved_overlap_km2,
    
    saved_minus_summary_km2 =
      saved_overlap_km2 -
      baseline_overlap_km2,
    
    new_minus_summary_km2 =
      inside_km2 -
      baseline_overlap_km2
  )

cat("\n--- THREE-WAY BASELINE COMPARISON ---\n")

print(
  three_way_check,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 10. COMPARE TOTAL OVERLAP AREAS
# ------------------------------------------------------------

total_comparison <- tibble(
  source = c(
    "New intersection",
    "Saved overlap geometry",
    "Validated summary"
  ),
  
  overlap_km2 = c(
    sum(inside_area$inside_km2),
    sum(saved_overlap_area$saved_overlap_km2),
    sum(validated_results$baseline_overlap_km2)
  )
)

cat("\n--- TOTAL OVERLAP COMPARISON ---\n")

print(total_comparison)


# ------------------------------------------------------------
# 11. VERIFY DATA COMPLETENESS
# ------------------------------------------------------------

stopifnot(
  nrow(three_way_check) == 3,
  !anyNA(three_way_check),
  all(is.finite(three_way_check$inside_km2)),
  all(is.finite(three_way_check$saved_overlap_km2))
)

cat("\n--- GEOMETRY VALIDITY ---\n")

print(
  c(
    new_inside_valid =
      all(st_is_valid(npf_inside)),
    
    new_outside_valid =
      all(st_is_valid(npf_outside)),
    
    saved_overlap_valid =
      all(st_is_valid(original_overlap))
  )
)


# ------------------------------------------------------------
# 12. DIAGNOSTIC INTERPRETATION
# ------------------------------------------------------------

max_new_saved_difference <- max(
  abs(three_way_check$new_minus_saved_km2)
)

max_saved_summary_difference <- max(
  abs(three_way_check$saved_minus_summary_km2)
)

cat("\n--- DIAGNOSTIC SUMMARY ---\n")

cat(
  "Maximum new vs saved difference:",
  max_new_saved_difference,
  "km2\n"
)

cat(
  "Maximum saved vs summary difference:",
  max_saved_summary_difference,
  "km2\n"
)

if (max_new_saved_difference < 0.1) {
  
  cat(
    "\nNew intersection closely matches",
    "the saved overlap geometry.\n"
  )
  
} else {
  
  cat(
    "\nREVIEW REQUIRED:",
    "New intersection differs from",
    "the saved overlap geometry.\n"
  )
}

if (max_saved_summary_difference < 0.1) {
  
  cat(
    "\nSaved overlap closely matches",
    "the validated summary.\n"
  )
  
} else {
  
  cat(
    "\nREVIEW REQUIRED:",
    "Saved overlap differs from",
    "the validated summary.\n"
  )
}

cat(
  "\nSCRIPT 15 DIAGNOSTIC COMPLETE.\n",
  "Review comparisons before final map generation.\n"
)


# ============================================================
# 13. VERIFY VECTOR VS RASTER AREA ESTIMATES
# ============================================================

vector_validation <- read_csv(
  here(
    "outputs",
    "vector_first_overlap_validation.csv"
  ),
  show_col_types = FALSE
)

cat("\n--- VECTOR VALIDATION COLUMN NAMES ---\n")
print(names(vector_validation))

cat("\n--- VECTOR VALIDATION CONTENTS ---\n")
print(vector_validation, n = Inf, width = Inf)

cat("\n--- SAVED VECTOR OVERLAP AREAS ---\n")
print(saved_overlap_area, n = Inf, width = Inf)

cat("\nSCRIPT 15 VECTOR COMPARISON READY.\n")



# ============================================================
# 14. REFINED FINAL BASELINE EXPOSURE MAP
# ============================================================

library(ggplot2)
library(rnaturalearth)

# ------------------------------------------------------------
# 14.1 Prepare geographic layers
# ------------------------------------------------------------

map_inside <- original_overlap |>
  mutate(
    map_class = as.character(RelativeIntensity)
  )

map_outside <- npf_outside |>
  mutate(
    map_class = "Outside expert distribution"
  )

# Combine map categories into a single legend
map_polygons <- rbind(
  map_inside |>
    select(map_class, geometry = any_of(attr(map_inside, "sf_column"))),
  map_outside |>
    select(map_class, geometry = any_of(attr(map_outside, "sf_column")))
)

map_polygons$map_class <- factor(
  map_polygons$map_class,
  levels = c(
    "Low",
    "Medium",
    "High",
    "Outside expert distribution"
  )
)

land <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
) |>
  filter(admin == "Australia") |>
  st_transform(target_crs)

# ------------------------------------------------------------
# 14.2 Map extent
# ------------------------------------------------------------

map_bbox <- st_bbox(npf)

x_min <- as.numeric(map_bbox["xmin"]) - 90000
x_max <- as.numeric(map_bbox["xmax"]) + 90000

y_min <- as.numeric(map_bbox["ymin"]) - 90000
y_max <- as.numeric(map_bbox["ymax"]) + 90000

# ------------------------------------------------------------
# 14.3 Create a scale bar in projected metres
# ------------------------------------------------------------

scale_length_m <- 200000

scale_x <- x_min + 90000
scale_y <- y_min + 70000

scale_bar <- data.frame(
  x = c(
    scale_x,
    scale_x + scale_length_m / 2
  ),
  xend = c(
    scale_x + scale_length_m / 2,
    scale_x + scale_length_m
  ),
  y = scale_y,
  yend = scale_y,
  segment = c("A", "B")
)

# ------------------------------------------------------------
# 14.4 North arrow
# ------------------------------------------------------------

north_x <- x_max - 110000
north_y <- y_max - 120000

# ------------------------------------------------------------
# 14.5 Map colours
# ------------------------------------------------------------

map_colours <- c(
  "Low" = "#66A68D",
  "Medium" = "#E4B253",
  "High" = "#C96962",
  "Outside expert distribution" = "#7F8994"
)

# ------------------------------------------------------------
# 14.6 Prepare geographic labels
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
  st_transform(target_crs)

# ------------------------------------------------------------
# 14.7 Construct final exposure map
# ------------------------------------------------------------

final_exposure_map <- ggplot() +
  
  # Land and coastline
  
  geom_sf(
    data = land,
    fill = "#F1EFEA",
    colour = "#C5C9C7",
    linewidth = 0.25
  ) +
  
  # Fishing intensity and overlap polygons
  
  geom_sf(
    data = map_polygons,
    aes(fill = map_class),
    colour = NA
  ) +
  
  # CSIRO expert distribution boundary
  
  geom_sf(
    data = csiro,
    aes(linetype = "CSIRO expert range"),
    fill = NA,
    colour = "#175676",
    linewidth = 0.65
  ) +
  
  # Geographic labels
  
  geom_sf_text(
    data = filter(region_labels, type == "Ocean"),
    aes(label = name),
    colour = "#527D91",
    fontface = "italic",
    size = 4
  ) +
  
  geom_sf_text(
    data = filter(region_labels, type == "Land"),
    aes(label = name),
    colour = "#858A85",
    fontface = "bold",
    size = 4.2
  ) +
  
  # Scale bar
  
  geom_segment(
    data = scale_bar,
    aes(
      x = x,
      xend = xend,
      y = y,
      yend = yend
    ),
    linewidth = 3,
    colour = c("#243746", "#FFFFFF")
  ) +
  
  geom_segment(
    aes(
      x = scale_x,
      xend = scale_x + scale_length_m,
      y = scale_y,
      yend = scale_y
    ),
    linewidth = 0.35,
    colour = "#243746"
  ) +
  
  annotate(
    "text",
    x = scale_x,
    y = scale_y - 30000,
    label = "0",
    size = 3.4
  ) +
  
  annotate(
    "text",
    x = scale_x + scale_length_m / 2,
    y = scale_y - 30000,
    label = "100",
    size = 3.4
  ) +
  
  annotate(
    "text",
    x = scale_x + scale_length_m,
    y = scale_y - 30000,
    label = "200 km",
    size = 3.4
  ) +
  
  # Grid north arrow
  
  annotate(
    "segment",
    x = north_x,
    xend = north_x,
    y = north_y - 45000,
    yend = north_y,
    arrow = arrow(
      length = grid::unit(0.25, "cm")
    ),
    linewidth = 0.7,
    colour = "#243746"
  ) +
  
  annotate(
    "text",
    x = north_x,
    y = north_y + 28000,
    label = "GN",
    fontface = "bold",
    size = 3.5,
    colour = "#243746"
  ) +
  
  # Legend
  
  scale_fill_manual(
    values = map_colours,
    name = "Fishing intensity and range overlap",
    labels = c(
      "Low" = "Low (<0.25 days/km²)",
      "Medium" = "Medium (0.25-1.0)",
      "High" = "High (1.0-2.0)",
      "Outside expert distribution" = "Outside range"
    ),
    drop = FALSE
  ) +
  
  scale_linetype_manual(
    name = NULL,
    values = c(
      "CSIRO expert range" = "dashed"
    )
  ) +
  
  guides(
    fill = guide_legend(
      order = 1,
      nrow = 1,
      title.position = "top"
    ),
    linetype = guide_legend(
      order = 2,
      override.aes = list(
        colour = "#175676",
        linewidth = 0.9
      )
    )
  ) +
  
  # Coordinate system
  
  coord_sf(
    crs = st_crs(target_crs),
    xlim = c(x_min, x_max),
    ylim = c(y_min, y_max),
    datum = st_crs(4326),
    expand = FALSE
  ) +
  
  # Scientific titles and interpretation
  
  labs(
    title = "Potential trawl exposure of giant guitarfish",
    subtitle = paste(
      "2024 Northern Prawn Fishery",
      "| CSIRO expert distribution"
    ),
    x = "Longitude",
    y = "Latitude",
    caption = paste(
      "Coloured fishing areas fall within the expert range; grey areas fall outside it.",
      "Baseline vector overlap, before bathymetric screening.",
      "Potential spatial exposure does not represent observed bycatch or mortality.",
      "Sources: ABARES (2024 fishing intensity), CSIRO expert range, Natural Earth.",
      sep = "\n"
    )
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    
    plot.title = element_text(
      face = "bold",
      size = 21,
      colour = "#173247"
    ),
    
    plot.subtitle = element_text(
      size = 12,
      colour = "#536572",
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
      size = 10.5
    ),
    
    legend.key.width = grid::unit(0.8, "cm"),
    
    legend.spacing.y = grid::unit(0.05, "cm"),
    
    plot.caption = element_text(
      size = 9,
      colour = "#647480",
      hjust = 0,
      lineheight = 1.2,
      margin = margin(t = 10)
    ),
    
    plot.margin = margin(18, 22, 15, 18),
    
    plot.background = element_rect(
      fill = "white",
      colour = NA
    )
  )

# ------------------------------------------------------------
# 14.8 Display and export
# ------------------------------------------------------------

print(final_exposure_map)

ggsave(
  filename = here(
    "figures",
    "15_final_spatial_exposure_map.png"
  ),
  plot = final_exposure_map,
  width = 13.33,
  height = 7.5,
  dpi = 300,
  bg = "white"
)

cat("\nFinal spatial exposure map exported.\n")

