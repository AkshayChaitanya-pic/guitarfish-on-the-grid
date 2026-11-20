
# ============================================================
# GUITARFISH ON THE GRID
# Script 09: Coastal elevation mismatch investigation
# ============================================================

library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(tidyterra)
library(here)

dir.create(here("figures"), showWarnings = FALSE)
dir.create(here("outputs"), showWarnings = FALSE)

# ------------------------------------------------------------
# 1. Load data
# ------------------------------------------------------------

npf <- st_read(
  here("data", "processed", "npf_rfi_2024_fixed.gpkg"),
  quiet = TRUE
)

shp_files <- list.files(
  here("data", "raw", "csiro_guitarfish"),
  pattern = "\\.shp$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

stopifnot(length(shp_files) == 1)

guitarfish <- st_read(shp_files[1], quiet = TRUE)

gebco_files <- list.files(
  here("data", "raw", "gebco"),
  pattern = "\\.tif(f)?$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

stopifnot(length(gebco_files) == 1)

gebco <- rast(gebco_files[1])

# ------------------------------------------------------------
# 2. Prepare spatial geometries
# ------------------------------------------------------------

npf <- st_make_valid(npf)
guitarfish <- st_make_valid(guitarfish)

npf <- st_transform(npf, 3577)
guitarfish <- st_transform(guitarfish, 3577)

range_union <- st_union(st_geometry(guitarfish))

# Intersect NPF polygons with CSIRO distribution
overlap <- suppressWarnings(
  st_intersection(
    npf,
    st_sf(geometry = range_union)
  )
)

stopifnot(nrow(overlap) > 0)

# Transform to GEBCO CRS
overlap_ll <- st_transform(overlap, crs(gebco))

# Crop raster to NPF study extent
study_raster <- crop(
  gebco,
  vect(st_transform(npf, crs(gebco)))
)

# ------------------------------------------------------------
# 3. Classify GEBCO elevation
# ------------------------------------------------------------

# 1 = submerged, 0 to 100 m depth
# 2 = submerged, deeper than 100 m
# 3 = elevation >= 0 m

depth_class <- ifel(
  study_raster >= -100 & study_raster < 0,
  1,
  ifel(
    study_raster < -100,
    2,
    ifel(study_raster >= 0, 3, NA)
  )
)

names(depth_class) <- "DepthClass"

# ------------------------------------------------------------
# 4. Fractional area by fishing-intensity class
# ------------------------------------------------------------

cell_area <- cellSize(study_raster, unit = "km")

classes <- c("Low", "Medium", "High")
results <- vector("list", length(classes))

for (i in seq_along(classes)) {
  
  class_name <- classes[i]
  
  class_overlap <- overlap_ll |>
    filter(RelativeIntensity == class_name)
  
  class_union <- st_union(st_geometry(class_overlap))
  
  coverage <- rasterize(
    vect(st_sf(geometry = class_union)),
    study_raster,
    cover = TRUE,
    background = 0
  )
  
  weighted_area <- cell_area * coverage
  
  category_area <- zonal(
    weighted_area,
    depth_class,
    fun = "sum",
    na.rm = TRUE
  )
  
  names(category_area) <- c("DepthClass", "Area_km2")
  
  category_area$RelativeIntensity <- class_name
  
  results[[i]] <- category_area
}

area_summary <- bind_rows(results) |>
  mutate(
    DepthCategory = case_when(
      DepthClass == 1 ~ "0-100 m",
      DepthClass == 2 ~ "Deeper than 100 m",
      DepthClass == 3 ~ "GEBCO elevation >= 0 m",
      TRUE ~ "Unknown"
    )
  ) |>
  select(
    RelativeIntensity,
    DepthCategory,
    Area_km2
  )

cat("\n--- COASTAL ELEVATION AUDIT ---\n")
print(area_summary, digits = 6)

write.csv(
  area_summary,
  here("outputs", "coastal_elevation_audit.csv"),
  row.names = FALSE
)


# ------------------------------------------------------------
# 5. Map GEBCO >= 0 m within overlap
# ------------------------------------------------------------

# Merge all NPF–guitarfish overlap polygons
overlap_union <- st_union(st_geometry(overlap_ll))

# Calculate fractional raster coverage
overlap_coverage <- terra::rasterize(
  terra::vect(st_sf(geometry = overlap_union)),
  study_raster,
  cover = TRUE,
  background = 0
)

# Identify GEBCO cells at or above sea level
# within the mapped NPF–guitarfish overlap
non_submerged <- terra::ifel(
  study_raster >= 0 & overlap_coverage > 0,
  1,
  NA
)

names(non_submerged) <- "CoastalFlag"

# Convert flagged cells into a plotting data frame
# Missing values are removed to avoid a grey background
flag_df <- as.data.frame(
  non_submerged,
  xy = TRUE,
  na.rm = TRUE
)

flag_df <- flag_df |>
  dplyr::filter(CoastalFlag == 1)

cat(
  "\nNumber of flagged GEBCO cells:",
  nrow(flag_df),
  "\n"
)

# Australian land polygons for geographic context
australia <- rnaturalearth::ne_countries(
  country = "Australia",
  scale = "medium",
  returnclass = "sf"
)

australia <- st_transform(australia, 4326)

# Prepare spatial boundaries for plotting
npf_ll <- st_transform(npf, 4326)
range_ll <- st_transform(guitarfish, 4326)

# ------------------------------------------------------------
# 6. Create diagnostic map
# ------------------------------------------------------------

p <- ggplot() +
  
  # Ocean background
  annotate(
    "rect",
    xmin = 129,
    xmax = 143,
    ymin = -19,
    ymax = -10,
    fill = "#EAF3F8",
    colour = NA
  ) +
  
  # Australian land
  geom_sf(
    data = australia,
    fill = "#E8E5DD",
    colour = "#777777",
    linewidth = 0.25
  ) +
  
  # NPF fishing-intensity boundaries
  geom_sf(
    data = npf_ll,
    fill = NA,
    colour = "#667788",
    linewidth = 0.25
  ) +
  
  # Flagged GEBCO cells
  geom_tile(
    data = flag_df,
    aes(
      x = x,
      y = y,
      fill = "GEBCO elevation >= 0 m"
    ),
    width = terra::res(study_raster)[1],
    height = terra::res(study_raster)[2],
    alpha = 0.85
  ) +
  
  # CSIRO giant guitarfish distribution boundary
  geom_sf(
    data = range_ll,
    fill = NA,
    colour = "#2463A6",
    linewidth = 0.35,
    linetype = "dashed"
  ) +
  
  scale_fill_manual(
    values = c(
      "GEBCO elevation >= 0 m" = "#E85D4A"
    ),
    name = "Flagged cells"
  ) +
  
  coord_sf(
    xlim = c(129, 143),
    ylim = c(-19, -10),
    expand = FALSE
  ) +
  
  labs(
    title = "Coastal elevation mismatches in mapped trawling overlap",
    subtitle = paste(
      "GEBCO cells >= 0 m within 2024 NPF",
      "and CSIRO giant guitarfish expert range"
    ),
    x = "Longitude",
    y = "Latitude",
    caption = paste(
      "Sources: ABARES NPF 2024; CSIRO FishMap; GEBCO 2026.",
      "Flagged cells do not demonstrate fishing activity on land."
    )
  ) +
  
  theme_minimal(base_size = 11) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.caption = element_text(
      size = 8,
      colour = "grey40"
    )
  )

print(p)

ggsave(
  here("figures", "09_coastal_elevation_diagnostic.png"),
  plot = p,
  width = 11,
  height = 7,
  dpi = 300,
  bg = "white"
)

cat("\nScript 09 completed.\n")

