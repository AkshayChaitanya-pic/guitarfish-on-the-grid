
# ============================================================
# GUITARFISH ON THE GRID
# Script 07: GEBCO bathymetry inspection and validation
# ============================================================

library(terra)
library(sf)
library(dplyr)
library(here)
library(ggplot2)

# ------------------------------------------------------------
# 1. Locate GEBCO GeoTIFF
# ------------------------------------------------------------

gebco_dir <- here("data", "raw", "gebco")

gebco_files <- list.files(
  gebco_dir,
  pattern = "\\.tif(f)?$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

cat("\n--- GEBCO FILES ---\n")
print(gebco_files)

if (length(gebco_files) != 1) {
  stop("Expected exactly one GeoTIFF. Check the GEBCO folder.")
}

# ------------------------------------------------------------
# 2. Load bathymetry raster
# ------------------------------------------------------------

gebco <- rast(gebco_files[1])

cat("\n--- RASTER INFORMATION ---\n")
print(gebco)

cat("\n--- COORDINATE SYSTEM ---\n")
print(crs(gebco))

cat("\n--- SPATIAL EXTENT ---\n")
print(ext(gebco))

cat("\n--- RASTER RESOLUTION ---\n")
print(res(gebco))

cat("\n--- RASTER DIMENSIONS ---\n")
print(dim(gebco))

# ------------------------------------------------------------
# 3. Inspect elevation values
# ------------------------------------------------------------

cat("\n--- ELEVATION SUMMARY ---\n")
print(global(gebco, "min", na.rm = TRUE))
print(global(gebco, "max", na.rm = TRUE))
print(global(gebco, "mean", na.rm = TRUE))

# GEBCO elevation convention:
# Negative values = below sea level
# Positive values = above sea level

# ------------------------------------------------------------
# 4. Load NPF spatial data
# ------------------------------------------------------------

npf <- st_read(
  here("data", "processed", "npf_rfi_2024_fixed.gpkg"),
  quiet = TRUE
)

# Transform NPF geometry into raster CRS
npf_gebco <- st_transform(
  npf,
  crs(gebco)
)

cat("\n--- NPF BOUNDING BOX ---\n")
print(st_bbox(npf_gebco))

# ------------------------------------------------------------
# 5. Check raster coverage
# ------------------------------------------------------------

npf_extent <- st_bbox(npf_gebco)

gebco_extent <- ext(gebco)

coverage_ok <-
  xmin(gebco_extent) <= npf_extent["xmin"] &&
  xmax(gebco_extent) >= npf_extent["xmax"] &&
  ymin(gebco_extent) <= npf_extent["ymin"] &&
  ymax(gebco_extent) >= npf_extent["ymax"]

cat("\n--- RASTER COVERAGE CHECK ---\n")
cat("GEBCO covers NPF bounding box:", coverage_ok, "\n")

if (!coverage_ok) {
  warning("GEBCO does not fully cover the NPF bounding box.")
}

# ------------------------------------------------------------
# 6. Preliminary bathymetry map
# ------------------------------------------------------------

# Reduce plotting resolution for efficient visualisation
gebco_plot <- aggregate(
  gebco,
  fact = 4,
  fun = mean,
  na.rm = TRUE
)

p_bathymetry <- ggplot() +
  tidyterra::geom_spatraster(
    data = gebco_plot
  ) +
  scale_fill_gradientn(
    colours = c(
      "#123B65",
      "#2677A3",
      "#7FC5D1",
      "#DCEEDB",
      "#F2E8D5"
    ),
    name = "Elevation (m)"
  ) +
  geom_sf(
    data = npf_gebco,
    fill = NA,
    colour = "#C74F45",
    linewidth = 0.3
  ) +
  coord_sf(
    crs = st_crs(gebco),
    expand = FALSE
  ) +
  labs(
    title = "Seafloor depth across the Northern Prawn Fishery",
    subtitle = "GEBCO 2026 bathymetry and mapped 2024 fishing intensity",
    caption = "Source: GEBCO 2026; ABARES NPF 2024"
  ) +
  theme_minimal(base_size = 12)

print(p_bathymetry)

# ------------------------------------------------------------
# 7. Export preliminary figure
# ------------------------------------------------------------

dir.create(
  here("figures"),
  showWarnings = FALSE,
  recursive = TRUE
)

ggsave(
  here("figures", "gebco_npf_bathymetry_draft.png"),
  p_bathymetry,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)

cat("\nBathymetry inspection completed.\n")


# ============================================================
# 8. DEPTH CLASSIFICATION WITHIN THE NPF
# ============================================================

# Convert NPF polygons into a terra vector
npf_vect <- terra::vect(npf_gebco)

# Assign each raster cell to a depth category
# GEBCO values are elevations in metres
depth_class <- terra::ifel(
  gebco >= -100 & gebco < 0,
  1,
  terra::ifel(
    gebco < -100,
    2,
    terra::ifel(
      gebco >= 0,
      3,
      NA
    )
  )
)

names(depth_class) <- "depth_class"

# Mask to the mapped fishing-intensity polygons
npf_depth <- terra::mask(
  terra::crop(depth_class, npf_vect),
  npf_vect
)

cat("\n--- NPF DEPTH CLASSIFICATION ---\n")

print(
  terra::freq(
    npf_depth
  )
)

# Calculate cell areas in square kilometres
cell_area <- terra::cellSize(
  npf_depth,
  unit = "km"
)

# Calculate area by depth class
depth_area <- terra::zonal(
  cell_area,
  npf_depth,
  fun = "sum",
  na.rm = TRUE
)

cat("\n--- APPROXIMATE AREA BY DEPTH CLASS ---\n")
print(depth_area)

# Check the area total
cat(
  "\nTotal raster-classified area (km2):",
  sum(depth_area[, 2], na.rm = TRUE),
  "\n"
)

# ============================================================
# 9. PRELIMINARY DEPTH CLASS MAP
# ============================================================

depth_class_plot <- as.factor(npf_depth)

levels(depth_class_plot) <- data.frame(
  value = c(1, 2, 3),
  depth = c(
    "0–100 m below sea level",
    "Deeper than 100 m",
    "At or above sea level"
  )
)

plot(
  depth_class_plot,
  main = "GEBCO depth classes within mapped NPF fishing area"
)


# ============================================================
# 10. FRACTIONAL-COVERAGE DEPTH AREA ESTIMATION
# ============================================================

# Rasterise the NPF footprint using fractional cell coverage
# This accounts for partially covered boundary cells.

npf_union <- sf::st_union(sf::st_geometry(npf_gebco))

npf_union_sf <- sf::st_sf(
  geometry = npf_union
)

npf_union_vect <- terra::vect(npf_union_sf)

# Create a coverage fraction raster on the GEBCO grid
coverage <- terra::rasterize(
  npf_union_vect,
  gebco,
  cover = TRUE,
  background = 0
)

# Convert coverage to weighted area (km2)
weighted_area <- terra::cellSize(
  gebco,
  unit = "km"
) * coverage

# Calculate area by depth category
fractional_depth_area <- terra::zonal(
  weighted_area,
  depth_class,
  fun = "sum",
  na.rm = TRUE
)

cat("\n--- FRACTIONAL-COVERAGE DEPTH AREA ---\n")
print(fractional_depth_area)

fractional_total <- sum(
  fractional_depth_area[, 2],
  na.rm = TRUE
)

cat(
  "\nFractional raster area total:",
  round(fractional_total, 2),
  "km2\n"
)

# Compare with the exact projected polygon area
polygon_area <- sum(
  as.numeric(
    sf::st_area(
      sf::st_transform(npf_gebco, 3577)
    )
  )
) / 1e6

cat(
  "Original polygon area:",
  round(polygon_area, 2),
  "km2\n"
)

cat(
  "Difference:",
  round(fractional_total - polygon_area, 2),
  "km2\n"
)

cat(
  "Relative difference:",
  round(
    100 * (fractional_total - polygon_area) /
      polygon_area,
    3
  ),
  "%\n"
)
