# ============================================================
# GUITARFISH ON THE GRID
# Script 05: CSIRO giant guitarfish distribution inspection
# ============================================================

library(sf)
library(dplyr)
library(here)

# Locate extracted shapefile directory
csiro_dir <- here("data", "raw", "csiro_guitarfish")

# Find shapefiles
shp_files <- list.files(
  csiro_dir,
  pattern = "\\.shp$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

cat("\n--- CSIRO SHAPEFILES ---\n")
print(shp_files)

# Ensure exactly one shapefile was found
if (length(shp_files) != 1) {
  stop("Expected one shapefile. Check extracted folder contents.")
}

# Import expert distribution
csiro_range <- st_read(
  shp_files[1],
  quiet = TRUE
)

# Inspect dataset
cat("\n--- DATA STRUCTURE ---\n")
print(dim(csiro_range))
print(names(csiro_range))
print(st_crs(csiro_range))

cat("\n--- GEOMETRY ---\n")
print(table(st_geometry_type(csiro_range)))
print(table(st_is_valid(csiro_range)))

cat("\n--- SPATIAL EXTENT ---\n")
print(st_bbox(csiro_range))

cat("\n--- ATTRIBUTE PREVIEW ---\n")
print(head(st_drop_geometry(csiro_range)))

# Initial diagnostic map
plot(
  st_geometry(csiro_range),
  col = "#7BC8C3",
  border = "#167A77",
  main = "CSIRO Giant Guitarfish Expert Distribution"
)