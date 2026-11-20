# ============================================================
# GUITARFISH ON THE GRID
# Script 02: NPF geometry validation and repair
# ============================================================

library(sf)
library(dplyr)
library(here)

# Load the 2024 fishing intensity layer
gdb_path <- here("data", "raw", "NPF.gdb")

npf_rfi_2024 <- st_read(
  gdb_path,
  layer = "NPF_rfi_2024",
  quiet = TRUE
)

# Identify invalid polygons
invalid <- !st_is_valid(npf_rfi_2024)

cat("Invalid polygons:", sum(invalid), "\n")

# Diagnose the geometry problems
print(
  st_is_valid(
    npf_rfi_2024[invalid, ],
    reason = TRUE
  )
)

# Repair geometries
npf_rfi_fixed <- st_make_valid(npf_rfi_2024)

# Verify repairs
cat(
  "Invalid polygons after repair:",
  sum(!st_is_valid(npf_rfi_fixed)),
  "\n"
)

# Confirm geometry types
print(table(st_geometry_type(npf_rfi_fixed)))

# Compare total mapped area before and after repair
# Use an Australian equal-area projection for area calculations
npf_fixed_aea <- st_transform(
  npf_rfi_fixed,
  3577
)

cat(
  "Repaired total polygon area (km2):",
  sum(as.numeric(st_area(npf_fixed_aea))) / 1e6,
  "\n"
)

# Save processed layer locally
st_write(
  npf_rfi_fixed,
  here("data", "processed", "npf_rfi_2024_fixed.gpkg"),
  delete_dsn = TRUE,
  quiet = TRUE
)

cat("Processed data saved successfully.\n")

# ============================================================
# 8. SPATIAL QUALITY CONTROL
# ============================================================

# Use Australian Albers for area calculations
crs_area <- 3577

# Original geometries
original_aea <- st_transform(npf_rfi_2024, crs_area)

# Repaired geometries
fixed_aea <- st_transform(npf_rfi_fixed, crs_area)

# Compare area before and after repair
area_original <- as.numeric(st_area(original_aea))
area_fixed <- as.numeric(st_area(fixed_aea))

area_comparison <- data.frame(
  feature = seq_along(area_original),
  original_km2 = area_original / 1e6,
  repaired_km2 = area_fixed / 1e6,
  difference_km2 = (area_fixed - area_original) / 1e6
)

cat("\n--- GEOMETRY REPAIR AREA COMPARISON ---\n")

print(
  area_comparison[
    order(-abs(area_comparison$difference_km2)),
  ][1:5, ]
)

cat(
  "\nTotal absolute area change (km2):",
  sum(abs(area_comparison$difference_km2)),
  "\n"
)

# ============================================================
# 9. CHECK POLYGON OVERLAPS
# ============================================================

# Sum of all polygon areas
summed_area <- sum(as.numeric(st_area(fixed_aea))) / 1e6

# Unique area covered by polygons
union_geometry <- st_union(st_geometry(fixed_aea))

unique_area <- as.numeric(st_area(union_geometry)) / 1e6

# Difference indicates overlapping area counted more than once
overlap_area <- summed_area - unique_area

cat("\n--- SPATIAL OVERLAP CHECK ---\n")

cat("Summed polygon area:", round(summed_area, 2), "km2\n")
cat("Unique polygon area:", round(unique_area, 2), "km2\n")
cat("Excess area from overlaps:", round(overlap_area, 2), "km2\n")