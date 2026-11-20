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