
# ============================================================
# GUITARFISH ON THE GRID
# Script 08: Depth-constrained fishing-intensity overlap
# ============================================================

library(terra)
library(sf)
library(dplyr)
library(here)
library(ggplot2)

# ------------------------------------------------------------
# 1. Load datasets
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
# 2. Spatial preparation
# ------------------------------------------------------------

stopifnot(all(st_is_valid(npf)))
stopifnot(all(st_is_valid(guitarfish)))

npf <- st_transform(npf, crs(gebco))
guitarfish <- st_transform(guitarfish, crs(gebco))

# Preserve intensity classes
classes <- c("Low", "Medium", "High")

stopifnot(
  all(unique(npf$RelativeIntensity) %in% classes),
  !anyNA(npf$RelativeIntensity)
)

# Limit computation to the NPF study extent
study_raster <- crop(
  gebco,
  vect(npf)
)

# Area of each raster cell (km2)
cell_area <- cellSize(
  study_raster,
  unit = "km"
)

# Candidate depth range:
# GEBCO elevation -100 m to below 0 m
shallow <- ifel(
  study_raster >= -100 & study_raster < 0,
  1,
  0
)

# ------------------------------------------------------------
# 3. Create fractional coverage for expert range
# ------------------------------------------------------------

range_union <- st_union(st_geometry(guitarfish))

range_sf <- st_sf(
  geometry = range_union
)

range_coverage <- rasterize(
  vect(range_sf),
  study_raster,
  cover = TRUE,
  background = 0
)

# ------------------------------------------------------------
# 4. Calculate class-specific overlap
# ------------------------------------------------------------

results <- vector("list", length(classes))

for (i in seq_along(classes)) {
  
  current_class <- classes[i]
  
  class_sf <- npf |>
    filter(RelativeIntensity == current_class)
  
  # Union polygons to avoid double counting
  class_union <- st_union(st_geometry(class_sf))
  
  class_union_sf <- st_sf(
    geometry = class_union
  )
  
  # Fraction of each GEBCO cell covered by NPF class
  class_coverage <- rasterize(
    vect(class_union_sf),
    study_raster,
    cover = TRUE,
    background = 0
  )
  
  # Area of mapped intensity class
  total_area <- global(
    cell_area * class_coverage,
    "sum",
    na.rm = TRUE
  )[1, 1]
  
  # Baseline area using range coverage
  #
  # Multiplying two coverage fractions is an
  # approximation for their intersection within
  # a cell, not an exact geometric intersection.
  baseline_area <- global(
    cell_area * class_coverage * range_coverage,
    "sum",
    na.rm = TRUE
  )[1, 1]
  
  # Depth-constrained area
  depth_area <- global(
    cell_area * class_coverage *
      range_coverage * shallow,
    "sum",
    na.rm = TRUE
  )[1, 1]
  
  results[[i]] <- data.frame(
    RelativeIntensity = current_class,
    total_km2 = total_area,
    baseline_km2 = baseline_area,
    depth_constrained_km2 = depth_area
  )
}

comparison <- bind_rows(results) |>
  mutate(
    baseline_percent = 100 * baseline_km2 / total_km2,
    depth_percent = 100 * depth_constrained_km2 / total_km2,
    area_reduction_km2 =
      baseline_km2 - depth_constrained_km2,
    percent_reduction = if_else(
      baseline_km2 > 0,
      100 * area_reduction_km2 / baseline_km2,
      NA_real_
    )
  )

cat("\n--- DEPTH-CONSTRAINED OVERLAP BY CLASS ---\n")
print(comparison, digits = 5)

# ------------------------------------------------------------
# 5. Overall comparison
# ------------------------------------------------------------

overall <- comparison |>
  summarise(
    total_km2 = sum(total_km2),
    baseline_km2 = sum(baseline_km2),
    depth_constrained_km2 =
      sum(depth_constrained_km2)
  ) |>
  mutate(
    baseline_percent =
      100 * baseline_km2 / total_km2,
    depth_percent =
      100 * depth_constrained_km2 / total_km2,
    area_reduction_km2 =
      baseline_km2 - depth_constrained_km2,
    percent_reduction =
      100 * area_reduction_km2 / baseline_km2
  )

cat("\n--- OVERALL COMPARISON ---\n")
print(overall, digits = 5)

# ------------------------------------------------------------
# 6. Diagnostic checks
# ------------------------------------------------------------

stopifnot(
  all(comparison$depth_constrained_km2 >= 0),
  all(comparison$depth_constrained_km2 <=
        comparison$baseline_km2 + 1e-6),
  all(comparison$baseline_km2 <=
        comparison$total_km2 + 1e-6)
)

cat("\nAll basic area consistency checks passed.\n")

# ------------------------------------------------------------
# 7. Export results
# ------------------------------------------------------------

dir.create(
  here("outputs"),
  showWarnings = FALSE,
  recursive = TRUE
)

write.csv(
  comparison,
  here("outputs", "depth_constrained_overlap_by_class.csv"),
  row.names = FALSE
)

write.csv(
  overall,
  here("outputs", "depth_constrained_overlap_overall.csv"),
  row.names = FALSE
)

cat("\nScript 08 completed.\n")


# ============================================================
# 8. DECOMPOSE DEPTH-MASK EXCLUSIONS
# ============================================================

# GEBCO elevation categories
depth_categories <- list(
  shallow = study_raster >= -100 & study_raster < 0,
  deep = study_raster < -100,
  non_submerged = study_raster >= 0
)

exclusion_results <- vector("list", length(classes))

for (i in seq_along(classes)) {
  
  current_class <- classes[i]
  
  class_sf <- npf |>
    filter(RelativeIntensity == current_class)
  
  class_union <- st_union(st_geometry(class_sf))
  
  class_coverage <- terra::rasterize(
    terra::vect(st_sf(geometry = class_union)),
    study_raster,
    cover = TRUE,
    background = 0
  )
  
  # Area-weighted baseline overlap
  overlap_weight <- cell_area *
    class_coverage *
    range_coverage
  
  class_areas <- sapply(
    depth_categories,
    function(mask) {
      terra::global(
        overlap_weight * mask,
        "sum",
        na.rm = TRUE
      )[1, 1]
    }
  )
  
  exclusion_results[[i]] <- data.frame(
    RelativeIntensity = current_class,
    shallow_km2 = class_areas["shallow"],
    deep_km2 = class_areas["deep"],
    non_submerged_km2 = class_areas["non_submerged"]
  )
}

exclusion_summary <- dplyr::bind_rows(
  exclusion_results
) |>
  dplyr::mutate(
    total_km2 =
      shallow_km2 +
      deep_km2 +
      non_submerged_km2
  )

cat("\n--- DEPTH-MASK EXCLUSION AUDIT ---\n")
print(exclusion_summary, digits = 6)

cat("\n--- TOTAL BY CATEGORY ---\n")

print(
  exclusion_summary |>
    summarise(
      across(
        c(
          shallow_km2,
          deep_km2,
          non_submerged_km2,
          total_km2
        ),
        sum
      )
    ),
  digits = 6
)

# Check consistency with earlier overlap estimate
stopifnot(
  isTRUE(
    all.equal(
      sum(exclusion_summary$total_km2),
      sum(comparison$baseline_km2),
      tolerance = 1e-5
    )
  )
)

write.csv(
  exclusion_summary,
  here("outputs", "depth_mask_exclusion_audit.csv"),
  row.names = FALSE
)

cat("\nDepth-mask exclusion audit completed.\n")

