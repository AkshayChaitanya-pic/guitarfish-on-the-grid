# ============================================================
# GUITARFISH ON THE GRID
# Script 16: Spatial data export for GEE and ArcGIS Pro
# ============================================================

library(sf)
library(terra)
library(dplyr)
library(here)

# ------------------------------------------------------------
# 1. Create export directories
# ------------------------------------------------------------

gee_dir <- here("data", "exports", "gee")
arcgis_dir <- here("data", "exports", "arcgis")

dir.create(
  gee_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  arcgis_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------
# 2. Load existing validated spatial datasets
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
    "data", "raw", "csiro_guitarfish",
    "CAAB_FISHMAPPolygon.shp"
  ),
  quiet = TRUE
)

overlap <- st_read(
  here(
    "data", "processed",
    "npf_guitarfish_overlap_2024.gpkg"
  ),
  quiet = TRUE
)

obis <- readRDS(
  here(
    "data", "processed",
    "obis_northern_australia_screened.rds"
  )
)

gebco <- rast(
  here(
    "data", "raw", "gebco",
    "gebco_2026_n-9.0_s-19.0_w128.0_e143.0_geotiff.tif"
  )
)

# ------------------------------------------------------------
# 3. Inspect vector attributes
# ------------------------------------------------------------

cat("\n--- NPF ATTRIBUTES ---\n")
print(names(npf))

cat("\n--- CSIRO ATTRIBUTES ---\n")
print(names(csiro))

cat("\n--- OVERLAP ATTRIBUTES ---\n")
print(names(overlap))

cat("\n--- OBIS ATTRIBUTES ---\n")
print(names(obis))

# ------------------------------------------------------------
# 4. Confirm spatial metadata
# ------------------------------------------------------------

layers <- list(
  NPF = npf,
  CSIRO = csiro,
  Overlap = overlap,
  OBIS = obis
)

spatial_summary <- do.call(
  rbind,
  lapply(names(layers), function(nm) {
    
    x <- layers[[nm]]
    
    data.frame(
      layer = nm,
      features = nrow(x),
      epsg = st_crs(x)$epsg,
      geometry = paste(
        unique(as.character(st_geometry_type(x))),
        collapse = ", "
      ),
      invalid = sum(!st_is_valid(x), na.rm = TRUE)
    )
  })
)

cat("\n--- VECTOR EXPORT SUMMARY ---\n")
print(spatial_summary)

cat("\n--- GEBCO RASTER ---\n")
print(gebco)

cat("\nSpatial datasets loaded successfully.\n")

# ============================================================
# 5. PREPARE STREAMLINED EXPORT LAYERS
# ============================================================

npf_export <- npf |>
  transmute(
    npf_id = Id,
    intensity = as.character(RelativeIntensity),
    effort = as.character(Range),
    units = as.character(Units),
    season = as.character(Season)
  )

csiro_export <- csiro |>
  transmute(
    species = as.character(SCIENTIFIC),
    common = as.character(COMMON_NAM),
    min_depth = MIN_DEPTH,
    max_depth = MAX_DEPTH,
    updated = as.character(LAST_UPDAT)
  )

overlap_export <- overlap |>
  transmute(
    npf_id = Id,
    intensity = as.character(RelativeIntensity),
    effort = as.character(Range),
    species = as.character(SCIENTIFIC),
    area_km2 = area_km2
  )

obis_export <- obis |>
  transmute(
    record_id = as.character(id),
    species = as.character(scientificName),
    year = year_recovered,
    longitude = longitude,
    latitude = latitude,
    record_type = as.character(basisOfRecord),
    dataset_id = as.character(dataset_id),
    flags = as.character(flags),
    on_land = as.integer(
      grepl(
        "ON_LAND",
        coalesce(as.character(flags), "")
      )
    ),
    inside_npf = as.integer(inside_npf),
    inside_csiro = as.integer(inside_csiro),
    period = case_when(
      is.na(year_recovered) ~ "Undated",
      year_recovered < 2020 ~ "Historical",
      TRUE ~ "Recent"
    )
  )

cat("\nStreamlined export layers prepared.\n")

# ============================================================
# 6. EXPORT GOOGLE EARTH ENGINE VECTOR ASSETS
# ============================================================

export_gee_shapefile <- function(x, filename) {
  
  x <- st_transform(x, 4326)
  
  stopifnot(
    all(st_is_valid(x)),
    !any(st_is_empty(x))
  )
  
  # Check shapefile field-name compatibility
  
  attribute_names <- names(st_drop_geometry(x))
  
  stopifnot(
    all(nchar(attribute_names) <= 10)
  )
  
  output_path <- file.path(
    gee_dir,
    paste0(filename, ".shp")
  )
  
  st_write(
    x,
    output_path,
    delete_layer = TRUE,
    quiet = TRUE
  )
  
  cat(
    "\nExported:",
    filename,
    "| Features:",
    nrow(x),
    "\n"
  )
}

export_gee_shapefile(
  npf_export,
  "npf_intensity"
)

export_gee_shapefile(
  csiro_export,
  "csiro_range"
)

export_gee_shapefile(
  overlap_export,
  "guitarfish_overlap"
)

# ============================================================
# 7. EXPORT ARCGIS PRO VECTOR LAYERS
# ============================================================

arcgis_gpkg <- file.path(
  arcgis_dir,
  "guitarfish_spatial_layers.gpkg"
)

arcgis_layers <- list(
  npf_intensity = npf_export,
  csiro_range = csiro_export,
  guitarfish_overlap = overlap_export,
  obis_occurrences = obis_export
)

for (layer_name in names(arcgis_layers)) {
  
  x <- st_transform(
    arcgis_layers[[layer_name]],
    3577
  )
  
  stopifnot(
    all(st_is_valid(x)),
    !any(st_is_empty(x))
  )
  
  st_write(
    x,
    arcgis_gpkg,
    layer = layer_name,
    delete_layer = TRUE,
    quiet = TRUE
  )
  
  cat(
    "\nArcGIS layer exported:",
    layer_name,
    "| Features:",
    nrow(x),
    "\n"
  )
}

print(st_layers(arcgis_gpkg))

# ============================================================
# 8. EXPORT ORIGINAL GEBCO BATHYMETRY
# ============================================================

gebco_source <- here(
  "data", "raw", "gebco",
  "gebco_2026_n-9.0_s-19.0_w128.0_e143.0_geotiff.tif"
)

gee_raster <- file.path(
  gee_dir,
  "gebco_bathymetry.tif"
)

arcgis_raster <- file.path(
  arcgis_dir,
  "gebco_bathymetry.tif"
)

stopifnot(
  file.copy(
    gebco_source,
    gee_raster,
    overwrite = TRUE
  ),
  file.copy(
    gebco_source,
    arcgis_raster,
    overwrite = TRUE
  )
)

raster_hashes <- tools::md5sum(
  c(
    gebco_source,
    gee_raster,
    arcgis_raster
  )
)

print(raster_hashes)

stopifnot(
  length(unique(unname(raster_hashes))) == 1
)

cat("\nGEBCO raster copies verified.\n")

# ============================================================
# 9. FINAL EXPORT INVENTORY
# ============================================================

cat("\n--- GEE EXPORT FILES ---\n")

print(list.files(gee_dir))

cat("\n--- ARCGIS EXPORT FILES ---\n")

print(list.files(arcgis_dir))

cat("\n--- ARCGIS GEOPACKAGE LAYERS ---\n")

print(st_layers(arcgis_gpkg))

cat("\nSpatial data export completed successfully.\n")