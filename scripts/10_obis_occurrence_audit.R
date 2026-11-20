
# ============================================================
# GUITARFISH ON THE GRID
# Script 10: OBIS occurrence data quality audit
# ============================================================

library(dplyr)
library(sf)
library(ggplot2)
library(here)

dir.create(here("outputs"), showWarnings = FALSE)
dir.create(
  here("data", "processed"),
  showWarnings = FALSE,
  recursive = TRUE
)

# ------------------------------------------------------------
# 1. Load OBIS records
# ------------------------------------------------------------

obis <- readRDS(
  here("data", "raw", "obis_guitarfish_raw.rds")
)

cat("\n--- DATASET DIMENSIONS ---\n")
cat("Records:", nrow(obis), "\n")
cat("Columns:", ncol(obis), "\n")

cat("\n--- AVAILABLE FIELDS ---\n")
print(names(obis))

# ------------------------------------------------------------
# 2. Validate essential fields
# ------------------------------------------------------------

required_fields <- c(
  "scientificName",
  "decimalLongitude",
  "decimalLatitude"
)

stopifnot(
  all(required_fields %in% names(obis))
)

# Convert coordinates safely to numeric
obis <- obis |>
  mutate(
    longitude = suppressWarnings(
      as.numeric(decimalLongitude)
    ),
    latitude = suppressWarnings(
      as.numeric(decimalLatitude)
    )
  )

# Identify valid global coordinates
obis <- obis |>
  mutate(
    valid_coordinates =
      !is.na(longitude) &
      !is.na(latitude) &
      is.finite(longitude) &
      is.finite(latitude) &
      between(longitude, -180, 180) &
      between(latitude, -90, 90),
    
    zero_coordinates =
      valid_coordinates &
      longitude == 0 &
      latitude == 0,
    
    usable_coordinates =
      valid_coordinates &
      !zero_coordinates
  )

# ------------------------------------------------------------
# 3. Coordinate quality summary
# ------------------------------------------------------------

coordinate_summary <- obis |>
  summarise(
    total_records = n(),
    missing_or_invalid_coordinates =
      sum(!valid_coordinates),
    zero_zero_coordinates =
      sum(zero_coordinates, na.rm = TRUE),
    usable_coordinate_records =
      sum(usable_coordinates, na.rm = TRUE)
  )

cat("\n--- COORDINATE QUALITY ---\n")
print(coordinate_summary)

# ------------------------------------------------------------
# 4. Unique locations and duplicates
# ------------------------------------------------------------

obis_valid <- obis |>
  filter(usable_coordinates)

location_summary <- obis_valid |>
  summarise(
    records_with_coordinates = n(),
    unique_exact_locations = n_distinct(
      paste(longitude, latitude, sep = "_")
    )
  ) |>
  mutate(
    repeated_location_records =
      records_with_coordinates -
      unique_exact_locations
  )

cat("\n--- LOCATION DUPLICATION ---\n")
print(location_summary)

# ------------------------------------------------------------
# 5. Temporal coverage
# ------------------------------------------------------------

if ("year" %in% names(obis)) {
  
  obis_valid <- obis_valid |>
    mutate(
      observation_year = suppressWarnings(
        as.integer(as.character(year))
      )
    )
  
} else {
  
  obis_valid$observation_year <- NA_integer_
  
}

temporal_summary <- obis_valid |>
  summarise(
    records_with_year = sum(
      !is.na(observation_year)
    ),
    earliest_year = if (
      all(is.na(observation_year))
    ) NA_integer_ else min(
      observation_year,
      na.rm = TRUE
    ),
    latest_year = if (
      all(is.na(observation_year))
    ) NA_integer_ else max(
      observation_year,
      na.rm = TRUE
    )
  )

cat("\n--- TEMPORAL COVERAGE ---\n")
print(temporal_summary)

year_counts <- obis_valid |>
  filter(!is.na(observation_year)) |>
  count(observation_year, name = "records")

# ------------------------------------------------------------
# 6. Data-source concentration
# ------------------------------------------------------------

if ("datasetName" %in% names(obis_valid)) {
  
  source_summary <- obis_valid |>
    count(
      datasetName,
      sort = TRUE,
      name = "records"
    )
  
} else {
  
  source_summary <- data.frame(
    datasetName = "Not available",
    records = nrow(obis_valid)
  )
  
}

cat("\n--- TOP OBIS DATA SOURCES ---\n")
print(head(source_summary, 15))

# ------------------------------------------------------------
# 7. Northern Australia screening
# ------------------------------------------------------------

# Broad geographic screening window.
# This is NOT the NPF fishing footprint.
north_bbox <- obis_valid |>
  filter(
    between(longitude, 125, 145),
    between(latitude, -20, -9)
  )

cat("\n--- NORTHERN AUSTRALIA SCREEN ---\n")
cat("Records:", nrow(north_bbox), "\n")

cat(
  "Unique exact locations:",
  n_distinct(
    paste(
      north_bbox$longitude,
      north_bbox$latitude,
      sep = "_"
    )
  ),
  "\n"
)

# ------------------------------------------------------------
# 8. Spatial comparison with CSIRO range and NPF
# ------------------------------------------------------------

npf <- st_read(
  here(
    "data",
    "processed",
    "npf_rfi_2024_fixed.gpkg"
  ),
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

guitarfish <- st_read(
  shp_files[1],
  quiet = TRUE
)

npf <- st_transform(st_make_valid(npf), 4326)

guitarfish <- st_transform(
  st_make_valid(guitarfish),
  4326
)

north_sf <- st_as_sf(
  north_bbox,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)

# Test intersection with unioned polygons
npf_union <- st_union(st_geometry(npf))
range_union <- st_union(st_geometry(guitarfish))

north_sf$inside_npf <- lengths(
  st_intersects(north_sf, npf_union)
) > 0

north_sf$inside_csiro <- lengths(
  st_intersects(north_sf, range_union)
) > 0

spatial_summary <- north_sf |>
  st_drop_geometry() |>
  summarise(
    screened_records = n(),
    records_inside_npf = sum(inside_npf),
    records_inside_csiro = sum(inside_csiro),
    records_inside_both = sum(
      inside_npf & inside_csiro
    )
  )

cat("\n--- SPATIAL COMPARISON ---\n")
print(spatial_summary)

# ------------------------------------------------------------
# 9. Export summary tables
# ------------------------------------------------------------

write.csv(
  coordinate_summary,
  here("outputs", "obis_coordinate_quality.csv"),
  row.names = FALSE
)

write.csv(
  location_summary,
  here("outputs", "obis_location_duplicates.csv"),
  row.names = FALSE
)

write.csv(
  temporal_summary,
  here("outputs", "obis_temporal_summary.csv"),
  row.names = FALSE
)

write.csv(
  year_counts,
  here("outputs", "obis_year_counts.csv"),
  row.names = FALSE
)

write.csv(
  source_summary,
  here("outputs", "obis_source_summary.csv"),
  row.names = FALSE
)

write.csv(
  spatial_summary,
  here("outputs", "obis_spatial_summary.csv"),
  row.names = FALSE
)

# Save screened occurrences for subsequent analyses
saveRDS(
  north_sf,
  here(
    "data",
    "processed",
    "obis_northern_australia_screened.rds"
  )
)


# ============================================================
# 10. METADATA AND SPATIAL DISCREPANCY AUDIT
# ============================================================

# ------------------------------------------------------------
# 10A. Check alternative date fields
# ------------------------------------------------------------

date_fields <- c(
  "year",
  "eventDate",
  "date_start",
  "date_mid",
  "date_year"
)

date_completeness <- data.frame(
  field = date_fields,
  non_missing = sapply(
    date_fields,
    function(x) {
      sum(
        !is.na(obis[[x]]) &
          nzchar(trimws(as.character(obis[[x]])))
      )
    }
  )
)

cat("\n--- DATE FIELD COMPLETENESS ---\n")
print(date_completeness)

# ------------------------------------------------------------
# 10B. Check source metadata
# ------------------------------------------------------------

source_fields <- c(
  "datasetName",
  "dataset_id",
  "institutionCode",
  "basisOfRecord",
  "occurrenceID"
)

source_completeness <- data.frame(
  field = source_fields,
  non_missing = sapply(
    source_fields,
    function(x) {
      sum(
        !is.na(obis[[x]]) &
          nzchar(trimws(as.character(obis[[x]])))
      )
    }
  )
)

cat("\n--- SOURCE FIELD COMPLETENESS ---\n")
print(source_completeness)

# ------------------------------------------------------------
# 10C. Inspect records inside NPF but outside CSIRO
# ------------------------------------------------------------

discrepant_records <- north_sf |>
  filter(inside_npf & !inside_csiro) |>
  st_drop_geometry() |>
  select(
    any_of(c(
      "scientificName",
      "longitude",
      "latitude",
      "eventDate",
      "year",
      "datasetName",
      "dataset_id",
      "basisOfRecord",
      "coordinateUncertaintyInMeters",
      "flags"
    ))
  )

cat("\n--- NPF RECORDS OUTSIDE CSIRO RANGE ---\n")
print(discrepant_records, width = Inf)

# ------------------------------------------------------------
# 10D. Unique locations by spatial category
# ------------------------------------------------------------

unique_spatial_summary <- north_sf |>
  st_drop_geometry() |>
  group_by(inside_npf, inside_csiro) |>
  summarise(
    records = n(),
    unique_locations = n_distinct(
      paste(longitude, latitude, sep = "_")
    ),
    .groups = "drop"
  )

cat("\n--- UNIQUE SPATIAL LOCATIONS ---\n")
print(unique_spatial_summary)

# ------------------------------------------------------------
# 10E. Export diagnostic results
# ------------------------------------------------------------

write.csv(
  date_completeness,
  here("outputs", "obis_date_field_completeness.csv"),
  row.names = FALSE
)

write.csv(
  source_completeness,
  here("outputs", "obis_source_field_completeness.csv"),
  row.names = FALSE
)

write.csv(
  discrepant_records,
  here("outputs", "obis_npf_outside_csiro_records.csv"),
  row.names = FALSE
)

write.csv(
  unique_spatial_summary,
  here("outputs", "obis_unique_spatial_locations.csv"),
  row.names = FALSE
)

cat("\nOBIS metadata and spatial audit completed.\n")


# ============================================================
# 11. RECOVER OBIS OBSERVATION YEARS
# ============================================================

# Extract a plausible year from date strings.
# Supports ISO dates and timestamps.
extract_year <- function(x) {
  
  x <- as.character(x)
  
  candidate <- suppressWarnings(
    as.integer(substr(x, 1, 4))
  )
  
  candidate[
    is.na(candidate) |
      candidate < 1800 |
      candidate > 2026
  ] <- NA_integer_
  
  candidate
}

obis <- obis |>
  mutate(
    year_original = suppressWarnings(
      as.integer(as.character(year))
    ),
    
    year_event = extract_year(eventDate),
    year_start = extract_year(date_start),
    year_mid = extract_year(date_mid),
    year_date_field = suppressWarnings(
      as.integer(as.character(date_year))
    ),
    
    year_recovered = coalesce(
      year_event,
      year_start,
      year_mid,
      year_date_field,
      year_original
    )
  )

# Summarise recovered temporal coverage
recovered_temporal_summary <- obis |>
  summarise(
    total_records = n(),
    records_with_year = sum(!is.na(year_recovered)),
    earliest_year = min(
      year_recovered,
      na.rm = TRUE
    ),
    latest_year = max(
      year_recovered,
      na.rm = TRUE
    )
  )

cat("\n--- RECOVERED TEMPORAL COVERAGE ---\n")
print(recovered_temporal_summary)

# Distribution of records by observation year
recovered_year_counts <- obis |>
  filter(!is.na(year_recovered)) |>
  count(year_recovered, name = "records") |>
  arrange(year_recovered)

cat("\n--- RECORDS BY YEAR ---\n")
print(recovered_year_counts, n = Inf)

# Check temporal coverage in northern Australia
north_temporal <- obis |>
  filter(
    usable_coordinates,
    between(longitude, 125, 145),
    between(latitude, -20, -9)
  ) |>
  summarise(
    northern_records = n(),
    records_with_year = sum(!is.na(year_recovered)),
    earliest_year = min(
      year_recovered,
      na.rm = TRUE
    ),
    latest_year = max(
      year_recovered,
      na.rm = TRUE
    )
  )

cat("\n--- NORTHERN AUSTRALIA TEMPORAL COVERAGE ---\n")
print(north_temporal)

write.csv(
  recovered_temporal_summary,
  here("outputs", "obis_recovered_temporal_summary.csv"),
  row.names = FALSE
)

write.csv(
  recovered_year_counts,
  here("outputs", "obis_recovered_year_counts.csv"),
  row.names = FALSE
)

cat("\nTemporal recovery completed.\n")


# ============================================================
# 12. NORTHERN AUSTRALIA RECORD PROVENANCE
# ============================================================

# Reattach recovered years to northern observations
# using the existing spatial screening criteria
north_records <- obis |>
  filter(
    usable_coordinates,
    between(longitude, 125, 145),
    between(latitude, -20, -9)
  )

# ------------------------------------------------------------
# 12A. Northern records by dataset
# ------------------------------------------------------------

north_sources <- north_records |>
  count(
    dataset_id,
    datasetName,
    basisOfRecord,
    sort = TRUE,
    name = "records"
  )

cat("\n--- NORTHERN DATA SOURCES ---\n")
print(north_sources, n = Inf)

# ------------------------------------------------------------
# 12B. Recent observations
# ------------------------------------------------------------

recent_north <- north_records |>
  filter(
    !is.na(year_recovered),
    between(year_recovered, 2020, 2026)
  )

recent_summary <- recent_north |>
  summarise(
    recent_records = n(),
    unique_locations = n_distinct(
      paste(longitude, latitude, sep = "_")
    ),
    earliest_year = min(year_recovered),
    latest_year = max(year_recovered)
  )

cat("\n--- RECENT NORTHERN RECORDS ---\n")
print(recent_summary)

# ------------------------------------------------------------
# 12C. Northern records by year
# ------------------------------------------------------------

north_year_counts <- north_records |>
  filter(!is.na(year_recovered)) |>
  count(year_recovered, name = "records") |>
  arrange(year_recovered)

cat("\n--- NORTHERN RECORDS BY YEAR ---\n")
print(north_year_counts, n = Inf)

# ------------------------------------------------------------
# 12D. Export results
# ------------------------------------------------------------

write.csv(
  north_sources,
  here("outputs", "obis_northern_sources.csv"),
  row.names = FALSE
)

write.csv(
  recent_summary,
  here("outputs", "obis_recent_northern_summary.csv"),
  row.names = FALSE
)

write.csv(
  north_year_counts,
  here("outputs", "obis_northern_year_counts.csv"),
  row.names = FALSE
)

cat("\nNorthern OBIS provenance audit completed.\n")


# ============================================================
# 13. UPDATE PROCESSED NORTHERN OCCURRENCE DATA
# ============================================================

# Recreate the northern spatial dataset using the
# final OBIS table containing recovered observation years

north_final <- obis |>
  filter(
    usable_coordinates,
    between(longitude, 125, 145),
    between(latitude, -20, -9)
  ) |>
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )

# Recalculate spatial intersection flags
north_final$inside_npf <- lengths(
  st_intersects(north_final, npf_union)
) > 0

north_final$inside_csiro <- lengths(
  st_intersects(north_final, range_union)
) > 0

# Confirm expected record count
stopifnot(nrow(north_final) == 105)

# Save updated processed data
saveRDS(
  north_final,
  here(
    "data",
    "processed",
    "obis_northern_australia_screened.rds"
  )
)

cat("\nUpdated northern OBIS dataset saved.\n")
