
# ============================================================
# GUITARFISH ON THE GRID
# Script 11: OBIS spatial evidence and temporal screening
# ============================================================

library(sf)
library(dplyr)
library(here)

# ------------------------------------------------------------
# 1. Load audited OBIS data
# ------------------------------------------------------------

obis <- readRDS(
  here(
    "data",
    "processed",
    "obis_northern_australia_screened.rds"
  )
)

stopifnot(
  inherits(obis, "sf"),
  "year_recovered" %in% names(obis),
  "inside_npf" %in% names(obis),
  "inside_csiro" %in% names(obis)
)

cat("\n--- DATA VALIDATION ---\n")
cat("Records:", nrow(obis), "\n")
cat("CRS:", st_crs(obis)$epsg, "\n")
cat("Recovered years:", sum(!is.na(obis$year_recovered)), "\n")

# ------------------------------------------------------------
# 2. Define observation periods
# ------------------------------------------------------------

obis <- obis |>
  mutate(
    observation_period = case_when(
      is.na(year_recovered) ~ "Undated",
      year_recovered < 2020 ~ "Historical (<2020)",
      year_recovered >= 2020 ~ "Recent (2020-2026)"
    )
  )

period_summary <- obis |>
  st_drop_geometry() |>
  count(observation_period, name = "records")

cat("\n--- OBSERVATION PERIODS ---\n")
print(period_summary)


# ------------------------------------------------------------
# 3. Spatial evidence by observation period
#    Corrected: avoid summary-name collisions
# ------------------------------------------------------------

spatial_period_summary <- obis |>
  st_drop_geometry() |>
  group_by(observation_period) |>
  summarise(
    records = n(),
    
    unique_locations = n_distinct(
      paste(longitude, latitude, sep = "_")
    ),
    
    n_inside_npf = sum(inside_npf, na.rm = TRUE),
    
    n_inside_csiro = sum(inside_csiro, na.rm = TRUE),
    
    n_inside_both = sum(
      inside_npf & inside_csiro,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )

cat("\n--- CORRECTED SPATIAL EVIDENCE BY PERIOD ---\n")
print(spatial_period_summary)

# Validate against the detailed category summary
expected_both <- obis |>
  st_drop_geometry() |>
  filter(inside_npf & inside_csiro) |>
  count(observation_period, name = "expected_both")

validation <- spatial_period_summary |>
  left_join(expected_both, by = "observation_period") |>
  mutate(expected_both = coalesce(expected_both, 0L))

stopifnot(
  all(validation$n_inside_both == validation$expected_both),
  sum(spatial_period_summary$n_inside_both) == 11
)

write.csv(
  spatial_period_summary,
  here("outputs", "obis_spatial_period_summary.csv"),
  row.names = FALSE
)

cat("\nSpatial summary validated successfully.\n")


# ------------------------------------------------------------
# 4. Detailed spatial categories
# ------------------------------------------------------------

obis <- obis |>
  mutate(
    spatial_category = case_when(
      inside_npf & inside_csiro ~
        "NPF and CSIRO range",
      
      inside_npf & !inside_csiro ~
        "NPF only",
      
      !inside_npf & inside_csiro ~
        "CSIRO range only",
      
      TRUE ~ "Outside both"
    )
  )

spatial_category_summary <- obis |>
  st_drop_geometry() |>
  count(
    observation_period,
    spatial_category,
    name = "records"
  )

cat("\n--- SPATIAL CATEGORIES BY PERIOD ---\n")
print(spatial_category_summary, n = Inf)

# ------------------------------------------------------------
# 5. Recent occurrence locations inside NPF
# ------------------------------------------------------------

recent_npf <- obis |>
  filter(
    year_recovered >= 2020,
    inside_npf
  )

recent_npf_summary <- recent_npf |>
  st_drop_geometry() |>
  summarise(
    records = n(),
    
    unique_locations = n_distinct(
      paste(longitude, latitude, sep = "_")
    ),
    
    inside_csiro = sum(inside_csiro),
    
    outside_csiro = sum(!inside_csiro)
  )

cat("\n--- RECENT NPF OCCURRENCES ---\n")
print(recent_npf_summary)

# ------------------------------------------------------------
# 6. Observation basis and quality flags
# ------------------------------------------------------------

quality_summary <- obis |>
  st_drop_geometry() |>
  count(
    observation_period,
    basisOfRecord,
    flags,
    name = "records",
    sort = TRUE
  )

cat("\n--- RECORD TYPES AND FLAGS ---\n")
print(quality_summary, n = Inf)

# ------------------------------------------------------------
# 7. Export reproducible summaries
# ------------------------------------------------------------

write.csv(
  period_summary,
  here("outputs", "obis_period_summary.csv"),
  row.names = FALSE
)

write.csv(
  spatial_period_summary,
  here("outputs", "obis_spatial_period_summary.csv"),
  row.names = FALSE
)

write.csv(
  spatial_category_summary,
  here("outputs", "obis_spatial_category_by_period.csv"),
  row.names = FALSE
)

write.csv(
  recent_npf_summary,
  here("outputs", "obis_recent_npf_summary.csv"),
  row.names = FALSE
)

write.csv(
  quality_summary,
  here("outputs", "obis_record_quality_by_period.csv"),
  row.names = FALSE
)

cat("\nScript 11 completed successfully.\n")
