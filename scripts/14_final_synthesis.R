
# ============================================================
# GUITARFISH ON THE GRID
# Script 14: Final scientific synthesis
# Step 1: Cross-dataset consistency audit
# ============================================================

library(dplyr)
library(readr)
library(here)

# ------------------------------------------------------------
# 1. Load validated analytical outputs
# ------------------------------------------------------------

area <- read_csv(
  here("outputs", "exposure_area_summary.csv"),
  show_col_types = FALSE
)

depth <- read_csv(
  here("outputs", "depth_constrained_overlap_by_class.csv"),
  show_col_types = FALSE
)

overall <- read_csv(
  here("outputs", "depth_constrained_overlap_overall.csv"),
  show_col_types = FALSE
)

exclusions <- read_csv(
  here("outputs", "depth_mask_exclusion_audit.csv"),
  show_col_types = FALSE
)

obis_period <- read_csv(
  here("outputs", "obis_period_summary.csv"),
  show_col_types = FALSE
)

obis_spatial <- read_csv(
  here("outputs", "obis_spatial_period_summary.csv"),
  show_col_types = FALSE
)

obis_categories <- read_csv(
  here("outputs", "obis_spatial_category_by_period.csv"),
  show_col_types = FALSE
)

# ------------------------------------------------------------
# 2. Verify expected data structures
# ------------------------------------------------------------

stopifnot(
  nrow(area) == 3,
  nrow(depth) == 3,
  nrow(overall) == 1,
  nrow(exclusions) == 3,
  nrow(obis_period) == 3,
  nrow(obis_spatial) == 3,
  setequal(
    area$intensity,
    c("Low", "Medium", "High")
  )
)

cat("\n--- INPUT STRUCTURES VALIDATED ---\n")

# ------------------------------------------------------------
# 3. Compare independent class-level summaries
# ------------------------------------------------------------

class_check <- area |>
  left_join(
    depth,
    by = c("intensity" = "RelativeIntensity")
  ) |>
  mutate(
    baseline_difference_km2 =
      baseline_overlap_km2 - baseline_km2,
    
    screened_difference_km2 =
      screened_overlap_km2 - depth_constrained_km2,
    
    total_difference_km2 =
      mapped_area_km2 - total_km2
  )

cat("\n--- CLASS-LEVEL CONSISTENCY ---\n")

print(
  class_check |>
    select(
      intensity,
      baseline_difference_km2,
      screened_difference_km2,
      total_difference_km2
    )
)

# Area summary was rounded to 0.1 km2
stopifnot(
  all(abs(class_check$baseline_difference_km2) < 0.11),
  all(abs(class_check$screened_difference_km2) < 0.11),
  all(abs(class_check$total_difference_km2) < 0.11)
)

# ------------------------------------------------------------
# 4. Reconcile class totals with overall results
# ------------------------------------------------------------

area_totals <- depth |>
  summarise(
    mapped_km2 = sum(total_km2),
    baseline_km2 = sum(baseline_km2),
    screened_km2 = sum(depth_constrained_km2)
  )

overall_check <- tibble(
  metric = c(
    "Mapped area",
    "Baseline overlap",
    "GEBCO-screened overlap"
  ),
  
  class_sum_km2 = c(
    area_totals$mapped_km2,
    area_totals$baseline_km2,
    area_totals$screened_km2
  ),
  
  reported_overall_km2 = c(
    overall$total_km2,
    overall$baseline_km2,
    overall$depth_constrained_km2
  )
) |>
  mutate(
    difference_km2 =
      class_sum_km2 - reported_overall_km2
  )

cat("\n--- OVERALL AREA RECONCILIATION ---\n")
print(overall_check)

# Allow 2 km2 for previously rounded outputs
stopifnot(
  all(abs(overall_check$difference_km2) < 2)
)

# ------------------------------------------------------------
# 5. Reconcile GEBCO exclusion categories
# ------------------------------------------------------------

exclusion_check <- exclusions |>
  mutate(
    component_sum_km2 =
      shallow_km2 +
      deep_km2 +
      non_submerged_km2,
    
    difference_km2 =
      component_sum_km2 - total_km2
  )

cat("\n--- GEBCO EXCLUSION RECONCILIATION ---\n")

print(
  exclusion_check |>
    select(
      RelativeIntensity,
      component_sum_km2,
      total_km2,
      difference_km2
    )
)

stopifnot(
  all(abs(exclusion_check$difference_km2) < 2)
)

# ------------------------------------------------------------
# 6. Reconcile OBIS observation totals
# ------------------------------------------------------------

cat("\n--- OBIS TOTALS ---\n")

obis_totals <- tibble(
  metric = c(
    "All northern records",
    "Records inside NPF",
    "Records inside CSIRO",
    "Records inside both"
  ),
  
  value = c(
    sum(obis_period$records),
    sum(obis_spatial$n_inside_npf),
    sum(obis_spatial$n_inside_csiro),
    sum(obis_spatial$n_inside_both)
  )
)

print(obis_totals)

stopifnot(
  sum(obis_period$records) == 105,
  sum(obis_spatial$records) == 105,
  sum(obis_spatial$n_inside_npf) == 26,
  sum(obis_spatial$n_inside_csiro) == 75,
  sum(obis_spatial$n_inside_both) == 11,
  sum(obis_categories$records) == 105
)

# ------------------------------------------------------------
# 7. Reconcile detailed OBIS spatial categories
# ------------------------------------------------------------

category_check <- obis_categories |>
  group_by(observation_period) |>
  summarise(
    category_total = sum(records),
    .groups = "drop"
  ) |>
  left_join(
    obis_period,
    by = "observation_period"
  ) |>
  mutate(
    difference = category_total - records
  )

cat("\n--- OBIS CATEGORY RECONCILIATION ---\n")
print(category_check)

stopifnot(
  all(category_check$difference == 0)
)

cat("\nALL SCRIPT 14 STEP 1 CHECKS PASSED.\n")


# ============================================================
# 8. GENERATE FINAL SCIENTIFIC RESULTS
# ============================================================

# Compute percentages from the validated area values
baseline_pct <- 100 *
  overall$baseline_km2[1] /
  overall$total_km2[1]

screened_pct <- 100 *
  overall$depth_constrained_km2[1] /
  overall$total_km2[1]

# Quantify the GEBCO screening exclusions
deep_excluded <- sum(exclusions$deep_km2)
coastal_excluded <- sum(exclusions$non_submerged_km2)

total_excluded <- deep_excluded + coastal_excluded

# Verify the exclusion categories match the area difference
stopifnot(
  abs(
    total_excluded -
      (
        overall$baseline_km2[1] -
          overall$depth_constrained_km2[1]
      )
  ) < 2
)

# Derive OBIS counts from validated summaries
recent_npf_count <- obis_spatial |>
  filter(observation_period == "Recent (2020-2026)") |>
  pull(n_inside_npf)

stopifnot(length(recent_npf_count) == 1)

# ============================================================
# 9. BUILD RESULTS TABLE
# ============================================================

final_results <- tibble(
  metric = c(
    "Total mapped NPF area",
    "NPF area overlapping CSIRO distribution",
    "Baseline spatial overlap",
    "GEBCO-screened overlap area",
    "GEBCO-screened overlap",
    "Area excluded by GEBCO screen",
    "Excluded area classified as deeper than 100 m",
    "Excluded area classified as elevation >= 0 m",
    "Northern OBIS records",
    "OBIS records inside NPF",
    "OBIS records inside CSIRO",
    "OBIS records inside both",
    "Recent OBIS records inside NPF"
  ),
  
  value = c(
    overall$total_km2[1],
    overall$baseline_km2[1],
    baseline_pct,
    overall$depth_constrained_km2[1],
    screened_pct,
    total_excluded,
    deep_excluded,
    coastal_excluded,
    sum(obis_period$records),
    sum(obis_spatial$n_inside_npf),
    sum(obis_spatial$n_inside_csiro),
    sum(obis_spatial$n_inside_both),
    recent_npf_count
  ),
  
  unit = c(
    "km2", "km2", "percent",
    "km2", "percent",
    "km2", "km2", "km2",
    "records", "records",
    "records", "records", "records"
  )
)

cat("\n--- FINAL SCIENTIFIC RESULTS ---\n")
print(final_results, n = Inf)

# ============================================================
# 10. DOCUMENT INTERPRETATION LIMITATIONS
# ============================================================

limitations <- tibble(
  component = c(
    "NPF fishing intensity",
    "CSIRO distribution",
    "Temporal alignment",
    "GEBCO depth screening",
    "OBIS occurrence evidence",
    "Spatial overlap interpretation"
  ),
  
  limitation = c(
    "Relative fishing-intensity polygons do not measure guitarfish bycatch or mortality.",
    "Expert-derived distribution represents a broad geographic range, not abundance or confirmed presence at every location.",
    "CSIRO distribution was last updated in 2008, whereas the fishing-intensity layer represents 2024.",
    "Most area excluded by the depth screen is classified as elevation >= 0 m, potentially reflecting coastal alignment and elevation uncertainty.",
    "Records vary in date, sampling effort, source and spatial-quality flags; they do not represent a systematic abundance survey.",
    "Geographic coincidence identifies potential spatial exposure, not actual animal-trawl encounters or individual risk."
  )
)

cat("\n--- SCIENTIFIC LIMITATIONS ---\n")
print(limitations, n = Inf, width = Inf)

# ============================================================
# 11. EXPORT FINAL TABLES
# ============================================================

write_csv(
  final_results,
  here("outputs", "final_scientific_results.csv")
)

write_csv(
  limitations,
  here("outputs", "final_scientific_limitations.csv")
)

cat("\nFinal scientific tables exported successfully.\n")
