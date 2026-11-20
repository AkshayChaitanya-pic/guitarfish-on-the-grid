
# ============================================================
# GUITARFISH ON THE GRID
# Script 04: Giant guitarfish occurrence data acquisition
# Source: Ocean Biodiversity Information System (OBIS)
# ============================================================

if (!requireNamespace("robis", quietly = TRUE)) {
  install.packages("robis")
}

library(robis)
library(dplyr)
library(here)

# Download accepted species records
# WoRMS AphiaID 280875 = Glaucostegus typus
guitarfish_raw <- robis::occurrence(taxonid = 280875)

# Initial dataset audit
cat("\n--- OBIS OCCURRENCE AUDIT ---\n")
cat("Number of records:", nrow(guitarfish_raw), "\n")
cat("Number of columns:", ncol(guitarfish_raw), "\n")

print(names(guitarfish_raw))

# Check available coordinates
if (all(c("decimalLongitude", "decimalLatitude") %in%
        names(guitarfish_raw))) {
  
  print(
    summary(
      guitarfish_raw[
        c("decimalLongitude", "decimalLatitude")
      ]
    )
  )
}

# Check species names in returned records
if ("scientificName" %in% names(guitarfish_raw)) {
  print(table(guitarfish_raw$scientificName, useNA = "ifany"))
}

# Save raw download locally
saveRDS(
  guitarfish_raw,
  here("data", "raw", "obis_guitarfish_raw.rds")
)

cat("\nOBIS data downloaded and saved.\n")

# ============================================================
# 5. OCCURRENCE QUALITY AND SPATIAL COVERAGE AUDIT
# ============================================================

library(sf)
library(dplyr)
library(ggplot2)

# Keep records with valid numeric coordinates
guitarfish_coords <- guitarfish_raw |>
  filter(
    !is.na(decimalLongitude),
    !is.na(decimalLatitude),
    between(decimalLongitude, -180, 180),
    between(decimalLatitude, -90, 90)
  )

cat("\n--- COORDINATE QUALITY ---\n")

cat("Records with valid coordinate ranges:",
    nrow(guitarfish_coords), "\n")

# Count unique coordinate pairs
unique_locations <- guitarfish_coords |>
  distinct(decimalLongitude, decimalLatitude)

cat("Unique coordinate pairs:",
    nrow(unique_locations), "\n")

# Identify repeated sampling locations
top_locations <- guitarfish_coords |>
  count(
    decimalLongitude,
    decimalLatitude,
    sort = TRUE
  ) |>
  head(10)

cat("\n--- MOST FREQUENT COORDINATES ---\n")
print(top_locations)

# Geographic coverage in northern Australia
# Preliminary bounding box, NOT the actual NPF boundary
northern_records <- guitarfish_coords |>
  filter(
    between(decimalLongitude, 120, 145),
    between(decimalLatitude, -20, -9)
  )

cat("\n--- NORTHERN AUSTRALIA (PRELIMINARY) ---\n")
cat("Records:", nrow(northern_records), "\n")

cat(
  "Unique locations:",
  nrow(
    northern_records |>
      distinct(decimalLongitude, decimalLatitude)
  ),
  "\n"
)

# Inspect source datasets
cat("\n--- TOP OBIS DATASETS ---\n")

print(
  guitarfish_coords |>
    count(datasetName, sort = TRUE) |>
    head(10)
)

# Inspect temporal distribution
cat("\n--- RECORDS BY YEAR ---\n")

print(
  guitarfish_coords |>
    count(date_year, sort = TRUE) |>
    head(15)
)

# Inspect OBIS quality flags
cat("\n--- QUALITY FLAGS ---\n")

print(
  guitarfish_coords |>
    count(dropped, sort = TRUE)
)

# ============================================================
# 6. FIRST OCCURRENCE DISTRIBUTION MAP
# ============================================================

guitarfish_sf <- st_as_sf(
  guitarfish_coords,
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326,
  remove = FALSE
)

p_occurrences <- ggplot() +
  geom_sf(
    data = guitarfish_sf,
    colour = "#16877B",
    alpha = 0.3,
    size = 0.7
  ) +
  coord_sf(
    xlim = c(110, 160),
    ylim = c(-35, 0),
    expand = FALSE
  ) +
  labs(
    title = "Where have giant guitarfish been recorded?",
    subtitle = "OBIS occurrence records | Preliminary spatial audit",
    caption = "Source: OBIS | Raw occurrence records, not independently validated"
  ) +
  theme_minimal(base_size = 12)

print(p_occurrences)
