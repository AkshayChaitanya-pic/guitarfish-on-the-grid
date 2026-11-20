# ============================================================
# GUITARFISH ON THE GRID
# Script 01: Northern Prawn Fishery data exploration
#
# Purpose:
# Inspect the 2024 fishing intensity and fishing footprint
# datasets before conducting spatial exposure analysis.
#
# Data source: ABARES
# ============================================================

library(sf)
library(dplyr)
library(ggplot2)
library(here)

# 1. Locate the geodatabase
gdb_path <- here("data", "raw", "NPF.gdb")

# 2. Load 2024 relative fishing intensity
npf_rfi_2024 <- st_read(
  gdb_path,
  layer = "NPF_rfi_2024",
  quiet = TRUE
)

# 3. Load 2024 fishing footprint
npf_fp_2024 <- st_read(
  gdb_path,
  layer = "NPF_fp_2024",
  quiet = TRUE
)

# 4. Inspect the data
cat("\n--- RELATIVE FISHING INTENSITY ---\n")

print(dim(npf_rfi_2024))
print(names(npf_rfi_2024))
print(st_crs(npf_rfi_2024))

cat("\n--- INTENSITY ATTRIBUTES ---\n")

print(
  st_drop_geometry(npf_rfi_2024) |>
    distinct(RelativeIntensity, Range, Units, Season)
)

cat("\n--- FISHING FOOTPRINT ---\n")

print(dim(npf_fp_2024))
print(names(npf_fp_2024))

# 5. Check spatial geometry
cat("\n--- GEOMETRY VALIDITY ---\n")

print(table(st_is_valid(npf_rfi_2024)))
print(table(st_is_valid(npf_fp_2024)))

# 6. Inspect spatial extent
cat("\n--- STUDY EXTENT ---\n")

print(st_bbox(npf_rfi_2024))

# 7. Produce the first exploratory map
p_npf <- ggplot() +
  geom_sf(
    data = npf_rfi_2024,
    aes(fill = RelativeIntensity),
    colour = NA
  ) +
  coord_sf(datum = NA) +
  labs(
    title = "Northern Prawn Fishery",
    subtitle = "Relative fishing intensity, 2024",
    fill = "Intensity",
    caption = "Source: ABARES"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

print(p_npf)