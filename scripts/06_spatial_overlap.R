
# ============================================================
# GUITARFISH ON THE GRID
# Script 06: CSIRO distribution × NPF fishing intensity
# Purpose: Preliminary spatial exposure analysis
# ============================================================

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

csiro_dir <- here("data", "raw", "csiro_guitarfish")

shp_file <- list.files(
  csiro_dir,
  pattern = "\\.shp$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

stopifnot(length(shp_file) == 1)

guitarfish <- st_read(shp_file, quiet = TRUE)

# ------------------------------------------------------------
# 2. Standardise coordinate reference systems
# ------------------------------------------------------------

target_crs <- 3577

npf <- st_transform(npf, target_crs)

guitarfish <- st_transform(guitarfish, target_crs)

# ------------------------------------------------------------
# 3. Calculate spatial intersection
# ------------------------------------------------------------

overlap <- suppressWarnings(
  st_intersection(npf, guitarfish)
)

cat("\n--- SPATIAL INTERSECTION ---\n")
cat("Intersecting features:", nrow(overlap), "\n")

# ------------------------------------------------------------
# 4. Calculate overlap areas
# ------------------------------------------------------------

overlap$area_km2 <- as.numeric(
  st_area(overlap)
) / 1e6

summary_table <- overlap |>
  st_drop_geometry() |>
  group_by(RelativeIntensity) |>
  summarise(
    overlap_km2 = sum(area_km2),
    .groups = "drop"
  ) |>
  mutate(
    percent_overlap = 100 * overlap_km2 /
      sum(overlap_km2)
  ) |>
  arrange(
    factor(
      RelativeIntensity,
      levels = c("Low", "Medium", "High")
    )
  )

cat("\n--- EXPOSURE BY INTENSITY ---\n")
print(summary_table)

cat(
  "\nTotal intersecting area:",
  round(sum(overlap$area_km2), 2),
  "km2\n"
)

# ------------------------------------------------------------
# 5. Compare with total mapped intensity area
# ------------------------------------------------------------

total_npf_area <- as.numeric(
  st_area(st_union(st_geometry(npf)))
) / 1e6

total_overlap_area <- as.numeric(
  st_area(st_union(st_geometry(overlap)))
) / 1e6

cat("\n--- OVERLAP SUMMARY ---\n")

cat(
  "Total mapped NPF intensity area:",
  round(total_npf_area, 2),
  "km2\n"
)

cat(
  "Area inside CSIRO distribution:",
  round(total_overlap_area, 2),
  "km2\n"
)

cat(
  "Percentage of mapped intensity area inside range:",
  round(
    100 * total_overlap_area / total_npf_area,
    2
  ),
  "%\n"
)

# ------------------------------------------------------------
# 6. First overlap visualisation
# ------------------------------------------------------------

p_overlap <- ggplot() +
  geom_sf(
    data = guitarfish,
    fill = "#B5DAD4",
    colour = NA,
    alpha = 0.5
  ) +
  geom_sf(
    data = overlap,
    aes(fill = RelativeIntensity),
    colour = NA
  ) +
  scale_fill_manual(
    values = c(
      "Low" = "#63C5BE",
      "Medium" = "#F1B65B",
      "High" = "#D95B50"
    )
  ) +
  coord_sf(
    crs = st_crs(target_crs),
    default_crs = st_crs(target_crs),
    xlim = st_bbox(npf)[c("xmin", "xmax")],
    ylim = st_bbox(npf)[c("ymin", "ymax")],
    expand = FALSE
  ) +
  labs(
    title = "Guitarfish on the Grid",
    subtitle = "Potential range overlap with NPF fishing intensity, 2024",
    fill = "Relative intensity",
    caption = "Sources: CSIRO expert distribution (2008); ABARES NPF (2024)"
  ) +
  theme_minimal(base_size = 12)

print(p_overlap)

# ------------------------------------------------------------
# 7. Save outputs locally
# ------------------------------------------------------------

write.csv(
  summary_table,
  here("outputs", "npf_guitarfish_overlap_summary.csv"),
  row.names = FALSE
)

st_write(
  overlap,
  here("data", "processed", "npf_guitarfish_overlap_2024.gpkg"),
  delete_dsn = TRUE,
  quiet = TRUE
)

ggsave(
  here("figures", "guitarfish_npf_overlap_draft.png"),
  p_overlap,
  width = 12,
  height = 7,
  dpi = 300
)

cat("\nOverlap analysis completed.\n")


# ============================================================
# 8. CLASS-SPECIFIC SPATIAL OVERLAP
# ============================================================


# Total mapped area for each fishing-intensity class
npf_class_areas <- npf |>
  mutate(
    total_km2 = as.numeric(st_area(npf)) / 1e6
  ) |>
  st_drop_geometry() |>
  group_by(RelativeIntensity) |>
  summarise(
    total_km2 = sum(total_km2),
    .groups = "drop"
  )

# Area within the CSIRO expert distribution, by class
overlap_class_areas <- overlap |>
  st_drop_geometry() |>
  group_by(RelativeIntensity) |>
  summarise(
    inside_range_km2 = sum(area_km2),
    .groups = "drop"
  )

# Combine the two
class_comparison <- npf_class_areas |>
  left_join(
    overlap_class_areas,
    by = "RelativeIntensity"
  ) |>
  mutate(
    inside_range_km2 = coalesce(inside_range_km2, 0),
    outside_range_km2 = pmax(
      total_km2 - inside_range_km2,
      0
    ),
    percent_inside = 100 *
      inside_range_km2 / total_km2
  ) |>
  arrange(
    factor(
      RelativeIntensity,
      levels = c("Low", "Medium", "High")
    )
  )

cat("\n--- CLASS-SPECIFIC OVERLAP ---\n")
print(class_comparison, width = Inf)

# Export reproducible results
write.csv(
  class_comparison,
  here("outputs", "npf_overlap_by_intensity_2024.csv"),
  row.names = FALSE
)

# ============================================================
# 9. VISUALISE PROPORTIONAL OVERLAP
# ============================================================

p_class_overlap <- ggplot(
  class_comparison,
  aes(
    x = factor(
      RelativeIntensity,
      levels = c("Low", "Medium", "High")
    ),
    y = percent_inside,
    fill = RelativeIntensity
  )
) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = sprintf("%.1f%%", percent_inside)),
    vjust = -0.5,
    fontface = "bold",
    size = 5
  ) +
  scale_fill_manual(
    values = c(
      "Low" = "#63C5BE",
      "Medium" = "#F1B65B",
      "High" = "#D95B50"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 110),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "How much fishing area overlaps the guitarfish range?",
    subtitle = "Percentage within CSIRO expert distribution, by intensity class",
    x = "Fishing intensity",
    y = "Area within expert range",
    caption = "Sources: ABARES NPF 2024; CSIRO expert distribution 2008"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )

print(p_class_overlap)

ggsave(
  here("figures", "npf_class_specific_overlap.png"),
  p_class_overlap,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

