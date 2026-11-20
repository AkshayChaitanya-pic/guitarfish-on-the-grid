
# ============================================================
# GUITARFISH ON THE GRID
# Script 13: Quantitative exposure results
# Step 1: Data audit and consistency checks
# ============================================================

library(dplyr)
library(readr)
library(here)

# ------------------------------------------------------------
# 1. Locate existing analysis outputs
# ------------------------------------------------------------

cat("\n--- AVAILABLE ANALYSIS OUTPUTS ---\n")

analysis_files <- list.files(
  here("outputs"),
  pattern = "overlap|depth|exclusion",
  full.names = FALSE,
  ignore.case = TRUE
)

print(analysis_files)

# ------------------------------------------------------------
# 2. Load depth-constrained results
# ------------------------------------------------------------

depth_class <- read_csv(
  here(
    "outputs",
    "depth_constrained_overlap_by_class.csv"
  ),
  show_col_types = FALSE
)

depth_overall <- read_csv(
  here(
    "outputs",
    "depth_constrained_overlap_overall.csv"
  ),
  show_col_types = FALSE
)

exclusion_audit <- read_csv(
  here(
    "outputs",
    "depth_mask_exclusion_audit.csv"
  ),
  show_col_types = FALSE
)

# ------------------------------------------------------------
# 3. Inspect structures before calculations
# ------------------------------------------------------------

cat("\n--- DEPTH-CONSTRAINED RESULTS BY CLASS ---\n")
print(depth_class, width = Inf)

cat("\n--- COLUMN NAMES ---\n")
print(names(depth_class))

cat("\n--- OVERALL DEPTH SUMMARY ---\n")
print(depth_overall, width = Inf)

cat("\n--- COASTAL / DEPTH EXCLUSION AUDIT ---\n")
print(exclusion_audit, width = Inf)

# ------------------------------------------------------------
# 4. Check missing values and numeric fields
# ------------------------------------------------------------

cat("\n--- MISSING VALUES BY COLUMN ---\n")
print(colSums(is.na(depth_class)))

cat("\n--- NUMERIC COLUMN SUMMARIES ---\n")

numeric_cols <- depth_class |>
  select(where(is.numeric))

print(summary(numeric_cols))

cat("\nScript 13, Step 1 completed.\n")


# ============================================================
# 5. VALIDATE RESULTS BEFORE PLOTTING
# ============================================================

library(ggplot2)
library(tidyr)
library(scales)

required_classes <- c("Low", "Medium", "High")

stopifnot(
  nrow(depth_class) == 3,
  setequal(depth_class$RelativeIntensity, required_classes),
  all(depth_class$total_km2 > 0),
  all(depth_class$baseline_km2 >= 0),
  all(depth_class$depth_constrained_km2 >= 0),
  all(depth_class$depth_constrained_km2 <=
        depth_class$baseline_km2 + 1),
  all(depth_class$baseline_km2 <=
        depth_class$total_km2 + 1),
  all(abs(
    depth_class$baseline_percent -
      100 * depth_class$baseline_km2 /
      depth_class$total_km2
  ) < 0.1),
  all(abs(
    depth_class$depth_percent -
      100 * depth_class$depth_constrained_km2 /
      depth_class$total_km2
  ) < 0.1),
  abs(sum(depth_class$baseline_km2) -
        depth_overall$baseline_km2[1]) < 2,
  abs(sum(depth_class$depth_constrained_km2) -
        depth_overall$depth_constrained_km2[1]) < 2
)

cat("\nQuantitative results validated.\n")

# ============================================================
# 6. PREPARE PLOTTING DATA
# ============================================================

plot_data <- depth_class |>
  select(
    RelativeIntensity,
    baseline_percent,
    depth_percent
  ) |>
  pivot_longer(
    cols = c(baseline_percent, depth_percent),
    names_to = "screening",
    values_to = "overlap_percent"
  ) |>
  mutate(
    RelativeIntensity = factor(
      RelativeIntensity,
      levels = c("High", "Medium", "Low")
    ),
    screening = recode(
      screening,
      baseline_percent = "Baseline overlap",
      depth_percent = "GEBCO 0-100 m screen"
    ),
    screening = factor(
      screening,
      levels = c(
        "Baseline overlap",
        "GEBCO 0-100 m screen"
      )
    )
  )

# ============================================================
# 7. BUILD THE QUANTITATIVE FIGURE
# ============================================================

p_exposure <- ggplot(
  plot_data,
  aes(
    x = overlap_percent,
    y = RelativeIntensity,
    fill = screening
  )
) +
  
  geom_col(
    position = position_dodge(width = 0.72),
    width = 0.62
  ) +
  
  geom_text(
    aes(label = sprintf("%.1f%%", overlap_percent)),
    position = position_dodge(width = 0.72),
    hjust = -0.12,
    size = 4,
    fontface = "bold"
  ) +
  
  scale_fill_manual(
    values = c(
      "Baseline overlap" = "#245A81",
      "GEBCO 0-100 m screen" = "#78AFA6"
    )
  ) +
  
  scale_x_continuous(
    limits = c(0, 110),
    breaks = seq(0, 100, 20),
    labels = label_percent(scale = 1),
    expand = expansion(mult = c(0, 0))
  ) +
  
  labs(
    title = "Mapped fishing area overlapping giant guitarfish distribution",
    subtitle = paste(
      "Percentage of each 2024 NPF fishing-intensity",
      "class overlapping the CSIRO expert distribution"
    ),
    x = "Proportion of mapped fishing-intensity area (%)",
    y = NULL,
    fill = NULL,
    caption = paste(
      "Baseline: vector intersection of NPF polygons and CSIRO expert range.",
      "GEBCO screen: overlapping area classified as 0-100 m water depth.",
      "Coastal elevation mismatches dominate excluded area.",
      "Spatial coincidence does not measure bycatch, abundance or mortality.",
      sep = "\n"
    )
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 17
    ),
    plot.subtitle = element_text(
      colour = "#555555",
      size = 11
    ),
    axis.text.y = element_text(
      face = "bold",
      size = 12
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(
      hjust = 0,
      colour = "#666666",
      size = 9
    )
  )

print(p_exposure)

# ============================================================
# 8. EXPORT FIGURE
# ============================================================

ggsave(
  here("figures", "13_exposure_by_intensity.png"),
  plot = p_exposure,
  width = 11,
  height = 6.5,
  dpi = 300,
  bg = "white"
)

cat("\nQuantitative exposure figure saved.\n")


# ============================================================
# 9. EXPORT ABSOLUTE AREA SUMMARY
# ============================================================

area_summary <- depth_class |>
  transmute(
    intensity = RelativeIntensity,
    mapped_area_km2 = round(total_km2, 1),
    baseline_overlap_km2 = round(baseline_km2, 1),
    screened_overlap_km2 = round(depth_constrained_km2, 1),
    baseline_overlap_percent = round(baseline_percent, 1),
    screened_overlap_percent = round(depth_percent, 1)
  )

print(area_summary)

write.csv(
  area_summary,
  here("outputs", "exposure_area_summary.csv"),
  row.names = FALSE
)

cat("\nAbsolute area summary exported.\n")
