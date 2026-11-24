
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
      depth_percent = "Depth-screened overlap"
    ),
    screening = factor(
      screening,
      levels = c(
        "Baseline overlap",
        "Depth-screened overlap"
      )
    )
  )

# Consistent scientific colour palette

exposure_colours <- c(
  "Baseline overlap" = "#175676",
  "Depth-screened overlap" = "#299D8F"
)

# Fishing-intensity class labels

intensity_labels <- c(
  "Low" = "Low\n(<0.25)",
  "Medium" = "Medium\n(0.25-1.0)",
  "High" = "High\n(1.0-2.0)"
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
    position = position_dodge(
      width = 0.76,
      reverse = TRUE
    ),
    width = 0.66
  ) +
  
  geom_text(
    aes(
      label = sprintf("%.1f%%", overlap_percent)
    ),
    position = position_dodge(
      width = 0.76,
      reverse = TRUE
    ),
    hjust = -0.15,
    size = 4.8,
    fontface = "bold",
    colour = "#243746"
  ) +
  
  scale_fill_manual(
    values = exposure_colours
  ) +
  
  scale_y_discrete(
    labels = intensity_labels
  ) +
  
  scale_x_continuous(
    limits = c(0, 110),
    breaks = seq(0, 100, 20),
    labels = label_percent(scale = 1),
    expand = expansion(mult = c(0, 0))
  ) +
  
  labs(
    title = "Potential guitarfish exposure by fishing intensity",
    subtitle = paste(
      "Spatial overlap with the CSIRO expert distribution",
      "in the 2024 Northern Prawn Fishery"
    ),
    x = "Proportion of mapped fishing area (%)",
    y = expression(
      "Fishing intensity (days/km"^2*")"
    ),
    fill = NULL,
    caption = paste(
      "Depth screen: submerged GEBCO cells within 0-100 m water depth.",
      "Coastal elevation classifications dominate the excluded area.",
      "Spatial overlap indicates potential exposure, not observed bycatch or mortality.",
      sep = "\n"
    )
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 19,
      colour = "#173247"
    ),
    
    plot.subtitle = element_text(
      colour = "#536572",
      size = 11.5,
      margin = margin(b = 18)
    ),
    
    axis.text.y = element_text(
      face = "bold",
      size = 12,
      colour = "#243746",
      lineheight = 1.1
    ),
    
    axis.text.x = element_text(
      size = 11,
      colour = "#536572"
    ),
    
    axis.title = element_text(
      size = 12,
      colour = "#243746"
    ),
    
    axis.title.y = element_text(
      margin = margin(r = 12)
    ),
    
    axis.title.x = element_text(
      margin = margin(t = 10)
    ),
    
    panel.grid.major.y = element_blank(),
    
    panel.grid.major.x = element_line(
      colour = "#E2E8ED",
      linewidth = 0.5
    ),
    
    panel.grid.minor = element_blank(),
    
    legend.position = "bottom",
    
    legend.text = element_text(
      size = 12
    ),
    
    legend.key.width = grid::unit(1.1, "cm"),
    
    plot.caption = element_text(
      hjust = 0,
      colour = "#647480",
      size = 9.5,
      lineheight = 1.2,
      margin = margin(t = 12)
    ),
    
    plot.margin = margin(
      t = 20,
      r = 25,
      b = 15,
      l = 15
    ),
    
    plot.background = element_rect(
      fill = "white",
      colour = NA
    )
  )

print(p_exposure)

# ============================================================
# 8. EXPORT FIGURE
# ============================================================

ggsave(
  here("figures", "13_exposure_by_intensity.png"),
  plot = p_exposure,
  width = 12,
  height = 6.75,
  dpi = 300,
  bg = "white"
)

cat("\nRefined quantitative exposure figure saved.\n")

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

# ============================================================
# 10. ABSOLUTE SPATIAL OVERLAP BY FISHING INTENSITY
# ============================================================

absolute_data <- depth_class |>
  select(
    RelativeIntensity,
    baseline_km2,
    depth_constrained_km2
  ) |>
  pivot_longer(
    cols = c(baseline_km2, depth_constrained_km2),
    names_to = "screening",
    values_to = "area_km2"
  ) |>
  mutate(
    RelativeIntensity = factor(
      RelativeIntensity,
      levels = c("High", "Medium", "Low")
    ),
    screening = recode(
      screening,
      baseline_km2 = "Baseline overlap",
      depth_constrained_km2 = "Depth-screened overlap"
    ),
    screening = factor(
      screening,
      levels = names(exposure_colours)
    )
  )

p_absolute <- p_exposure %+% absolute_data

p_absolute <- p_absolute +
  aes(x = area_km2) +
  
  scale_x_continuous(
    breaks = seq(0, 100000, 20000),
    labels = label_comma(),
    limits = c(0, 105000),
    expand = expansion(mult = c(0, 0))
  ) +
  
  labs(
    title = "Absolute spatial overlap by fishing intensity",
    subtitle = paste(
      "Geographic extent of potential giant guitarfish",
      "exposure in the 2024 Northern Prawn Fishery"
    ),
    x = expression("Overlapping area (km"^2*")")
  )

# Replace percentage annotations with area annotations

p_absolute$layers[[2]] <- geom_text(
  aes(label = scales::comma(round(area_km2))),
  position = position_dodge(
    width = 0.76,
    reverse = TRUE
  ),
  hjust = -0.15,
  size = 4.8,
  fontface = "bold",
  colour = "#243746"
)

print(p_absolute)

ggsave(
  here("figures", "13_absolute_overlap_area.png"),
  plot = p_absolute,
  width = 12,
  height = 6.75,
  dpi = 300,
  bg = "white"
)

cat("\nAbsolute overlap figure saved.\n")

# ============================================================
# 11. BATHYMETRIC SCREENING EXCLUSION ANALYSIS
# ============================================================

exclusion_plot_data <- exclusion_audit |>
  summarise(
    `Elevation >= 0 m` = sum(non_submerged_km2),
    `Water deeper than 100 m` = sum(deep_km2)
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "exclusion_reason",
    values_to = "area_km2"
  ) |>
  mutate(
    percentage = 100 * area_km2 / sum(area_km2),
    exclusion_reason = factor(
      exclusion_reason,
      levels = c(
        "Water deeper than 100 m",
        "Elevation >= 0 m"
      )
    ),
    label = sprintf(
      "%s km² (%.2f%%)",
      scales::comma(area_km2, accuracy = 0.01),
      percentage
    )
  )

print(exclusion_plot_data)

total_excluded <- sum(exclusion_plot_data$area_km2)

# ------------------------------------------------------------
# 11.1 Construct exclusion figure
# ------------------------------------------------------------

p_exclusion <- ggplot(
  exclusion_plot_data,
  aes(
    x = percentage,
    y = exclusion_reason,
    fill = exclusion_reason
  )
) +
  
  geom_col(
    width = 0.48
  ) +
  
  geom_text(
    aes(label = label),
    hjust = -0.08,
    size = 4.8,
    fontface = "bold",
    colour = "#243746"
  ) +
  
  scale_fill_manual(
    values = c(
      "Elevation >= 0 m" = "#175676",
      "Water deeper than 100 m" = "#E4B253"
    ),
    guide = "none"
  ) +
  
  scale_x_continuous(
    limits = c(0, 140),
    breaks = seq(0, 100, 20),
    labels = scales::label_percent(scale = 1),
    expand = expansion(mult = c(0, 0))
  ) +
  
  labs(
    title = "What did the bathymetric screen exclude?",
    subtitle = paste0(
      "Total excluded overlap area: ",
      scales::comma(total_excluded, accuracy = 0.01),
      " km²"
    ),
    x = "Proportion of total excluded area (%)",
    y = NULL,
    caption = paste(
      "GEBCO screening retained submerged cells within the documented 0-100 m depth range.",
      "Non-negative elevation classifications may reflect coastal alignment and raster resolution.",
      "Excluded areas should not automatically be interpreted as unsuitable guitarfish habitat.",
      "Source: GEBCO 2026 and spatial overlap analysis.",
      sep = "\n"
    )
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 21,
      colour = "#173247"
    ),
    
    plot.subtitle = element_text(
      colour = "#536572",
      size = 13,
      margin = margin(b = 18)
    ),
    
    axis.text.y = element_text(
      face = "bold",
      size = 12,
      colour = "#243746"
    ),
    
    axis.text.x = element_text(
      size = 11,
      colour = "#536572"
    ),
    
    axis.title.x = element_text(
      size = 12,
      margin = margin(t = 12)
    ),
    
    panel.grid.major.y = element_blank(),
    
    panel.grid.major.x = element_line(
      colour = "#E2E8ED",
      linewidth = 0.5
    ),
    
    panel.grid.minor = element_blank(),
    
    plot.caption = element_text(
      hjust = 0,
      colour = "#647480",
      size = 9.5,
      lineheight = 1.2,
      margin = margin(t = 15)
    ),
    
    plot.margin = margin(20, 25, 15, 15),
    
    plot.background = element_rect(
      fill = "white",
      colour = NA
    )
  )

print(p_exclusion)

# ------------------------------------------------------------
# 11.2 Export exclusion figure
# ------------------------------------------------------------

ggsave(
  here("figures", "13_bathymetric_exclusions.png"),
  plot = p_exclusion,
  width = 12,
  height = 6.75,
  dpi = 300,
  bg = "white"
)

cat("\nBathymetric exclusion figure saved.\n")
