# ============================================================
# GUITARFISH ON THE GRID
# Script 03: NPF fishing intensity visualisation
# ============================================================

library(sf)
library(dplyr)
library(ggplot2)
library(here)

# Load validated 2024 intensity polygons
npf <- st_read(
  here("data", "processed", "npf_rfi_2024_fixed.gpkg"),
  quiet = TRUE
)

# Use Natural Earth coastline data
if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
  install.packages("rnaturalearth")
}

australia <- rnaturalearth::ne_countries(
  country = "Australia",
  scale = "medium",
  returnclass = "sf"
)

# Transform layers to Australian Albers
npf <- st_transform(npf, 3577)
australia <- st_transform(australia, 3577)

# Set map extent from NPF with geographic padding
map_bbox <- st_bbox(
  st_transform(npf, 4326)
)

map_xlim <- c(map_bbox["xmin"] - 1, map_bbox["xmax"] + 1)
map_ylim <- c(map_bbox["ymin"] - 1, map_bbox["ymax"] + 1)

# Create map
p_npf_final <- ggplot() +
  geom_sf(
    data = australia,
    fill = "#E9E6DF",
    colour = "#7A858B",
    linewidth = 0.25
  ) +
  geom_sf(
    data = npf,
    aes(fill = RelativeIntensity),
    colour = NA
  ) +
  scale_fill_manual(
    values = c(
      "Low" = "#6EC9C4",
      "Medium" = "#F3BA62",
      "High" = "#D95A4E"
    ),
    breaks = c("Low", "Medium", "High"),
    labels = c(
      "Low (<0.25)",
      "Medium (0.25–1.0)",
      "High (1.0–2.0)"
    ),
    name = "Fishing effort\n(days/km²)"
  ) +
  coord_sf(
    crs = st_crs(3577),
    xlim = map_xlim,
    ylim = map_ylim,
    default_crs = st_crs(4326),
    expand = FALSE
  ) +
  labs(
    title = "Where is trawling concentrated?",
    subtitle = "Northern Prawn Fishery | Relative fishing intensity, 2024",
    caption = "Source: ABARES | Projection: Australian Albers (EPSG:3577)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 19
    ),
    plot.subtitle = element_text(
      colour = "#56636B"
    ),
    panel.background = element_rect(
      fill = "#EAF4F7",
      colour = NA
    ),
    panel.grid.major = element_line(
      colour = "#FFFFFF",
      linewidth = 0.25
    ),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(
      colour = "#66737B",
      size = 9
    )
  )

print(p_npf_final)

# Save high-resolution figure
ggsave(
  filename = here("figures", "npf_intensity_2024.png"),
  plot = p_npf_final,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)