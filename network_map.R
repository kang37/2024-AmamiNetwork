# Directed mobility networks over the outline of Amami Oshima ----
#
# This script uses the same node and weighted edge tables exported for Gephi.
# Reciprocal edges bend to opposite sides: each edge bends to the right of its
# travel direction, giving a clockwise curve. Self-loops are omitted because
# they do not represent movements between different locations.

library(dplyr)
library(ggplot2)
library(purrr)
library(sf)
library(targets)
library(tidyr)

tar_load(amami)

# Keep only the largest polygon, i.e. the main island of Amami Oshima.
amami_outline <- amami %>%
  st_make_valid() %>%
  st_cast("POLYGON") %>%
  mutate(.area = st_area(.)) %>%
  slice_max(.area, n = 1, with_ties = FALSE) %>%
  select(-.area) %>%
  st_transform(4326)

read_network_panel <- function(user_group, quarter_x) {
  nodes <- read.csv(sprintf(
    "data_proc/od_node_%s_%s.csv", user_group, quarter_x
  ))
  edges <- read.csv(sprintf(
    "data_proc/od_edge_%s_%s.csv", user_group, quarter_x
  ))

  edges %>%
    filter(Source != Target, Weight > 0) %>%
    left_join(
      nodes %>% select(Source = Id, x_start = Longitude, y_start = Latitude),
      by = "Source"
    ) %>%
    left_join(
      nodes %>% select(Target = Id, x_end = Longitude, y_end = Latitude),
      by = "Target"
    ) %>%
    filter(if_all(c(x_start, y_start, x_end, y_end), ~ !is.na(.x))) %>%
    mutate(
      user = factor(
        user_group,
        levels = c("local", "tourist"),
        labels = c("Residents", "Tourists")
      ),
      quarter = factor(
        quarter_x, levels = 1:4,
        labels = paste("Quarter", 1:4)
      ),
      edge_id = paste(user_group, quarter_x, Source, Target, sep = "_")
    )
}

network_edges <- crossing(
  user_group = c("local", "tourist"),
  quarter_x = 1:4
) %>%
  pmap_dfr(read_network_panel)

network_nodes <- crossing(
  user_group = c("local", "tourist"),
  quarter_x = 1:4
) %>%
  pmap_dfr(function(user_group, quarter_x) {
    read.csv(sprintf(
      "data_proc/od_node_%s_%s.csv", user_group, quarter_x
    )) %>%
      transmute(
        user = factor(
          user_group,
          levels = c("local", "tourist"),
          labels = c("Residents", "Tourists")
        ),
        quarter = factor(
          quarter_x, levels = 1:4,
          labels = paste("Quarter", 1:4)
        ),
        Id,
        Longitude,
        Latitude
      )
  })

# Construct quadratic Bezier curves. The clockwise perpendicular to the
# source-target vector is (dy, -dx), so positive curve_strength bends every
# directed edge to the right of its direction of travel. The last point stops
# just short of the target so that the arrowhead remains visible beside it.
curve_strength <- 0.18
curve_steps <- 18
curve_t <- seq(0, 0.94, length.out = curve_steps)

network_curves <- network_edges %>%
  mutate(
    dx = x_end - x_start,
    dy = y_end - y_start,
    control_x = (x_start + x_end) / 2 + curve_strength * dy,
    control_y = (y_start + y_end) / 2 - curve_strength * dx
  ) %>%
  crossing(curve_order = seq_along(curve_t)) %>%
  mutate(
    t = curve_t[curve_order],
    Longitude = (1 - t)^2 * x_start +
      2 * (1 - t) * t * control_x + t^2 * x_end,
    Latitude = (1 - t)^2 * y_start +
      2 * (1 - t) * t * control_y + t^2 * y_end
  ) %>%
  arrange(edge_id, curve_order)

network_map <- ggplot() +
  geom_sf(
    data = amami_outline,
    fill = NA, color = "grey55", linewidth = 0.35
  ) +
  geom_path(
    data = network_curves,
    aes(
      x = Longitude, y = Latitude,
      group = edge_id,
      linewidth = Weight, color = Weight, alpha = Weight
    ),
    lineend = "round",
    arrow = grid::arrow(
      angle = 22, length = grid::unit(0.9, "mm"), type = "closed"
    )
  ) +
  geom_point(
    data = network_nodes,
    aes(x = Longitude, y = Latitude),
    shape = 21, size = 0.75, stroke = 0.2,
    fill = "white", color = "black"
  ) +
  facet_grid(user ~ quarter, switch = "y") +
  scale_linewidth_continuous(
    trans = scales::pseudo_log_trans(sigma = 1),
    range = c(0.03, 3.4),
    breaks = c(1, 10, 100, 1000, 5000),
    name = "Movements"
  ) +
  scale_color_gradient(
    low = "#DCEAF2", high = "#08306B",
    trans = scales::pseudo_log_trans(sigma = 1),
    breaks = c(1, 10, 100, 1000, 5000),
    guide = "none"
  ) +
  scale_alpha_continuous(
    trans = scales::pseudo_log_trans(sigma = 1),
    range = c(0.06, 0.95),
    breaks = c(1, 10, 100, 1000, 5000),
    guide = "none"
  ) +
  coord_sf(
    xlim = c(129.05, 129.76), ylim = c(28.08, 28.55),
    expand = FALSE, datum = NA
  ) +
  guides(
    linewidth = guide_legend(
      title.position = "top", override.aes = list(alpha = 0.75)
    )
  ) +
  theme_void(base_size = 16) +
  theme(
    strip.placement = "outside",
    strip.background = element_rect(
      fill = "grey92", color = "grey45", linewidth = 0.4
    ),
    strip.text.x = element_text(size = 16, margin = margin(5, 5, 5, 5)),
    strip.text.y.left = element_text(
      angle = 90, size = 16, margin = margin(5, 5, 5, 5)
    ),
    panel.spacing = grid::unit(0.8, "lines"),
    legend.position = "bottom",
    legend.key.width = grid::unit(1.6, "cm"),
    plot.margin = margin(8, 8, 8, 8)
  )

ggsave(
  filename = paste0(
    "data_proc/mobility_network_map_combined_", Sys.Date(), ".png"
  ),
  plot = network_map,
  width = 15, height = 8.5, units = "in", dpi = 300,
  bg = "white"
)

print(network_map)
