## figures for 2026 ICBEN presentation
library(tidyverse)
library(ggplot2)

## color scheme:
# control: #00BFC4
# case: #F8766D

## figure 1: case and control zones
# done in QGIS, but the files are here:
# CRZ geofence from:  https://data.ny.gov/Transportation/MTA-Central-Business-District-Geofence-Beginning-J/srxy-5nxn/about_data
crz <- read_sf("/Users/ni2267/Documents/BEH/CRZ/CRZ_311/data/MTA Central Business District Geofence_ Beginning June 2024_20250619/geo_export_d6c2fec2-3927-49c2-98c0-6c2b3edffa39.shp")
crz <- st_transform(crz, crs = "WGS84")

# ctrl area created in QGIS
ctrl <- read_sf("/Users/ni2267/Documents/BEH/CRZ/CRZ_311/data/crz_control/crz_control.shp")
ctrl <- st_transform(ctrl, crs = "WGS84")
  
## figure 2: DiD example illustration 
# mock data
did_df <- tibble(
  time = rep(1:6, 2),
  group = rep(c("Control", "Treated"), each = 6),
  outcome = c(
    # Control
    10, 10.5, 11, 11.5, 12, 12.5,
    # Treated
    9, 9.5, 10, 10.1, 10.2, 10.3
  )
)

counterfactual <- tibble(
  time = c(3, 4, 5, 6),
  outcome = c(10, 10.5, 11, 11.5)
)

# Common plotting elements
cols <- c(
  "Control" = "#00BFC4",
  "Treated" = "#F8766D"
)

base_theme <- theme_minimal(base_size = 16) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

base_plot <- ggplot() +
  scale_color_manual(values = cols) +
  scale_x_continuous(
    breaks = 1:6,
    labels = c(
      "Pre 1",
      "Pre 2",
      "Pre 3",
      "Post 1",
      "Post 2",
      "Post 3"
    )
  ) +
  coord_cartesian(
    xlim = c(1, 6),
    ylim = c(8.5, 13)
  ) +
  labs(
    x = "Time",
    y = "Outcome",
    color = NULL
  ) +
  base_theme

# part 1: pre-intervention only 
p1 <- base_plot +
  geom_line(
    data = filter(did_df, time <= 3),
    aes(time, outcome, color = group),
    linewidth = 1.2
  ) +
  geom_point(
    data = filter(did_df, time <= 3),
    aes(time, outcome, color = group),
    size = 3
  )

ggsave(
  "/Users/ni2267/Documents/Columbia/Conferences/ICBEN_2026/presentation/did1.png",
  p1,
  width = 7,
  height = 5,
  dpi = 300)


# part 2: + intervention line 
p2 <- p1 +
  geom_vline(
    xintercept = 3.5,
    linetype = "dashed",
    linewidth = 1
  ) 

ggsave(
  "/Users/ni2267/Documents/Columbia/Conferences/ICBEN_2026/presentation/did2.png",
  p2,
  width = 7,
  height = 5,
  dpi = 300
)

# part 3: post-intervention control data 
p3 <- base_plot +
  geom_line(
    data = filter(did_df, time <= 3),
    aes(time, outcome, color = group),
    linewidth = 1.2
  ) +
  
  geom_point(
    data = filter(did_df, time <= 3),
    aes(time, outcome, color = group),
    size = 3
  ) +
  
  # Post-period control only
  geom_line(
    data = filter(did_df, group == "Control"),
    aes(time, outcome, color = group),
    linewidth = 1.2
  ) +
  
  geom_point(
    data = filter(did_df, group == "Control"),
    aes(time, outcome, color = group),
    size = 3
  ) +
  
  geom_vline(
    xintercept = 3.5,
    linetype = "dashed",
    linewidth = 1
  )

ggsave(
  "/Users/ni2267/Documents/Columbia/Conferences/ICBEN_2026/presentation/did3.png",
  p3,
  width = 7,
  height = 5,
  dpi = 300
)

# part 4: counterfactual line 
p4 <- p3 +
  
  geom_line(
    data = counterfactual,
    aes(time, outcome),
    inherit.aes = FALSE,
    color = "#F8766D",
    linetype = "dotted",
    linewidth = 1.2
  ) 

ggsave(
  "/Users/ni2267/Documents/Columbia/Conferences/ICBEN_2026/presentation/did4.png",
  p4,
  width = 7,
  height = 5,
  dpi = 300
)

# part 5: full did 
p5 <- base_plot +
  
  geom_line(
    data = did_df,
    aes(time, outcome, color = group),
    linewidth = 1.2
  ) +
  
  geom_point(
    data = did_df,
    aes(time, outcome, color = group),
    size = 3
  ) +
  
  geom_vline(
    xintercept = 3.5,
    linetype = "dashed",
    linewidth = 1
  ) +
  
  geom_line(
    data = counterfactual,
    aes(time, outcome),
    inherit.aes = FALSE,
    color = "#F8766D",
    linetype = "dotted",
    linewidth = 1.2
  ) 

ggsave(
  "/Users/ni2267/Documents/Columbia/Conferences/ICBEN_2026/presentation/did5.png",
  p5,
  width = 7,
  height = 5,
  dpi = 300
)

## fig 3: the plot of the main findings
# (did2 plot from main code)
