library(sf)
library(dplyr)
library(giscoR)
library(mapSpain)
library(ggplot2)

madrid <- esp_get_munic(region = "Madrid") %>% st_transform(3857)

# From GISCO Eurostat
hosp_eur <- gisco_get_healthcare(country = "ES") %>% st_transform(3857)
hosp_mad <- st_intersection(hosp_eur, st_union(madrid)) %>%
  filter(facility_type == "GENERAL")



# Create grid
grid_init <- st_make_grid(madrid, cellsize = 900)

grid_int <- st_intersects(grid_init, st_union(madrid), sparse = FALSE)
grid <- grid_init[grid_int]


# Min distance

mindist <- st_distance(grid, hosp_mad) %>%
  apply(1, min)

grid_sf <- st_sf(
  dist = mindist / 1000,
  grid
)


grid_sf_crop <- st_intersection(grid_sf, st_union(madrid))

library(showtext)

font_add_google("Arvo", "arvo")


showtext_auto()

p <- ggplot() +
  geom_sf(data = madrid, fill = NA, color = "grey10") +
  geom_sf(data = grid_sf_crop, aes(fill = dist), color = NA, size = 0.01) +
  geom_sf(data = hosp_mad, size = 1, aes(color = "Hospital"), alpha = 0.5) +
  scale_fill_gradientn(
    colors = hcl.colors(50, "Inferno", rev = TRUE, alpha = 0.8),
    labels = scales::label_comma(suffix = " km."),
    guide = guide_colourbar(barheight = 10)
  ) +
  scale_color_manual(values = "#e62e3a") +
  theme_void() +
  theme(
    text = element_text(family = "arvo"),
    plot.title = element_text(size = 75, face = "bold"),
    plot.subtitle = element_text(size = 60),
    plot.caption = element_text(size = 40),
    plot.margin = margin(20, 20, 20, 20),
    legend.text = element_text(size = 50),
    legend.title = element_text(size = 60)
  ) +
  labs(
    title = "Comunidad de Madrid, Spain",
    subtitle = "Linear distance to the closest hospital",
    fill = "Distance",
    color = "",
    caption = "Source: © European Union, 1995 - today, GISCO Healthcare Services"
  )


ggsave("2021/day27_heatmap.png", p,
  height = 8, width = 8, dpi = 300,
  bg = "white"
)

