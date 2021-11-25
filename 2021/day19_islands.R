# Day19: Islands
# Adapted from https://ropenspain.github.io/mapSpain/reference/esp_get_hypsobath.html

library(mapSpain)
library(ggplot2)
library(dplyr)
library(sf)
library(nominatimlite)
library(showtext)

bbox_can <- nominatimlite::bbox_to_poly(
  xmin = -18.5,
  xmax = -13.3,
  ymin = 27.3,
  ymax = 29.5
)

hypsobath <- esp_get_hypsobath()
hypsobath <- hypsobath[!sf::st_is_empty(hypsobath), ]



# Crop

can <- hypsobath %>% st_crop(st_transform(bbox_can, st_crs(hypsobath)))

# Tints from Wikipedia
# https://en.wikipedia.org/wiki/Wikipedia:WikiProject_Maps/Conventions/Topographic_maps

bath_tints <- colorRampPalette(
  rev(
    c(
      "#D8F2FE", "#C6ECFF", "#B9E3FF",
      "#ACDBFB", "#A1D2F7", "#96C9F0",
      "#8DC1EA", "#84B9E3", "#79B2DE",
      "#71ABD8"
    )
  )
)

hyps_tints <- colorRampPalette(
  rev(
    c(
      "#F5F4F2", "#E0DED8", "#CAC3B8", "#BAAE9A",
      "#AC9A7C", "#AA8753", "#B9985A", "#C3A76B",
      "#CAB982", "#D3CA9D", "#DED6A3", "#E8E1B6",
      "#EFEBC0", "#E1E4B5", "#D1D7AB", "#BDCC96",
      "#A8C68F", "#94BF8B", "#ACD0A5"
    )
  )
)

levels <- sort(unique(hypsobath$val_inf))

# Create palette
br_bath <- length(levels[levels < 0])
br_terrain <- length(levels) - br_bath

pal <- c(bath_tints((br_bath - 1)), hyps_tints((br_terrain + 1)))


can$labs <- prettyNum(can$val_inf, big.mark = ".", decimal.mark = ",")

font_add_google("Lobster", "lobster")
showtext_auto()

# Plot Canary Islands
day19 <- ggplot(can) +
  geom_sf(aes(fill = as.factor(val_inf)),
    color = NA
  ) +
  coord_sf(
    xlim = c(-18.6, -13),
    ylim = c(27.3, 29.5),
    expand = FALSE,
  ) +
  scale_fill_manual(
    values = pal,
    labels = function(x) {
      prettyNum(as.numeric(x), big.mark = ".", decimal.mark = ",")
    }
  ) +
  guides(fill = guide_legend(
    title = "Elevation (meters)",
    direction = "horizontal",
    label.position = "bottom",
    title.position = "top",
    keyheight = 1,
    keywidth = 1,
    nrow = 1,
  )) +
  theme(
    text = element_text(family = "lobster", size = 20),
    rect = element_rect(fill = "#d3e6f5", colour = "#d3e6f5"),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold"),
    panel.background = element_rect(fill = "#d3e6f5"),
    plot.subtitle = element_text(face = "italic"),
    plot.caption = element_text(
      face = "italic",
      hjust = 0
    ),
    legend.position = "bottom",
    legend.margin = margin(t = 0, b = 20),
    legend.text = element_text(angle = 20)
  ) +
  labs(
    title = "Canary Islands (Spain)",
    subtitle = "Fortunate Isles",
    caption = paste0(
      "Source: Atlas Nacional de España (ANE) CC BY 4.0 http://www.ign.es",
      "\nCreated with https://CRAN.R-project.org/package=mapSpain",
      "\n@dieghernan"
    )
  )

ggsave("2021/day19_islands.png", width = 7, height = 4.6)

ggs
