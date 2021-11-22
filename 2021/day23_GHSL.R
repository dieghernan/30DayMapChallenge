remotes::install_github("paleolimbot/ggspatial")

library(raster)
library(terra)
library(sf)
library(tidyverse)
library(plotwidgets)
library(ggspatial)
library(giscoR)
library(showtext)

# Heavily based on https://dominicroye.github.io/en/2021/firefly-cartography/

GHSL1 <-
  raster("~/R/mapslib/30dmc/GHS_SMOD_POP2015_GLOBE_R2019A_54009_1K_V2_0_17_3.tif")

GHSL2 <-
  raster("~/R/mapslib/30dmc/GHS_SMOD_POP2015_GLOBE_R2019A_54009_1K_V2_0_17_4.tif")

GHSL3 <-
  raster("~/R/mapslib/30dmc/GHS_SMOD_POP2015_GLOBE_R2019A_54009_1K_V2_0_18_3.tif")

GHSL4 <-
  raster("~/R/mapslib/30dmc/GHS_SMOD_POP2015_GLOBE_R2019A_54009_1K_V2_0_18_4.tif")

mergeall <- merge(GHSL1, GHSL2, GHSL3, GHSL4)


# Countries
cntries <- gisco_get_countries(
  region = c("Europe", "Africa"),
  resolution = 1
) %>%
  st_transform(st_crs(mergeall)) %>%
  st_crop(mergeall)

# Tiles
tiles <-
  maptiles::get_tiles(cntries,
    "Esri.WorldImagery",
    crop = TRUE,
    zoom = 6,
    verbose = TRUE
  )

plotRGB(tiles)



# function to change saturation from RGB
saturation <- function(rgb, s = .5, l = .1) {
  hsl <- rgb2hsl(as.matrix(rgb))
  hsl[2, ] <- s
  hsl[3, ] <- hsl[3, ] * (1 - l)

  rgb_new <- as.vector(t(hsl2rgb(hsl)))

  return(rgb_new)
}


# Desaturate tile
bm_desat <- app(tiles, saturation, s = .05, l = .3)

plotRGB(bm_desat)

# Manipulate raster and convert to polygons
newrast <- mergeall
newrast[values(newrast) < 23] <- NA
tosf <- rasterToPolygons(newrast) %>% st_as_sf()

buff <-
  tosf %>%
  st_geometry() %>%
  st_buffer(dist = tosf$layer * 200) %>%
  st_union()

# Final plot
font_add_google("Oxygen", "oxygen")
showtext_auto()

bm_desat_crop <- crop(bm_desat, mergeall)

new <- ggplot(cntries) +
  annotation_spatial(bm_desat_crop) +
  geom_sf(fill = NA, col = "white", size = 0.1) +
  geom_sf(
    data = buff,
    col = adjustcolor("#6bb857", alpha.f = 0.7),
    fill = "#eff7ed",
    alpha = 0.9,
    size = 0.5
  ) +
  theme_void() +
  coord_sf(ylim = c(4300000.0, 6000000.0)) +
  labs(
    title = "Degree of Urbanization in Western Europe",
    subtitle = "GHS-SMOD (2016), Dense Urban and Urban Centre grid cells",
    caption = "github/dieghernan, based in @dr_xeo https://dominicroye.github.io/en/2021/firefly-cartography/"
  ) +
  theme(
    text = element_text(colour = "white", family = "oxygen"),
    plot.title = element_text(size = 60),
    plot.subtitle = element_text(size = 35),
    plot.caption = element_text(size = 20)
  )


new
ggsave(
  "2021/day23_ghsl.png",
  new,
  width = 7,
  height = 7,
  units = "in",
  dpi = 300,
  bg = "black"
)
