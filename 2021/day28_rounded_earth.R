library(sf)
library(s2)
library(dplyr)
library(giscoR)
library(nominatimlite)
library(geosphere)
library(ggplot2)
library(ggrepel)

# Dev version
remotes::install_github("paleolimbot/ggspatial")
library(ggspatial)



# Get flights ----
airports <- geo_lite_sf(c(
  "London Airport",
  "Singapore Airport",
  "Auckland Airport",
  "Nadi Airport",
  "Los Angeles Airport"
))

dist <- st_distance(airports[1:4, ], airports[2:5, ], by_element = TRUE, which = "Great Circle") %>%
  units::set_units("km")


all <- sum(dist)
dist

porc <- c(0, 100 * cumsum(units::drop_units(dist)) / units::drop_units(all)) %>%
  round(0.1) / 100

air1 <- airports[1:4, ] %>% as("Spatial")
air2 <- airports[2:5, ] %>% as("Spatial")


lines <- gcIntermediate(air1, air2,
                        sp = TRUE,
                        addStartEnd = TRUE, n = 100,
                        breakAtDateLine = FALSE
)

lines_sf <- st_union(st_as_sf(lines))




# Get coastallines and Antartica ----
# Need to add background and shape of Antartica since the geotiff
# doesn't cover completly the south pole
g <- as_s2_geography(TRUE)
coast <- st_as_s2(gisco_get_coastallines(epsg = 4326, resolution = 3))
ant <- st_as_s2(gisco_get_countries(country = "Antarctica", epsg = 4326, resolution = 3))
grat <- st_graticule(ndiscr = 300)

# Dateline

dateline <- st_graticule(
  x = c(-180, -90, -180, 90),
  crs = st_crs(4326), ndiscr = 300
)


# Background
b <- s2_buffer_cells(as_s2_geography("POINT(150 -10)"), 9800000) # visible half
coast_v <- s2_intersection(b, coast) # visible coastallines
ant_v <- s2_intersection(b, ant)
grat_v <- s2_intersection(b, grat)
dateline_v <- s2_intersection(b, dateline)


# Proj
proj4 <- "+proj=ortho +lat_0=-10 +lon_0=150"

coast_end <- coast_v %>%
  st_as_sfc() %>%
  st_transform(proj4)
lines_end <- lines_sf %>% st_transform(proj4)
b_end <- b %>%
  st_as_sfc() %>%
  st_transform(proj4)
ant_end <- ant_v %>%
  st_as_sfc() %>%
  st_transform(proj4)
labs_end <- airports %>% st_transform(proj4)
grat_end <- grat_v %>%
  st_as_sfc() %>%
  st_transform(proj4)

dateline_end <- dateline_v %>%
  st_as_sfc() %>%
  st_transform(proj4)



# Select only lines from graticule
grat_end <- grat_end[!st_is_empty(grat_end)]



l <- st_sfc(
  st_linestring(rbind(c(180, 90), c(180, -90))),
  crs = st_crs(proj4)
) %>%
  st_transform(3857) %>%
  st_segmentize(100) %>%
  st_transform(4326)



# Tile

library(terra)
# download.file("https://eoimages.gsfc.nasa.gov/images/imagerecords/144000/144898/BlackMarble_2016_01deg_geo.tif",
#               "2021/BlackMarble_2016_01deg_geo.tif",
#               mode = "wb")

tile <- rast("2021/BlackMarble_2016_01deg_geo.tif")
tile_end <- project(tile, st_crs(proj4)$proj4string)


# Get labels of airports
points <- lines_end %>% st_cast("POINT")
heatrow <- points[1]
la <- points[length(points)]
rest <- st_geometry(labs_end)[2:4]
labs_end <- c(heatrow, rest, la)

labs_end <- st_sf(st_drop_geometry(airports), labs_end)
labs_end <- labs_end %>% mutate(cities = trimws(gsub("Airport", "", query)))
labs_end$IATA <- c("LHR", "SIN", "AKL", "NAN", "LAX")


# Create line for legend, this takes some work
bbox_marble <- st_bbox(b_end)

scale <- st_sfc(
  st_linestring(rbind(
    c(bbox_marble[1], -bbox_marble[3] * 1.3),
    c(bbox_marble[3], -bbox_marble[3] * 1.3)
  )),
  crs = st_crs(proj4)
)


scale_points <- st_sample(scale, 100, type = "regular") %>%
  st_cast("POINT")

scale_point_labs <- scale_points[pmax(1, porc * 100)]
scale_point_labs_df <- cbind(st_drop_geometry(labs_end), st_coordinates(scale_point_labs))



scale_point_labs_df$dist <- paste(
  c(0, formatC(cumsum(units::drop_units(dist)), big.mark = ",", digits = 5)),
  "km."
)


scale_df <- data.frame(
  x = bbox_marble[1],
  xend = bbox_marble[3],
  y = -bbox_marble[3] * 1.3,
  yend = -bbox_marble[3] * 1.3
)

# Label dateline
lab_dateline <- st_coordinates(dateline_end) %>%
  as.data.frame() %>%
  arrange(desc(Y))
lab_dateline_end <- lab_dateline[as.integer(nrow(lab_dateline) * 0.27), ]
lab_dateline_end$text <- "International Date Line"



# Plot----

library(showtext)

font_add("skyfont", regular = "2021/Skyfont-NonCommercial.ttf")
font_add("freesans",
         regular = "2021/FreeSans.ttf",
         bold = "2021/FreeSansBold.ttf"
)


showtext_auto()


p <- ggplot(b_end) +
  # Legends
  geom_segment(
    data = scale_df, color = "#0033ff", alpha = .5, size = 4,
    aes(x = x, y = y, yend = yend, xend = xend), lineend = "round"
  ) +
  geom_point(data = scale_point_labs_df, aes(x = X, y = Y), color = "#0033ff", shape = 21, fill = "#ccd6ff", size = 5, alpha = 0.4) +
  geom_text(
    data = scale_point_labs_df, aes(
      x = X, y = Y, label = IATA,
      family = "skyfont",
      hjust = 0.5
    ),
    size = 23,
    nudge_y = 450000,
    color = "white"
  ) +
  geom_text_repel(
    data = scale_point_labs_df, aes(X, Y * 1.07,
                                    label = dist,
                                    segment.color = "transparent"
    ),
    size = 10, color = "white",
    direction = "y",
    seed = 3
  ) +
  # Background
  geom_sf(fill = "#05050f", col = "grey20", size = 1) +
  geom_sf(data = ant_end, fill = "#2a3354", col = NA) +
  # Tile
  layer_spatial(tile_end) +
  #  geom_sf(data=coast_end, fill=NA, col=adjustcolor("grey90", alpha.f = 0.5)) +
  # Graticules
  geom_sf(data = grat_end, color = "white", alpha = 0.1, size = 0.3) +
  geom_sf(data = dateline, color = "yellow", alpha = 0.3, size = 1, linetype = "dotted") +
  # Flighs
  geom_sf(data = lines_end, color = "#0033ff", alpha = .5, size = 4) +
  geom_sf(data = labs_end, color = "#0033ff", shape = 21, fill = "#ccd6ff", size = 5, alpha = 0.4) +
  geom_sf_text(
    data = labs_end, aes(label = cities, family = "skyfont"), size = 20,
    color = "white",
    nudge_y = 600000
  ) +
  geom_text(
    data = lab_dateline_end, aes(X, Y, label = text),
    color = "yellow", size = 9, alpha = 0.5,
    hjust = 1,
    nudge_x = -100000,
    angle = -2
  ) +
  theme_void() +
  theme(
    text = element_text(
      family = "freesans", colour = "white",
      size = 30
    ),
    plot.title = element_text(
      size = 100,
      hjust = .5
    ),
    plot.subtitle = element_text(
      size = 80,
      hjust = .5,
      margin = margin(b = 5)
    ),
    plot.background = element_rect(fill = "black")
  ) +
  labs(
    title = "Flight Plan: London-Auckland-L.A",
    subtitle = "Stops: Singapore-Fiyi",
    caption = "My honeymoon destinations"
  )



ggsave("2021/day28_rounded_earth.png", p,
       height = 9, width = 8, dpi = 300, bg = "black"
)
