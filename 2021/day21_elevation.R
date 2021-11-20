library(mapSpain)
library(sf)
library(elevatr)
library(rayshader)
library(raster)
library(dplyr)
library(ggplot2)
library(nominatimlite)
library(sysfonts)

sysfonts::font_add("pacifico",
  regular = "2021/Pacifico-Regular.ttf"
)

zoom <- 12

lava <- nominatimlite::geo_address_lookup_sf("13249829", type = "R", points_only = FALSE)

# Get La Palma bbox----

palma <- esp_get_nuts(region = "ES707", moveCAN = FALSE, epsg = 4326)


square_bbox <- function(x, expand = .1) {
  bbx <- st_bbox(st_transform(x, 3857))
  xtick <- bbx[1] + (bbx[3] - bbx[1]) / 2
  ytick <- bbx[2] + (bbx[4] - bbx[2]) / 2

  x_dim <- (bbx[3] - bbx[1])
  y_dim <- (bbx[4] - bbx[2])


  max_dim <- (max(x_dim, y_dim) / 2) * (1 + expand)

  square <-
    c(xtick - max_dim, ytick - max_dim, xtick + max_dim, ytick + max_dim)
  names(square) <- names(bbx)
  class(square) <- "bbox"


  bbx_end <- st_as_sfc(square)
  bbx_end <- st_set_crs(bbx_end, 3857)
  bbx_end <- st_transform(bbx_end, st_crs(x))

  return(bbx_end)
}


# Get square around munic
square <- square_bbox(palma)

cent <- st_coordinates(square)


sq <- st_sf(d = 1, square) %>% as_Spatial()

# Get DEM ----

DEM <- get_elev_raster(sq,
  z = zoom, clip = "bbox",
  override_size_check = TRUE
) %>%
  crop(extent(sq))


minR <- min(dim(DEM)[1:2])
maxR <- max(dim(DEM)[1:2])
dim(DEM)
# Preserve space
if (maxR > 1000) {
  DEM <- aggregate(DEM, fact = max(2, round(maxR / 1000)))
}
dim(DEM)
# Assign min to -10 - It's sea on this DEM provider
DEM[is.na(DEM)] <- -10
DEM[values(DEM) < -10] <- -10

overlay_raster <- esp_getTiles(st_sf(d = 1, square), type = "PNOA", zoom = zoom, verbose = TRUE)


tmppng <- tempfile(fileext = ".png")

DEM <- crop(DEM, raster(overlay_raster))


# Plot and save layer as png ----

# Convert to png
png(tmppng,
  height = dim(overlay_raster)[1],
  width = dim(overlay_raster)[2]
)
par(mar = c(0, 0, 0, 0))
plotRGB(overlay_raster)
plot(st_geometry(lava), add = TRUE, col=adjustcolor("red", alpha.f =
                                                      0.7),
     border = "red2", lwd=2)
dev.off()

img_overlay <- png::readPNG(tmppng)



# Parameters for rayshader and movie ----

# Correction to zscale
fact <-
  (max(values(DEM), na.rm = TRUE) - min(values(DEM), na.rm = TRUE)) / 100


# mp4 config

n_frames <- 360*2

transition_values <- function(from,
                              to,
                              steps = 10,
                              one_way = FALSE,
                              type = "cos") {
  if (!(type %in% c("cos", "lin"))) {
    stop("type must be one of: 'cos', 'lin'")
  }

  range <- c(from, to)
  middle <- mean(range)
  half_width <- diff(range) / 2

  # define scaling vector starting at 1 (between 1 to -1)
  if (type == "cos") {
    scaling <-
      cos(seq(0, 2 * pi / ifelse(one_way, 2, 1), length.out = steps))
  } else if (type == "lin") {
    if (one_way) {
      xout <- seq(1, -1, length.out = steps)
    } else {
      xout <- c(
        seq(1, -1, length.out = floor(steps / 2)),
        seq(-1, 1, length.out = ceiling(steps / 2))
      )
    }
    scaling <- approx(
      x = c(-1, 1),
      y = c(-1, 1),
      xout = xout
    )$y
  }

  middle - half_width * scaling
}

theta_val <-
  transition_values(
    from = 0,
    to = (360),
    steps = n_frames,
    one_way = TRUE,
    type = "lin"
  )
theta_val <- ifelse(theta_val > 360, theta_val - 360, theta_val)


phi_val1 <-
  transition_values(
    from = 90,
    to = 10,
    steps = (n_frames / 3),
    one_way = TRUE,
    type = "cos"
  )

phi_val <- c(phi_val1, rep(10, n_frames / 3), rev(phi_val1))

zoom_val <- transition_values(
  from = .9,
  to = .2,
  steps = 180*2,
  one_way = TRUE,
  type = "lin"
)
zoom_val
zoom_val <- c(zoom_val, rep(.2, 120*2),
              transition_values(
                from = .2,
                to = .9,
                steps = 60*2,
                one_way = TRUE,
                type = "lin"
              ))

# Rayshade! ----
sub <- "Canary Islands, Spain"
title <- "Cumbre Vieja volcanic eruption\nLava flow"

foot <- paste0(
  "Infraestructura de Datos Espaciales de Espa\u00f1a (IDEE)"
)


# Rayshade 3D with png overlay

DEM_mat <- raster_to_matrix(DEM)



rgl::rgl.close()


DEM_mat %>%
  sphere_shade(texture = "desert") %>%
  add_overlay(img_overlay) %>%
  plot_3d(DEM_mat, zscale = 1 + fact / 1, baseshape = "circle",
          water = FALSE)


# Render mp4
render_movie(
  "2021/day21_elevation.mp4",
  title_text = title,
  title_position = "north",
  title_size = 16,
  title_font = "pacifico",
  type = "custom",
  frames = n_frames,
  fps = 30,
  phi = phi_val,
  zoom = zoom_val,
  theta = theta_val,
  progbar = TRUE
)

render_m
rgl::rgl.close()
