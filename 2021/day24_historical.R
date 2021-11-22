# https://dominicroye.github.io/es/2019/visualizar-el-crecimiento-urbano/
devtools::install_github("datawookie/feedeR")


library(feedeR)
library(sf)
library(fs)
library(tidyverse)
library(lubridate)
library(classInt)
library(ggplot2)
library(rvest)
library(tidygeocoder)

url <-
  "http://www.catastro.minhap.es/INSPIRE/buildings/ES.SDGC.bu.atom.xml"

# importamos los RSS con enlaces de provincias
prov_enlaces <- feed.extract(url)
str(prov_enlaces) # estructura es lista

# extraemos la tabla con los enlaces
prov_enlaces_tab <- as_tibble(prov_enlaces$items) %>%
  mutate(title = repair_encoding(title))


# filtramos la provincia y obtenemos la url RSS
val_atom <-
  filter(prov_enlaces_tab, str_detect(title, "Segovia")) %>% pull(link)


# importamos la RSS
val_enlaces <- feed.extract(val_atom)

# obtenemos la tabla con los enlaces de descarga
val_enlaces_tab <- val_enlaces$items
val_enlaces_tab <- mutate(val_enlaces_tab,
  title = repair_encoding(title),
  link = repair_encoding(link)
)


# filtramos la tabla con el nombre de la ciudad
val_link <-
  filter(val_enlaces_tab, str_detect(title, "40900")) %>% pull(link)
val_link


# creamos un archivo temporal
temp <- tempfile()

# descargamos los datos
download.file(URLencode(val_link), temp)

# descomprimimos a una carpeta llamda buildings
unzip(temp, exdir = "buildings_old")

# obtenemos la ruta con el archivo
file_val <- dir_ls("buildings_old", regexp = "40900.building.gml")

# importamos los datos
buildings_val <- st_read(file_val)


buildings_val <- mutate(buildings_val,
  beginning = str_replace(beginning, "^-", "0000") %>%
    ymd_hms() %>% as_date()
)

bbox <- nominatimlite::bbox_to_poly(
  xmin =
    -4.13604,
  ymin = 40.9463154175,
  xmax = -4.1142047255,
  ymax = 40.9535838565
) %>% st_transform(st_crs(buildings_val))

crop <- st_crop(buildings_val, bbox) %>% filter(beginning < as.Date("1960-01-01"))

library(showtext)
font_add(
  family = "ibarra", regular = "./2021/IbarraRealNova-Regular.ttf",
  bold = "./2021/IbarraRealNova-Bold.ttf",
  italic = "./2021/IbarraRealNova-Italic.ttf",
  bolditalic = "./2021/IbarraRealNova-BoldItalic.ttf"
)
showtext_auto()

library(ggspatial)

ggplot(crop) +
  geom_sf(fill = "cornsilk", color = "#887e6a") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "#fffff3"),
    panel.border = element_rect(
      colour = "#887e6a",
      fill = NA,
      size = 1.5
    ),
    text = element_text(
      family = "ibarra",
      colour = "#887e6a",
      face = "bold"
    ),
    plot.title = element_text(size = 120, hjust = 0.5),
    plot.subtitle = element_text(
      size = 100, hjust = .5,
      margin = margin(b = 20)
    ),
    plot.margin = margin(30, 30, 30, 30)
  ) +
  labs(
    title = "City of Segovia",
    subtitle = "1960"
  ) +
  annotation_north_arrow(
    style = north_arrow_nautical(
      fill = c(
        "#887e6a",
        "#fffff3"
      ),
      line_col = "#887e6a",
      text_size = 40,
      text_col = "#887e6a",
      text_family = "serif"
    ), which_north = TRUE, height = unit(3.5, "cm"),
    width = unit(3.5, "cm")
  )

ggsave("2021/day24_historical.png", width = 10, height = 7, dpi = 300, bg = "#fffff3")
