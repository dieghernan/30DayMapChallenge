library(mapSpain)
library(sf)
library(dplyr)
library(ggplot2)

# Filter
del <- esp_dict_region_code(c("Canarias", "Ceuta", "Melilla", "Baleares"), destination = "codauto")


# Get provs

prov <- esp_get_prov() %>%
  st_transform(3857) %>%
  filter(!codauto %in% del)


# Create Voronois
cents <- prov %>% st_centroid(of_largest_polygon = TRUE)
vor <- cents %>%
  st_union() %>%
  st_voronoi() %>%
  st_collection_extract("POLYGON") %>%
  st_sf(id = 1)


ggplot(vor) +
  geom_sf()


# Asign new CCAA
newccaa <- vor %>%
  st_join(cents) %>%
  mutate(
    newccaa = codauto,
    newpro = cpro
  ) %>%
  select(newccaa, newpro)

ggplot(newccaa) +
  geom_sf(aes(fill = newpro))

# Reclassifly munics
munics_pol <- esp_get_munic_siane() %>%
  st_transform(3857) %>%
  filter(!codauto %in% del)

oldccaa <- munics_pol %>%
  group_by(
    codauto, cpro
  ) %>%
  summarise(n = n())

ggplot(oldccaa) +
  geom_sf(aes(fill = codauto))

munics_cent <- munics_pol %>%
  st_centroid() %>%
  st_cast("POINT")

munics_new <- st_join(munics_pol, newccaa, largest = TRUE) %>%
  select(LAU_CODE, newccaa, newpro) %>%
  st_drop_geometry() %>%
  unique()

# Get final map

data("esp_codelist")

munics_end <- munics_pol %>%
  st_transform(3857) %>%
  left_join(munics_new) %>%
  mutate(newcode = coalesce(newccaa, codauto)) %>%
  left_join(esp_codelist %>% select(ccaa.shortname.es, codauto), by = c("newcode" = "codauto"))

newccaa_pol <- munics_end %>%
  select(newcode, ccaa.shortname.es, newpro) %>%
  group_by(newcode, ccaa.shortname.es, newpro) %>%
  summarise(nmuns = n()) %>%
  filter(!newcode %in% del) %>%
  st_cast("MULTIPOLYGON")

noholes <- sfheaders::sf_remove_holes(newccaa_pol)



noholes$type <- "New"
oldccaa$type <- "Current"

oldccaa <- oldccaa %>%
  left_join(esp_codelist %>% select(ccaa.shortname.es, codauto))

all <- oldccaa %>% bind_rows(noholes)

library(showtext)
font_add(
  family = "ibarra", regular = "./2021/IbarraRealNova-Regular.ttf",
  bold = "./2021/IbarraRealNova-Bold.ttf",
  italic = "./2021/IbarraRealNova-Italic.ttf",
  bolditalic = "./2021/IbarraRealNova-BoldItalic.ttf"
)
showtext_auto()


end <- ggplot(all) +
  geom_sf(aes(fill = ccaa.shortname.es),
    col = "grey20",
    size = 0.15, linetype = "dotted"
  ) +
  scale_fill_manual(
    values =
      adjustcolor(
        pals::brewer.paired(15),
        alpha.f = 0.95
      )
  ) +
  theme_minimal() +
  facet_wrap(vars(type)) +
  guides(fill = guide_legend(
    direction = "horizontal",
    title.position = "top",
    nrow = 4
  )) +
  labs(
    title = "Reimagining boundaries",
    subtitle = "Internal boundaries of Spain",
    fill = "Autonomous Communities"
  ) +
  theme(
    legend.position = "bottom",
    text = element_text(family = "ibarra"),
    title = element_text(size = 30, face = "bold"),
    plot.subtitle = element_text(size = 25),
    legend.text = element_text(size = 20),
    panel.grid = element_line(size = 0.3),
    strip.text = element_text(size = 22),
    axis.text = element_text(size = 18)
  )

end

ggsave("2021/day22_boundaries.png", end,
  dpi = 300, width = 7, height = 5,
  bg = "white"
)

ggplot(noholes) +
  geom_sf(aes(fill = ccaa.shortname.es), col = "grey10") +
  scale_fill_manual(values = hcl.colors(15, "Tofino")) +
  theme_void()

oldccaa <- oldccaa %>% left_join(esp_codelist %>% select(ccaa.shortname.es, codauto))

pals::brewer.paired(15) %>% scales::show_col()

ggplot(oldccaa) +
  geom_sf(aes(fill = ccaa.shortname.es), col = "grey10") +
  scale_fill_manual(values = pals::brewer.paired(15)) +
  theme_void()

## Annex: Voronoi with capitals

cents_mun <- munics_pol %>% st_centroid(of_largest_polygon = TRUE)

# Capitals
cents_mun2 <- cents_mun %>% filter(name == ine.prov.name)

# Get missing cents
cents_mun3 <- cents_mun %>% filter(name %in%
  c(
    "A Coruña",
    "Oviedo/Uviéu",
    "Logroño",
    "Santander",
    "Vitoria-Gasteiz",
    "Bilbao",
    "Donostia/San Sebastián",
    "Pamplona/Iruña",
    "València",
    "Castelló de la Plana"
  ))

cents_munend <- cents_mun2 %>%
  bind_rows(cents_mun3) %>%
  arrange(codauto, cpro)



vor2 <- cents_munend %>%
  st_union() %>%
  st_voronoi() %>%
  st_collection_extract("POLYGON") %>%
  st_sf(id = 1)

newccaa2 <- vor2 %>%
  st_join(cents_munend) %>%
  mutate(
    newccaa = codauto,
    newpro = cpro
  ) %>%
  select(newccaa, newpro)

ggplot(newccaa2) +
  geom_sf(aes(fill = newpro))

munics_new2 <- st_join(munics_pol, newccaa2, largest = TRUE) %>%
  select(LAU_CODE, newccaa, newpro) %>%
  st_drop_geometry() %>%
  unique()


munics_end2 <- munics_pol %>%
  st_transform(3857) %>%
  left_join(munics_new2) %>%
  mutate(newcode = coalesce(newccaa, codauto)) %>%
  left_join(esp_codelist %>% select(ccaa.shortname.es, codauto), by = c("newcode" = "codauto"))

newccaa_pol2 <- munics_end2 %>%
  select(newcode, ccaa.shortname.es, newpro) %>%
  group_by(newcode, ccaa.shortname.es, newpro) %>%
  summarise(nmuns = n()) %>%
  filter(!newcode %in% del) %>%
  st_cast("MULTIPOLYGON")


noholes2 <- sfheaders::sf_remove_holes(newccaa_pol2)

noholes2$type <- "Centroid: Capital"

ggplot(noholes2) +
  geom_sf(aes(fill = ccaa.shortname.es), col = "grey10") +
  scale_fill_manual(values = pals::brewer.paired(15)) +
  theme_void()


all2 <- oldccaa %>% bind_rows(noholes2)

all2$type <- factor(all2$type, levels = c("Current", "Centroid: Capital"))


end2 <- ggplot(all2) +
  geom_sf(aes(fill = ccaa.shortname.es),
    col = "grey20",
    size = 0.15, linetype = "dotted"
  ) +
  scale_fill_manual(
    values =
      adjustcolor(
        pals::brewer.paired(15),
        alpha.f = 0.95
      )
  ) +
  theme_minimal() +
  facet_wrap(vars(type)) +
  guides(fill = guide_legend(
    direction = "horizontal",
    title.position = "top",
    nrow = 4
  )) +
  labs(
    title = "Reimagining boundaries",
    subtitle = "Internal boundaries of Spain",
    fill = "Autonomous Communities"
  ) +
  theme(
    legend.position = "bottom",
    text = element_text(family = "ibarra"),
    title = element_text(size = 30, face = "bold"),
    plot.subtitle = element_text(size = 25),
    legend.text = element_text(size = 20),
    panel.grid = element_line(size = 0.3),
    strip.text = element_text(size = 22),
    axis.text = element_text(size = 18)
  )

end2

ggsave("2021/day22_boundaries_alt.png", end2,
  dpi = 300, width = 7, height = 5,
  bg = "white"
)
