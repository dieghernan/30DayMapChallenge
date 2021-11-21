library(mapSpain)
library(sf)
library(dplyr)
library(ggplot2)

# Filter
del <- esp_dict_region_code(c("Canarias", "Ceuta", "Melilla", "Baleares"), destination = "codauto")


# Get provs

prov <- esp_get_prov() %>% st_transform(3857) %>% filter(!codauto %in% del)


# Create Voronois
cents <- prov %>% st_centroid(of_largest_polygon = TRUE)
vor <- cents %>% st_union() %>%
  st_voronoi() %>% st_collection_extract("POLYGON") %>%
  st_sf(id=1)


ggplot(vor) +
  geom_sf()


# Asign new CCAA
newccaa <- vor %>% st_join(cents) %>% mutate(newccaa=codauto,
                                             newpro = cpro) %>%
  select(newccaa, newpro)

ggplot(newccaa) +
  geom_sf(aes(fill=newpro))

# Reclassifly munics
munics_pol <- esp_get_munic_siane() %>% st_transform(3857) %>%
  filter(!codauto %in% del)

oldccaa <- munics_pol %>% group_by(
  codauto, cpro
) %>% summarise(n=n())

ggplot(oldccaa) +
  geom_sf(aes(fill=codauto))

munics_cent <- munics_pol %>% st_centroid() %>% st_cast("POINT")

munics_new <- st_join(munics_pol,newccaa, largest = TRUE) %>%
  select(LAU_CODE, newccaa, newpro) %>%
  st_drop_geometry() %>% unique()

# Get final map

data("esp_codelist")

munics_end <- munics_pol %>%
  st_transform(3857) %>%
  left_join(munics_new)  %>%
  mutate(newcode = coalesce(newccaa, codauto)) %>%
left_join(esp_codelist %>% select(ccaa.shortname.es, codauto), by=c("newcode" = "codauto"))

newccaa_pol <- munics_end %>% select(newcode, ccaa.shortname.es, newpro) %>%
  group_by(newcode, ccaa.shortname.es, newpro) %>%
  summarise(nmuns = n())  %>%
  filter(!newcode %in% del) %>%
  st_cast("MULTIPOLYGON")

noholes <- sfheaders::sf_remove_holes(newccaa_pol)



noholes$type <-"New"
oldccaa$type <- "Current"

oldcc

all <- oldccaa %>% bind_rows(noholes)

library(showtext)
font_add(family = "pacifico", regular = "./2021/Pacifico-Regular.ttf")
showtext_auto()


ggplot(all) +
  geom_sf(aes(fill=ccaa.shortname.es), col="grey20") +
  scale_fill_manual(values=
                      adjustcolor(
                      pals::brewer.paired(15),
                      alpha.f = 0.95)) +
  theme_void() +
  facet_wrap(vars(type))  +
  guides(fill=guide_legend(direction = "horizontal",
                           title.position = "top",
                           nrow = 4)) +
  labs(title="Reimagining boundaries",
       subtitle = "Internal boundaries of Spain",
       fill="Autonomous Communities") +
  theme(legend.position = "bottom",
        text = element_text(family = "pacifico"))


ggplot(noholes) +
  geom_sf(aes(fill=ccaa.shortname.es), col="grey10") +
  scale_fill_manual(values=hcl.colors(15, "Tofino")) +
  theme_void()

oldccaa <- oldccaa %>% left_join(esp_codelist %>% select(ccaa.shortname.es, codauto))

pals::brewer.paired(15) %>% scales::show_col()

ggplot(oldccaa) +
  geom_sf(aes(fill=ccaa.shortname.es), col="grey10") +
  scale_fill_manual(values=pals::brewer.paired(15)) +
  theme_void()

