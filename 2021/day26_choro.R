# Download from
# http://www.infoelectoral.mir.es/infoelectoral/min/areaDescarga.html?method=inicio

library(tidyverse)
library(readxl)
library(mapSpain)

res <- read_excel("2021/02_201911_1.xlsx",
  skip = 5
)

mat <- res[, 14:80]
max_votes <- colnames(mat)[max.col(mat, ties.method = "first")]



vecvotes <- lapply(seq_len(length(max_votes)), function(x) {
  ncol <- match(max_votes[x], colnames(res))

  end <- res[x, ncol] %>% pull()
}) %>% unlist()



# Build final table
res_end <- res %>%
  mutate(LAU_CODE = paste0(
    sprintf("%02d", `Código de Provincia`),
    sprintf("%03d", `Código de Municipio`)
  )) %>%
  select(LAU_CODE, `Nombre de Municipio`) %>%
  mutate(
    Winner = max_votes,
    N_Votes = vecvotes
  )

show <- res_end %>%
  group_by(Winner) %>%
  summarise(n = sum(N_Votes)) %>%
  arrange(desc(n)) %>%
  slice(1:11)


munics <- esp_get_munic_siane(year = "2019-11-10")


# Join

munics_end <- munics %>% left_join(res_end)


# Labels

munics_end <- munics_end %>% mutate(
  label = if_else(Winner %in% show$Winner, Winner, "Other")
)


munics_end$label <- factor(munics_end$label, levels = c(show$Winner, "Other"))

provs <- esp_get_prov_siane()
linecan <- esp_get_can_provinces()
boxcan <- esp_get_can_box()

library(showtext)

font_add_google("Playfair Display", family = "playfair")

showtext_auto()

ggplot(munics_end) +
  geom_sf(aes(fill = label), col = NA) +
  scale_fill_manual(values = c(
    "#c10201",
    "#1db4e8",
    "#faa000",
    "#83b431",
    "#008347",
    "#e6324f",
    "#dc022c",
    "#bcd001",
    "#a444b4",
    "grey30",
    "#e6d229",
    "grey80"
  )) +
  geom_sf(data = provs, fill = NA, col = "white", size = 0.4) +
  geom_sf(data = linecan, lwd = 0.5) +
  geom_sf(data = boxcan, lwd = 0.5) +
  guides(fill = guide_legend(
    direction = "horizontal",
    title.position = "top",
    keyheight = 2,
    keywidth = 2
  )) +
  theme_void() +
  theme(
    legend.position = "top",
    text = element_text(family = "playfair"),
    plot.title = element_text(size = 80, hjust = .5, face = "bold"),
    plot.subtitle = element_text(size = 70, hjust = .5),
    plot.caption = element_text(size = 50),
    plot.margin = margin(20, 20, 20, 20),
    legend.margin = margin(t = 20),
    legend.text = element_text(size = 50)
  ) +
  labs(
    fill = "",
    title = "Spanish general election (Nov 2019)",
    subtitle = "Most voted party by Municipality",
    caption = "Source: http://www.infoelectoral.mir.es"
  )

ggsave("2021/day26_choro.png",
  height = 12, width = 15, dpi = 300,
  bg = "white"
)
