# Day 4 of #30DayChartChallenge

# load libraries
library("tidyverse")
library("rnaturalearth")
library("rnaturalearthdata")
library("MetBrewer")
library("ggtext")
library("gt")
library("gtExtras")
library("patchwork")
library("png")
library("ggpubr")

library("showtext")
font_add_google("Cabin Sketch")
font_add_google("Open Sans")
showtext_auto()

# read data
# Forest area is land under natural or planted stands of trees of at least 5 meters in situ, whether productive or not, and excludes tree stands in agricultural production systems (for example, in fruit plantations and agroforestry systems) and trees in urban parks and gardens.

forestation <- read_csv(here::here("..", "data", "worldbank", "forestation_tidy.csv")) |> 
  pivot_wider(id_cols = c(country_name, country_code), names_from = Year, values_from = surface, names_prefix = "y") |> 
  mutate(surface_diff = y2020 - y2000)


world <- ne_countries(scale = "medium", returnclass = "sf")

world_forestation <- world |> 
  left_join(forestation, by = c("iso_a3" = "country_code")) |> 
  filter(iso_a3 != "ATA") # Antactica is removed due to missing data to save space on the map

europe <- world_forestation |> 
  filter(region_un == "Europe")

pworld <- ggplot(data = world_forestation) +
  geom_sf(aes(fill = surface_diff), size = 0.1) +
  scale_fill_gradient2(low = "#811e18", high = "#1a472a", mid = "#EEEEEE", na.value = "#FFFFFF") +
  # coord_sf() +
  theme_classic() +
  labs(
    title = "<b>Global Forest Area Development 2000-2020<b>",
    subtitle = "Change of natural or planted forest area (in % of land area) over the last two decades. Some<br>countries lost/destroyed wooded area by up to ~17% of their landmass. Others increased<br>forest coverage by up to ~15%. The top 6 in each direction are listed in the tables."
  ) +
  theme(
    text = element_text(family = "Open Sans"),
    plot.title = element_markdown(family = "Cabin Sketch", size = 76),
    plot.subtitle = element_markdown(family = "Open Sans", size = 40, lineheight = 0.4),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  )

# plot the legend plot
legend_plot <- world_forestation |> 
  filter(!is.na(surface_diff)) |> 
  ggplot() +
  aes(
    x = reorder(iso_a3, -surface_diff), 
    y = surface_diff,
    fill = surface_diff
  ) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low = "#811e18", high = "#1a472a", mid = "#EEEEEE") +
  scale_y_continuous(
    breaks = c(-15, -10, -5, 0, 5, 10, 15),
    labels = c("-15", "-10", "-5", "0", "5", "10", "15")
  ) +
  labs(
    caption = "Change of the countries' Forest areas<br>(sorted by % of land area)"
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "Open Sans"),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.major.y = element_line(color = "black", size = 0.1),
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.position = "none"
  ) +
  theme( # text styling
    plot.title = element_blank(),
    plot.caption = element_markdown(family = "Open Sans", size = 20, hjust = 0.5, lineheight = 0.4),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_markdown(family = "Open Sans", size = 20)
  )
  
# Create min/max table

top6 <- as_tibble(world_forestation) |> 
  filter(!is.na(surface_diff)) |> 
  select(name, surface_diff) |> 
  arrange(desc(surface_diff)) |> 
  mutate(surface_diff = round(surface_diff, 2)) |> 
  head(6) |> 
  gt()  |> 
  gt_color_rows(surface_diff, 
                palette = c("#ffffff", "#1a472a"),
                domain = c(0, 14.94),
                use_paletteer = FALSE) |> 
  tab_options(
    column_labels.hidden = TRUE
  )

bad6 <- as_tibble(world_forestation) |> 
  filter(!is.na(surface_diff)) |> 
  select(name, surface_diff) |> 
  arrange(surface_diff) |> 
  mutate(surface_diff = round(surface_diff, 2)) |> 
  head(6) |> 
  arrange(desc(surface_diff)) |> 
  gt()  |> 
  gt_color_rows(surface_diff, 
                palette = c("#811e18", "#ffffff"),
                domain = c(0, -17.34),
                use_paletteer = FALSE) |> 
  tab_options(
    column_labels.hidden = TRUE
  )

gtsave(bad6, "bad6.png", zoom = 10)
gtsave(top6, "top6.png", zoom = 20)

bad6_img <- readPNG("bad6.png", native = TRUE)
bad6_plot <- ggplot() +
  background_image(bad6_img) +
  coord_fixed()

top6_img <- readPNG("top6.png", native = TRUE)
top6_plot <- ggplot() +
  background_image(top6_img) +
  coord_fixed()

## COMPOSE patchwork
layout <- "
AAAAAA
AAAAAA
AAAAAA
#BCCD#
"

patched <- pworld + top6_plot + legend_plot + bad6_plot +
  plot_layout(design = layout) + 
  plot_annotation(
    caption = "<span >DataViz by @c_gebhard  | <b>#30DayChartChallenge 2022, Day 04</b> | Data by WorldBank.org under CC BY 4.0 Interntional License</span>",
    theme = theme(
      plot.caption = element_markdown(family = "Open Sans", size = 20),
    )
  )
  
  
ggsave(here::here("..", "plots", "2022_04.png"), plot = patched, height = 6, width = 8, dpi = "retina")
