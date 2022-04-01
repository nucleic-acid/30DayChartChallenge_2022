library(tidyverse)
library(ggbeeswarm)
library(ggsvg)
library(ggtext)
library(fontawesome)

library("showtext")
font_add_google("Open Sans")
font_add_google("Bitter")
showtext_auto()

# read data and remove Organisations
laureates <- read_csv(here::here("..", "data", "nobel", "laureates.csv")) |> 
  filter(gender != "org") |> 
  mutate(
    flabel = ifelse(
      gender == "male",
      fa("male", fill = "#b9b9b8", fill_opacity = 0.7),
      fa("female", fill = "#9b332b", fill_opacity = 0.8)
    ),
    category = factor(category, levels = c("Chemistry", "Economic Sciences", "Physics",  "Peace", "Literature", "Physiology or Medicine"))
  )

laureates |> 
  count(year, sort = TRUE)

table(laureates$gender, laureates$category)

p <- laureates |> 
  ggplot(aes(x=category, y = year)) +
  geom_point_svg(
    aes(svg=flabel), 
    size = 2.5,
    position = position_quasirandom(bandwidth = 0.9, varwidth = TRUE)
    ) +
  labs(
    title = "<span style = 'font-size:64pt; font-family:Bitter;'><b>Missing not at random?</b></span>",
    subtitle = "<span style = 'font-size:30pt; font-family:Open Sans;'>The number of <span style = 'color: #9b332b;'><b>female</b></span> Nobel Prize laureates is astonishingly low, when compared to male<br>laureates. In recent years some fields show a trend in the right direction,<br>but there is still an ignobel gap in terms of gender equality.</span>",
    caption = "<span style = 'font-size:16pt; font-family:Open Sans;'>Note: Awarded Organisations are not included in this data.<br>DataViz by @c_gebhard | <b>#30DayChartChallenge 2022, Day 02</b> | Data by NobelPrize.org released under CC0 license</span>",
    y = "<span style = 'font-size: 20pt; font-family:Open Sans;'><b>Award Year</b></span>"
  ) +
  scale_x_discrete(labels = c(
    "<span style = 'font-size: 20pt;font-family:Open Sans;'><b>Chemistry</b></span>",
    "<span style = 'font-size: 20pt;font-family:Open Sans;'><b>Economic<br>Sciences</b></span>",
    "<span style = 'font-size: 20pt;font-family:Open Sans;'><b>Physics</b></span>",
    "<span style = 'font-size: 20pt;font-family:Open Sans;'><b>Peace</b></span>",
    "<span style = 'font-size: 20pt;font-family:Open Sans;'><b>Literature</b></span>",
    "<span style = 'font-size: 20pt;font-family:Open Sans;'><b>Physiology<br>or Medicine</b></span>"
  )
  ) +
  scale_y_continuous(
    breaks = c(1900, 1925, 1950, 1975, 2000, 2025),
    labels = c(
      "<span style = 'font-size: 20pt;font-family:Open Sans;'>1900</span>", 
      "<span style = 'font-size: 20pt;font-family:Open Sans;'>1925</span>", 
      "<span style = 'font-size: 20pt;font-family:Open Sans;'>1950</span>", 
      "<span style = 'font-size: 20pt;font-family:Open Sans;'>1975</span>", 
      "<span style = 'font-size: 20pt;font-family:Open Sans;'>2000</span>", 
      "<span style = 'font-size: 20pt;font-family:Open Sans;'>2025</span>")
  ) +
  coord_flip() +
  annotate(
    geom = "richtext",
    label = "<span style='font-family: Open Sans; font-size: 16pt;'><b>Marie Curie</b><br> was the only woman to be<br>awarded two Nobel Prizes.</span>",
    x = "Economic Sciences",
    y = 1925,
    hjust = 0,
    lineheight = 0.6,
    fill = NA,
    label.color = NA
  ) +
  annotate(
    geom = "curve", x = 2.1, y = 1923, xend = 2.7, yend = 1905,
    curvature = -.3, arrow = arrow(length = unit(1, "mm")),
    color = "#999999"
  ) +
  annotate(
    geom = "curve", x = 1.9, y = 1923, xend = 1.4, yend = 1911,
    curvature = .3, arrow = arrow(length = unit(1, "mm")),
    color = "#999999"
  ) +
  theme_classic(base_family = "Open Sans") +
  theme(
    text = element_text(family = "Open Sans"),
    plot.title.position = "plot",
    plot.title = element_markdown(lineheight = 1.2),
    plot.subtitle = element_markdown(lineheight = 0.8),
    plot.caption = element_markdown(lineheight = 0.8),
    panel.grid.major.x = element_line(color = "#444444", size = 0.1),
    panel.grid.minor = element_blank(),
  ) +
  theme(axis.text.x = element_markdown(lineheight = 1),
        axis.text.y = element_markdown(lineheight = 1),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_markdown(),
        axis.title.y = element_blank(),
        axis.line = element_blank(),
        plot.margin = margin(10,10,10,10,"pt")
  )


ggsave(here::here("..", "plots", "2022_02.png"), plot = p, height = 5.5, width = 5.5, dpi = "retina")
