# Day 21 of #30DayChartChallenge
# Data downloaded from https://www-genesis.destatis.de/genesis/online?sequenz=statistikTabellen&selectionname=12612 (on 2022-03-24)


# load libraries
library(tidyverse)
library(MetBrewer)
library(ggtext)

library("showtext")
font_add_google("Kalam")
font_add_google("Open Sans")
font_add('fa-solid', '../fonts/Font Awesome 6 Free-Solid-900.otf')
showtext_auto()


# translation table of German category to numerical value
conversion <- tribble(
  ~de, ~child_n,
  "erstes_kind", 1,
  "zweites_kind", 2,
  "drittes_kind", 3
)

# read data and translate with above table
parental_age <- read_csv(here::here("..", "data", "destatis", "parental_age.csv")) |> 
  pivot_longer(c(erstes_kind, zweites_kind, drittes_kind), names_to = "child_de", values_to = "age") |> 
  left_join(conversion, by = c("child_de" = "de")) |> 
  select(-child_de)
  

# create annotation tibble
annotations <- tribble(
  ~parent, ~year, ~age, ~label, ~child_n,
  "father", 2014.1, 32.5, "<span style='font-family:fa-solid; color: #7fa074'>&#xf77c;</span>", 1,
  "father", 2014.1, 34.7, "<span style='font-family:fa-solid; color: #ffffff'>&#xf77c;</span><span style='font-family:fa-solid; color: #7fa074'>&#xf77c;</span>", 1,
  "father", 2014.1, 36.3, "<span style='font-family:fa-solid; color: #ffffff'>&#xf77c;</span><span style='font-family:fa-solid; color: #ffffff'>&#xf77c;</span><span style='font-family:fa-solid; color: #7fa074'>&#xf77c;</span>", 1,
  "mother", 2014.1, 29.2, "<span style='font-family:fa-solid; color: #b695bc'>&#xf77c;</span>", 1,
  "mother", 2014.1, 31.5, "<span style='font-family:fa-solid; color: #ffffff'>&#xf77c;</span><span style='font-family:fa-solid; color: #b695bc'>&#xf77c;</span>", 1,
  "mother", 2014.1, 32.7, "<span style='font-family:fa-solid; color: #ffffff'>&#xf77c;</span><span style='font-family:fa-solid; color: #ffffff'>&#xf77c;</span><span style='font-family:fa-solid; color: #b695bc'>&#xf77c;</span>", 1
)

# plot
pplot <- ggplot(parental_age) +
  aes(x = year, y = age, group = child_n) +
  geom_ribbon(
    aes(ymin = 29, ymax = age, fill = parent, color = parent), 
    alpha = 0.3,
    size = 1.2,
    outline.type = "upper"
    ) +
  facet_wrap(~parent) +
  scale_y_continuous(
    breaks = c(29:37),
    labels = c(
      "<span style = 'font-size: 30pt;'><b>29</b></span>",
      "30",
      "31",
      "32",
      "33",
      "34",
      "35",
      "36",
      "37"
    )
  ) +
  scale_fill_met_d("Cassatt2", direction = -1) +
  scale_color_met_d("Cassatt2", direction = -1) +
  labs(
    title = "<span'>Parental Age at Childbirth in Germany</span>",
    subtitle = "<span>The mean age of both, <span style='color: #b695bc'><b>mother</b></span> and <span style='color: #7fa074'><b>father</b></span>, at birth of their first child (<span style='font-family:fa-solid; color: #000000'>&#xf77c;</span>)<br> increased over the last years in Germany. The same applies to mothers<br>giving birth to a second (<span style='font-family:fa-solid; color: #aaaaaa'>&#xf77c;</span><span style='font-family:fa-solid; color: #000000'>&#xf77c;</span>) and third (<span style='font-family:fa-solid; color: #aaaaaa'>&#xf77c;</span><span style='font-family:fa-solid; color: #aaaaaa'>&#xf77c;</span><span style='font-family:fa-solid; color: #000000'>&#xf77c;</span>) child, but not as much<br>to their partners.",
    caption = "<span>DataViz by @c_gebhard | <b>#30DayChartChallenge, Day 21</b> | Data: Statistisches Bundesamt (Destatis), Genesis-Online; Datenlizenz by-2-0</span>",
    y = "age at birth (years)",
    x = "year of birth"
  ) +
  geom_richtext(
    data = annotations,
    mapping = aes(x = year, y = age, label = label),
    label.colour = NA, fill = NA,
    size = 12,
    hjust = 0
  ) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    text = element_text(family = "Kalam"),
    strip.text = element_blank(),
    axis.line = element_blank(),
    panel.grid.major.y = element_line(size = .4, colour = "black", linetype = "dotted"),
    plot.margin = margin(15,20,15,15, "pt"),
    plot.title.position = "plot",
    legend.position = "none",
    plot.title = element_markdown(family = "Kalam", size = 72),
    plot.subtitle = element_markdown(family = "Kalam", size = 36, lineheight = 0.4),
    axis.title = element_markdown(family = "Kalam", size = 22, face = "bold"),
    axis.text = element_markdown(family = "Kalam", size = 20),
    axis.text.y = element_markdown(),
    plot.caption = element_markdown(family = "Open Sans", size = 16)
  )

ggsave(here::here("..", "plots", "2022_21.png"), plot = pplot, height = 5.5, width = 5.5, dpi = "retina")
  