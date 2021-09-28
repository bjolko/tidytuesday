library(dplyr)
library(tidytext)
library(ggplot2)
library(showtext)
library(glue)

# Data ----
tidyweek <- '2021-09-28'

papers <- 
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/papers.csv') %>% 
  mutate(
    decade = floor(year / 10) * 10
  ) %>% 
  select(decade, title) %>% 
  unnest_tokens(word, title) %>% 
  anti_join(get_stopwords()) %>% 
  filter(!word %in% additional_stopwords) %>% 
  count(decade, word) %>% 
  group_by(decade) %>% 
  mutate(total_words = sum(n), pct = n / total_words) %>% 
  arrange(-n) %>% 
  slice(1)

additional_stopwords <- c('evidence', 'policy', 'analysis', 'effect', 'effects', 'market', 'trade')

# Chart settings ----
background_color <- '#f2f7fb'
chart_color <- '#0063a7'

text_font <- 'News Cycle'
title_font <- 'Arvo'
font_add_google(text_font, text_font)
font_add_google(title_font, title_font)
showtext_auto()

# Chart ----

papers %>% 
  ggplot(aes(x = decade, y = pct)) + 
  geom_point(size = 3, color = chart_color) +
  geom_segment(aes(xend = decade, y = 0, yend = pct), color = chart_color) +
  geom_text(aes(label = word), vjust = -1, color = chart_color) +
  scale_x_continuous(
    limits = c(1965, 2025),
    breaks = seq(1970, 2020, 10),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 0.1L),
    limits = c(0, 0.027),
    breaks = seq(0.005, 0.025, 0.01),
    expand = c(0, 0)
  ) +
  labs(
    title = 'Health > Market in 2000s according to NBER papers',
    subtitle = 'Frequency of the most popular word in each decade',
    caption = 'Data: National Bureau of Economic Research | Elvira Nassirova'
  ) +
  theme(
    axis.title = element_blank(),
    plot.title = element_text(family = title_font, face = 'bold', size = 20, margin = margin(b = 7)),
    plot.subtitle = element_text(family = title_font, size = 15, margin = margin(b = 12)),
    plot.caption = element_text(size = 9, margin = margin(t = 15)),
    plot.title.position = 'plot',
    plot.caption.position = 'plot',
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = '#c3cfdb'),
    panel.background = element_blank(),
    plot.margin = margin(10, 15, 5, 15),
    plot.background = element_rect(fill = background_color, color = NA),
    text = element_text(family = text_font, size = 15),
  )

# Export ----

ggsave(
  glue('{tidyweek}.pdf'), 
  width = 12, 
  height = 6, 
  device = cairo_pdf
)

pdftools::pdf_convert(
  glue('{tidyweek}.pdf'), 
  filenames = glue('{tidyweek}.png'),
  format = 'png', 
  dpi = 450
)

