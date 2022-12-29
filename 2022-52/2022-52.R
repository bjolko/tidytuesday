library(dplyr)
library(ggplot2)
library(ggchicklet) # Rounded bar charts
library(showtext)
library(ggtext)
library(glue)

# Data ----
tidyweek <- '2022-12-27'

tuesdata <- tidytuesdayR::tt_load(tidyweek)
tuesdata <- tidytuesdayR::tt_load(2022, week = 52)

tlBooks <- tuesdata$tlBooks
tlFootnotes <- tuesdata$tlFootnotes

# Colors ----

background_color <- '#291B20'
light_color <- '#8B80AE'
text_color <- '#B7A9D3'

palette <- c(
  book = '#DA132E',
  episode = '#FDEDB8',
  story = '#3871C0'
)

# Fonts ----
title_font <- 'VT323'
text_font <- 'Red Hat Mono'

font_add_google(title_font, text_font)
font_add_google(text_font, text_font)
showtext_auto()

# Chart ----

tlBooks %>% 
  count(setting, format) %>% 
  mutate(setting = factor(setting, levels = c('secondary', 'primary'), labels = c('Secondary', 'Primary'))) %>% 
  ggplot(aes(x = setting, y = n, fill = format)) + 
  geom_chicklet(color = background_color, width = 0.5, size = 1) +
  scale_y_continuous(position = 'right', limits = c(0, 1650), expand = c(0, 0)) +
  scale_fill_manual(values = palette) +
  guides(fill = FALSE) +
  coord_flip() +
  labs(
    title = glue("NO <span style='color:{palette['episode']};'>EPISODES</span> IN SECONDARY UNIVERSE OF STAR TREK TIMELINES"),
    subtitle = glue("<span style='color:{palette['book']};'><b>BOOK</b></span> format takes the majority, and <span style='color:{palette['story']};'><b>STORY</b></span> is the smallest piece for both settings"),
    caption = 'Data: {rtrek} package | Elvira Nassirova'
  ) +
  theme(
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(color = text_color),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = text_color),
    panel.background = element_blank(),
    plot.background = element_rect(fill = background_color, color = NA),
    plot.title = element_markdown(family = title_font, size = 36, face = 'bold', margin = margin(l = 5, b = 5, t = 5)),
    plot.subtitle = element_markdown(family = title_font, size = 15, margin = margin(l = 5, b = 20)),
    plot.caption = element_text(size = 8, margin = margin(t = 15)),
    plot.title.position = 'plot',
    plot.caption.position = 'plot',
    text = element_text(family = text_font, color = text_color, size = 10)
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

