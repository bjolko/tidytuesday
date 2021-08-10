# Libraries ----

library(dplyr)
library(ggplot2)
library(ggtext)
library(glue)
library(showtext) # Fonts
library(stringr)

# Data Load ----
tidyweek <- '2021-08-10'
tuesdata <- tidytuesdayR::tt_load(tidyweek)

investment <- tuesdata$investment
chain_invesment <- tuesdata$chain_investment
ipd <- tuesdata$ipd

# Chart ----
## Settings ----
# https://colorhunt.co/palette/0820322c394b334756ff4c29
highlight_color <- '#BD4B4B'
backlight_color <- 'gray65'
line_color <- 'grey87'
background_color <- 'grey95'

font_add_google('Crimson Text', 'Crimson Text')
font_add_google('Shadows Into Light', 'Shadows Into Light')
showtext_auto()

## Line chart ----

investment %>% 
  filter(group_num == 12) %>% 
  ggplot(
    aes(
      x = year, 
      y = gross_inv, 
      group = category,
      col = if_else(category == 'Highways and streets', highlight_color, backlight_color),
      size = if_else(category == 'Highways and streets', 0.5, 0.25)
    )
  ) +
  geom_line() + 
  scale_color_identity() +
  scale_size_identity() +
  scale_y_continuous(
    labels = scales::dollar_format(),
    limits = c(0, 100000),
    breaks = seq(0, 95000, 30000)
  ) +
  coord_cartesian(expand = FALSE, clip = 'off') +
  labs(
    title = glue("Gross investments in <span style='color:{highlight_color};'>Highways and streets</span> and other <span style='color:{backlight_color};'>Transportation categories</span>, 1947-2017")
  ) +
  theme(
    plot.title.position = 'plot',
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = line_color),
    panel.background = element_blank(),
    plot.background = element_rect(fill = background_color, color = NA),
    plot.margin = margin(15, 25, 15, 20),
    plot.title = element_textbox(color = 'grey20', size = 25, family = 'Crimson Text', width = unit(1, 'npc'), margin = margin(b = 15)),
    axis.text = element_text(color = 'grey20', size = 14, family = 'Shadows Into Light')
  )

# Export ----
  
ggsave(
  glue('{tidyweek}.pdf'), 
  width = 10, 
  height = 5, 
  device = cairo_pdf
)

pdftools::pdf_convert(
  glue('{tidyweek}.pdf'), 
  filenames = glue('{tidyweek}.png'),
  format = 'png', 
  dpi = 450
)
