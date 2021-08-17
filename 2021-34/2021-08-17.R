# Libraries ----

library(dplyr)
library(ggplot2)
library(stringr)
library(ggdist)
library(showtext)
library(glue)

# Data Load ----

tidyweek <- '2021-08-17'
tuesdata <- tidytuesdayR::tt_load(tidyweek)

computer <- tuesdata$computer

# Speech length distribution ----

font_add_google('Space Mono', 'Space Mono')
showtext_auto()

legend_colors <- c('#00B5E2FF', '#F8485E')

df <- 
  computer %>% 
  distinct(value_id, line, char_type = str_to_lower(char_type)) %>% 
  mutate(
    count_words = str_count(line, ' ') + 1,
    char_type = str_to_title(char_type)
  )

medians <- 
    df %>% 
    group_by(char_type) %>% 
    summarise(median_words = median(count_words, na.rm = TRUE))

df %>% 
  ggplot(aes(x = count_words, y = char_type, fill = char_type)) +
  stat_slab(alpha = 0.5, adjust = .5, height = .7) +
  stat_pointinterval(.width = c(.5, .95)) +
  geom_text(
    data = medians,
    aes(label = median_words, y = char_type, x = median_words),
    vjust = -1,
    family = 'Space Mono'
  ) +
  coord_cartesian(ylim = c(1.5, NA), xlim = c(0.01, NA)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 50), breaks = seq(0, 50, 10)) +
  scale_fill_manual(values = legend_colors) + 
  labs(
    title = 'Star Trek VUI: Number of words per interaction',
    subtitle = 'People tend to speak more! On average, they use 8 words in a phrase',
    caption = 'Data: SpeechInteraction.org | Elvira Nassirova'
  ) +
  theme(
    legend.position = 'none',
    plot.margin = margin(10, 15, 5, 15),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(color = 'gray50'),
    axis.text.y = element_text(size = 16, colour = legend_colors, vjust = -3),
    axis.text.x = element_text(hjust = 1),
    plot.title.position = 'plot',
    plot.caption.position = 'plot',
    plot.title = element_text(face = 'bold', size = 25),
    plot.subtitle = element_text(margin = margin(b = 15)),
    plot.caption = element_text(margin = margin(t = 15), size = 8, color = 'gray50'),
    panel.background = element_blank(),
    plot.background = element_rect(fill = 'white'),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(color = 'gray95'),
    text = element_text(family = 'Space Mono')
  )

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
