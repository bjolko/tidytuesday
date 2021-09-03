# Libraries ----
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggrepel)
library(showtext)
library(patchwork)
library(glue)

# Data Load ----
tidyweek <- '2021-08-31'
tuesdata <- tidytuesdayR::tt_load(tidyweek)

bird_baths <- 
  tuesdata$bird_baths %>% 
  filter(!is.na(survey_year)) %>% # Remove Total count
  rename(bird_present = bird_count)

# Data Preparation ----
df_presence <- 
  bird_baths %>% 
  group_by(urban_rural, survey_year, bird_type) %>% 
  summarise(presence = mean(bird_present)) %>% 
  ungroup %>% 
  group_by(survey_year, urban_rural) %>% 
  arrange(-presence) %>% 
  mutate(rank = row_number()) %>% 
  ungroup

top_10_2015 <- 
  df_presence %>% 
  group_by(urban_rural, survey_year) %>% 
  filter(rank <= 5, survey_year == 2015) %>% 
  ungroup %>% 
  select(urban_rural, bird_type)

# Charts ----
text_font <- 'Barlow'
title_font <- 'Luckiest Guy'
font_add_google(text_font, text_font)
font_add_google(title_font, title_font)
showtext_auto()


make_chart <- function(location_type, chart_color){
  
  df_presence %>%
    filter(urban_rural == location_type) %>% 
    inner_join(top_10_2015) %>% 
    mutate(
      bird_type_label = ifelse(survey_year == 2015, bird_type, NA),
    ) %>% 
    ggplot(aes(x = survey_year, y = presence, group = bird_type)) +
    geom_line(size = 1, color = chart_color) +
    scale_x_continuous(limits = c(2014, 2015.9), breaks = c(2014, 2015), position = 'top') +
    scale_y_continuous(limits = c(.14, .38), breaks = seq(.15, .35, .05), label = scales::label_percent(accuracy = 1L)) +
    labs(
      title = location_type
    ) +
    geom_text_repel(
      aes(label = bird_type_label),
      size = 6,
      hjust = 0,
      direction = 'y',
      xlim = c(2015.03, NA),
      segment.color = 'transparent',
      color = chart_color,
      family = text_font,
    ) +
    theme(
      panel.background = element_blank(),
      panel.grid = element_blank(),
      legend.position = 'none',
      plot.margin = margin(10, 15, 15, 15),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_text(size = 25, color = 'gray10', face = 'bold'),
      axis.text.y = element_text(size = 14, color = 'gray10'),
      plot.title.position = 'panel',
      plot.caption.position = 'plot',
      plot.title = element_text(family = title_font, size = 40, margin = margin(b = 20), color = chart_color),
      text = element_text(family = text_font),
    )
}

p_urban <- make_chart('Urban', '#034AA6')
p_rural <- 
  make_chart('Rural', '#D97762') +
  theme(
    axis.text.y = element_blank()
  )

p_urban + p_rural +
  plot_annotation(
    title = 'What is probability to meet the most frequent Australian\nbirds of 2015?',
    subtitle = 'Changes in Australian bird baths inhabitants',
    caption = 'Data: Cleary et al, 2016 | Elvira Nassirova',
    theme = theme(
      plot.title = element_text(size = 30, margin = margin(t = 15, b = 10), face = 'bold'),
      plot.subtitle = element_text(size = 18, margin = margin(b = 20)),
      plot.caption = element_text(size = 12)
    )
  )

ggsave(
  glue('{tidyweek}.pdf'), 
  width = 12, 
  height = 9, 
  device = cairo_pdf
)

pdftools::pdf_convert(
  glue('{tidyweek}.pdf'), 
  filenames = glue('{tidyweek}.png'),
  format = 'png', 
  dpi = 450
)
