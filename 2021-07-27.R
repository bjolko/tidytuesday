# Libraries ----

library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(ggdist)
library(ggrepel)
library(patchwork)
library(ggpubr)
library(showtext)
library(emojifont)

# font_add_google('Merriweather', 'Merriweather')
# font_add_google('Montserrat', 'Montserrat')
# showtext_auto()

# Data ----

# tuesdata <- tidytuesdayR::tt_load('2021-07-27')
# olympics <- tuesdata$olympics

fencing <- olympics %>% 
  filter(sport == 'Fencing') %>% 
  mutate(
    weapon = str_match(event, "\\'s (.+?),")[, 2] %>% str_to_lower(),
    event_type = str_extract(event, 'Team|Individual'),
  ) %>% 
  filter(weapon %in% c('epee', 'foil', 'sabre'))

# Charts ----

## Theme settings

background_color <- 'grey97'
line_color <- 'grey90'
font_color <- 'grey20'
caption_color <- 'grey55'
weapons_colors <- c(epee = '#D86262', foil = '#47648F', sabre = '#CDD560')
light_colors <- c(epee = '#FFBFBF', foil = '#9DAFCB', sabre = '#F8FDBD')

axis_font <- 'Merriweather'
title_font <- 'Montserrat'

clean_theme <-   
  theme(
    panel.grid = element_blank(),
    plot.background = element_rect(fill = background_color, color = background_color),
    panel.background = element_rect(fill = background_color, color = background_color),
    axis.ticks = element_blank(),
    axis.text = element_text(family = axis_font),
    panel.grid.major.x = element_line(color = line_color),
    plot.margin = margin(10, 0, 15, 10),
    plot.title.position = 'plot',
    plot.title = element_text(margin = margin(10, 0, 10, 0), color = font_color, family = title_font, size = 13, face = 'bold'),
    plot.subtitle = element_text(margin = margin(0, 0, 20, 0), color = font_color, family = title_font, size = 11),
    legend.position = 'none'
  )

## Hight -----

fencing %>% 
  group_by(weapon) %>% 
  summarise(median(height, na.rm = T))

p_height <- 
  fencing %>% 
  group_by(weapon) %>% 
  mutate(
    avg_height = mean(height, na.rm = T), 
  ) %>% 
  ungroup %>% 
  mutate(
    weapon = reorder(weapon, -avg_height)
  ) %>% 
  ggplot(aes(x = height, y = weapon, fill = weapon)) +
  stat_halfeye() +
  scale_x_continuous(limits = c(150, 210), breaks = seq(150, 200, 10)) +
  scale_fill_manual(values = weapons_colors) + 
  labs(
    title = str_wrap('Épée is for tall people'),
    subtitle = str_wrap('\tMedian height for épée participants is 181 cm compared to 174 cm in foil'),
    x = '',
    y = ''
  ) +
  clean_theme

p_height

## Sex ----

df_sex <- 
  fencing %>% 
  count(weapon, sex) %>% 
  group_by(sex) %>% 
  mutate(
    weapon = reorder(weapon, n),
    sex_emoji = if_else(sex == 'M', 'Male', 'Female'),
  ) %>% 
  ungroup %>% 
  mutate(
    weapon_sex = paste0(weapon, sex),
    fill_color = case_when( 
      weapon == 'epee' & sex == 'M' ~ weapons_colors[['epee']],
      weapon == 'epee' ~ light_colors[['epee']],
      weapon == 'foil' & sex == 'M' ~ weapons_colors[['foil']],
      weapon == 'foil' ~ light_colors[['foil']],
      weapon == 'sabre' & sex == 'M' ~ weapons_colors[['sabre']],
      weapon == 'sabre' ~ light_colors[['sabre']]
    )
  )

p_sex <- 
  df_sex %>% 
  ggplot(aes(x = weapon, y = n, fill = weapon_sex)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  geom_text(
    aes(label = sex_emoji),
    position = position_dodge(width = 1),
    hjust = -0.3,
    color = df_sex$fill_color
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3800)) +
  scale_fill_manual(values = df_sex$fill_color) +
  coord_flip() +
  guides(fill = 'none') +
  labs(
    title = str_wrap('Women are 7% of sabre participants'),
    subtitle = str_wrap('Number of women among partiticipants is growing since 1960'),
    x = '',
    y = ''
  ) +
  clean_theme

p_sex

## Participants ----

df_participants <- 
  fencing %>% 
  count(year, weapon) 

y_max <- ceiling(max(df_participants$n) / 100) * 100
x_max <- max(df_participants$year)
x_max_coef <- 10

p_participants <- 
    df_participants %>% 
    ggplot(aes(x = year, y = n, col = weapon, group = weapon)) +
    geom_hline(    
      yintercept = seq(0, y_max, 100),
      color = line_color, 
      size = 0.6
    ) +
    geom_line(size = 0.7) +
    geom_rect(aes(xmin = x_max, xmax = x_max + x_max_coef, ymin = -10, ymax = y_max + 1, fill = TRUE), fill = background_color, color = NA) +
    geom_text_repel(
      data = subset(df_participants, year == max(year)),
      aes(label = weapon),
      size = 4,
      direction = 'y',
      xlim = c(x_max + 5, NA),
      hjust = 0,
      segment.size = .7,
      segment.alpha = .5,
      segment.linetype = 'dotted',
      box.padding = .4,
      segment.curvature = -0.1,
      segment.ncp = 3,
      segment.angle = 20
    ) +
    scale_x_continuous(expand = c(0, 0), limits = c(1900, x_max + x_max_coef), breaks = seq(1900, x_max, 25)) + 
    scale_y_continuous(limits = c(-10, y_max + 1), breaks = seq(0, y_max, 100)) + 
    scale_color_manual(values = weapons_colors) +
    guides(col = 'none') +
    labs(
      title = str_wrap('Foil participants decreasing since 1992'),
      subtitle = str_wrap('After 2000s number of participants per each weapon is around the same — 100-150'),
      x = '',
      y = ''
    ) +
  clean_theme +
  theme(
    panel.grid.major.x = element_blank()
  )

p_participants

# Union ----

plot <- ggarrange(
  p_participants, 
  ggarrange(p_height, p_sex, ncol = 2),
  nrow = 2
)

annotate_figure(
  plot, 
  top = text_grob(
    'Fencing in Summer Olympics 1896-2016', 
    color = font_color, 
    face = 'bold', 
    size = 20,
    hjust = 1.71,
    family = title_font
  ),
  bottom = text_grob(
    'Datasource: The Olympics (www.sports-reference.com) from Kaggle',
    color = caption_color,
    size = 9,
    hjust = -1.27,
    family = title_font
  )
) + 
bgcolor(background_color) +
border(background_color)

# Export ----
ggsave(
  '2021-07-27.pdf', 
  width = 20, 
  height = 10, 
  device = cairo_pdf
)


pdftools::pdf_convert(
  '2021-07-27.pdf',
  filenames = '2021-07-27.png',
  format = 'png', 
  dpi = 450
)
