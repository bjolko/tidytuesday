library(dplyr)
library(stringr)
library(ggplot2)
library(ggdist)
library(ggtext)
library(glue)
library(showtext)
library(patchwork)

tidyweek <- '2021-08-24'
lemurs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv')

lemurs_clean <- 
  lemurs %>% 
  filter(
    str_to_lower(birth_type) %in% c('cb', 'wb'),
    str_to_lower(sex) %in% c('f', 'm'),
  ) %>% 
  mutate(
    sex = recode(sex, `F` = 'Female', `M` = 'Male'),
    birth_type = recode(birth_type, CB = 'Captive', WB = 'Wild')
  )

background_color <- '#F0F0F2'
legend_colors <- c('#A6907C', '#B0BF7A')
alpha_level <- 0.75

text_font <- 'Dosis'
title_font <- 'Bebas Neue'
font_add_google(text_font, text_font)
font_add_google(title_font, title_font)
showtext_auto()

t <- theme(
    panel.background = element_blank(),
    plot.background = element_rect(fill = background_color, color = NA),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(color = 'gray85'),
    legend.position = 'none',
    plot.margin = margin(10, 15, 15, 15),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    # axis.text = element_blank(),
    plot.title.position = 'plot',
    plot.caption.position = 'plot',
    plot.title = element_markdown(family = title_font, size = 18, margin = margin(b = 10), hjust = 0),
    plot.subtitle = element_text(size = 14, margin = margin(b = 20), hjust = 0),
    text = element_text(family = text_font, size = 10, color = 'gray20')
  )
theme_set(t)

# General exploration
## Significant difference btw CB & WB for both sex, CB have the same weight for both sex
lemurs_clean %>% 
  group_by(sex, birth_type) %>% 
  summarise(
    cnt = n(),
    weight = round(median(weight_g, na.rm = T)), # Is there a correlation ?
  ) %>% 
  knitr::kable()


# Density plot for Weight
## For WB peaks are at low weight, for CB -- at 2.5+. Maybe there is something else influencing this metric? E.g. age?
p1 <- 
  lemurs_clean %>% 
  ggplot(aes(x = weight_g, fill = birth_type)) +
  stat_slab(alpha = alpha_level) +
  labs(
    title = glue("Weight"),
    subtitle = "We can see significant difference, the peaks are in different sides of the distribution. So maybe\nthere is another variable infuencing them?",
  ) +
  scale_x_continuous(expand = c(0, 0), limits = c(-50, 5000), labels = scales::comma, breaks = seq(1000, 5000, 1000)) +
  scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
  scale_fill_manual(values = legend_colors) +
  theme(
    axis.text = element_text(color = 'gray10'),
  )

p1
  
# Density plot for max Age
## More mature species for WB, more young for CB (maybe they go to the wild then?)
p2 <- 
  lemurs_clean %>% 
  ggplot(aes(x = age_max_live_or_dead_y, fill = birth_type)) +
  stat_slab(alpha = alpha_level) +
  labs(
    title = glue("Age"),
    subtitle = "Wild born lemurs representatives are in general older then Captive ones. We should stratify them"
  ) +
  scale_x_continuous(expand = c(0, 0), limits = c(-0.4, 40), labels = scales::comma, breaks = seq(10, 40, 10)) +
  scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
  scale_fill_manual(values = legend_colors) +
  theme(
    axis.text = element_text(color = 'gray10'),
  )

p2

# Idea: Analyze by age groups
p3 <- 
  lemurs_clean %>% 
  filter(
    !is.na(age_max_live_or_dead_y),
  ) %>% 
  mutate(
    age_group = cut(age_max_live_or_dead_y, seq(-1, 50, 10), labels = c('0-9', '10-19', '20-29', '30-39', '40-49')),
    age_group = recode(age_group, `(9, 19]` = '10-19')
  ) %>% 
  filter(
    !age_group %in% c('0-9', '40-49') # Remove because of small number of observations for one group
  ) %>%
  ggplot(aes(y = weight_g, x = age_group, fill = birth_type)) +
  geom_boxplot(alpha = alpha_level, width = 0.35) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = legend_colors) +
  labs(
    title = glue("Weight by Age groups"),
    subtitle = 'Here we go: if analyze by age groups, median Weight is not that different. Note: we excluded\ngroups with small number of observations',
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = 'gray85'),
    axis.text = element_text(color = 'gray10'),
  )

p3

p1 / p2 / p3 + 
  plot_annotation(
    title = glue("<span style='color:{legend_colors[1]};'>Captive</span> and <span style='color:{legend_colors[2]};'>Wild</span> born lemurs"),
    subtitle = str_wrap("Hey stranger, during analyzing Lemurs data as a part of TidyTuesday challenge I noticed that Wild born animals are more 'happy' in terms of life expectancy and weight. But maybe there is a reason for that? Let's see!", 75),
    caption = 'Data: Duke Lemur Center (lemur.duke.edu) | Elvira Nassirova',
    theme = theme(
      plot.title = element_markdown(size = 30, margin = margin(t = 15, b = 10)),
      plot.subtitle = element_text(size = 18, margin = margin(b = 10))
    )
  )

ggsave(
  glue('{tidyweek}.pdf'), 
  width = 8, 
  height = 11, 
  device = cairo_pdf
)

pdftools::pdf_convert(
  glue('{tidyweek}.pdf'), 
  filenames = glue('{tidyweek}.png'),
  format = 'png', 
  dpi = 450
)
