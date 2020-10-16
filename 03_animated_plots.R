

options(scipen = 10) # force R to show full numbers, not scientific notation
options("OutDec" = ",") # Czech decimal mark

library(reschola)
library(tidyverse)
library(magrittr)
library(scales)
library(naniar)
source("shared.R")
set_reschola_ggplot_fonts() # make ggplot2 use Roboto fonts without you having to set it
library(here)
library(gganimate)
library(transformr)
library(corrr)
library(officer)
library(lme4)
library(lmerTest)
library(broom.mixed)
library(flextable)
library(ggeffects)

here <- here::here


# magickPath <- shortPathName("C:/Program Files/ImageMagick-7.0.10-Q16-HDRI/magick.exe")
# animation::ani.options(convert = magickPath)


load(here("data-intermediate/envir.RData"))

#
# # jawdropping plot in motion
# sdq_dist_anim <- dfm %>% group_by(wave_ind) %>%
#   drop_na(gender_girl) %>%
#   select(id_pupil, gender_girl, is_intervention2, contains("10")) %>%
#   pivot_longer(contains("10")) %>%
#   ggplot(aes(value, fill = gender_girl)) +
#   geom_bar(aes(y = ..prop..), position = "dodge2") +
#   scale_fill_discrete(name = "pohlaví", labels = gender_labs) +
#   facet_wrap( ~ name, labeller = labeller(name = ctt_dim_labs)) +
#   theme_schola(
#     multiplot = TRUE,
#     legend.position = c(.825, 0.3),
#     plot.tag.position = c(.85, 0.125),
#     plot.tag = element_text(size = 11)
#   ) +
#   scale_y_percent_cz() +
#   guides(fill = guide_legend(ncol = 2)) +
#   labs(tag = "individuální\nvlna č. {closest_state}") +
#   transition_states(
#     wave_ind,
#     wrap = FALSE
#   ) +
#   ease_aes('cubic-in-out')
#
#
# anim_save(
#   "sdq_dist_anim_prez.gif",
#   animation = sdq_dist_anim,
#   path = here("data-processed"),
#   height = 3.251349,
#   width = 5.409448818897638,
#   units = "in",
#   res = 300,
#   fps = 20,
#   detail = 3,
#   duration = 9,
#   renderer = magick_renderer()
# )
#

# CTT group
# sdq_dist_anim_group <- dfm %>% group_by(wave_ind) %>%
#   filter(wave_ind %in% 1:3) %>%
#   select(id_pupil, gender_girl, is_intervention2, contains("10")) %>%
#   pivot_longer(contains("10")) %>%
#   ggplot(aes(value, fill = is_intervention2)) +
#   geom_bar(position = "dodge2") +
#   scale_fill_discrete(name = "skupina", labels = intervention_labs) +
#   facet_wrap( ~ name, labeller = labeller(name = ctt_dim_labs)) +
#   theme_schola(
#     multiplot = TRUE,
#     legend.position = c(.825, 0.3),
#     plot.tag.position = c(.85, 0.125),
#     plot.tag = element_text(size = 11)
#   ) +
#   guides(fill = guide_legend(ncol = 2)) +
#   labs(tag = "individuální\nvlna č. {closest_state}") +
#   transition_states(
#     wave_ind,
#     transition_length = 3,
#     state_length = 3,
#     wrap = FALSE
#   ) +
#   ease_aes('cubic-in-out') +
#   view_follow()
#
#
# anim_save(
#   "sdq_dist_anim_prez.gif",
#   animation = sdq_dist_anim_group,
#   path = here("data-processed"),
#   height = 3.251349,
#   width = 5.409448818897638,
#   units = "in",
#   res = 300
# )

# IRT SDQ animated
# sdq_irt_dist_anim <- dfm %>%
#   drop_na(gender_girl) %>% group_by(wave_ind) %>%
#   select(id_pupil, gender_girl, is_intervention2, emo:pro) %>%
#   pivot_longer(emo:pro) %>%
#   ggplot(aes(value, col = gender_girl)) +
#   geom_density(key_glyph = draw_key_smooth) +
#   scale_color_discrete(name = "pohlaví", labels = gender_labs) +
#   facet_wrap( ~ name, nrow = 2, labeller = labeller(name = dim_labs)) +
#   guides(col = guide_legend(ncol = 2)) +
#   labs(tag = "individuální\nvlna č. {closest_state}") +
#   theme_schola(
#     multiplot = TRUE,
#     legend.position = c(.85, 0.3),
#     plot.tag.position = c(.85, 0.125),
#     plot.tag = element_text(size = 11)
#   ) +
#   ease_aes('cubic-in-out') +
#   transition_states(wave_ind,
#                     wrap = FALSE)
#
# anim_save(
#   "sdq_irt_dist_anim_prez.gif",
#   animation = sdq_irt_dist_anim,
#   path = here("data-processed"),
#   height = 3.251349,
#   width = 5.409448818897638,
#   units = "in",
#   res = 300,
#   fps = 20,
#   detail = 3,
#   duration = 9,
#   renderer = magick_renderer()
# )

# group
sdq_irt_dist_anim_group <- dfm %>%
  filter(wave_ind %in% 1:3) %>% group_by(wave_ind) %>%
  select(id_pupil, gender_girl, is_intervention2, emo:pro) %>%
  pivot_longer(emo:pro) %>%
  ggplot(aes(value, col = is_intervention2)) +
  geom_density(key_glyph = draw_key_smooth) +
  scale_color_discrete(name = "skupina", labels = intervention_labs) +
  facet_wrap( ~ name, nrow = 2, labeller = labeller(name = dim_labs)) +
  guides(color = guide_legend(nrow = 2, keyheight = .7, title.vjust = -.3)) +
  scale_x_continuous(n.breaks = 7) +

  labs(x = "úroveň dimenze vyjádřená v **měřítku SDQ** *(tj. 0-10 bodů, 5 = průměr)*",
       y = "„relativní četnost“") +
  coord_cartesian(xlim = c(-.3,10.3), expand = FALSE) +
      labs(tag = "individuální vlna č. {closest_state}\n{paste0(rep('-', 28*frame/nframes), collapse = '')}") +
  theme_schola("scatter",
    multiplot = TRUE,
    axis.title.x = ggtext::element_markdown(),
    axis.title.y = element_text(),
    legend.position = c(.807, 0.1),
    plot.tag.position = c(.74, 0.4),
    plot.tag = element_text(size = 11, hjust = 0)
  ) +
  ease_aes('cubic-in-out') +
  transition_states(
    wave_ind,
    wrap = FALSE
  )


anim_save(
  "sdq_irt_dist_anim_group_prez.gif",
  animation = sdq_irt_dist_anim_group,
  path = here("data-processed"),
  height = 3.251349,
  width = 5.409448818897638,
  units = "in",
  res = 300,
  fps = 20,
  detail = 3,
  duration = 5,
  # renderer = magick_renderer()
)

