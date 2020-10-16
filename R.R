change_by_wave %>%
  # filter() %>%
  ggplot(aes(x = wave_contr, y = estimate)) +
  geom_pointrange(aes(
    col = school,
    ymin = (-1.96 * std.error) + estimate,
    ymax = (1.96 * std.error) + estimate
  ),
  size = .2,
  position = position_dodge2(width = .5),
  key_glyph = draw_key_point
  ) +
  geom_line(aes(group = school, col = school),
    position = position_dodge2(width = .5),
    key_glyph = draw_key_point,
    alpha = .25
  ) +
  scale_x_discrete(labels = c("2. vlna", "3. vlna")) +
  scale_colour_discrete(labels = function(x) str_wrap(x, width = 15)) +
  labs(
    x = "zvýšení SDQ oproti 1. vlně",
    y = "zvýšení SDQ oproti ref. školce"
  ) +
  facet_wrap(~dim) +
  guides(col = guide_legend(
    title = "školka",
    ncol = 2, byrow = TRUE,
    keyheight = .1, keywidth = .1,
  )) +
  theme_schola(
    axis.title = element_text(),
    # legend.title = element_text(),
    legend.text = element_text(size = 10),
    # legend.key.size = unit(2, "pt"),
    legend.position = c(.85, .2)
  )
