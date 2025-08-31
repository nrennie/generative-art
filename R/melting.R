library(tidyverse)
library(ggstream)
library(PrettyCols)
library(ggnewscale)

# Parameters
s <- 1234
col_palette <- PrettyColsPalettes[["Teals"]][[1]]

# Data
set.seed(s)
plot_data <- map_dfr(1:30, ~ {
  x <- 1:sample(1:70, 1)
  tibble(x = x + sample(1:150, 1)) %>%
    mutate(
      y = sample(1:10, length(x), replace = T),
      k = .x %>% as.character()
    )
})
n_fill <- length(unique(plot_data$k))

# Plot
ggplot(
  data = plot_data,
  mapping = aes(x = x, y = y, fill = k)
) +
  geom_stream(
    bw = 3,
    extra_span = 0.001,
    true_range = "none",
    position = position_nudge(y = 400)
  ) +
  scale_fill_manual(values = sample(grDevices::colorRampPalette(col_palette)(n_fill))) +
  geom_stream(
    bw = 1.5,
    extra_span = 0.001,
    true_range = "none",
    position = position_nudge(y = 300)
  ) +
  new_scale_fill() +
  scale_fill_manual(values = sample(grDevices::colorRampPalette(col_palette)(n_fill))) +
  geom_stream(
    bw = 0.9,
    extra_span = 0.001,
    true_range = "none",
    position = position_nudge(y = 200)
  ) +
  new_scale_fill() +
  scale_fill_manual(values = sample(grDevices::colorRampPalette(col_palette)(n_fill))) +
  geom_stream(
    bw = 0.7,
    extra_span = 0.001,
    true_range = "none",
    position = position_nudge(y = 100)
  ) +
  new_scale_fill() +
  scale_fill_manual(values = sample(grDevices::colorRampPalette(col_palette)(n_fill))) +
  geom_stream(
    bw = 0.5,
    extra_span = 0.001,
    true_range = "none",
    position = position_nudge(y = 0)
  ) +
  new_scale_fill() +
  scale_fill_manual(values = sample(grDevices::colorRampPalette(col_palette)(n_fill))) +
  geom_stream(
    bw = 0.3,
    extra_span = 0.001,
    true_range = "none",
    position = position_nudge(y = -100)
  ) +
  new_scale_fill() +
  scale_fill_manual(values = sample(grDevices::colorRampPalette(col_palette)(n_fill))) +
  geom_stream(
    bw = 0.1,
    extra_span = 0.001,
    true_range = "none",
    position = position_nudge(y = -200)
  ) +
  new_scale_fill() +
  scale_fill_manual(values = sample(grDevices::colorRampPalette(col_palette)(n_fill))) +
  geom_stream(
    bw = 0.001,
    extra_span = 0.001,
    true_range = "none",
    position = position_nudge(y = -300)
  ) +
  new_scale_fill() +
  scale_fill_manual(values = sample(grDevices::colorRampPalette(col_palette)(n_fill))) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(
    clip = "on",
    xlim = c(30, 160),
    ylim = c(NA, 400)
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(
      fill = col_palette[1],
      colour = col_palette[1]
    )
  )

# Save
ggplot2::ggsave("Images/melting.png",
  width = 900, height = 900,
  units = "px"
)
