library(tidyverse)
library(ggfx)
library(ggforce)

bg_col <- "#151D28"
col_palette <- PrettyCols::prettycols("Lucent")
n <- 21
layers <- 4
s <- 1234
max_var <- 0.35
min_r <- 0.05
max_r <- 0.08

set.seed(s)
plot_data <- data.frame(
  x = runif(n * layers, -1, 1),
  y = tanh(rnorm(n * layers, mean = 0, sd = max_var)),
  layer = rep(1:layers, each = n),
  col = sample(grDevices::colorRampPalette(col_palette)(n * layers))
) |>
  mutate(r = scales::rescale(layer, to = c(max_r, min_r))) |>
  arrange(desc(layer))

sigma_values <- rev(seq(0.3, 5, length.out = layers))

g <- ggplot() +
  scale_alpha(range = c(0.3, 1)) +
  scale_fill_identity() +
  scale_x_continuous(limits = c(-1 - max_r, 1 + max_r)) +
  scale_y_continuous(limits = c(-1 - max_r, 1 + max_r)) +
  coord_fixed(expand = FALSE, clip = "off") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg_col, colour = bg_col)
  )

g_list <- purrr::map(
  .x = 1:layers,
  .f = ~ with_blur(geom_circle(
    data = filter(plot_data, layer == .x),
    mapping = aes(x0 = x, y0 = y, r = r, fill = col, alpha = layer),
    colour = "transparent"
  ), sigma = sigma_values[.x], stack = TRUE)
)

p <- g + g_list

ggplot2::ggsave("Images/nightlights.png", p,
  width = 900, height = 900,
  units = "px", bg = bg_col
)
