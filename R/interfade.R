library(tidyverse)
library(PrettyCols)


col_palette <- PrettyColsPalettes[["Bright"]][[1]]
contrast_col <- PrettyColsPalettes[["Relax"]][[1]][3]
bg_col <- "#fafafa"
min_alpha <- 0.05
max_alpha <- 0.95
alpha_levels <- 5
width <- 0.4

hor_data <- data.frame(
  y = seq_len(length(col_palette)),
  fill = col_palette
)
ver_data <- data.frame(
  x = seq_len(alpha_levels),
  alpha = seq(min_alpha, max_alpha, length.out = alpha_levels)
)

ggplot() +
  geom_rect(
    data = hor_data,
    mapping = aes(xmin = 1 - 2*width, xmax = alpha_levels + 2*width,
                  ymin = y - width, ymax = y + width,
                  fill = fill)
  ) +
  geom_rect(
    data = ver_data,
    mapping = aes(xmin = x - width, xmax = x + width, 
                  ymin = 1 - 2*width, ymax = length(col_palette) + 2*width,
                  alpha = alpha),
    fill = contrast_col
  ) +
  scale_fill_identity() +
  scale_alpha_identity() +
  theme_void() +
  theme(
    plot.background = element_rect(
      fill = bg_col, colour = bg_col
    )
  )

ggplot2::ggsave("Images/interfade.png",
                width = 4, height = 4, units = "in"
)

