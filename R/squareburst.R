library(tidyverse)
library(scales)

n_x <- 20
n_y <- 20
max_jitter <- 0.6
min_size <- 0
max_size <- 0.85
linewidth <- 0.1
alpha <- 0.7
col_palette <- PrettyCols::prettycols("Bright")
line_col <- "#DDC4E8"
bg_col <- "#24112C"
s <- 1234

col_palette <- "white"
line_col <- "black"
bg_col <- "white"
alpha <- 1
linewidth <- 0.3

set.seed(s)
plot_data <- expand.grid(x = 1:n_x, y = 1:n_y) |> 
  as_tibble() |> 
  mutate(
    dist = sqrt((y - mean(1:n_y))^2 + (x - mean(1:n_x))^2),
    dist = rescale(-dist, to = c(min_size, max_size))
  ) |> 
  mutate(
    col = sample(grDevices::colorRampPalette(col_palette)(n_x * n_y))
  ) |> 
  mutate(
    x = x + runif(n_x * n_y, -max_jitter, max_jitter),
    y = y + runif(n_x * n_y, -max_jitter, max_jitter)
  )
plot_data <- plot_data[sample(1:nrow(plot_data)),]

ggplot() +
  geom_rect(
    data = plot_data,
    mapping = aes(xmin = x - dist, xmax = x + dist,
                  ymin = y - dist, ymax = y + dist),
    fill = "white",
    colour = "white",
    linewidth = linewidth
  ) +
  geom_rect(
    data = plot_data,
    mapping = aes(xmin = x - dist, xmax = x + dist,
                  ymin = y - dist, ymax = y + dist,
                  fill = col),
    colour = line_col,
    alpha = alpha,
    linewidth = linewidth
  ) +
  scale_fill_identity() +
  coord_fixed(expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    plot.margin = margin(5, 5, 5, 5)
  )

ggsave("Images/squareburst.png",
                width = 1200, height = 1200,
                units = "px"
)



ggsave("Images/squareburst_bw.png",
       width = 1200, height = 1200,
       units = "px"
)
  



