library(tidyverse)
library(scales)

# Functions
generate_x_values <- function(min_val, max_val, y, col_palette) {
  n_x <- sample(min_val:max_val, 1)
  x_vals <- c(0, cumsum(runif(n_x)))
  x_vals <- x_vals / max(x_vals)
  plot_data <- data.frame(
    xmin = x_vals[1:n_x],
    xmax = x_vals[2:(n_x + 1)],
    ymin = rep(y, n_x),
    ymax = rep(y + 1, n_x),
    col = rep(col_palette, ceiling(n_x / length(col_palette)))[1:n_x]
  )
}

sedigraph <- function(
    n_x_min = 5,
    n_x_max = 10,
    n_dots = 100,
    col_palette = c(
      "#a0462c", "#c38c39", "#d3d4cf",
      "#8a3e4c", "#3d3c41", "#a93830"
    ),
    bg_col = "#28272A",
    dot_size = 0.5,
    s = 1234) {
  # Data
  set.seed(s)
  plot_data <- generate_x_values(n_x_min, n_x_max, 1, col_palette)
  circle_data <- data.frame(
    x = rescale(-1 * abs(rnorm(n_dots, 0, 0.8)), to = c(1, 2)),
    y = runif(n_dots)
  )
  # Plot
  ggplot() +
    geom_rect(
      data = plot_data,
      mapping = aes(
        xmin = ymin, xmax = ymax,
        ymin = xmin, ymax = xmax,
        fill = col
      ),
      colour = bg_col,
      linewidth = 1,
    ) +
    geom_point(
      data = circle_data,
      mapping = aes(x = x, y = y),
      colour = bg_col,
      size = dot_size
    ) +
    scale_fill_identity() +
    scale_x_reverse() +
    coord_flip(expand = FALSE) +
    theme_void()
}


# Save
sedigraph(s = 2025,
          n_x_min = 6,
          n_x_max = 12,
          col_palette = c("#99e6f0", "#cc444c", "#feef97", "#debe44"),
          bg_col = "#e9f1f7",
          n_dots = 50000, dot_size = 0.05)



ggplot2::ggsave("Images/sedigraph.png",
  width = 5, height = 5, units = "in"
)
