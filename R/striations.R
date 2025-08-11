library(tidyverse)

# Functions
generate_x_values <- function(min_val, max_val, y) {
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

striations <- function(
    n_x_min = 20,
    n_x_max = 45,
    n_y = 5,
    col_palette = c(
      "#a0462c", "#c38c39", "#d3d4cf",
      "#8a3e4c", "#3d3c41", "#a93830"
    ),
    s = 1234) {
  # Data
  set.seed(s)
  plot_data <- purrr::map(
    .x = 1:n_y,
    .f = ~ generate_x_values(n_x_min, n_x_max, .x)
  ) |>
    list_rbind()

  # Plot
  ggplot() +
    geom_rect(
      data = plot_data,
      mapping = aes(
        xmin = xmin, xmax = xmax,
        ymin = ymin, ymax = ymax,
        fill = col
      )
    ) +
    scale_fill_identity() +
    coord_cartesian(expand = FALSE) +
    theme_void()
}


# Save
striations(s = 2025)
ggplot2::ggsave("Images/striations.png", 
  width = 4, height = 4, units = "in"
)
