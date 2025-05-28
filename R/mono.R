library(tidyverse)
library(plotcolr)

mono <- function(
    n_x = 8,
    n_y = 8,
    max_diff = 5,
    base_col = "#C84630",
    bg_col = "white",
    linewidth = 2,
    s = 1234) {
  # Data
  grid_data <- withr::with_seed(
    seed = s,
    code = {
      base_rgb <- col2rgb(base_col)
      base_hsl <- rgb2hsl(base_rgb)
      new_h <- (base_hsl["h"] +
        (runif(n_x * n_y, -max_diff * 2.5, max_diff * 2.5) + 360)) %% 360
      new_s <- pmin(pmax((100 * base_hsl["s"] +
        runif(n_x * n_y, -max_diff, max_diff)) / 100, 0), 1)
      new_l <- pmin(pmax((100 * base_hsl["l"] +
        runif(n_x * n_y, -max_diff, max_diff)) / 100, 0), 1)
      col_grid <- purrr::map(
        .x = 1:(n_x * n_y),
        .f = ~ hsl2rgb(c(new_h[.x], new_s[.x], new_l[.x]))
      ) |>
        bind_rows() |>
        as.matrix()
      grid_data <- expand.grid(x = 1:n_x, y = 1:n_y) |>
        as_tibble() |>
        mutate(col = rgb(col_grid, maxColorValue = 255))
      grid_data
    }
  )
  # Plot
  p <- ggplot(
    data = grid_data,
    mapping = aes(x = x, y = y, fill = col)
  ) +
    geom_tile(
      colour = bg_col, linewidth = linewidth
    ) +
    scale_fill_identity() +
    coord_cartesian(expand = FALSE) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = bg_col, colour = bg_col),
      plot.margin = margin(2, 2, 2, 2)
    )
  return(p)
}


# Save
mono()
ggsave("Images/mono1.png", width = 5, height = 5)

mono(
  n_x = 20, n_y = 20, linewidth = 1, base_col = "#446DF6",
  bg_col = "#04154E", max_diff = 15
)
ggsave("Images/mono2.png", width = 5, height = 5)
