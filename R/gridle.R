library(ggplot2)
library(ggforce)

gridle <- function(
    n_x = 10,
    n_y = 10,
    col_palette = PrettyCols::prettycols("Celestial"),
    bg_col = "#FAFAFA",
    min_size = 0.1,
    max_size = 0.45,
    alpha = 0.7,
    interpolate = TRUE,
    s = 1234) {
  # Data
  set.seed(s)
  plot_data <- data.frame(
    x = rep(seq_len(n_x), each = n_y) + runif(n_x * n_y),
    y = rep(seq_len(n_y), times = n_x) + runif(n_x * n_y),
    size = runif(n_x * n_y, min_size, max_size)
  )
  if (interpolate) {
    plot_data$cols <- sample(grDevices::colorRampPalette(col_palette)(n_x * n_y))
  } else {
    plot_data$cols <- sample(col_palette, size = n_x * n_y, replace = TRUE)
  }

  # Plot
  g <- ggplot() +
    geom_circle(
      data = plot_data,
      mapping = aes(x0 = x, y0 = y, r = size),
      fill = "white",
      colour = "white"
    ) +
    geom_circle(
      data = plot_data,
      mapping = aes(x0 = x, y0 = y, r = size, fill = cols, colour = cols),
      alpha = alpha
    ) +
    scale_fill_identity() +
    scale_colour_identity() +
    coord_fixed() +
    theme_void() +
    theme(
      plot.background = element_rect(
        fill = bg_col, colour = bg_col
      )
    )

  return(g)
}

gridle()
ggsave("Images/gridle_1.png", height = 4, width = 4)

gridle(
  n_x = 50,
  n_y = 50, alpha = 0.3,
  bg_col = PrettyCols::prettycols("Autumn")[1],
  col_palette = PrettyCols::prettycols("Autumn")[2:12]
) +
  coord_fixed(expand = FALSE)
ggsave("Images/gridle_2.png", height = 4, width = 4)

gridle(
  n_x = 20,
  n_y = 20,
  bg_col = "black",
  col_palette = PrettyCols::prettycols("Neon")
)
ggsave("Images/gridle_3.png", height = 4, width = 4)
