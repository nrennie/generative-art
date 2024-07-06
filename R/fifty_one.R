make_square <- function(x0, y0, buffer, col_palette, bg_col) {
  # draw square
  x_vals <- c(x0, x0 + 1, x0 + 1, x0, x0)
  y_vals <- c(y0, y0, y0 + 1, y0 + 1, y0)
  square_m <- matrix(c(x_vals, y_vals), byrow = FALSE, ncol = 2)
  square_sf <- sf::st_polygon(list(square_m))
  # draw line
  side <- sample(c("x", "y"), 1)
  if (side == "y") {
    xs1 <- x0
    xs2 <- x0 + 1
    ys1 <- runif(1, y0 + buffer, y0 + 1 - buffer)
    ys2 <- runif(1, y0 + buffer, y0 + 1 - buffer)
  } else {
    xs1 <- runif(1, x0 + buffer, x0 + 1 - buffer)
    xs2 <- runif(1, x0 + buffer, x0 + 1 - buffer)
    ys1 <- y0
    ys2 <- y0 + 1
  }
  line_sf <- sf::st_linestring(
    matrix(c(xs1, ys1, xs2, ys2), byrow = T, ncol = 2)
  )
  # split
  split_sf <- lwgeom::st_split(square_sf, line_sf) |>
    sf::st_collection_extract(c("POLYGON")) |>
    sf::st_as_sf()
  split_sf$col <- c(bg_col, sample(col_palette, size = 1))
  return(split_sf)
}

fifty_one <- function(
    n_x = 4,
    n_y = 4,
    buffer = 0.1,
    col_palette = c("#8EA604", "#F5BB00", "#EC9F05", "#D76A03", "#BF3100"),
    bg_col = "#FAFAFA",
    s = 1234) {
  # arg checking
  if (buffer < 0 | buffer > 0.5) {
    stop("'buffer' needs to be between 0 and 0.5")
  }
  set.seed(s)
  # data generation
  plot_grid <- expand.grid(x = 1:n_x, y = 1:n_y)
  all_data <- purrr::map2(
    .x = plot_grid$x,
    .y = plot_grid$y,
    .f = ~ make_square(
      x0 = .x,
      y0 = .y,
      buffer = buffer,
      col_palette = col_palette,
      bg_col = bg_col
    )
  ) |>
    dplyr::bind_rows()
  # plot
  ggplot2::ggplot(data = all_data) +
    ggplot2::geom_sf(
      mapping = ggplot2::aes(fill = col),
      colour = NA
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(
        fill = bg_col, colour = bg_col
      ),
      panel.background = ggplot2::element_rect(
        fill = bg_col, colour = bg_col
      ),
      plot.margin = ggplot2::margin(0, 0, 0, 0)
    )
}

fifty_one(n_x = 15, n_y = 15, buffer = 0.15, s = 2024)
ggplot2::ggsave("Images/fifty_one_r.png", width = 900, height = 900, units = "px")
