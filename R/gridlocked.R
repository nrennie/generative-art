make_square <- function(x0, y0, col_palette) {
  # draw square
  x_vals <- c(x0, x0 + 1, x0 + 1, x0, x0)
  y_vals <- c(y0, y0, y0 + 1, y0 + 1, y0)
  square_m <- matrix(c(x_vals, y_vals), byrow = FALSE, ncol = 2)
  square_sf <- sf::st_polygon(list(square_m))
  # draw line
  options_vec <- c("A", "B", "C")
  option_choose <- sample(options_vec, 1)
  if (option_choose %in% c("A", "C")) {
    xs1 <- x0
    xs2 <- x0 + 1
    ys1 <- y0 + 0.5
    ys2 <- y0 + 0.5
    line_sf1 <- sf::st_linestring(
      matrix(c(xs1, ys1, xs2, ys2), byrow = T, ncol = 2)
    )
  }
  if (option_choose %in% c("B", "C")) {
    xs1 <- x0 + 0.5
    xs2 <- x0 + 0.5
    ys1 <- y0
    ys2 <- y0 + 1
    line_sf2 <- sf::st_linestring(
      matrix(c(xs1, ys1, xs2, ys2), byrow = T, ncol = 2)
    )
  }
  # split
  div_square_sf <- square_sf
  if (exists("line_sf1")) {
    div_square_sf <- lwgeom::st_split(div_square_sf, line_sf1) |>
      sf::st_collection_extract(c("POLYGON")) |>
      sf::st_as_sf()
  }
  if (exists("line_sf2")) {
    div_square_sf <- lwgeom::st_split(div_square_sf, line_sf2) |>
      sf::st_collection_extract(c("POLYGON")) |>
      sf::st_as_sf()
  }
  
  div_square_sf$col <- sample(col_palette, size = nrow(div_square_sf))
  return(list(div_square_sf = div_square_sf, square_sf = square_sf))
}

gridlocked <- function(
    n_x = 4,
    n_y = 4,
    col_palette = c("#8EA604", "#F5BB00", "#EC9F05", "#D76A03", "#BF3100"),
    bg_col = "#FAFAFA",
    linewidth = 3,
    s = 1234) {
  set.seed(s)
  # data generation
  plot_grid <- expand.grid(x = 1:n_x, y = 1:n_y)
  all_data <- purrr::map2(
    .x = plot_grid$x,
    .y = plot_grid$y,
    .f = ~ make_square(
      x0 = .x,
      y0 = .y,
      col_palette = col_palette
    )
  )
  small_data <-  purrr::map(.x = all_data, .f = ~.x[[1]]) |> 
    dplyr::bind_rows()
  big_data <- purrr::map(.x = all_data, .f = ~.x[[2]]) |>
    sf::st_sfc() |> 
    sf::st_sf()
  # plot
  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = small_data,
      mapping = ggplot2::aes(fill = col),
      linewidth = linewidth,
      colour = NA
    ) +
    ggplot2::geom_sf(
      data = big_data,
      colour = bg_col,
      linewidth = linewidth,
      fill = NA
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(0, 0.1)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(0, 0.1)) +
    ggplot2::scale_fill_identity() +
    ggplot2::coord_sf() +
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


gridlocked(
  n_x = 5,
  n_y = 5,
  col_palette = c("#496f84", "#e3dfaf", "#af1328", "#686144", 
                  "#0b3768", "#010310", "#b0878d", "#e85826"),
  bg_col = "#e4d8c8",
  s = 2025
)
ggplot2::ggsave("Images/gridlocked.png", width = 900, height = 900, units = "px")






