semi <- function(x0, y0) {
  semi_data <- data.frame(
    theta = seq(
      from = 0,
      to = pi,
      length.out = 1000
    )
  ) |>
    dplyr::mutate(
      x = cos(theta),
      y = sin(theta)
    )
  x <- c(semi_data$x, -1, -1, 1, 1) + x0
  y <- c(semi_data$y, 0, -0.5, -0.5, 0) + y0
  plot_data <- data.frame(x = x, y = y)
  return(plot_data)
}

under_semi <- function(x0, y0) {
  semi_data <- data.frame(
    theta = seq(
      from = pi,
      to = 2 * pi,
      length.out = 1000
    )
  ) |>
    dplyr::mutate(
      x = cos(theta),
      y = sin(theta)
    )
  x <- c(semi_data$x, 1, 1, -1, -1) + x0
  y <- c(semi_data$y, 0, 0.5, 0.5, 0) + y0
  plot_data <- data.frame(x = x, y = y)
  return(plot_data)
}

crochet <- function(
    n_x = 4,
    n_y = 4,
    col_palette = c("#A053A1", "#DB778F", "#E69F52", "#09A39A", "#5869C7"),
    bg_col = "#004B67",
    s = 1234) {
  # Process params
  n_y <- n_y * 2

  # Data
  grid_data1 <- expand.grid(
    x = seq(0, by = 4, length.out = n_x),
    y = seq(0, by = 1.5, length.out = n_y)
  )
  plot_data1 <- purrr::map2_df(
    .x = grid_data1$x,
    .y = grid_data1$y,
    .f = ~ under_semi(x0 = .x, y0 = .y),
    .id = "grp"
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(grp = paste0(grp, "-1"))

  grid_data2 <- expand.grid(
    x = seq(0, by = 4, length.out = n_x) - 2,
    y = seq(0, by = 1.5, length.out = n_y) + 0.75
  )
  plot_data2 <- purrr::map2_df(
    .x = grid_data2$x,
    .y = grid_data2$y,
    .f = ~ semi(x0 = .x, y0 = .y),
    .id = "grp"
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(grp = paste0(grp, "-2"))
  plot_data <- rbind(plot_data1, plot_data2)
  plot_levels <- plot_data |>
    dplyr::select(grp) |>
    dplyr::distinct() |>
    tidyr::separate_wider_delim(grp,
      delim = "-",
      names = c("a", "b"), cols_remove = FALSE
    ) |>
    dplyr::mutate(a = as.numeric(a), b = as.numeric(b)) |>
    dplyr::arrange(a, b) |>
    dplyr::select(-c(a, b)) |>
    dplyr::pull(grp)
  final_data <- plot_data |>
    dplyr::mutate(grp = factor(grp, levels = plot_levels)) |>
    dplyr::arrange(grp) |>
    dplyr::mutate(col = factor(rep(1:n_y, each = nrow(plot_data) / n_y)))

  # Colours
  choose_colours <- withr::with_seed(
    seed = s,
    code = {
      col_choices <- colorRampPalette(col_palette)(n_y)
      choose_colours <- sample(col_choices, size = n_y)
      choose_colours
    }
  )
  names(choose_colours) <- 1:n_y
  bottom_col <- head(choose_colours, 1)
  top_col <- tail(choose_colours, 1)

  # Plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = final_data,
      mapping = ggplot2::aes(x = x, y = y, group = grp, fill = col)
    ) +
    ggplot2::annotate(
      "rect",
      xmin = min(plot_data$x), xmax = max(plot_data$x),
      ymin = 0.25, ymax = 0.5, fill = bottom_col
    ) +
    ggplot2::annotate(
      "rect",
      xmin = min(plot_data$x), xmax = max(plot_data$x),
      ymin = (floor(max(plot_data$y)) - 1.25), ymax = (floor(max(plot_data$y)) - 1),
      fill = top_col
    ) +
    ggplot2::scale_fill_manual(values = choose_colours) +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::scale_y_continuous(limits = c(0.24, (floor(max(plot_data$y)) - 0.99))) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      plot.margin = ggplot2::margin(3, 3, 3, 3),
      plot.background = ggplot2::element_rect(
        fill = bg_col,
        colour = bg_col
      )
    )
  return(p)
}


# Save images -------------------------------------------------------------

p <- crochet(n_x = 12, n_y = 17) # n_y = 1 + ((2/1.5) * n_x) for square-ish
ggplot2::ggsave("Images/crochet1.png", p,
  width = 4, height = 4, units = "in", bg = "#004B67"
)

p <- crochet(
  n_x = 24, n_y = 33,
  col_palette = PrettyCols::prettycols("Beach"),
  bg_col = "#2C1125"
)
ggplot2::ggsave("Images/crochet2.png", p,
  width = 4, height = 4, units = "in", bg = "#2C1125"
)
