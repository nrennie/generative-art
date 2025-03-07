# Half circle  ------------------------------------------------------------

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

# Params
n_x <- 12
n_y <- 32 # 2 * (2/1.5) * n_x for square(ish)
col_palette <- PrettyCols::prettycols("Celestial")[1:5]
bg_col <- PrettyCols::prettycols("Celestial")[6]
s <- 1234

set.seed(s)

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
col_choices <- colorRampPalette(col_palette)(n_y)
choose_colours <- sample(col_choices, size = n_y)

# Plot
p <- ggplot2::ggplot() +
  ggplot2::geom_polygon(
    data = final_data,
    mapping = ggplot2::aes(x = x, y = y, group = grp, fill = col),
    alpha = 0.8
  ) +
  ggplot2::scale_fill_manual(values = choose_colours) +
  ggplot2::coord_fixed(expand = FALSE) +
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.position = "none",
    plot.margin = ggplot2::margin(1, 1, 1, 1),
    plot.background = ggplot2::element_rect(
      fill = bg_col,
      colour = bg_col
    )
  )


# Save
ggplot2::ggsave("Images/crochet.png", p,
  width = 4, height = 4, units = "in", bg = bg_col
)
