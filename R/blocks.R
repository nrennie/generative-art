library(tidyverse)
library(monochromeR)
library(patchwork)

# Parameters
col_palette <- c("#087E8B", "#88AA33", "#034C3C", "#320E3B", "#7E2395", "#D1236C")
bg_col <- "#2B3A50"

# First block
block_1 <- function(col_palette, bg_col, n_y = 10, margin = 0.05) {
  # Block params
  n_x <- length(col_palette)
  # Data
  plot_data <- expand.grid(y = 1:n_y, x = 1:n_x) |>
    as_tibble() |>
    select(x, y) |>
    mutate(
      fill = unlist(purrr::map(
        .x = col_palette,
        .f = ~ sample(monochromeR::generate_palette(.x,
          modification = "go_both_ways",
          n_colours = n_y
        ))
      ))
    )
  # Plot
  ggplot() +
    geom_rect(
      data = plot_data,
      mapping = aes(
        xmin = x + margin, xmax = x + 0.5,
        ymin = y, ymax = y + 0.5,
        fill = fill
      ),
    ) +
    scale_fill_identity() +
    scale_x_continuous(limits = c(1, n_x + 1)) +
    theme_void() +
    theme(
      plot.background = element_rect(
        fill = bg_col, colour = bg_col
      ),
      panel.background = element_rect(
        fill = bg_col, colour = bg_col
      ),
      plot.margin = margin(0, 0, 0, 0)
    )
}




# Second block
block_2 <- function(col_palette, bg_col, n_y = 12, margin = 0.05) {
  # Block params
  n_x <- length(col_palette)
  # Data
  plot_data <- expand.grid(y = 1:n_y, x = 1:n_x) |>
    as_tibble() |>
    select(x, y) |>
    mutate(
      fill1 = rep(col_palette, each = n_y),
      fill2 = rep(unlist(purrr::map(
        .x = col_palette,
        .f = ~ sample(monochromeR::generate_palette(.x,
          modification = "go_both_ways",
          n_colours = n_y
        ), size = 1)
      )), each = n_y)
    )
  # Plot
  ggplot() +
    geom_rect(
      data = plot_data,
      mapping = aes(
        xmin = x + margin, xmax = x + 0.5,
        ymin = y, ymax = y + 0.5, fill = fill1
      ),
    ) +
    geom_rect(
      data = plot_data,
      mapping = aes(
        xmin = x + 0.5, xmax = x + 1 - margin,
        ymin = y, ymax = y + 0.5, fill = fill2
      ),
    ) +
    scale_fill_identity() +
    scale_x_continuous(limits = c(1, n_x + 1)) +
    theme_void() +
    theme(
      plot.background = element_rect(
        fill = bg_col, colour = bg_col
      ),
      panel.background = element_rect(
        fill = bg_col, colour = bg_col
      ),
      plot.margin = margin(0, 0, 0, 0)
    )
}

# Third block
block_3 <- function(col_palette, bg_col, n_y = 4, margin = 0.05) {
  # Block params
  n_x <- length(col_palette)
  # Data
  plot_data <- expand.grid(y = 1:n_y, x = 1:n_x) |>
    as_tibble() |>
    select(x, y) |>
    mutate(
      fill = rep(purrr::map(
        .x = col_palette,
        .f = ~ sample(monochromeR::generate_palette(.x,
          modification = "go_both_ways",
          n_colours = n_y
        ), size = 4)
      ), each = n_y)
    ) |>
    unnest_wider(fill, names_sep = "")
  # Plot
  ggplot() +
    geom_rect(
      data = plot_data,
      mapping = aes(
        xmin = x + margin, xmax = x + 0.5,
        ymin = y + margin, ymax = y + 0.5, fill = fill1
      )
    ) +
    geom_rect(
      data = plot_data,
      mapping = aes(
        xmin = x + margin, xmax = x + 0.5,
        ymin = y + 0.5, ymax = y + 1 - margin, fill = fill2
      ),
    ) +
    geom_rect(
      data = plot_data,
      mapping = aes(
        xmin = x + 0.5, xmax = x + 1 - margin,
        ymin = y + 0.5, ymax = y + 1 - margin, fill = fill3
      ),
    ) +
    scale_fill_identity() +
    scale_x_continuous(limits = c(1, n_x + 1)) +
    theme_void() +
    theme(
      plot.background = element_rect(
        fill = bg_col, colour = bg_col
      ),
      panel.background = element_rect(
        fill = bg_col, colour = bg_col
      ),
      plot.margin = margin(0, 0, 0, 0)
    )
}



# Join together with patchwork
set.seed(1234)
b1 <- block_1(col_palette, bg_col, n_y = 8) + coord_flip() + scale_x_reverse()
b2 <- block_2(col_palette, bg_col) + coord_flip() + scale_x_reverse()
b3 <- block_2(col_palette, bg_col, n_y = 7) + coord_flip() + scale_x_reverse()
b4 <- block_3(col_palette, bg_col, n_y = 6) + coord_flip() + scale_x_reverse()
all_plots <- list(b2, b1, b3, b4)

wrap_plots(all_plots, nrow = 1) &
  theme(plot.margin = margin(0, 0, 0, 0))

width <- 900
ggplot2::ggsave("Images/blocks.png",
  height = width,
  width = length(all_plots) * width, units = "px"
)
