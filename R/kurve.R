library(tidyverse)
library(PrettyCols)
library(packcircles)
library(ggforce)

# Parameters
n_x <- 3
n_y <- 3
n <- 20
max_r <- 0.25
buffer <- 0.01
col_palette <- PrettyColsPalettes[["Velvet"]][[1]]
bg_col <- "grey5"
s <- 1234

# Data
set.seed(s)
plot_data <- data.frame(
  x = rep(1, n_x * n_y),
  y = rep(1, n_x * n_y)
) |>
  as_tibble() |>
  mutate(
    id = row_number(),
    fill_col = sample(grDevices::colorRampPalette(col_palette)(n_x * n_y))
  )

# Circle data
circle_data <- data.frame(
  areas = runif(n * n_x * n_y, 0, max_r),
  grp = rep(1:(n_x * n_y), each = n)
) |>
  group_by(grp) |>
  group_split() |>
  map(.f = ~ circleProgressiveLayout(.x)) |>
  bind_rows() |>
  mutate(
    id = rep(1:(n_x * n_y), each = n),
    x = x + 1, y = y + 1,
    r = radius - buffer,
    fill_col = sample(grDevices::colorRampPalette(col_palette)(n * n_x * n_y)),
    alpha = runif(n * n_x * n_y, 0.8, 1)
  )

# Plot
ggplot() +
  geom_raster(
    data = plot_data,
    mapping = aes(x = x, y = y, fill = fill_col)
  ) +
  geom_circle(
    data = circle_data,
    mapping = aes(
      x0 = x, y0 = y, r = r
    ),
    fill = "white",
    colour = bg_col,
    linewidth = 0.3
  ) +
  geom_circle(
    data = circle_data,
    mapping = aes(
      x0 = x, y0 = y, r = r, fill = fill_col,
      alpha = alpha
    ),
    colour = bg_col,
    linewidth = 0.3
  ) +
  facet_wrap(~id, ncol = n_x, nrow = n_y) +
  scale_fill_identity() +
  scale_alpha_identity() +
  coord_cartesian(
    clip = "on",
    expand = FALSE,
    xlim = c(0.5, 1.5),
    ylim = c(0.5, 1.5)
  ) +
  theme_void() +
  theme(
    strip.text = element_blank(),
    plot.margin = margin(5, 5, 5, 5),
    plot.background = element_rect(
      fill = bg_col, colour = bg_col
    ),
    panel.background = element_rect(
      fill = bg_col, colour = bg_col
    ),
    aspect.ratio = 1,
    panel.spacing = unit(0.3, "lines")
  )

# Save
ggplot2::ggsave("Images/kurve.png",
  width = 900, height = 900,
  units = "px"
)
