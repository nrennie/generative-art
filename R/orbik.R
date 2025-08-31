library(tidyverse)
library(elementalist)
library(PrettyCols)

# Parameters
n_x <- 5
n_y <- 5
x0 <- 0.5
y0 <- 0.5
buffer <- 0.2
overlap <- 0.05
col_palette <- PrettyColsPalettes[["Disco"]][[1]][1:4]
bg_col <- PrettyColsPalettes[["Disco"]][[1]][5]
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
  ) |>
  mutate(
    side = sample(c("x", "y"), n_x * n_y, replace = TRUE),
  ) |>
  mutate(
    temp_data = map(side, ~ {
      if (.x == "y") {
        list(
          xs1 = x0 - overlap,
          xs2 = x0 + 1 + overlap,
          ys1 = runif(1, y0 + buffer, y0 + 1 - buffer),
          ys2 = runif(1, y0 + buffer, y0 + 1 - buffer)
        )
      } else {
        list(
          xs1 = runif(1, x0 + buffer, x0 + 1 - buffer),
          xs2 = runif(1, x0 + buffer, x0 + 1 - buffer),
          ys1 = y0 - overlap,
          ys2 = y0 + 1 + overlap
        )
      }
    })
  ) |>
  unnest_wider(temp_data)

line_data <- plot_data |>
  pivot_longer(
    cols = c(xs1, xs2, ys1, ys2),
    names_to = c("coord", "num"),
    names_pattern = "([xy]s)(\\d+)",
    values_to = "value"
  ) |>
  pivot_wider(
    names_from = coord,
    values_from = value
  )

# Plot
ggplot() +
  geom_tile_theme(
    data = plot_data,
    mapping = aes(x = x, y = y, fill = fill_col),
    element = element_rect_round(unit(0.25, "snpc")),
    colour = bg_col,
    linewidth = 0,
    width = 1,
    height = 1
  ) +
  geom_line_theme(
    data = line_data,
    mapping = aes(
      x = xs,
      y = ys,
      group = id
    ),
    colour = bg_col,
    linewidth = 1
  ) +
  scale_fill_identity() +
  facet_wrap(~id, nrow = n_y, ncol = n_x) +
  coord_cartesian(clip = "off", xlim = c(0.5, 1.5), ylim = c(0.5, 1.5)) +
  theme(
    plot.margin = margin(2, 2, 0, 0),
    plot.background = element_rect(
      fill = bg_col, colour = bg_col
    ),
    panel.background = element_blank(),
    elementalist.geom_line = element_line_glow(amount = 5),
    strip.text = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.spacing = unit(0, "lines")
  )

# Save
ggplot2::ggsave("Images/orbik.png",
  width = 900, height = 900,
  units = "px"
)
