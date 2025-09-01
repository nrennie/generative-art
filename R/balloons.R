library(tidyverse)
library(PrettyCols)
library(ggforce)
library(packcircles)
library(scales)

# Parameters
n <- 200
max_r <- 0.1
buffer <- 0.01
crop <- 0.5
col_palette <- PrettyColsPalettes[["Coast"]][[1]]
bg_col <- "#EEEBE7"
s <- 2025

# Data
set.seed(s)
circle_data <- data.frame(
  areas = runif(n, 0, max_r)
) |>
  circleProgressiveLayout() |>
  mutate(
    r = radius - buffer,
    fill_col = sample(grDevices::colorRampPalette(col_palette)(n)),
    alpha = rescale(y, to = c(0, 1)) + sample(c(0, 1), size = n, replace = T, prob = c(0.9, 0.1)),
    alpha = pmin(alpha, 1)
  )

max_x <- max(c(
  max(abs(circle_data$x + circle_data$radius)),
  max(abs(circle_data$x - circle_data$radius))
))
max_y <- max(c(
  max(abs(circle_data$y + circle_data$radius)),
  max(abs(circle_data$y - circle_data$radius))
))

biggest <- max(c(max_x, max_y))
x_lim <- c(-biggest + crop, biggest - crop)
y_lim <- c(-biggest, biggest - (2* crop))

# Plot
ggplot() +
  geom_circle(
    data = circle_data,
    mapping = aes(
      x0 = x, y0 = y, r = r, fill = fill_col,
      alpha = alpha
    ),
    colour = "transparent"
  ) +
  scale_fill_identity() +
  scale_alpha_identity() +
  coord_fixed(
    clip = "on",
    expand = FALSE,
    xlim = x_lim,
    ylim = y_lim
  ) +
  theme_void() +
  theme(
    strip.text = element_blank(),
    plot.margin = margin(0, 0, 0, 0),
    plot.background = element_rect(
      fill = bg_col, colour = bg_col
    ),
    panel.background = element_rect(
      fill = bg_col, colour = bg_col
    )
  )

# Save
ggplot2::ggsave("Images/balloons.png",
  width = 900, height = 900,
  units = "px"
)
