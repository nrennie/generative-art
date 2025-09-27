library(tidyverse)

# Parameters
main_col <- "white"
bg_col <- "black"
n <- 5
padding <- 0.2

# Functions
make_polygon <- function(x0, grp) {
  polygon_data <- data.frame(
    x = c(0, 1, 1, x0, 0),
    y = c(0, 0, 1, 1, 0),
    grp = rep(grp, 5)
  )
  return(polygon_data)
}

# Data
plot_data <- map(
  .x = seq_len(n),
  .f = ~ make_polygon(x0 = 1 - seq(0, 1, length.out = n)[.x], grp = .x)
) |>
  bind_rows()

# Plot
ggplot(
  data = plot_data
) +
  geom_polygon(
    mapping = aes(x = x, y = y, group = grp),
    fill = main_col
  ) +
  facet_wrap(~grp, nrow = 1) +
  scale_x_continuous(expand = expansion(mult = padding)) +
  scale_y_continuous(expand = expansion(mult = padding)) +
  theme_void() +
  theme(
    panel.background = element_rect(
      fill = bg_col, colour = bg_col
    ),
    plot.background = element_rect(
      fill = main_col, colour = main_col
    ),
    strip.text = element_blank(),
    plot.margin = margin(10, 10, 10, 10),
    panel.spacing = unit(10, "pt")
  )

# Save
ggplot2::ggsave("Images/quadtri.png",
  width = 900 * n, height = 900,
  units = "px"
)
