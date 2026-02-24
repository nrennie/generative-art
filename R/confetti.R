
library(tidyverse)
library(ggforce)
library(PrettyCols)

s <- 2026
n <- 500
x0 <- 0
y0 <- 0
r <- 2
max_r <- 0.3
col_palette <- prettycols("Coast")
bg_col <- prettycols("Coast")[3]


set.seed(s)

# generate points
n_points <- ceiling(n * ((2*r)^2) / (pi*(r^2)))

c_data <- data.frame(
  x = x0 + runif(n_points, -r, r),
  y = y0 + runif(n_points, -r, r)
) |> 
  as_tibble() |> 
  mutate(dist = sqrt((x - x0)^2 + (y - y0)^ 2)) |> 
  filter(dist <= r)


c_data$c_r <- runif(nrow(c_data), 0, max_r)
c_data$c_alpha <- runif(nrow(c_data), 0.2, 0.9)
c_data$col <- sample(grDevices::colorRampPalette(col_palette)(nrow(c_data)))

ggplot(
  data = c_data
) +
  geom_circle(
    mapping = aes(x0 = x, y0 = y, r = c_r, fill= col,
                  alpha = c_alpha),
    colour = "transparent"
  ) +
  scale_x_continuous(limits = c(-r - max_r, r + max_r)) +
  scale_y_continuous(limits = c(-r - max_r, r + max_r)) +
  scale_fill_identity() +
  scale_alpha_identity() +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg_col, colour = bg_col)
  )

ggplot2::ggsave("Images/confetti.png",
                width = 900, height = 900,
                units = "px"
)












