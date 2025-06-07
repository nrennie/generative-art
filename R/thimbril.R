# Packages ----------------------------------------------------------------

library(tidyverse)
library(ggforce)
library(PrettyCols)


# Parameters --------------------------------------------------------------

n <- 7
nC <- 10
r <- 0.4
colPalette <- prettycols("Velvet")
bgCol <- "#FAFAFA"
s <- 2025


# Create data -------------------------------------------------------------

set.seed(s)
circle_data <- data.frame(
  x = runif(n * nC, 1, n),
  y = rep(1:n, each = nC)
)
circle_data <- circle_data[sample(1:(n * nC)), ]
new_palette <- grDevices::colorRampPalette(colPalette)(n * nC)
circle_data$col <- sample(new_palette)

xmin <- circle_data |> 
  group_by(y) |> 
  summarise(xmin = min(x)) |> 
  arrange(y) |> 
  pull(xmin)
xmax <- circle_data |> 
  group_by(y) |> 
  summarise(xmax = max(x)) |> 
  arrange(y) |> 
  pull(xmax)

rect_data <- data.frame(
  xmin = xmin,
  xmax = xmax,
  ymin = (1:n) - r,
  ymax = (1:n) + r,
  col = sample(new_palette, n)
)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_rect(
    data = rect_data,
    mapping = aes(
      xmin = xmin, xmax = xmax,
      ymin = ymin, ymax = ymax,
      fill = col
    ),
    colour = "transparent",
    alpha = 0.5
  ) +
  geom_circle(
    data = circle_data,
    mapping = aes(x0 = x, y0 = y, r = r),
    fill = "white",
    colour = "transparent",
    alpha = 1
  ) +
  geom_circle(
    data = circle_data,
    mapping = aes(x0 = x, y0 = y, r = r, fill = col),
    colour = "transparent",
    alpha = 0.4
  ) +
  scale_x_continuous(limits = c(1 - r, n + r)) +
  scale_y_continuous(limits = c(1 - r, n + r)) +
  scale_fill_identity() +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(
      fill = bgCol, colour = bgCol
    )
  )


# Save --------------------------------------------------------------------

ggsave("Images/thimbril.png", width = 5, height = 5, bg = bgCol)


