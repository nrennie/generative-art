library(tidyverse)
library(ggforce)

n_x <- 3
n_y <- 3
d <- 10
R <- 4
r <- 1
linewidth <- 0.5
line_col <- "white"
bg_col <- "grey20"
col1 <- ""

data.frame(
  
)


ggplot(iris, aes(Sepal.Length, Sepal.Width)) + 
  geom_voronoi_tile(aes(fill = Species, group = -1L), colour = "black") 

ggplot() +
  annotate("text", x = 0, y = 0, label = "here") +
  ggplot2::coord_fixed() +
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.position = "none",
    strip.text = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
    panel.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
    plot.margin = margin(0, 0, 0, 0)
  )



# one tile
porto_tile <- function(data = NULL, bg_col = ) {
  g <- ggplot(data) +
    annotate("text", x = 0, y = 0, label = "here") +
    ggplot2::coord_fixed() +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      strip.text = ggplot2::element_blank(),
      plot.margin = margin(0, 0, 0, 0)
    )
  if (!is.null(data)) {
    g <- g + facet_grid(b ~ a)
  }
  return(g)
}

porto <- function(n_x = 5, n_y = 5) {
  grid_data <- expand.grid(a = factor(1:n_x), b = factor(1:n_y))
  p <- porto_tile(data = grid_data)
  return(p)
}

porto()














