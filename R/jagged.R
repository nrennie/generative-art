library(interp)
library(tidyverse)

jagged <- function(n = 100,
                   col_palette = PrettyCols::prettycols("Coast"),
                   random = TRUE,
                   s = 2025) {
  # Data
  set.seed(s)
  x <- c(runif(n), 0, 0, 1, 1)
  y <- c(runif(n), 0, 1, 0, 1)
  plot_data <- triangles(tri.mesh(x = x, y = y)) |>
    as_tibble() |>
    mutate(i = 1:n()) |>
    rowwise() |>
    mutate(tri = list(
      tibble(
        x = x[c(node1, node2, node3, node1)],
        y = y[c(node1, node2, node3, node1)]
      )
    )) |>
    unnest("tri")

  # Plot
  g <- ggplot(data = plot_data) +
    geom_polygon(aes(x = x, y = y, group = i, fill = i),
      show.legend = FALSE,
      colour = NA
    ) +
    coord_cartesian(expand = FALSE, clip = "off") +
    theme_void() +
    theme(
      plot.margin = margin(-10, -10, -10, -10)
    )
  if (random) {
    new_palette <- grDevices::colorRampPalette(col_palette)(max(plot_data$i))
    new_palette <- sample(new_palette)
    g <- g +
      scale_fill_gradientn(colours = new_palette)
  } else {
    g <- g +
      scale_fill_gradientn(colours = col_palette)
  }
  g_3d <- rayshader::plot_gg(g,
    width = 4,
    height = 4,
    fov = 0,
    theta = 0,
    phi = 90,
    shadow_intensity = 0.1,
    sunangle = 315,
    scale = 200,
    preview = TRUE
  )
  return(g_3d)
}

# Examples
png("Images/jagged1.png", width = 4, height = 4, units = "in", res = 300)
jagged()
dev.off()

set.seed(1234)
cp <- sample(c(PrettyCols::prettycols("Velvet"), rep("black", 6)))
png("Images/jagged2.png", width = 4, height = 4, units = "in", res = 300)
jagged(n = 200, 
       col_palette = cp, 
       s = 2025)
dev.off()
