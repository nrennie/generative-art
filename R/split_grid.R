library(aRt)
library(ggplot2)

col_palette <- c(
  "grey25", "grey60", "grey90", "#D0312D"
)

split_grid(n_x = 17,
           n_y = 12,
           col_palette = col_palette,
           grid_col = "black",
           grid_width = 3,
           s = 2024) +
  theme(plot.margin = margin(5, 0, 5, 0))

ggsave("split_grid.png",
       height = 210,
       width = 297,
       units = "mm",
       bg = "black")
