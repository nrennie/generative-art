aRt::lines(200, 7, 0.7, main_col = "#B0C0BC", bg_col = "#466365", linewidth = 0.3) + 
  ggplot2::coord_polar()

ggplot2::ggsave("green.png", width = 900, height = 900, units = "px")
