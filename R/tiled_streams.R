library(aRt)
set.seed(1234)
ss <- sample(5:100, size = 25)
p <- lapply(ss, function(i) streams(type = sample(c("up", "down", "left", "right"), 1),
                                    s = i))
patchwork::wrap_plots(p) +
  patchwork::plot_layout(ncol = 5, nrow = 5) &
  ggplot2::theme(plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
                 plot.background = ggplot2::element_rect(fill = "white", colour = "white"),
                 panel.background = ggplot2::element_rect(fill = "white", colour = "white"))



set.seed(1234)
ss <- sample(5:50, size = 16)
p <- lapply(ss, function(i) streams(type = sample(c("up", "down", "left", "right"), 1),
                                    fill_col = grey.colors(i),
                                    line_col = NA,
                                    bg_col = "black",
                                    s = i))
patchwork::wrap_plots(p) +
  patchwork::plot_layout(ncol = 4, nrow = 4) &
  ggplot2::theme(plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
                 plot.background = ggplot2::element_rect(fill = "black", colour = "black"),
                 panel.background = ggplot2::element_rect(fill = "black", colour = "black"))
