library(aRt)
library(rcartocolor)
library(ggplot2)
library(cowplot)

set.seed(1234)
ss <- sample(3000:5000, size=48)
p <- lapply(ss, function(i) circles(s=i,
                                    bg_col = sample(carto_pal(12, "Bold"),
                                                    size=1)) + coord_fixed())
q <- plot_grid(plotlist=p, nrow = 4, ncol = 12)
save_plot("Images/circles_twitter_banner.png", q, base_height=5.21, base_width=15.625)
