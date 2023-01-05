library(aRt)
library(patchwork)

p1 <- blending(n = 500)
p1

p2 <- flow_fields(n = 10000, granualarity = 1000, x_freq = 1, y_freq = 1, alpha = 0.3, line_col = "black", bg_col = "transparent", s = 1234)
p2

p <- p1 +
  patchwork::inset_element(p2, left = 0, bottom = 0, right = 1, top = 1) 
p
