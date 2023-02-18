library(aRt)
library(ggplot2)
library(dplyr)
library(stringr)
library(patchwork)
library(rcartocolor)
library(ggforce)

# rectangles
p = rectangles(n = 120, max_height = 6, max_width = 4, size = 1, main_col = ggplot2::alpha("white", 0.5), col_palette = rcartocolor::carto_pal(n = 12, "Prism"), bg_col = "black", s = 123)
p
ggsave(p, filename="images/rectangles.png", dpi=300, width=21, height=21, units="cm")

# tiles
p = tiles(n_x=30, n_y=30, col_palette=MetBrewer::met.brewer("Pissaro", 6), s=43)
p
ggsave(p, filename="images/tiles.png", dpi=300, width=21, height=21, units="cm")

# vortex
p = vortex(n=20, start_val=40, col_scheme="rainbow", bg_col="black", s=6754)
p
ggsave(p, filename="images/vortex.png", dpi=300, width=21, height=21, units="cm")

# waves
p = waves(a=24, b=7, main_col=rcartocolor::carto_pal(n = 8, "Prism"), bg_col="#edad08", s=543)
p
ggsave(p, filename="images/waves.png", dpi=300, width=21, height=21, units="cm")

# polygons
p = polygons(n_x=15, n_y=15, gap_size=0.5, deg_jitter=0.5, colours=rcartocolor::carto_pal(7, "DarkMint"), rand = T, bg_col="gray97")
p
ggsave(p, filename="images/polygons.png", dpi=300, width=21, height=21, units="cm")
 
# flow fields
p = flow_fields(n = 10000, granualarity = 1000, x_freq = 1, y_freq = 1, alpha = 0.3, line_col = "black", bg_col = "white", s = 1234)
p
ggsave(p, filename="images/flow_fields.png", dpi=300, width=21, height=21, units="cm")

# fading
p = fading(n_layers=6, n_points=7, col_palette=rev(rcartocolor::carto_pal(n = 7, "Sunset")), s=1234)
p
ggsave(p, filename="images/fading.png", dpi=300, width=21, height=21, units="cm")

# dots
p = dots(n_x=400, n_y=80, jitter_size_width=0.5, jitter_size_height=5, col_palette = grey.colors(25), bg_col="black", s=1234)
p
ggsave(p, filename="images/dots.png", dpi=300, width=21, height=21, units="cm")

# circles
p = circles(n=100, smoothness=100, col_palette=rcartocolor::carto_pal(n = 12, "Bold"), line_col=NA, bg_col="#e73f74", s=1234)
p
ggsave(p, filename="images/circles.png", dpi=300, width=21, height=21, units="cm") 

# bullseye
p = bullseye(main_col="white", bg_col="black", s=53264)
p
ggsave(p, filename="images/bullseye.png", dpi=300, width=21, height=21, units="cm") 

# bubbles
p = bubbles(num_circles = 16, main_col = alpha("white", 0.6), col_palette = rcartocolor::carto_pal(n = 12, "Prism"), bg_col = "black", s = 734)
p
ggsave(p, filename="images/bubbles.png", dpi=300, width=21, height=21, units="cm") 

# bricks
p = bricks(n_y=30, colours=rcartocolor::carto_pal(7, "Magenta"), bg_col="gray97")
p
ggsave(p, filename="images/bricks.png", dpi=300, width=21, height=21, units="cm") 

#boxes
p = boxes(n=100, perc=0.1, col_palette=rcartocolor::carto_pal(n = 7, "Teal"), bg_col="black", s=544)
p
ggsave(p, filename="images/boxes.png", dpi=300, width=21, height=21, units="cm") 

# attraction
p = attraction(n=25000, a=-3.1, b=1, c=0.5, d=-1, main_col="white", bg_col="black")
p
ggsave(p, filename="images/attraction.png", dpi=300, width=21, height=21, units="cm") 

# windows
positions <- data.frame(
  id = rep(1:5, each = 4),
  x = c(0, 2, 2, 0,
        2.5, 3.5, 3.5, 2.5,
        4, 7, 7, 4,
        4, 7, 7, 4,
        7.5, 9, 9, 7.5),
  y = c(0, 0, 9, 9,
        0, 0, 9, 9,
        0, 0, 4.25, 4.25,
        4.75, 4.75, 9, 9,
        0, 0, 9, 9)
)
x1 <- runif(10*2*9, 0, 2)
y1 <- runif(10*2*9, 0, 9)
x2 <- runif(10*1*9, 2.5, 3.5)
y2 <- runif(10*1*9, 0, 9)
x3 <- runif(10*3*4.25, 4, 7)
y3 <- runif(10*3*4.25, 0, 4.25)
x4 <- runif(10*3*4.25, 4, 7)
y4 <- runif(10*3*4.25, 4.75, 9)
x5 <- runif(10*1.5*9, 7.5, 9)
y5 <- runif(10*1.5*9, 0, 9)
d <- data.frame(x=c(x1, x2, x3, x4, x5), y=c(y1, y2, y3, y4, y5))
d$size = sample(seq(0.5, 4.5, by=0.5), size=nrow(d), replace=T)
p <- ggplot() +
  geom_polygon(data=positions, mapping=aes(x = x, y = y, group = id),
               fill="black", colour="white", size=0.5) +
  geom_point(data=d, mapping=aes(x=x, y=y, size=I(size)), pch=21, colour="white", fill="black") +
  xlim(-0.5,9.5) +
  ylim(-0.5,9.5) +
  coord_fixed(expand = F) +
  theme_void() +
  theme(panel.background = element_rect(fill="black", colour="black"),
        plot.background = element_rect(fill="black", colour="black"))
p
ggsave(p, filename="images/windows.png", dpi=300, width=21, height=21, units="cm") 

# tartan
perc = 0.95
n = 1
col_palette = "TealGrn"
s = 1234
set.seed(s)
plot_df <- matrix(NA, ncol = 1000, nrow = n)
for (i in seq_len(n)) {
  k <- stats::runif(1000)
  vals <- sample(x = 1:1000, size = perc * 1000, replace = FALSE)
  k[sort(vals)] <- sort(k[vals])
  plot_df[i, ] <- k
}
colnames(plot_df) <- seq_len(ncol(plot_df))
rownames(plot_df) <- seq_len(nrow(plot_df))
plot_data <- tibble::tibble(times = seq_len(nrow(plot_df)), tibble::as_tibble(plot_df))
plot_data <- tidyr::pivot_longer(plot_data, cols = 2:(ncol(plot_df) + 1))
p1 <- ggplot2::ggplot(data = plot_data,
                      ggplot2::aes(x = .data$times, y = as.numeric(.data$name), fill = .data$value)) +
  ggplot2::geom_tile(size=1) +
  ggplot2::coord_flip(expand = FALSE) +
  rcartocolor::scale_fill_carto_c(palette = col_palette) +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "transparent"),
                 plot.background = ggplot2::element_rect(fill = "transparent"),
                 axis.text = ggplot2::element_blank(),
                 axis.ticks = ggplot2::element_blank(),
                 axis.title = ggplot2::element_blank(),
                 plot.margin = ggplot2::unit(c(-0.5, -0.5, -0.5, -0.5), "cm"),
                 legend.position = "none",
                 legend.key = ggplot2::element_blank())
p1
p2 <- ggplot2::ggplot(data = plot_data,
                      ggplot2::aes(x = .data$times, y = as.numeric(.data$name), fill = .data$value)) +
  ggplot2::geom_tile(alpha = 0.5, colour=NA, size=1) +
  ggplot2::coord_cartesian(expand = FALSE) +
  rcartocolor::scale_fill_carto_c(palette = col_palette) +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "transparent"),
                 plot.background = ggplot2::element_rect(fill = "transparent"),
                 axis.text = ggplot2::element_blank(),
                 axis.ticks = ggplot2::element_blank(),
                 axis.title = ggplot2::element_blank(),
                 plot.margin = ggplot2::unit(c(-0.5, -0.5, -0.5, -0.5), "cm"),
                 legend.position = "none",
                 legend.key = ggplot2::element_blank())
p2
p <- p1 +
  patchwork::inset_element(p2, left = 0, bottom = 0, right = 1, top = 1) &
  ggplot2::theme(plot.margin = ggplot2::unit(c(-0.5, -0.5, -0.5, -0.5), "cm"))
p
ggsave(p, filename="images/tartan.png", dpi=300, width=21, height=21, units="cm") ##redo

# carpet
gen_polygon <- function(x, y, height, width, group){
  data.frame(x = c(x, x+width, x+width, x),
             y = c(y, y, y+height, y+height),
             group = rep(group, 4),
             col = as.character(rep(sample(1:4, size = 1), 4)))
}
carpet_tile <- function(s){
  set.seed(s)
  n <- 5
  plot_data <- data.frame(x = c(),
                          y = c(),
                          group = c(),
                          col = c())
  for (i in 1:n){
    k <- gen_polygon(x = runif(1, 0, 5),
                     y = runif(1, 0, 5),
                     height = runif(1, 2, 6),
                     width = runif(1, 2, 6),
                     group = i)
    plot_data <- rbind(plot_data, k)
  }
  y1 <- runif(1, 0, 5)
  x1 <- runif(1, 0, 5)
  x2 <- x1 + 0.5
  p <- ggplot() +
    geom_polygon(data=plot_data,
                 mapping=aes(x = x, y = y, group = group, fill = col), colour = NA, alpha=0.7) +
    geom_segment(aes(x = min(plot_data$x), y = y1, xend = max(plot_data$x), yend = y1), colour = "#999933", size = 2) +
    geom_segment(aes(y = min(plot_data$y), x = x1, yend = max(plot_data$y), xend = x1), colour = "#aa4499", size = 2) +
    geom_segment(aes(y = min(plot_data$y), x = x2, yend = max(plot_data$y), xend = x2), colour = "#882255", size = 3) +
    scale_fill_manual(values=c("#cc6677", "#999933", "#aa4499", "#882255")) +
    coord_fixed(expand = F) +
    xlim((min(c(plot_data$x, plot_data$y, x1, x2))), (max(c(plot_data$x, plot_data$y, x1, x2)))) +
    ylim((min(c(plot_data$x, plot_data$y, y1))), (max(c(plot_data$x, plot_data$y, y1)))) +
    theme_void() +
    theme(legend.position = "none",
          panel.background = element_rect(fill = "#ddcc77", colour = "#ddcc77"),
          plot.background = element_rect(fill = "#ddcc77", colour = "#ddcc77"))
  p
}
set.seed(123)
ss <- sample(1:200, size=100, replace=F)
p <- lapply(ss, function(i) carpet_tile(i))
q = wrap_plots(p) +
  plot_layout(ncol=10, nrow=10) &
  theme(plot.margin = unit(c(0,0,0,0), "cm"))
ggsave(q, filename="images/carpet.png", dpi=300, width=21, height=21, units="cm")

# negative
circles_1 <- data.frame(
  x0 = rep(1:10, 10),
  y0 = rep(1:10, each = 10),
  alpha = rep(seq(0.05, 0.95, by = 0.1), each = 10),
  r = rep(0.4, 10)
)
circles_2 <- data.frame(
  x0 = rep(1:10, 10),
  y0 = rep(1:10, each = 10),
  r = rep(0.2, 10)
)
p = ggplot() +
  geom_circle(data = circles_1, mapping = aes(x0 = x0, y0 = y0, r = r, alpha = alpha), fill = "white") +
  geom_circle(data = circles_2, mapping = aes(x0 = x0, y0 = y0, r = r), fill = "black") +
  coord_fixed() +
  theme_void() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "black", colour = "black"),
        plot.background = element_rect(fill = "black", colour = "black"))
p
ggsave(p, filename="images/negative.png", dpi=300, width=21, height=21, units="cm")

# streams
set.seed(1234)
ss <- sample(5:100, size = 25)
p <- lapply(ss, function(i) streams(type = sample(c("up", "down", "left", "right"), 1), s = i))
q <- patchwork::wrap_plots(p) +
  patchwork::plot_layout(ncol = 5, nrow = 5) &
  ggplot2::theme(plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
                 plot.background = ggplot2::element_rect(fill = "white", colour = "white"),
                 panel.background = ggplot2::element_rect(fill = "white", colour = "white"))
ggsave(q, filename="images/streams.png", dpi=300, width=21, height=21, units="cm")

# tiled circles
set.seed(1234)
ss <- sample(3000:5000, size=36)
p <- lapply(ss, function(i) circles(s=i, bg_col = sample(carto_pal(12, "Bold"), size=1)))
q <- wrap_plots(p) +
  plot_layout(ncol=6, nrow=6) &
  theme(plot.margin = unit(c(0,0,0,0), "cm"))
ggsave(q, filename="images/tiled_circles.png", dpi=300, width=21, height=21, units="cm")

# fractals 1
p = fractals(N = 25, col_palette = MetBrewer::met.brewer("Demuth", n = 25),
         shift = 0, left = -1, right = 1,
         y_param = 3, resolution = 0.005, dist_max = 4)
ggsave(p, filename="images/fractals1.png", dpi=300, width=21, height=21, units="cm") 

# fractals 2
p <- fractals(N = 25, col_palette = rev(MetBrewer::met.brewer("Benedictus", n = 25)),
         shift = 0, left = -3, right = 3,
         y_param = 2, resolution = 0.005, dist_max = 4)
ggsave(p, filename="images/fractals2.png", dpi=300, width=21, height=21, units="cm") 

# fractals 3
p <- fractals(N = 20, col_palette = grey.colors(30),
         shift = -1, left = -1, right = 1,
         y_param = 2, resolution = 0.005, dist_max = 3)
ggsave(p, filename="images/fractals3.png", dpi=300, width=21, height=21, units="cm") 

# sunbursts
p = sunbursts(n = 25, x_means = c(0, 10, 5), y_means = c(0, 7, 8), xy_var = 190, high = "#b690d9", low = "#432263", s = 54)
p
ggsave(p, filename="images/sunbursts.png", dpi=300, width=21, height=21, units="cm") 

# pi
df <- readr::read_csv("G:/My Drive/GitHub/30DayChartChallenge/2022/data/pi.txt", col_names = F, col_types = "c")
pi_char <- df %>%
  pull(X1) %>%
  str_split(pattern =  "") %>%
  unlist()
pi_char <- pi_char[-2]
theta <- seq(11, 40 * pi, length.out = 1000)
r <- 0.5 + 0.5 * theta
plot_data <- tibble(x = r * cos(theta),
                    y = r * sin(theta),
                    colour = pi_char)
p <- ggplot(data = plot_data,
            mapping = aes(x = x, y = y, colour = colour, size = theta)) +
  geom_point() +
  scale_color_carto_d(palette = "Bold") +
  scale_size(range = c(0.05, 3)) +
  coord_fixed() +
  xlim(-(max(abs(c(plot_data$x, plot_data$y))) + 2), (max(abs(c(plot_data$x, plot_data$y))) + 2)) +
  ylim(-(max(abs(c(plot_data$x, plot_data$y))) + 2), (max(abs(c(plot_data$x, plot_data$y))) + 2)) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "black", colour = "black"),
        panel.background = element_rect(fill = "black", colour = "black"))
p
ggsave(p, filename="images/pi.png", dpi=300, width=21, height=21, units="cm")

# cover
set.seed(123)
x <- 1:50
y <- 1:50
plot_data <- tibble(expand.grid(x = x, y = y)) %>% 
  mutate(letters = sample(c(LETTERS, letters), size = 50^2, replace = T), 
         colour = sample(gray.colors(30), size = 50^2, replace = T), 
         pos = 0, 
         size = 7) 
plot_data[45*50+c(5:7), "colour"] <- "#FF007F"
plot_data[45*50+c(5:7), "pos"] <- 1
plot_data[45*50+c(5:7), "letters"] <- c("a", "R", "t")
plot_data[44*50+c(4:8), "letters"] <- NA
plot_data[46*50+c(4:8), "letters"] <- NA
plot_data[45*50+c(4,8), "letters"] <- NA
plot_data[5*50+c(25:30), "letters"] <- c("N", "i", "c", "o", "l", "a")
plot_data[5*50+c(25:30), "size"] <- 6
plot_data[5*50+c(25:30), "colour"] <- "#FF007F"
plot_data[5*50+c(25:30), "pos"] <- 1
plot_data[5*50+c(32:37), "letters"] <- c("R", "e", "n", "n", "i", "e")
plot_data[5*50+c(32:37), "size"] <- 6
plot_data[5*50+c(32:37), "colour"] <- "#FF007F"
plot_data[5*50+c(32:37), "pos"] <- 1
plot_data[6*50+c(24:38), "letters"] <- NA
plot_data[4*50+c(24:38), "letters"] <- NA
plot_data[5*50+c(24, 31, 38), "letters"] <- NA

plot_data <- plot_data %>% 
  arrange(pos) 
plot_data <- plot_data %>% 
  mutate(letters = factor(letters, levels = unique(plot_data$letters)))
p <- ggplot() +
  geom_text(data = plot_data,
            mapping = aes(x = x, y = y, label = letters, 
                          colour = I(colour), size = I(size)), 
            fontface = 2) +
  coord_fixed() +
  theme_void() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "black", colour = "black"),
        plot.background = element_rect(fill = "black", colour = "black"))
p
ggsave(p, filename="images/cover.png", dpi=300, width=21, height=21, units="cm")
