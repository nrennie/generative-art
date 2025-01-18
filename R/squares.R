library(ggplot2)
library(ggarrow)

draw_square <- function(x0 = 0,
                        y0 = 0,
                        jitter = 0.1,
                        grp = 1) {
  output <- data.frame(
    x = c(
      seq(x0 - runif(1, 0, jitter), x0 + 1 + runif(1, 0, jitter), length.out = 10),
      seq(x0 + 1, x0 + 1, length.out = 10),
      seq(x0 + 1 + runif(1, 0, jitter), x0 - runif(1, 0, jitter), length.out = 10),
      seq(x0, x0, length.out = 10)
    ),
    y = c(
      seq(y0, y0, length.out = 10),
      seq(y0 - runif(1, 0, jitter), y0 + 1 + runif(1, 0, jitter), length.out = 10),
      seq(y0 + 1, y0 + 1, length.out = 10),
      seq(y0 + 1 + runif(1, 0, jitter), y0 - runif(1, 0, jitter), length.out = 10)
    ),
    arc = rep(seq(0.1, 1, length.out = 10), times = 4),
    line_grp = paste0(grp, "-", c(rep(1, 10), rep(2, 10), rep(3, 10), rep(4, 10)))
  )
}

squares <- function(s = 1234,
                    n = 20,
                    scale = 5,
                    jitter = 0.15,
                    linewidth = 1,
                    line_col = "black",
                    bg_col = "#FAFAFA") {
  inputs <- withr::with_seed(
    seed = s,
    code = {
      inputs <- tibble::tibble(
        x0 = runif(n, 0, scale),
        y0 = runif(n, 0, scale),
        jitter = rep(jitter, n),
        grp = seq_len(n)
      )
      plot_data <- purrr::pmap_df(inputs, draw_square)
      plot_data
    }
  )
  plot_data$arc <- linewidth * plot_data$arc
  g <- ggplot() +
    geom_arrow(
      data = plot_data,
      mapping = aes(x = x, y = y, group = line_grp, linewidth = I(arc)),
      colour = line_col,
      arrow_head = arrow_head_minimal()
    ) +
    scale_x_continuous(limits = c(-0.5, scale + 1.5)) +
    scale_y_continuous(limits = c(-0.5, scale + 1.5)) +
    coord_fixed(expand = FALSE) +
    theme_void() +
    aRt:::theme_aRt(bg_col)
  return(g)
}

squares(n = 30, scale = 8, linewidth = 0.5, s = 2025)

ggplot2::ggsave("Images/squares.png", width = 900, height = 900, units = "px")
