#' Generates a tibble of line segments and colours
#'
#' @param xmin x-min coord
#' @param xmax x-max coord
#' @param ymin y-min coord
#' @param ymax y-max coord
#' @param low Hex value for left colour
#' @param high Hex value for right colour
#' @return Tibble
#' @noRd
make_square <- function(xmin, xmax, ymin, ymax, low, high) {
  xvals <- seq(xmin, xmax, by = 0.001)
  plot_data <- tibble::tibble(
    x = xvals,
    xend = xvals,
    y = ymin,
    yend = ymax,
    colour = grDevices::colorRampPalette(c(low, high))(length(xvals))
  )
  return(plot_data)
}

#' Generates generative art as a grid of gradient colour fades
#'
#' @param nx Number of columns. Default 5.
#' @param ny Number of rows. Default 5.
#' @param col_palette Vector of colours.
#' @param bg_col Background colour. Only shown if outline TRUE.
#' @param outline Whether or not to add an outline.
#' @param linewidth Linewidth of outline. Default 2.
#' @param col_n Number of colours to generate from . Default 100.
#' @param replace Whether or not to sample colours multiple times.
#' @param s Random seed. Default 1234.
#' @return Tibble
#' @examples
#' gradients2()
gradients2 <- function(
    nx = 5,
    ny = 5,
    col_palette = c("#A053A1", "#DB778F", "#E69F52", "#09A39A", "#5869C7", "#004B67"),
    bg_col = "black",
    outline = FALSE,
    linewidth = 2,
    col_n = 100,
    replace = FALSE,
    s = 1234) {
  if (col_n < nx * ny & isFALSE(replace)) {
    stop("Not enough colours. Increase 'col_n' or set 'replace' to TRUE.")
  }
  col_palette <- grDevices::colorRampPalette(col_palette)(col_n)
  inputs <- withr::with_seed(
    seed = s,
    code = {
      inputs <- tibble::tibble(
        xmin = rep(seq_len(nx), each = ny),
        xmax = rep(seq_len(nx) + 1, each = ny),
        ymin = rep(seq_len(ny), times = nx),
        ymax = rep(seq_len(ny) + 1, times = nx),
        low = sample(col_palette, size = nx * ny, replace = replace),
        high = sample(col_palette, size = nx * ny, replace = replace)
      )
      inputs
    }
  )

  plot_data <- purrr::pmap_df(inputs, make_square)
  p <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_segment(
      mapping = ggplot2::aes(
        x = .data$x,
        y = .data$y,
        xend = .data$xend,
        yend = .data$yend,
        colour = I(.data$colour)
      )
    ) +
    ggplot2::coord_fixed(expand = FALSE) +
    aRt:::theme_aRt("transparent")
  if (outline) {
    p <- p + ggplot2::geom_tile(
      data = expand.grid(
        x = 1:nx,
        y = 1:ny
      ),
      mapping = ggplot2::aes(
        x = x + 0.5, y = y + 0.5
      ),
      colour = bg_col,
      fill = "transparent",
      linewidth = linewidth
     ) +
      ggplot2::annotate(
        geom = "rect",
        xmin = 1, ymin = 1,
        xmax = nx + 1, ymax = ny + 1,
        colour = bg_col,
        fill = "transparent",
        linewidth = linewidth * 1.5
      ) 
  }
  return(p)
}

# Examples
gradients2(
  col_n = 25,
  col_palette = PrettyCols::prettycols("Disco")[1:5],
  outline = TRUE,
  linewidth = 1,
  s = 1234
)
ggplot2::ggsave("Images/gradients2.png", width = 900, height = 900, units = "px")
