# https://www.sumsar.net/blog/2019/01/image-dithering-in-r/


# Load packages -----------------------------------------------------------

library(imager)
library(dplyr)
library(tidyr)
library(ggplot2)

# Define monochrome function ----------------------------------------------

dither <- function(img_path,
                   n = 1,
                   rescale = 2,
                   palette = grey.colors(n = 4, start = 0.05),
                   filename = "dither.png") {
  
  # Utils functions
  rep_mat <- function(mat, nrow_out, ncol_out) {
    mat[rep(seq_len(nrow(mat)), length.out = nrow_out),
        rep(seq_len(ncol(mat)), length.out = ncol_out)]
  }
  
  recursive_bayer_pattern <- function(n) {
    if (n <= 0) {
      return(matrix(0))
    }
    m <- recursive_bayer_pattern(n - 1)
    rbind(
      cbind(4 * m + 0, 4 * m + 2),
      cbind(4 * m + 3, 4 * m + 1))
  }
  
  normalized_bayer_pattern <- function(n) {
    pattern <- recursive_bayer_pattern(n)
    (1 + pattern) / ( 1 + length(pattern) )
  }
  
  rep_bayer_cimg <- function(n, nrow_out, ncol_out) {
    bayer_matrix <- rep_mat(normalized_bayer_pattern(n), nrow_out, ncol_out)
    as.cimg(bayer_matrix)
  }
  
  # Perform dithering
  img <- imager::load.image(img_path)
  img <- resize(img,
                round(width(img)/rescale),round(height(img)/rescale))
  img <- grayscale(rm.alpha(img))
  m <- as.matrix(img)
  bayer_cimg <- rep_bayer_cimg(n, nrow(img), ncol(img))
  b <- as.matrix(bayer_cimg)
  mat <- m + (b/length(palette))
  colnames(mat) <- 1:ncol(mat)
  rownames(mat) <- 1:nrow(mat)
  
  # Data wrangling
  plot_df <- tibble::as_tibble(mat) |> 
    mutate(x = row_number()) |> 
    pivot_longer(-x, names_to = "y") |> 
    mutate(y = as.numeric(y)) |> 
    mutate(value = cut(value, breaks = length(palette))) |> 
    mutate(alpha = abs(((y - mean(y))*(x - mean(x))) / (max(x)*max(y))))
  
  # Plot
  p <- ggplot(data = plot_df,
         mapping = aes(x = x, y = y)) +
    geom_raster(mapping = aes(fill = value)) +
    geom_raster(mapping = aes(alpha = alpha),
                fill = "black") +
    scale_y_reverse() +
    scale_fill_manual(values = palette) +
    scale_alpha_continuous(range = c(0.05, 0.9)) +
    coord_fixed(expand = FALSE) +
    theme_void() +
    theme(legend.position = "none",
          plot.margin = margin(0,0,0,0))
  
  # Save
  ggsave(p, filename = filename,
         width = width(img),
         height = height(img),
         units = "px")
  
  # Return
  return(p)
}

dither("Photos/IMG_2478.JPG", palette = viridis::magma(n = 5), filename = "Images/dither.png")
dither("Photos/owl.jpg", palette = viridis::magma(n = 5), filename = "Images/owl.png")

