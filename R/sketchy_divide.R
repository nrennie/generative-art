
# Function to save to correct size ----------------------------------------

save_roughsf_fixed <- function(rsf, file, background = "white", wait = 4) {
  if (!requireNamespace("pagedown", quietly = TRUE)) {
    stop("pagedown is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  tfile <- tempfile(fileext = ".html")
  format <- substr(file, nchar(file) - 2, nchar(file))
  htmlwidgets::saveWidget(rsf, file = tfile, background = background,
                          selfcontained = TRUE)
  suppressMessages(pagedown::chrome_print(tfile, output = file,
                                          format = format,
                                          selector = ".roughsf.html-widget.html-widget-static-bound",
                                          wait = wait))
  suppressMessages(file.remove(tfile))
}

# Version 1 ---------------------------------------------------------------

num_lines <- 60
col_palette <- PrettyCols::prettycols("Lively")
s <- 1234
set.seed(s)
polygon1 <- sf::st_polygon(list(cbind(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0))))
endpoints <- tibble::tibble(x = c(seq(0, 1, length = 100),
                                  seq(0, 1, length = 100),
                                  rep(0, 100),
                                  rep(1, 100)),
                            y = c(rep(0, 100),
                                  rep(1, 100),
                                  seq(0, 1, length = 100),
                                  seq(0, 1, length = 100)))
choose_ends <- purrr::map(.x = 1:num_lines,
                          .f = ~as.matrix(dplyr::slice_sample(endpoints, n = 2)))
make_lines <- sf::st_multilinestring(x = choose_ends)
cropped_sf <- lwgeom::st_split(polygon1, make_lines) |>
  sf::st_collection_extract(c("POLYGON")) |>
  sf::st_as_sf()
cropped_sf$fill <- sample(col_palette,
                          size = nrow(cropped_sf),
                          replace = TRUE)
cropped_sf$fillstyle <- sample(c("hachure", "zigzag", "cross-hatch",
                                 "dots", "dashed", "zigzag-line"),
                               size = nrow(cropped_sf),
                               replace = TRUE)
cropped_sf$fillweight <- 1.2
r <- roughsf::roughsf(cropped_sf,
                      roughness = 5,
                      bowing = 1,
                      simplification = 1,
                      width = 600,
                      height = 600
)
r
save_roughsf_fixed(r,
                   file = "Images/sketchy_divide.png",
                   background = "#808080")
