# Load packages -----------------------------------------------------------

library(imager)
library(dplyr)
library(tidyr)
library(ggplot2)
library(showtext)


# Load fonts --------------------------------------------------------------

font_add_google("Special Elite", "elite")
showtext_auto()
showtext_opts(dpi = 300)


# Function ----------------------------------------------------------------

to_font <- function(
  img_path,
  size,
  rescale,
  filename,
  bg_col = "#fafafa",
  text_col = "grey10",
  chars = c("l", "I", "H", "M")) {
  
  # Read image
img <- imager::load.image(img_path)
img <- resize(img, round(width(img) / rescale), round(height(img) / rescale))
img <- grayscale(rm.alpha(img))
m <- as.matrix(img)
colnames(m) <- 1:ncol(m)
rownames(m) <- 1:nrow(m)

# Process image
m_df <- m |> 
  as_tibble() |> 
  mutate(x = row_number()) |> 
  pivot_longer(-x, names_to = "y") |> 
  mutate(y = as.numeric(y))
chars_map <- data.frame(value = rev(seq_len(length(chars))),
                        value_letter = chars)
plot_df <- m_df |> 
  mutate(value = ntile(value, n = length(chars))) |> 
  left_join(chars_map, by = "value") |> 
  drop_na()

# Plot
p <- ggplot() +
  geom_text(data = plot_df, 
            mapping = aes(x = x, y = y, label = value_letter),
            family = "elite",
            colour = text_col,
            size = size) +
  scale_y_reverse() +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = bg_col, colour = bg_col),
        panel.background = element_rect(fill = bg_col, colour = bg_col))
  
  # Save
  ggsave(p, filename = filename,
    width = rescale*width(img),
    height = rescale*height(img),
    units = "px")
  
}

to_font("Images/image.jpg", size = 2, rescale = 20, filename = "Images/to_font.png")


