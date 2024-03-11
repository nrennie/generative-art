# Load packages -----------------------------------------------------------

library(imager)
library(dplyr)
library(tidyr)
library(ggplot2)
library(showtext)


# Load fonts --------------------------------------------------------------

font_add_google("Special Elite", "elite")
showtext_auto()


# Parameters --------------------------------------------------------------

chars <- c("l", "I", "H", "M")
rescale <- 3
bg_col <- "#fafafa"
text_col <- "grey10"
img_path <- "Photos/park.jpg"


# Read image --------------------------------------------------------------

img <- imager::load.image(img_path)
img <- resize(img, round(width(img) / rescale), round(height(img) / rescale))
img <- grayscale(rm.alpha(img))
m <- as.matrix(img)
colnames(m) <- 1:ncol(m)
rownames(m) <- 1:nrow(m)


# Process image -----------------------------------------------------------

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


# Plot image --------------------------------------------------------------

p <- ggplot() +
  geom_text(data = plot_df, 
            mapping = aes(x = x, y = y, label = value_letter),
            family = "elite",
            colour = text_col,
            size = 1) +
  scale_y_reverse() +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = bg_col, colour = bg_col),
        panel.background = element_rect(fill = bg_col, colour = bg_col))

ggsave(p, filename = "Images/park.png",
       width = rescale*width(img),
       height = rescale*height(img),
       units = "px")
