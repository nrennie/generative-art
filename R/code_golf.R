library(tidyverse)
x=seq(0,50*pi,0.01)
y=sample(1:8,size=1)*sin(23*x) + sample(1:8,size=1)*cos(6*x)
c=sample(viridis::turbo(12),15708,replace=T)
ggplot(tibble(x,y,c),aes(x=x,y=y,colour=c)) +
  geom_path() +
  scale_colour_identity() +
  coord_polar() +
  theme_void()
