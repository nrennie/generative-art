import plotnine as pn
import random
import pandas as pd
import matplotlib.colors as mcolors

# parameters
n_x=175
max_y=36
size=0.05
linewidth=0.4
bg_col="#002952"
col_palette=["#FED600", "#005CBB"]
s=2025

# generate data
random.seed(s)
# start and end points
random.seed(s)
n_y_start=random.choices(range(round(max_y/2)), k=n_x)
n_y_end=random.choices(range(round(max_y/2)+1, max_y + 1), k=n_x)
# get x and y co-ordinates in dataframe
x_list=[]
y_list=[]
for i in range(n_x):
  x_list.extend([i]*len(range(n_y_start[i], n_y_end[i])))
  y_list.extend(range(n_y_start[i], n_y_end[i]))
plot_data = pd.DataFrame({'x': x_list, 'y': y_list})
# choose colours
cmap=mcolors.LinearSegmentedColormap.from_list('custom_cmap', col_palette, N=len(plot_data.index))
plot_data['col']=[mcolors.to_hex(cmap(i)) for i in range(len(plot_data.index))]
# plot data
p = (pn.ggplot(data=plot_data, mapping=pn.aes(x="x", y="y", group="x", colour="col")) +
  pn.geom_line(size=linewidth, show_legend=False) +
  pn.geom_point(size=size, show_legend=False) +
  pn.scale_colour_identity() +
  pn.coord_flip() +
  pn.theme_void() +
  pn.theme(plot_background=pn.element_rect(fill=bg_col, colour=bg_col)))

pn.ggsave(p, filename="Images/ukraine_nexus.png", height=5, width=5, dpi=300)
