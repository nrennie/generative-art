import plotnine as pn
import numpy as np
import pandas as pd
from matplotlib.colors import LinearSegmentedColormap

# Function to generate art
def dense(mu_x, mu_y, n_samples, lvls, col1, col2, s):
  """Generates plot of multivariate density."""
  # convert to array
  mu_x = np.array(mu_x)
  mu_y = np.array(mu_y)
  # generate data
  np.random.seed(s)
  # create data
  mv_data1 = np.random.multivariate_normal(mean=[mu_x[0], mu_y[0]], cov=[[1, 1], [1, 1]], size=n_samples)
  mv_data2 = np.random.multivariate_normal(mean=[5, 5], cov=[[1, 1], [1, 1]], size=n_samples)
  mv_data = 0.05 * mv_data1 + 0.95 * mv_data2
  plot_data = pd.DataFrame(mv_data, columns=['x', 'y'])
  g = (
    pn.ggplot(data=plot_data, mapping=pn.aes(x='x', y='y')) +
    pn.stat_density_2d(pn.aes(fill='..level..'), levels=lvls, geom = 'polygon') +
    pn.scale_fill_gradientn(colors=[col1, col2]) +
    pn.coord_cartesian(expand=False) +
    pn.theme_void() +
    pn.theme(legend_position='none', plot_background=pn.element_rect(fill=col1)))
  return g
