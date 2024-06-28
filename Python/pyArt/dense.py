import plotnine as pn
import numpy as np
import pandas as pd
from matplotlib.colors import LinearSegmentedColormap

mu_x = np.array([0])
mu_y = np.array([0])
n_samples = 100
lvls = 15
s = 1234
col1 = '#333333'
col2 = '#FFA500'

np.random.seed(s)
mu_x = np.array([0])
mu_y = np.array([0])
n_samples = 100

mv_data1 = np.random.multivariate_normal(mean=[mu_x[0], mu_y[0]], cov=[[1, 1], [1, 1]], size=n_samples)
mv_data2 = np.random.multivariate_normal(mean=[5, 5], cov=[[1, 1], [1, 1]], size=n_samples)

mv_data = 0.05 * mv_data1 + 0.95 * mv_data2

bivn_1 = np.random.multivariate_normal(mean=[0, 0], cov=[[1, 0], [0, 1]], size=100)
bivn_2 = np.random.multivariate_normal(mean=[0.5, 0.5], cov=[[0.4, 0], [0, 0.4]], size=100)

mv_data = 0.01 * bivn_1 + 0.99 * bivn_2

plot_data = pd.DataFrame(mv_data, columns=['x', 'y'])

g = (
    pn.ggplot(data=plot_data, mapping=pn.aes(x='x', y='y')) +
    pn.stat_density_2d(pn.aes(fill='..level..'), levels=lvls, geom = 'polygon') +
    pn.scale_fill_gradientn(colors=[col1, col2]) +
    pn.coord_cartesian(expand=False) +
    pn.theme_void() +
    pn.theme(legend_position='none', plot_background=pn.element_rect(fill=col1)))

