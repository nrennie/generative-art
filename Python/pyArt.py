import plotnine as pn

# Crosshatch
import pyArt.crosshatch as crosshatch
p = crosshatch.crosshatch(n_x=10, n_y=10, n_lines=50, line_overlap=0.2, line_slope=0.2, col_palette=["#413C58", "#D1495B", "#EDAE49", "#00798C", "#003D5B"], bg_col="#121212", linewidth=0.1, interpolate=True, s=123)
pn.ggsave(p, filename="Images/crosshatch.png", height=4, width=4, dpi=300)

# Nexus
import pyArt.nexus as nexus
p = nexus.nexus(n_x=200, max_y=10, size=0.001, linewidth=0.1, bg_col="#002e42", col_palette=["#552000", "#8a4d00", "#c17d17", "#f8b150", "#f5f5f5", "#93c6e1", "#5f93ac", "#2e627a", "#00344a"], s=1234)
pn.ggsave(p, filename="Images/nexus.png", height=5, width=5, dpi=300)
