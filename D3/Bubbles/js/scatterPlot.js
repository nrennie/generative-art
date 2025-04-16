window.onload = function() {
  
  const width = 400;
  const height = 400;

  // Generate data
  const data = d3.range(config.data.n).map(() => ({
    x: d3.randomUniform(config.data.xmin, config.data.xmax)(),
    y: d3.randomUniform(config.data.ymin, config.data.ymax)(),
    size: d3.randomUniform(config.data.rmin, config.data.rmax)(),
    fillCol: d3.randomUniform(0, 1)(),
    fillOpacity: d3.randomUniform(config.data.fillOpacityMin, config.data.fillOpacityMax)()
  }));

  let colour = d3.scaleOrdinal(config.style.colPalette);

  // Plot
  const chartContainer = d3.select("#scatter")
    .style('background-color', config.style.bgCol);

  const svg = chartContainer
    .append("svg")
    .attr("width", width)
    .attr("height", height);

  svg.selectAll('circle')
    .data(data)
    .enter()
    .append('circle')
    .attr('cx', d => d.x)
    .attr('cy', d => d.y)
    .attr('r', d => d.size)
    .attr('fill', d => colour(d.fillCol))
    .attr('fill-opacity', d => d.fillOpacity);

};


