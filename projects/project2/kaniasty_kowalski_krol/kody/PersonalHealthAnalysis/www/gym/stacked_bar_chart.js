r2d3.onRender(function (data, div, width, height, options) {
  
  div.selectAll('*').remove();
  
	var margin = { top: 40, right: 10, bottom: 20, left: 50 },
		plotWidth = width - margin.left - margin.right,
		plotHeight = height - margin.top - margin.bottom;

	var seriesNames = Object.keys(data[0]).slice(2);

	var stack = d3
		.stack()
		.keys(seriesNames)
		.order(d3.stackOrderNone)
		.offset(d3.stackOffsetNone);

	var layers = stack(data);

	var weeksWithData = data.filter(d => seriesNames.some(name => d[name] > 0));
  var xDomain = weeksWithData.map(d => `${d.year} W${d.week}`);

	var x = d3
		.scaleBand()
		.domain(xDomain)
		.rangeRound([0, plotWidth])
		.padding(0.1);

	var y = d3
		.scaleLinear()
		.domain([0, d3.max(layers, (layer) => d3.max(layer, (d) => d[1]))])
		.range([plotHeight, 0]);

  var pastel_palette = ['#D4B06D', '#7BA090', '#7094C8', '#A58CB8', '#C87BA2', '#DAA672'];

	var color = d3.scaleOrdinal(pastel_palette).domain(seriesNames);

  var svg = div.append("svg")
      .attr("width", width)
      .attr("height", height)
      .style("background-color", "transparent");

	var g = svg
		.append("g")
		.attr("transform", "translate(" + margin.left + "," + margin.top + ")");

	var layer = g
		.selectAll(".layer")
		.data(layers)
		.enter()
		.append("g")
		.attr("class", "layer")
		.style("fill", function (d, i) {
			return color(i);
		});

	var rect = layer
		.selectAll("rect")
		.data(function (d) {
			return d;
		})
		.enter()
		.append("rect")
		.attr("x", function (d) {
			return x(`${d.data.year} W${d.data.week}`);
		})
		.attr("y", function (d) {
			return y(d[1]);
		})
		.attr("height", function (d) {
			return y(d[0]) - y(d[1]);
		})
		.attr("width", x.bandwidth());

	g.append("g")
		.attr("class", "axis axis--x")
		.attr("transform", "translate(0," + plotHeight + ")")
		.style("font-size", "12px")
		.style("color", "#f4f4f4")
		.call(d3.axisBottom(x));

	g.append("g")
		.attr("class", "axis axis--y")
		.style("font-size", "12px")
		.style("color", "#f4f4f4")
		.call(d3.axisLeft(y));

	layer
		.selectAll("text")
		.data(function (d) {
			return d;
		})
		.enter()
		.append("text")
		.attr("x", function (d) {
			return x(`${d.data.year} W${d.data.week}`) + x.bandwidth() / 2;
		})
		.attr("y", function (d) {
			return y(d[1]) + (y(d[0]) - y(d[1])) / 2;
		})
		.attr("dy", "0.35em")
		.attr("text-anchor", "middle")
		.text(function (d) {
			return d[1] - d[0] > 0 ? d3.format(".2s")(d[1] - d[0]) : "";
		})
		.style("fill", "#fff")
		.style("font-size", "10px");

  var legend = svg.selectAll(".legend")
    .data(seriesNames.slice().reverse())
    .enter().append("g")
    .attr("class", "legend")
    .attr("transform", function(d, i) { return "translate(0," + i * 20 + ")"; });
  
  legend.append("rect")
      .attr("x", width - 18)
      .attr("width", 18)
      .attr("height", 18)
      .style("fill", color);
  
  legend.append("text")
      .attr("x", width - 24)
      .attr("y", 9)
      .attr("dy", ".35em")
      .style("text-anchor", "end")
      .style("fill", "#ffffff")
      .text(function(d) { return d; });


	var tooltip = d3
		.select("body")
		.append("div")
		.attr("class", "tooltip")
		.style("opacity", 0);


	rect
		.on("mouseover", function (event, d) {
      var muscleGroup = d3.select(this.parentNode).datum().key;
      var totalWeight = d[1] - d[0];
			tooltip.transition().duration(200).style("opacity", 0.9);
			tooltip
        .html("Muscle Group: " + muscleGroup + "<br>Total Weight: " + totalWeight)
				.style("left", event.pageX + "px")
				.style("top", event.pageY - 28 + "px");
		})
		.on("mouseout", function (event, d) {
			tooltip.transition().duration(500).style("opacity", 0);
		});
		
	const shadowHost = document.querySelector('.r2d3');
  const shadowRoot = shadowHost.shadowRoot;

  if (shadowRoot) {
    const svgElement = shadowRoot.querySelector('svg');
    if (svgElement) {
      svgElement.style.backgroundColor = 'transparent';
    }
  }
});