// test.js
// Initialization
svg.attr("font-family", "sans-serif")
  .attr("font-size", "12") // Set a default font size
  .attr("text-anchor", "middle");

var svgSize = 600;
var pack = d3.pack()
  .size([svgSize, svgSize])
  .padding(1.5);

var color = d3.scaleOrdinal(d3.schemeCategory20);

var group = svg.append("g");

// Create a separate tooltip
var tooltip = d3.select("body")
  .append("div")
  .style("opacity", 0)
  .attr("class", "tooltip")
  .style("background-color", "white")
  .style("border", "solid")
  .style("border-width", "2px")
  .style("border-radius", "5px")
  .style("padding", "5px");

// Resize
r2d3.onResize(function (width, height) {
  var minSize = Math.min(width, height);
  var scale = minSize / svgSize;

  group.attr("transform", "translate(" + (width - minSize) / 2 + "," + (height - minSize) / 2 + ")" +
    "scale(" + scale + "," + scale + ")");
});

// Rendering
r2d3.onRender(function (data, svg, width, height, options) {
  var root = d3.hierarchy({ children: data })
    .sum(function (d) { return d.frequency; });

  var nodes = pack(root).leaves();

  var node = group.selectAll(".node")
    .data(nodes)
    .enter().append("g")
    .attr("class", "node")
    .attr("transform", function (d) { return "translate(" + d.x + "," + d.y + ")"; });

  node.append("circle")
    .attr("id", function (d) { return "circle-" + d.data.emotion; })
    .attr("r", function (d) { return d.r; })
    .style("fill", function (d) { return color(d.data.emotion); })
    .on("mouseover", function (event, d) {
      // Display info on hover
      tooltip.transition()
        .duration(200)
        .style("opacity", .9);
      tooltip.html("Emotion: " + d.data.emotion + "<br>Frequency: " + d.data.frequency)
        .style("left", (event.pageX + 10) + "px")
        .style("top", (event.pageY - 28) + "px");
      //tooltip.style("visibility", "visible");
    })
    .on("mouseout", function (d) {
      // Hide info on mouseout
      tooltip.transition()
        .duration(500)
        .style("opacity", 0);
      //tooltip.style("visibility", "hidden");
    });

  node.append("text")
    .attr("dy", ".3em")
    .style("text-anchor", "middle")
    .text(function (d) { return d.data.emotion; });

  node.append("title")
    .text(function (d) { return d.data.emotion + "\nFrequency: " + d.data.frequency; });

  r2d3.resize(width, height);
});


