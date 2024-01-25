// progress.js

// Assume 'data' is a JavaScript array of objects from R
// Each object has 'date', 'average_weight', and 'muscle_group' properties

// Parse date strings into JavaScript Date objects
data.forEach(function (d) {
	d.date = new Date(d.date);
});

// Set the dimensions and margins of the graph
var margin = { top: 10, right: 30, bottom: 30, left: 60 },
	width = 460 - margin.left - margin.right,
	height = 400 - margin.top - margin.bottom;

// Append the svg object to the body of the page
var svg = d3
	.select("svg")
	.attr("width", width + margin.left + margin.right)
	.attr("height", height + margin.top + margin.bottom)
	.append("g")
	.attr("transform", "translate(" + margin.left + "," + margin.top + ")");

// Add X axis
var x = d3
	.scaleTime()
	.domain(
		d3.extent(data, function (d) {
			return d.date;
		})
	)
	.range([0, width]);
svg
	.append("g")
	.attr("transform", "translate(0," + height + ")")
	.call(d3.axisBottom(x));

// Add Y axis
var y = d3
	.scaleLinear()
	.domain([
		0,
		d3.max(data, function (d) {
			return +d.average_weight;
		}),
	])
	.range([height, 0]);
svg.append("g").call(d3.axisLeft(y));

// Group the data by muscle group
var sumstat = d3.group(data, function (d) {
	return d.muscle_group;
});

// Define a color scale
var color = d3
	.scaleOrdinal()
	.domain(Array.from(sumstat.keys()))
	.range(d3.schemeCategory10);

// Draw the lines
sumstat.forEach(function ([key, value]) {
	svg
		.append("path")
		.datum(value)
		.attr("fill", "none")
		.attr("stroke", color(key))
		.attr("stroke-width", 1.5)
		.attr(
			"d",
			d3
				.line()
				.x(function (d) {
					return x(d.date);
				})
				.y(function (d) {
					return y(d.average_weight);
				})
		);
});
