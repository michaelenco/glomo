var req = new XMLHttpRequest()
req.open('GET', '/admin/api/stat', false)
req.send()
data = JSON.parse(req.responseText)

var total_regs = data.length

Array.prototype.forEach.call(data, function(d,i) {
    d.date = new Date(d.timestamp)
})

var regs = crossfilter(data)

var active_users = crossfilter(
    Array.prototype.filter.call(data, function(d, i) {
	return (Date.now() - d.timestamp < 10*24*60*60*1000)
    }))

var dim = {
    date: regs.dimension(function(d) {return d.date}),
    country: regs.dimension(function(d) {return d.country}),
    active: active_users.dimension(function(d) {return d.country})
}
var group = {
    date: dim.date.group(d3.time.day),
    country: dim.country.group(),
    active: dim.active.group()
}

var countryColors = d3.scale.category20c()

var pie_charts  = {
    reg_chart: {
	selector: '#reg-chart',
	dim: dim.country,
	group: group.country,
	colors: countryColors
    },
    active_chart: {
	selector: '#active-chart',
	dim: dim.active,
	group: group.active,
	colors: countryColors
    }
	
}

function pieChart(opt) {
    var radius = 125
    var width = 250
    var height = 250
    var groupData = opt.group
    var dimData = opt.dim
    var selector = opt.selector
    var scale = opt.colors

    var total = function() {
	return dimData.groupAll().value()
    }

    var tooltip = d3.select(selector)
	.append('div')
	.attr('class','tooltip')
    tooltip.append('div')
	.attr('class','label')
    tooltip.append('div')
	.attr('class','count')
    tooltip.append('div')
	.attr('class','percent')

    var svg = d3.select(selector)
	.append('svg')
	.attr('width',width)
	.attr('height',height)
	.append('g')
	.attr('transform', 'translate(' + (width / 2) +  ',' + (height / 2) + ')')

    var arc = d3.svg.arc()
	.outerRadius(radius)
	.innerRadius(radius/2)

    var pie = d3.layout.pie()
	.value(function(d) { return d.value })
	.sort(null)

    var path = svg.selectAll('path')
	.data(pie(groupData.all()))
        .enter()
	.append('path')
	.attr('d', arc)
	.attr('fill', function(d, i) { 
	    return scale(d.data.key)
	})

    var totalText = svg.append('svg:text')
	.attr('class', 'total')
	.attr('text-anchor','middle')
	.attr('alignment-baseline','middle')
	.attr('font-size',radius/3+'px')
	.text(total())

    path.on('mouseover', function(d) {
	var percent = Math.round(1000*d.data.value/total())/10

	tooltip.select('.label').html(d.data.key)
	tooltip.select('.count').html(d.data.value)
	tooltip.select('.percent').html(percent+"%")
	tooltip.style('display','block')
    })

    path.on('mouseout', function() {	
	tooltip.style('display', 'none')	
    })
    path.on('mousemove', function(d) {
	tooltip.style('top', (d3.event.layerY + 10) + 'px')
	    .style('left', (d3.event.layerX + 10) + 'px')
    });
}

pieChart(pie_charts.reg_chart)
pieChart(pie_charts.active_chart)

/*var charts = [
    barChart()
	.dimension(dim.date)
	.group(group.date)
	.x(d3.time.scale()
		.domain([start, end])
		.rangeRound([1,500]))
]

var chart = d3.selectAll(".chart")
    .data(charts)
    .each(function(chart) { 
	chart.on("brush", renderAll)
	    .on("brushend", renderAll); 
    })

renderAll()

function render(method) {
    d3.select(this).call(method);
}

function renderAll() {
    chart.each(render)
}

function barChart() {
    if (!barChart.id) barChart.id = 0;
    var margin = {top: 10, right: 10, bottom: 20, left: 10},
	x,
	y = d3.scale.linear().range([100, 0]),
	id = barChart.id++,
	axis = d3.svg.axis().orient("bottom"),
	brush = d3.svg.brush(),
	brushDirty,
	dimension,
	group,
	round;
    function chart(div) {
	var width = x.range()[1],
	height = y.range()[0];
	y.domain([0, group.top(1)[0].value]);
	div.each(function() {
	    var div = d3.select(this),
	    g = div.select("g");
	    // Create the skeletal chart.
	    if (g.empty()) {
		div.select(".title").append("a")
		    .attr("href", "javascript:reset(" + id + ")")
		    .attr("class", "reset")
		    .text("reset")
		    .style("display", "none");
		g = div.append("svg")
		    .attr("width", width + margin.left + margin.right)
		    .attr("height", height + margin.top + margin.bottom)
		    .append("g")
		    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");
		g.append("clipPath")
		    .attr("id", "clip-" + id)
		    .append("rect")
		    .attr("width", width)
		    .attr("height", height);
		g.selectAll(".bar")
		    .data(["background", "foreground"])
		    .enter().append("path")
		    .attr("class", function(d) { return d + " bar"; })
		    .datum(group.all());
		g.selectAll(".foreground.bar")
		    .attr("clip-path", "url(#clip-" + id + ")");
		g.append("g")
		    .attr("class", "axis")
		    .attr("transform", "translate(0," + height + ")")
		    .call(axis);
		// Initialize the brush component with pretty resize handles.
		var gBrush = g.append("g").attr("class", "brush").call(brush);
		gBrush.selectAll("rect").attr("height", height);
		gBrush.selectAll(".resize").append("path").attr("d", resizePath);
	    }
	    // Only redraw the brush if set externally.
	    if (brushDirty) {
		brushDirty = false;
		g.selectAll(".brush").call(brush);
		div.select(".title a").style("display", brush.empty() ? "none" : null);
		if (brush.empty()) {
		    g.selectAll("#clip-" + id + " rect")
			.attr("x", 0)
			.attr("width", width);
		} else {
		    var extent = brush.extent();
		    g.selectAll("#clip-" + id + " rect")
			.attr("x", x(extent[0]))
			.attr("width", x(extent[1]) - x(extent[0]));
		}
	    }
	    g.selectAll(".bar").attr("d", barPath);
	});
	function barPath(groups) {
	    var path = [],
	    i = -1,
	    n = groups.length,
	    d;
	    while (++i < n) {
		d = groups[i];
		path.push("M", x(d.key), ",", height, "V", y(d.value), "h9V", height);
	    }
	    return path.join("");
	}
	function resizePath(d) {
	    var e = +(d == "e"),
	    x = e ? 1 : -1,
	    y = height / 3;
	    return "M" + (.5 * x) + "," + y
		+ "A6,6 0 0 " + e + " " + (6.5 * x) + "," + (y + 6)
		+ "V" + (2 * y - 6)
		+ "A6,6 0 0 " + e + " " + (.5 * x) + "," + (2 * y)
		+ "Z"
		+ "M" + (2.5 * x) + "," + (y + 8)
		+ "V" + (2 * y - 8)
		+ "M" + (4.5 * x) + "," + (y + 8)
		+ "V" + (2 * y - 8);
	}
    }
    brush.on("brushstart.chart", function() {
	var div = d3.select(this.parentNode.parentNode.parentNode);
	div.select(".title a").style("display", null);
    });
    brush.on("brush.chart", function() {
	var g = d3.select(this.parentNode),
	extent = brush.extent();
	if (round) g.select(".brush")
	    .call(brush.extent(extent = extent.map(round)))
		.selectAll(".resize")
		.style("display", null);
	g.select("#clip-" + id + " rect")
	    .attr("x", x(extent[0]))
	    .attr("width", x(extent[1]) - x(extent[0]));
	dimension.filterRange(extent);
    });
    brush.on("brushend.chart", function() {
	if (brush.empty()) {
	    var div = d3.select(this.parentNode.parentNode.parentNode);
	    div.select(".title a").style("display", "none");
	    div.select("#clip-" + id + " rect").attr("x", null).attr("width", "100%");
	    dimension.filterAll();
	}
    });
    chart.margin = function(_) {
	if (!arguments.length) return margin;
	margin = _;
	return chart;
    };
    chart.x = function(_) {
	if (!arguments.length) return x;
	x = _;
	axis.scale(x);
	brush.x(x);
	return chart;
    };
    chart.y = function(_) {
	if (!arguments.length) return y;
	y = _;
	return chart;
    };
    chart.dimension = function(_) {
	if (!arguments.length) return dimension;
	dimension = _;
	return chart;
    };
    chart.filter = function(_) {
	if (_) {
	    brush.extent(_);
	    dimension.filterRange(_);
	} else {
	    brush.clear();
	    dimension.filterAll();
	}
	brushDirty = true;
	return chart;
    };
    chart.group = function(_) {
	if (!arguments.length) return group;
	group = _;
	return chart;
    };
    chart.round = function(_) {
	if (!arguments.length) return round;
	round = _;
	return chart;
    };
    return d3.rebind(chart, brush, "on");
}*/


