var req = new XMLHttpRequest()
req.open('GET', '/admin/api/stat', false)
req.send()
data = JSON.parse(req.responseText)
var userData = data.users
var smsData=data.sms
var fileData=data.file
var chatData=data.chat
var sessionData=data.session
var total_regs = data.length

var date_formatter = d3.time.format("%d.%m.%y")	
var countryColors = d3.scale.category20()

Array.prototype.forEach.call(userData, function(d,i) {
    d.date = new Date(d.timestamp)
})
Array.prototype.forEach.call(smsData, function(d,i) {
    d.date = new Date(d.timestamp)
})
Array.prototype.forEach.call(chatData, function(d,i) {
    d.date = new Date(d.timestamp)
})
Array.prototype.forEach.call(fileData, function(d,i) {
    d.date = new Date(d.timestamp)
})

var total_sessions = sessionData.length
var average = 0
sessionData.forEach(function(d) {
    average+=d.length/total_sessions
})
average = Math.round(average/1000000)
$('#average').html(average+' sec')

var file_types = ['image','audio','video','application']
function getMiB(bytes) {
    return Math.floor(bytes/100000)/10
}

var dataset ={
    reg: crossfilter(userData),
    active_users: crossfilter(
	Array.prototype.filter.call(userData, function(d, i) {
	    return (Date.now() - d.timestamp < 10*24*60*60*1000)
	})),
    sms: crossfilter(smsData),
    file: crossfilter(fileData),
    chat: crossfilter(chatData)
}
var dim = {
    date: dataset.reg.dimension(function(d) {return d.date}),
    country: dataset.reg.dimension(function(d) {return d.country}),
    active: dataset.active_users.dimension(function(d) {return d.country}),
    sms_country: dataset.sms.dimension(function(d) {return d.country}),
    sms_date: dataset.sms.dimension(function(d) {return d.date}),
    file_date: dataset.file.dimension(function(d) {return d.date}),
    file_country: dataset.file.dimension(function(d) {return d.country}),
    chat_country: dataset.chat.dimension(function(d) {return d.country}),
    chat_date: dataset.chat.dimension(function(d) {return d.date}),
}

var first_user_date = dim.date.bottom(1).length?dim.date.bottom(1)[0].date:new Date()

var date_interval = [
    new Date(first_user_date.getTime()-24*60*60*1000),
    new Date(new Date().getTime()+24*60*60*1000)
]

var filters = {
    country: [],
    date: date_interval.map(function(d) {
	return new Date(d.getTime())
    })
}

//select2 initialization
var select_data = dim.country.group().all()
    .map(function(d) {
	return {id: d.key, text: d.key}
    })
var select2 = $('#select2').select2({
    placeholder: "All countries",
    data: select_data
})

select2.on('change', function(e) {
    var res = select2.val() || []
    filters.country = res
    apply_filters()
})
/////////////////////////////////
//date filters initialization
var date_input1=$('#d1').datepicker({
	dateFormat: "dd.mm.y",
	minDate: date_interval[0],
	maxDate: date_interval[1]
    })
    .datepicker('setDate',date_interval[0])
    .on('change', function() {
	filters.date[0] = $(this).datepicker('getDate')
	apply_filters()
    })

var date_input2=$('#d2').datepicker({
	dateFormat:"dd.mm.y",
	minDate: date_interval[0],
	maxDate: date_interval[1]
    })
    .datepicker('setDate',date_interval[1])
    .on('change', function() {
	filters.date[1] = $(this).datepicker('getDate')
	apply_filters()
    })
////////////////////////////////////
var charts = [
    pieChart({
	selector: '#reg-chart',
	dim: dim.country,
	colors: countryColors
    }),
    pieChart({
	selector: '#active-chart',
	dim: dim.active,
	colors: countryColors
    }),
    pieChart({
	selector: "#sms-chart",
	dim: dim.sms_country,
	colors: countryColors
    }),
    pieChart({
	selector: "#chat-chart",
	dim: dim.chat_country,
	colors: countryColors
    }),
    lineChart({
	selector: "#reg-timeline",
	dim: [dim.date,dim.sms_date],
	labels: ['registered_users', 'sms'],
	colors: countryColors
    }),
    stackedBarsChart({
	selector: "#file-timeline",
	dim: dim.file_date,
	colors: countryColors,
	reduce_fn: {
	    add: function(p,v) {
		var type = v.type.split('/')[0]
		if(!p.files[type])
		    p.files[type] = {
			count:0,
			size:0,
		    }
		p.files[type].size+=v.size
		p.files[type].count+=1
		p.total_size+=v.size
		p.total+=1
		return p
	    },
	    remove: function(p,v) {
		var type = v.type.split('/')[0]
		p.files[type].size-=v.size
		p.files[type].count-=1
		p.total_size-=v.size
		p.total-=1
		return p
	    },
	    init: function() {
		return {
		    files: {},
		    total_size: 0,
		    total:0
		}
	    }
	}
    })	
]
function redraw() {
    charts.forEach(function(d) {d.redraw()})
}
redraw()
//////////////////////////////////////
function apply_filters() {
    function countryFilter(d) {
	if(filters.country.length)
	    return filters.country.indexOf(d)>=0
	else return true
    }

    select2.val(filters.country)
	.trigger('change.select2')
    date_input1.datepicker('setDate',filters.date[0])
    date_input2.datepicker('setDate',filters.date[1])

    dim.country.filter(countryFilter)
    dim.active.filter(countryFilter)
    dim.sms_country.filter(countryFilter)
    dim.file_country.filter(countryFilter)

    dim.date.filter(filters.date)
    dim.sms_date.filter(filters.date)
    dim.chat_date.filter(filters.date)

    redraw()
}
/////////////////////////////////////
function pieChart(opt) {
    var radius = 125
    var width = 250
    var height = 250
    var dim = opt.dim
    var selector = opt.selector
    var scale = opt.colors

    var total = function () {
	return dim.groupAll().value()
    }
    var filtered_total = function() {
	return dim.top(Infinity).length 
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
    
    var totalText = svg.append('svg:text')
	.attr('class', 'total')
	.attr('text-anchor','middle')
	.attr('alignment-baseline','middle')
	.attr('font-size',radius/3+'px')

    var path = svg.selectAll('path')
	
    return {
	redraw: function() {
	    path = path.data(pie(dim.group().all()))
		.attr('d', arc)
		.attr('class', function(d) {
		    var cls = 'pie-segment '
		    if(filters.country.length) {
			if(filters.country.indexOf(d.data.key)>=0) {
			    cls+="active"
			} else {
			    cls+='inactive'
			}
		    }
		    return cls
		})

	    path
		.enter()
		.append('path')
		.attr('class', function(d) {
		    var cls = 'pie-segment '
		    if(filters.country.length) {
			if(filters.country.indexOf(d.data.key)>=0) {
			    cls+="active"
			} else {
			    cls+='inactive'
			}
		    }
		    return cls
		})
		.attr('fill', function(d) { 
		    return scale(d.data.key)
		})
		.attr('d', arc)

	    totalText.text(filtered_total())

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

	    path.on('click', function(d) {
		var index = filters.country.indexOf(d.data.key)
		if(index>=0) {
		    filters.country.splice(index,1)
		} else {
		    filters.country.push(d.data.key)
		}
		apply_filters()
	    })
	}
    }
} 
///////////////////////////////////////
function lineChart(opt) {
    var selector = opt.selector
    var dims = opt.dim
    var labels = opt.labels

    var margin = {
	top: 20,
	bottom: 80,
	left: 60,
	right: 20
    }
    
    var width = 800-margin.left-margin.right
    var height = 300-margin.top-margin.bottom


    var scale = {
	x: d3.time.scale()
	    .domain(date_interval)
	    .range([0, width]),
	y: d3.scale.linear()
	    .range([height,0])
    }

    var axis = {
	x: d3.svg.axis().scale(scale.x)
	    .orient("bottom")
	    .ticks(10),
	y: d3.svg.axis().scale(scale.y)
	    .orient('left')
	    .ticks(5)
    }

    var svg = d3.select(selector)
	.append('svg')
	.attr('width',width+margin.left+margin.right)
	.attr('height',height+margin.top+margin.bottom)
	.append('g')
	    .attr("transform", "translate("+margin.left+","+margin.top+")")

    svg.append('g')
	.attr('class', 'x axis')
	.attr("transform", "translate(0," + height + ")")
	.call(axis.x)

    svg.append('g')
	.attr('class', 'y axis')
	.call(axis.y)

    var line_gen = d3.svg.line()
	.x(function(d) {return scale.x(new Date(d.key.getTime()+12*60*60*1000))})
	.y(function(d) {return scale.y(d.value)})
	//.interpolate('basis')

    var brush = d3.svg.brush()
	.x(scale.x)
	.on('brush',function() {
	    extent1 = brush.extent().map(d3.time.day.round)
	    d3.select(this)
		.call(brush.extent(extent1))
	    if(extent1[0]-extent1[1]==0) {
		filters.date = date_interval
	    } else {
		filters.date = extent1
	    }
	    apply_filters()
	})

    svg.append('g')
	.attr('class','brush')
	.call(brush)
	.selectAll('rect')
	    .attr("y", -6)
	    .attr("height", height + 7)

    var tooltip = d3.select(selector)
	.append('div')
	.attr('class','tooltip')
    tooltip.append('div')
	.attr('class','label')

    labels.forEach(function(d) {
	tooltip.append('div')
	    .attr('class',d)
    })

    var tooltip_line = svg.append('line')
	.attr('class', 'tooltip-line')
	.attr('y1',0)
	.attr('y2', height)
   
    var mouse_catcher = svg.append('rect')
	.attr('width',width)
	.attr('height', height)
	.style('fill','none')
	.style('pointer-events','all')
	//propagate mouse events to brush under catcher
	.on('mousedown', function() {
	    brush_elm = svg.select(".brush").node()
	    new_click_event = new Event('mousedown')
	    new_click_event.pageX = d3.event.pageX
	    new_click_event.clientX = d3.event.clientX
	    new_click_event.pageY = d3.event.pageY
	    new_click_event.clientY = d3.event.clientY
	    brush_elm.dispatchEvent(new_click_event)
	})
	.on('mouseover', function() {
	    tooltip.style('display', 'block')
	    tooltip_line.style('display', 'block')
	})
	.on('mouseout', function() {
	    tooltip.style('display', 'none')
	    tooltip_line.style('display', 'none')
	})
	.on('mousemove', function() {
	    var hovered_date = scale.x.invert(d3.mouse(this)[0])
	    var hovered_day = d3.time.day(hovered_date)
	    var values = dims.map(function(d) {
		var el = d.group(d3.time.day)
		    .all()
		    .find(function(d) { 
			return d.key-hovered_day==0 
		    })

		
		return el?el.value:0
	    })

	    tooltip.select('.label').html(date_formatter(hovered_day))
	    labels.forEach(function (d,i) {
		tooltip.select('.'+d)
		    .html(d+": "+values[i])
	    })
    
	    tooltip.style('display','block')
	   
	    tooltip_line.attr('x1',d3.mouse(this)[0]) 
	    tooltip_line.attr('x2',d3.mouse(this)[0])

	    tooltip.style('top', (d3.event.layerY + 10) + 'px')
		.style('left', (d3.event.layerX + 10) + 'px')
	    return true
	})

    return {
	redraw: function() {
	    var max_counts = dims.map(function(d) {
		var tmp = d.group(d3.time.day).top(1) 
		return tmp.length?tmp[0].value:0
	    })
	    var y_domain = [
		0,
		Math.max.apply(null,max_counts)
	    ]
	    scale.y.domain(y_domain)

	    svg.select('.x.axis')
		.call(axis.x) 
		.selectAll("text")  
		    .style("text-anchor", "end")
		    .attr("dx", "-.8em")
		    .attr("dy", ".15em")
		    .attr("transform", "rotate(-65)" )

	    svg.select('.y.axis')
		.call(axis.y)

	    var global_counter = 0
	    var path = svg.selectAll('.line')
		.data(dims.map(function(d) {
			return d.group(d3.time.day).all()
		    }))
		.attr('d', line_gen)
	    path.enter()
		    .append('path')
		    .attr('class','line')
		    .attr('d', function(d) {
			return line_gen(d)
		    })
		    .style('stroke', function(d) {
			var clr = opt.colors(labels[global_counter])
			global_counter++
			return clr
		    })
	    if(!(filters.date[0]-date_interval[0]==0)
	       || !(filters.date[1]-date_interval[1]==0))
		svg.select('.brush').call(brush.extent(filters.date))
	    else
		svg.select('.brush').call(brush.clear())
	}
    }
}
//////////////////////////////////////
function stackedBarsChart(opt) {
    var selector = opt.selector
    var dim = opt.dim
    var reduce_fn = opt.reduce_fn
    var group = dim.group(d3.time.day)
	.reduce(reduce_fn.add, reduce_fn.remove, reduce_fn.init)

    var margin = {
	top: 20,
	bottom: 80,
	left: 60,
	right: 20
    }
    
    var width = 800-margin.left-margin.right
    var height = 300-margin.top-margin.bottom


    var scale = {
	x: d3.time.scale()
	    .domain(date_interval)
	    .range([0, width]),
	y: d3.scale.linear()
	    .range([height,0])
    }

    var axis = {
	x: d3.svg.axis().scale(scale.x)
	    .orient("bottom")
	    .ticks(10),
	y: d3.svg.axis().scale(scale.y)
	    .orient('left')
	    .tickFormat(function(d) {
		return getMiB(d)+" MiB"
	    })
	    .ticks(5)
    }

    var svg = d3.select(selector)
	.append('svg')
	.attr('width',width+margin.left+margin.right)
	.attr('height',height+margin.top+margin.bottom)
	.append('g')
	    .attr("transform", "translate("+margin.left+","+margin.top+")")

    svg.append('g')
	.attr('class', 'x axis')
	.attr("transform", "translate(0," + height + ")")
	.call(axis.x)

    svg.append('g')
	.attr('class', 'y axis')
	.call(axis.y)

    var brush = d3.svg.brush()
	.x(scale.x)
	.on('brush',function() {
	    extent1 = brush.extent().map(d3.time.day.round)
	    d3.select(this)
		.call(brush.extent(extent1))
	    if(extent1[0]-extent1[1]==0) {
		filters.date = date_interval
	    } else {
		filters.date = extent1
	    }
	    apply_filters()
	})

    svg.append('g')
	.attr('class','brush')
	.call(brush)
	.selectAll('rect')
	    .attr("y", -6)
	    .attr("height", height + 7)

    var tooltip = d3.select(selector)
	.append('div')
	.attr('class','tooltip')
    tooltip.append('div')
	.attr('class','label')
    tooltip.append('div')
	.attr('class','total')
    tooltip.append('div')
	.attr('class','total_count')

    file_types.forEach(function(d) {
	tooltip.append('div')
	    .attr('class',d+" count")
	tooltip.append('div')
	    .attr('class',d+" size")
    })

    return {
	redraw: function() {
	    scale.y.domain([
		0,
		Math.max.apply(null,group.all().map(function(d) {return d.value.total_size}))
	    ])

	    var tmp = new Date()
	    var tmp2 = new Date()
	    tmp2.setDate(tmp2.getDate()-1)
	    var bar_width = scale.x(tmp)-scale.x(tmp2)

	    var data = group.all()
	    svg.select('.x.axis')
		.call(axis.x) 
		.selectAll("text")  
		    .style("text-anchor", "end")
		    .attr("dx", "-.8em")
		    .attr("dy", ".15em")
		    .attr("transform", "rotate(-65)" )

	    svg.select('.y.axis')
		.call(axis.y)

	    var bars = svg.selectAll('.bar')
		.data(group.all())
		
	    bars.enter()
		    .append('g')
		    .attr('class','bar')
		    .attr('transform', function(d) {
			return 'translate('+scale.x(d.key)+',0)'
		    })

	    var stacks = bars.selectAll('.stack')
		.data(function(d) {
		    var files = []
		    var y=0
		    file_types.forEach(function(t) {
			if(d.value.files[t]) {
			    files.push({name: t, y0: y, y1: y+=d.value.files[t].size})
			}
		    })
		    return files
		})
		.attr('y',function(d) {
		    return scale.y(d.y1)
		})
		.attr('height', function(d) {
		    return scale.y(d.y0)-scale.y(d.y1)
		})


	    stacks.enter()
		    .append('rect')
		    .attr('class','stack')
		    .attr('width',bar_width)
		    .attr('y',function(d) {
			return scale.y(d.y1)
		    })
		    .attr('height', function(d) {
			return scale.y(d.y0)-scale.y(d.y1)
		    })
		    .style('fill', function(d) {
			return opt.colors(d.name)
		    })

	    bars.on('mouseover', function(d) {

		tooltip.select('.label').html()
		    tooltip.select('.label').html(date_formatter(d.key))
		    tooltip.select('.total').html("total size: "+getMiB(d.value.total_size)+"MiB")
		    tooltip.select('.total_size').html("total count: "+d.value.total)
		    tooltip.style('display','block')
	
		for(i=0; i<file_types.length; i++) {
		    var f=file_types[i]
		    if(d.value.files[f] && d.value.files[f].count) {
			tooltip.select('.size.'+f).html(
			    f+"/* size: "+getMiB(d.value.files[f].size)+"MiB"
			    +" ("+Math.floor(100*d.value.files[f].size/d.value.total_size)+"%)")
			tooltip.select('.count.'+f).html(
			    f+"/* count: "
			    +d.value.files[f].count
			    +" ("+Math.floor(100*d.value.files[f].count/d.value.total)+"%)")
		    } else {
			tooltip.select('.size.'+f).html("")
			tooltip.select('.count.'+f).html("")
		    }
		}	    
	    })

	    bars.on('mouseout', function() {	
		tooltip.style('display', 'none')	
	    })
	    bars.on('mousemove', function(d) {
		tooltip.style('top', (d3.event.layerY + 10) + 'px')
		    .style('left', (d3.event.layerX + 10) + 'px')
	    });

	    if(!(filters.date[0]-date_interval[0]==0)
	       || !(filters.date[1]-date_interval[1]==0))
		svg.select('.brush').call(brush.extent(filters.date))
	    else
		svg.select('.brush').call(brush.clear())
	}
    }
}
