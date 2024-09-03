var datas = [[5,  4],[4, 3], [95, 149], [200, 14]]
	, height = 300
	, width = 300
	, padding = 20
	, xscale = d3.scale.linear()
									.domain([0, d3.max(datas, function(data){return data[0]})])
									.range([padding, width - padding])
	, yscale = d3.scale.linear()
									.domain([0, d3.max(datas, function(data){return data[1]})])
									.range([height - padding, padding])
	, rscale = d3.scale.linear()
									.domain([0, d3.max(datas, function(data){return data[0]})])
									.range([1, 8])

var svg = d3.select('body')
	.append('svg')
		.attr({
			height: height
		, width: width
		, fill: 'green'
		})

svg.selectAll('circle')
	.data(datas)
	.enter()
	.append('circle')
		.attr({
			r: function(data){
				return rscale(data[0])
			},
		 cx: function(data){
				return xscale(data[0])
		},
			cy: function(data){
				return yscale(data[1])
			},
		fill: 'grey'
		})
