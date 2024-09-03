var datas = [4, 188, 48, 99, 30]
	, height = 200
	, width = 400
	, padding = 20

var svg = d3.select('body')
						.append('svg')
						.attr('height', height)
						.attr('width', width)

svg.selectAll('rect')
	 .data(datas)
	 .enter()
	 .append('rect')
  	 .attr('height', function(data){
	  	 return data
  	 })
		 .attr('width', function(data){
			 return (width / datas.length - padding)
		 })
		 .attr('fill', 'grey')
		 .attr('x', function(data, index){
			 return index * (width / datas.length)
		 })
		 .attr('y'), function(data){
			 return height - data
		 }

		 svg.selectAll('text')
			 .data(datas)
			 .enter()
			 .append('text')
			 	.attr('fill', 'black')
				.attr('y', function(data, index){
					return height - data
				})
				.attr('x', function(data, index){
					return index * (width / datas.length)
				})
				.text(function(data){
					return data
				})
				.attr('font-size', '1.2em')
				.attr('font-family', 'Fira Code')
