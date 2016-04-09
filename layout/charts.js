function bertin(id, data, candidate, colors) {

//var maxValue = d3.max(data, function(d) { return +d.delegates; })

var chart_margin = {top: 10, right: 10, bottom: 10, left: 10},
    chart_width = 458 - chart_margin.left - chart_margin.right,
    chart_height = 80 - chart_margin.top - chart_margin.bottom;

/*var x = d3.scale.ordinal()
	.rangeRoundBands([0, chart_width], .1);*/

var y_votes = d3.scale.linear()
    .range([chart_height, 0])
    .domain([0, 1077221]); // Max votes for Trump (in Florida)
var y_del = d3.scale.linear()
    .range([chart_height, 0])
    .domain([0, 99]); // Max delegates sent for Trump (in Florida also...);

/*var xAxis = d3.svg.axis()
    .scale(x)
    .orient("bottom")
    .ticks(0, "");

var yAxis = d3.svg.axis()
    .scale(y)
    .orient("left")
    .ticks(0, "");*/

var chart_svg = d3.select(id).append("svg")
    .attr("width", chart_width + chart_margin.left + chart_margin.right)
    .attr("height", chart_height + chart_margin.top + chart_margin.bottom)
  .append("g")
    .attr("transform", "translate(" + chart_margin.left + "," + chart_margin.top + ")");

	//x.domain(data.map(function(d) { return d.state; }));
	//y_votes.domain([0, d3.max(data, function(d) { return +d.votes; })]);
	//y_del.domain([0, d3.max(data, function(d) { return +d.delegates; })]);

  /*svg.append("g")
      .attr("class", "x axis")
      .attr("transform", "translate(0," + chart_height + ")")
      .call(xAxis);*/

  /*svg.append("g")
      .attr("class", "y axis")
      .call(yAxis);*/
    /*.append("text")
      .attr("transform", "rotate(-90)")
      .attr("y", 6)
      .attr("dy", ".71em")
      .style("text-anchor", "end")
      .text("Popular votes");*/





   // BACKGROUND INVISIBLE BARS FOR HOVERING //
  chart_svg.selectAll(".bar")
      .data(data)
    .enter().append("rect")
      .attr("class", "bar")
      .attr("bar_state", function(d){
      	return "st"+d.ansi_code;
      })
      //.attr("x", function(d) { return x(d.state); })
      .attr("x", function(d, i) { return i*14; })
      //.attr("width", x.rangeBand())
      .attr("width", 10)
      .attr("y", 0)
      .attr("height", chart_height+chart_margin.top+chart_margin.bottom)
      .attr("fill", "#fff")
      .on("mouseover", function(d){
      	console.log(candidate);
      	console.log(d.state);
      	console.log(d.votes);
      	console.log(d.delegates);
      	currentState = "st"+d.ansi_code;
      	console.log("current state: "+currentState);
      	highlight_map();
      	highlight_chart();
      	highlight_stacked();
      });

	// POPULAR VOTE //
  chart_svg.selectAll(".bar_votes")
      .data(data)
    .enter().append("rect")
      .attr("class", "bar bar_votes")
      .attr("bar_state", function(d){
      	return "st"+d.ansi_code;
      })
      //.attr("x", function(d) { return x(d.state); })
      .attr("x", function(d, i) { return i*14; })
      //.attr("width" x.rangeBand())
      .attr("width", 5)
      .attr("y", function(d) { return y_votes(d.votes); })
      .attr("height", function(d) { return chart_height - y_votes(d.votes); })
      .attr("fill", colors[0])
      .on("mouseover", function(d){
      	console.log(candidate);
      	console.log(d.state);
      	console.log(d.votes);
      	console.log(d.delegates);
      	currentState = "st"+d.ansi_code;
      	console.log("current state: "+currentState);
      	highlight_map();
      	highlight_chart();
      	highlight_stacked();
      });

   // DELEGATES SENT //
  chart_svg.selectAll(".bar_del")
      .data(data)
    .enter().append("rect")
      .attr("class", "bar bar_del")
      .attr("bar_state", function(d){
      	return "st"+d.ansi_code;
      })
      //.attr("x", function(d) { return x(d.state); })
      .attr("x", function(d, i) { return (i*14)+5; })
      //.attr("width", x.rangeBand())
      .attr("width", 5)
      .attr("y", function(d) { return y_del(d.delegates); })
      .attr("height", function(d) { return chart_height - y_del(d.delegates); })
      .attr("fill", colors[1])
      .on("mouseover", function(d){
      	console.log(candidate);
      	console.log(d.state);
      	console.log(d.votes);
      	console.log(d.delegates);
      	currentState = "st"+d.ansi_code;
      	console.log("current state: "+currentState);
      	highlight_map();
      	highlight_chart();
      	highlight_stacked();
      });

   // SCALES //
   chart_svg.append("line")
	   	.attr("class", "chart_line_votes")
	   	.attr("x1", 0)
	   	.attr("y1", function() { return y_votes(1000000);} )
	   	.attr("x2", chart_width)
	   	.attr("y2", function() { return y_votes(1000000);} )
	   	.style("stroke", "#555")
	   	.style("stroke-width", .5)
	   	.style("stroke-dasharray", 3);
	chart_svg.append("text")
		.attr("class", "chart_line_votes_text")
	   	.attr("x", 0)
	   	.attr("y", function() { return y_votes(1039500);} )
	   	.style("text-anchor", "start")
	   	.style("fill", "#555")
	   	.text("1M votes");

   chart_svg.append("line")
	   	.attr("class", "chart_line_del")
	   	.attr("x1", 0)
	   	.attr("y1", function() { return y_del(50);} )
	   	.attr("x2", chart_width)
	   	.attr("y2", function() { return y_del(50);} )
	   	.style("stroke", "#aaa")
	   	.style("stroke-width", .5)
	   	.style("stroke-dasharray", 1);
	chart_svg.append("text")
		.attr("class", "chart_line_del_text")
	   	.attr("x", chart_width)
	   	.attr("y", function() { return y_del(54);} )
	   	.style("text-anchor", "end")
	   	.style("fill", "#aaa")
	   	.text("50 del.");

/*	function type(d) {
	  d.delegates = +d.delegates;
	  return d;
	}*/

}

function cumulChart(id, dataset) {
	//Width and height
      var h = 150;
      var w = 700;

		//Set up stack method
      var stack = d3.layout.stack();

      //Data, stacked
      stack(dataset);

      //Set up scales
      var xScale = d3.scale.ordinal()
        .domain(d3.range(dataset[0].length))
        .rangeRoundBands([0, h], 0.2); // This is actually the Y scale (candidates)
    
      var yScale = d3.scale.linear()
        .domain([0,       
          d3.max(dataset, function(d) {
            return d3.max(d, function(d) {
              return d.y0 + d.y;
            });
          })
        ])
        .range([0, w]); // This is actually the X Scale (States)
        
      //Easy colors accessible via a 10-step ordinal scale
      var colors = d3.scale.category10();
    
      //Create SVG element
      var svg = d3.select(id)
            .append("svg")
            .attr("width", w)
            .attr("height", h);
  
      // Add a group for each row of data
      var groups = svg.selectAll("g")
        .data(dataset)
        .enter()
        .append("g");
        /*.style("fill", function(d, i) {
          return colors(i);
        });*/
  
      // Add a rect for each data value
      var rects = groups.selectAll("rect")
        .data(function(d) { return d; })
        .enter()
        .append("rect")
        .attr("class", "stacked")
        .attr("stacked_state", function(d) { return "st"+d.state; })
        .attr("x", function(d) {
          return yScale(d.y0);
        })
        .attr("y", function(d, i) {
          return xScale(i);
        })
        .attr("width", function(d) {
          return yScale(d.y);
        })
        .attr("height", xScale.rangeBand())
        .style("fill", function(d, i) {
          return repColorsLight[i];
        })
        .style("stroke", function(d, i) {
          return repColors[i];
        })
        .on("mouseover", function(d) {
          console.log(d.state);
        }).on("mouseover", function(d){
      	currentState = "st"+d.state;
      	highlight_map();
      	highlight_chart();
      	highlight_stacked();
      });

/*	var cumulChart_margin = {top: 10, right: 10, bottom: 10, left: 10},
	    cumulChart_width = 458 - cumulChart_margin.left - cumulChart_margin.right,
	    cumulChart_height = 80 - cumulChart_margin.top - cumulChart_margin.bottom;

	var x_del = d3.scale.linear()
	    .range([0, cumulChart_width])
	    .domain([0, 2472]); // Total number of delegates to send
	var x_votes = d3.scale.linear()
	    .range([0, cumulChart_width])
	    .domain([0, 40000000]); // 40M estimate number of potential voters

    var cumulChart_svg = d3.select(id).append("svg")
	    .attr("width", cumulChart_width + cumulChart_margin.left + cumulChart_margin.right)
	    .attr("height", cumulChart_height + cumulChart_margin.top + cumulChart_margin.bottom)
	  .append("g")
	    .attr("transform", "translate(" + cumulChart_margin.left + "," + cumulChart_margin.top + ")");

	// DELEGATES COUNT //
  cumulChart_svg.selectAll(".cumulBar_del")
      .data(data)
    .enter().append("rect")
      .attr("class", "cumulBar cumulBar_del")
      .attr("cumulBar_state", function(d){
      	return "st"+d.ansi_code;
      })
      .attr("x", function(d, i) { return x_del(i*110); })
      .attr("width", function(d) {return x_del(d.delegates);} )
      .attr("y", 0)
      .attr("height", 10)
      .attr("fill", colors[1])
      .on("mouseover", function(d){
      	console.log(candidate);
      	console.log(d.state);
      	console.log(d.votes);
      	console.log(d.delegates);
      	currentState = "st"+d.ansi_code;
      	console.log("current state: "+currentState);
      	highlight_map();
      	highlight_chart();
      });*/
   /* // VOTES COUNT //
  cumulChart_svg.selectAll(".cumulBar_votes")
      .data(data)
    .enter().append("rect")
      .attr("class", "cumulBar cumulBar_votes")
      .attr("cumulBar_state", function(d){
      	return "st"+d.ansi_code;
      })
      .attr("x", function(d, i) { return x_votes(i*1100000); })
      .attr("width", function(d) {return x_votes(d.votes);} )
      .attr("y", 10)
      .attr("height", 10)
      .attr("fill", colors[0])
      .on("mouseover", function(d){
      	console.log(candidate);
      	console.log(d.state);
      	console.log(d.votes);
      	console.log(d.delegates);
      	currentState = "st"+d.ansi_code;
      	console.log("current state: "+currentState);
      	highlight_map();
      	highlight_chart();
      });
*/
} // cumulChart()