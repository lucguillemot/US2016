function bertin(id, data, candidate, colors, party) {

	var chart_margin = {top: 10, right: 10, bottom: 10, left: 10},
    chart_width = 458 - chart_margin.left - chart_margin.right,
    chart_height = 80 - chart_margin.top - chart_margin.bottom;

	if(party == "republicans") {
		var y_votes = d3.scale.linear()
		    .range([chart_height, 0])
		    .domain([0, 20866873/19]); //Total number of votes cast
		var y_del = d3.scale.linear()
		    .range([chart_height, 0])
		    .domain([0, 1512/19]); // number of sent delegates
	}
	else {
		var y_votes = d3.scale.linear()
		    .range([chart_height, 0])
		    .domain([0, 16297485/17]);//Total number of votes cast
		var y_del = d3.scale.linear()
		    .range([chart_height, 0])
		    .domain([0, 2824/17]); // number of sent delegates
	}

	var chart_svg = d3.select(id).append("svg")
    .attr("width", chart_width + chart_margin.left + chart_margin.right)
    .attr("height", chart_height + chart_margin.top + chart_margin.bottom)
  	  .append("g")
    .attr("transform", "translate(" + chart_margin.left + "," + chart_margin.top + ")");

  	// BACKGROUND INVISIBLE BARS FOR HOVERING //
  	chart_svg.selectAll(".bar")
      .data(data)
    .enter().append("rect")
      .attr("class", "bar")
      .attr("bar_state", function(d){
      	return "st"+d.ansi_code;
      })
      .attr("x", function(d, i) { return i*14; })
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
      	highlight_chart_state_name();
      });

	// POPULAR VOTE //
  	chart_svg.selectAll(".bar_votes")
      .data(data)
    .enter().append("rect")
      .attr("class", "bar bar_votes "+candidate)
      .attr("bar_state", function(d){
      	return "st"+d.ansi_code;
      })
      .attr("x", function(d, i) { return i*14; })
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
      	highlight_chart_state_name();
      });

   // DELEGATES SENT //
  chart_svg.selectAll(".bar_del")
      .data(data)
    .enter().append("rect")
      .attr("class", "bar bar_del "+candidate)
      .attr("bar_state", function(d){
      	return "st"+d.ansi_code;
      })
      .attr("x", function(d, i) { return (i*14)+5; })
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
      	highlight_chart_state_name();
      });

   // SCALES //
   chart_svg.append("line")
	   	.attr("class", "chart_line_votes"+party)
	   	.attr("x1", 0)
	   	.attr("y1", function() { return y_votes(1000000);} )
	   	.attr("x2", chart_width)
	   	.attr("y2", function() { return y_votes(1000000);} )
	   	.style("stroke", "#555")
	   	.style("stroke-width", .5)
	   	.style("stroke-dasharray", 3);
	chart_svg.append("text")
		.attr("class", "chart_line_votes_text"+party)
	   	.attr("x", 0)
	   	.attr("y", function() { return y_votes(829500);} )
	   	.style("text-anchor", "start")
	   	.style("fill", "#555")
	   	.text("1M votes");

   chart_svg.append("line")
	   	.attr("class", "chart_line_del"+party)
	   	.attr("x1", 0)
	   	.attr("y1", function() { return y_del(50);} )
	   	.attr("x2", chart_width)
	   	.attr("y2", function() { return y_del(50);} )
	   	.style("stroke", "#aaa")
	   	.style("stroke-width", .5)
	   	.style("stroke-dasharray", 1);
	chart_svg.append("text")
		.attr("class", "chart_line_del_text"+party)
	   	.attr("x", chart_width)
	   	.attr("y", function() { return y_del(54);} )
	   	.style("text-anchor", "end")
	   	.style("fill", "#aaa")
	   	.text("50 del.");

	 // STATE NAME //
	 chart_svg.selectAll("text")
      .data(data)
    .enter().append("text")
		.attr("class", "chart_state_name")
		.attr("chart_state_name", function(d){
	      	return "st"+d.ansi_code;
	      })
	   	.attr("x", function(d, i) { return i*14+5; })
	   	.attr("y", chart_height+(chart_margin.top))
	   	.style("text-anchor", "middle")
	   	.style("fill", "#fff")
	   	.text(function(d) {
	   		return d.state;
	   	});

}

function update_bertin(data, candidate, colors, party) {
	var chart_margin = {top: 10, right: 10, bottom: 10, left: 10},
    chart_width = 458 - chart_margin.left - chart_margin.right,
    chart_height = 80 - chart_margin.top - chart_margin.bottom;

	if(party == "republicans") {
		var y_votes = d3.scale.linear()
		    .range([chart_height, 0])
		    .domain([0, 20866873/19]); //Total number of votes cast
		var y_del = d3.scale.linear()
		    .range([chart_height, 0])
		    .domain([0, 1512/19]); // number of sent delegates
	}
	else {
		var y_votes = d3.scale.linear()
		    .range([chart_height, 0])
		    .domain([0, 16297485/17]); //Total number of votes cast
		var y_del = d3.scale.linear()
		    .range([chart_height, 0])
		    .domain([0, 2824/17]); // number of sent delegates
	}

	d3.selectAll(".bar_votes."+candidate)
		.data(data)
		.transition()
		.duration(500)
    	.attr("y", function(d) { return y_votes(d.votes); })
    	.attr("height", function(d) { return chart_height - y_votes(d.votes); })
    	.style("fill", colors[0]);
    d3.selectAll(".bar_del."+candidate)
      .data(data)
      .transition()
      .duration(500)
      .attr("y", function(d) { return y_del(d.delegates); })
      .attr("height", function(d) { return chart_height - y_del(d.delegates); })
      .style("fill", colors[1]);
    
   /* d3.selectAll(".chart_line_votes"+party)
    	.transition()
    	.duration(500)
    	.attr("y1", function() { return y_votes(1000000);} )
	   	.attr("y2", function() { return y_votes(1000000);} );
	d3.selectAll(".chart_line_votes_text"+party)
    	.transition()
    	.duration(500)
	   	.attr("y", function() { return y_votes(829500);} );
	d3.selectAll(".chart_line_del"+party)
    	.transition()
    	.duration(500)
    	.attr("y1", function() { return y_del(50);} )
	   	.attr("y2", function() { return y_del(50);} );
	d3.selectAll(".chart_line_del_text"+party)
    	.transition()
    	.duration(500)
	   	.attr("y", function() { return y_del(54);} );*/
}


// // // // // // // // // // // // // // // // // // // // // // // // // // // //
// // STACKED // // // // // // // // // // // // // // // // // // // // // // // 
 // // // // // // // // // // // // // // // // // // // // // // // // // // // 

function cumulChart(id, dataset, party) {
	//Width and height
	var m = {top: 10, right: 10, bottom: 10, left: 10}, // margins
  		h = 150 - m.left - m.right, // height
  		w = 1158 - m.top - m.bottom; // width

	//Set up stack method
  	var stack = d3.layout.stack();

  	//Data, stacked
  	stack(dataset);

  	//Set up scales
  	if (party == "republicans") {
		var xScale = d3.scale.ordinal()
			.domain(d3.range(dataset[0].length))
			.rangeRoundBands([0, h], 0.2); // This is actually the Y scale (candidates)
	   	var yScale = d3.scale.linear()
		    .domain([0, 1237])
		    .range([0, w]); // This is actually the X Scale (States)
	}
	else {
		var xScale = d3.scale.ordinal()
			.domain(d3.range(dataset[0].length))
			.rangeRoundBands([0, h/2], 0.2); // This is actually the Y scale (candidates)
	   	var yScale = d3.scale.linear()
		    .domain([0, 2382])
		    .range([0, w]); // This is actually the X Scale (States)
	}
 
  	//Create SVG element
  	var svg = d3.select(id)
        .append("svg")
        .attr("width", w)
        .attr("height", h);

  	// Add a group for each row of data
  	var groups = svg.selectAll("g")
	    .data(dataset)
	    .enter()
	    .append("g")
	    .attr("class", "g_stacked "+party);

  	// Add a rect for each data value
  	var rects = groups
  		.selectAll("rect")
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
	    	if (party == "republicans") {
	    		return repColorsLight[i];
	    	}
	    	else {
	    		return demColorsLight[i];
	    	}
	    })
	    .style("stroke", function(d, i) {
	      	if (party == "republicans") {
	    		return repColors[i];
	    	}
	    	else {
	    		return demColors[i];
	    	}
	    })
	    .on("mouseover", function(d) {
	      console.log(d.state);
	    })
	    .on("mouseover", function(d){
		  	currentState = "st"+d.state;
		  	currentCandidate = d.x;
		  	highlight_map();
		  	highlight_chart();
		  	highlight_stacked();
		  	highlight_chart_state_name();
	  	});

 	svg.append("line")
	   	.attr("class", "stacked_del_line")
	   	.attr("x1", w)
	   	.attr("y1", 0)
	   	.attr("x2", w)
	   	.attr("y2", h)
	   	.style("stroke", "#333")
	   	.style("stroke-width", 1)
	   	.style("stroke-dasharray", 3);
	svg.append("text")
		.attr("class", "stacked_del_line_text")
	   	.attr("x", w-10)
	   	.attr("y", 10)
	   	.style("text-anchor", "end")
	   	.style("fill", "#333")
	   	.text("1237 delegates to win");

} // cumulChart()

function update_cumulChart(dataset, party) {
	var m = {top: 10, right: 10, bottom: 10, left: 10}, // margins
  		h = 150 - m.left - m.right, // height
  		w = 1158 - m.top - m.bottom; // width

	//Set up stack method
	var stack = d3.layout.stack();

	//Data, stacked
	stack(dataset);

	//Set up scales
	if (party == "republicans") {
		var xScale = d3.scale.ordinal()
			.domain(d3.range(dataset[0].length))
			.rangeRoundBands([0, h], 0.2); // This is actually the Y scale (candidates)
	   	var yScale = d3.scale.linear()
		    .domain([0, 1237])
		    .range([0, w]); // This is actually the X Scale (States)
	}
	else {
		var xScale = d3.scale.ordinal()
			.domain(d3.range(dataset[0].length))
			.rangeRoundBands([0, h/2], 0.2); // This is actually the Y scale (candidates)
	   	var yScale = d3.scale.linear()
		    .domain([0, 2382])
		    .range([0, w]); // This is actually the X Scale (States)
	}

    var gr = d3.selectAll(".g_stacked."+party)
    	.data(dataset);
      gr.selectAll(".stacked")
		.data(function(d) { return d; })
		.transition()
      	.duration(500)
      	.attr("x", function(d) {
	      return yScale(d.y0);
	    })
	    .attr("y", function(d, i) {
	      return xScale(i);
	    })
	    .attr("width", function(d) {
	      return yScale(d.y);
	    });
	
	d3.selectAll(".stacked_del_line_text")
		.transition()
		.duration(500)
		.text(function() {
			if (party == "republicans") {
				return "1237 delegates to win";
			}
			else {
				return "2382 delegates to win";
			}
		});
}