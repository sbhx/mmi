$.getJSON("d3data.json",
          function(data) {

              //Create SVG element
              var svg = d3.select("body")
                  .append("svg")
                  .attr("width", data.w)
                  .attr("height", data.h)
                  .style("background", "#222222");
              
              var lineFunction =
                  d3.svg.line()
                  .interpolate("linear")
                  .x(function(d) {return d.x;})
                  .y(function(d) {return d.y;})

              // var lineGraph =
              //     svg.append("path")
              //     .attr("d", lineFunction(data.lines))
              //     .attr("stroke", "grey")
              //     .attr("stroke-width", 2)
              //     .attr("fill", "none");
              
              svg.selectAll("line")
                  .data(data.lines)
                  .enter()
                  .append("line")
                  .attr("x1", function(d) {return d.x1;})
                  .attr("y1", function(d) {return d.y1;})
                  .attr("x2", function(d) {return d.x2;})
                  .attr("y2", function(d) {return d.y2;})
                  .attr("stroke-width", 2)
                  .attr("stroke", "grey");
	      
              var tooltip = svg.append("text");
		  
              svg.selectAll("circle")
                  .data(data.circles)
                  .enter()
                  .append("circle")
                  .attr("text", function(d) {return d.text;})
                  .attr("cx", function(d) {return d.cx;})
                  .attr("cy", function(d) {return d.cy;})
                  .attr("r", function(d) {return d.r;})
                  .style("fill", function(d) {return d.fill;})
                  .style("stroke", function(d) {return d.stroke;})
                  .style("opacity", function(d) {return d.opacity;})
                  .on("mouseover", function() {
                      var circle = d3.select(this)
                          .style("stroke", "cyan");
                      tooltip.attr("x", circle.attr("cx"))
                          .attr("y", circle.attr("cy") - circle.attr("r"))
                          .text(circle.attr("text"));
                  })
                  .on("mouseout", function() {
                      d3.select(this)
                          .style("stroke", function(d) {return d.fill;});
                      tooltip.text("");                      
                  })
                  // .append("title")
                  // .text(function(d) {return d.text;})
              ;

              tooltip.text("")
		  .attr("x", 100)
		  .attr("y", 100)
		  .attr("font-family", "sans-serif")
		  .attr("font-size", "20px")
		  .attr("fill", "cyan")
	      
          });


