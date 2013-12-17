      // Create the Google Map…
      var map = new google.maps.Map(d3.select("#map").node(), {
          zoom: 11,
          center: new google.maps.LatLng(32, 35),
          mapTypeId: google.maps.MapTypeId.ROADMAP
      });
      
      var info =
      d3.select("#info")
      .append("svg")
      .attr("width", 1200)
      .attr("height", 600);
      
      var tooltip =
      info.append("text")
      .text("")
      .style("font-size", 20)
      .style("font-family", "Arial")
      .style("encoding", "UTF-8")
      .attr("x", 20)
      .attr("y", 20)
      .attr("fill", "#406");

      var canvas =
      info.append("svg")
      .attr("x", 20)
      .attr("y", 60)
      .attr("width", 1200)
      .attr("height", 500);      
      
      canvas.append("line");
      canvas.append("line");
      canvas.append("line");
      canvas.append("line");
      canvas.append("line");
      canvas.append("line");

      var amounts =
      [canvas.append("text")
       .attr("x", 100)
       .attr("y", 20),
       canvas.append("text")
       .attr("x", canvas.attr("width")-120)
       .attr("y", 20)];
      
      function updateReport(d) {
          title.text(
              //JSON.stringify(d.plotting)
              d.desc
          );
              
          
          var p=d.plotting;

          amounts[0].text(p.domains[0][1]);
          amounts[1].text(p.domains[1][1]);

          xscale = d3.scale.linear()
              .domain([0,1])
              .range([150, canvas.attr("width")-150]);
          yscale0 = d3.scale.linear()
              .domain(p.domains[0])
              .range([canvas.attr("height")-20, 20]);
          yscale1 = d3.scale.linear()
              .domain(p.domains[1])
              .range([canvas.attr("height")-20, 20]);
          
          canvas.selectAll("line")
          .data(p.lines)
              .attr("x1", xscale(0))
              .attr("y1", function(l) {return yscale0(l.ys[0]);})
              .attr("x2", xscale(1))
              .attr("y2", function(l) {return yscale1(l.ys[1]);})
              .attr("stroke-width", 6)
              .attr("stroke", function(l) {return l.color;});
      }

      var title =
      info.append("text")
      .text("")
      .style("font-size", 16)
      .style("font-family", "Arial")
      .style("encoding", "UTF-8")
      .attr("x", canvas.attr("width")/2-20)
      .attr("y", 40)
      .attr("fill", "black");
     
      // Load the station data. When the data comes back, create an overlay.
      d3.json("data.json", function(data) {
          
          var overlay = new google.maps.OverlayView();
          
          // Add the container when the overlay is added to the map.
          overlay.onAdd = function() {
              // https://groups.google.com/forum/#!topic/d3-js/eYnbIcfwhvw
              var layer = d3.select(this.getPanes().overlayMouseTarget)
                  .append("div")
                  .attr("class", "mysvg");
              
              // Draw each marker as a separate SVG element.
              // We could use a single SVG, but what size would it have?
              overlay.draw = function() {
                  var projection = this.getProjection(),
                  padding = 10;
                  
                  var marker = layer.selectAll("svg")
                      .data(data)
                      .each(transform) // update existing markers
                          .enter().append("svg:svg")
                      .each(transform)
                          .attr("class", "marker");
                  
                  // Add a circle.
                  marker.append("svg:circle")
                      .attr("r", 10)
                      .attr("cx", padding)
                      .attr("cy", padding);
                  
                  // Add a label.
                  // marker.append("svg:text")
                  //     .attr("x", padding + 7)
                  //     .attr("y", padding)
                  //     .attr("dy", ".31em")
                  //     //.text(function(d) { return d.desc; });
                  
                  
                  function transform(d) {
                      proj = projection.fromLatLngToDivPixel(
                          new google.maps.LatLng(d["mean-y"], d["mean-x"]));
                      return d3.select(this)
                          .style("left", (proj.x - padding) + "px")
                          .style("top", (proj.y - padding) + "px")
                          .style("opacity", 0.5)
                          .on("mouseover", function() {
                              tooltip.text(d.desc);
                              d3.select(this)
                                  .attr("r", 20)
                                  .attr("fill", "green");
                          })
                          .on("mouseout", function() {
                              tooltip.text("");
                              d3.select(this)
                                  .attr("r", 10)
                                  .attr("fill", "#505");
                          }).
                          on("click", function() {
                              updateReport(d);
                          });
                  }

                  
              };
          };
          
          // Bind our overlay to the map…
          overlay.setMap(map);
      });
      
