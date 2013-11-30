$.getJSON("data.json",
          function(data) {
              
              // console.log(data);

              var map = new google.maps.Map(document.getElementById('map'), {
                  zoom: data.zoom,
                  center: new google.maps.LatLng(data.center.lat, data.center.lon),
                  mapTypeId: google.maps.MapTypeId.ROADMAP
              });

              var infowindow = new google.maps.InfoWindow();

              var marker, i;

              for (i = 0; i < data.markers.length; i++) {
                  marker = new google.maps.Marker({
                      position: new google.maps.LatLng(data.markers[i].lat, data.markers[i].lon),
                      map: map,
                      icon: {
                          path: google.maps.SymbolPath.CIRCLE,
                          strokeColor: data.markers[i].color,
                          scale: 2
                      }
                  });

                  google.maps.event.addListener(marker, 'click', (function(marker, i) {
                      return function() {
                          infowindow.setContent(data.markers[i].label);
                          infowindow.open(map, marker);
                      }
                  })(marker, i));
              }
              

          });

