$(document).ready(function() {
 
  document.getElementById("map-canvas").onclick = function() {
    geojson_shape="https://storage.googleapis.com/maps-devrel/google.json";
    Shiny.onInputChange("test_layer", geojson_shape);
  };
 
  Shiny.addCustomMessageHandler("updateGoogleMap",
     function(geojson_shape) {map.data.loadGeoJson(geojson_shape);
                            });
  
 
});