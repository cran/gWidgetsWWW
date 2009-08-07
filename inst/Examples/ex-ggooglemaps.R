w <- gwindow("Test of widgets")
g <- ggroup(cont = w, horizontal = FALSE)
glabel("How big is Central Park? Click around perimeter to see.", cont=g)
gmap <- ggooglemaps(x = "Central Park New York, NY", title = "Central Park, New York, NY", cont = g)

## set zoom level
gmap$setZoom(13)
addHandlerClicked(gmap, handler = function(h,...) {
  cat('alert("hi")');
  ## add a marker
#  gmap$addMarker(h$latlng)
})

b <- gbutton("draw polygon", cont = g, handler = function(h,...) {
  ## draw polygon using current markers
 cat('alert("hi")');
 #gmap$addPolygon()
})

gstatusbar("Powered by RApache and gWidgetsWWW", cont = w)
visible(w) <- TRUE



 
