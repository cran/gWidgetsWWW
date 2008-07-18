## Simple interface to googlemaps
## see the footer for ideas on how to access GMap2 API

## Use obj[,] <- data.frame(lat,long,title) to set markers
## * markers: [geoCodeAddr], [geoCodeAddr, title], [lat, long], [lat,long,title]

## center and markers set *prior* to rendering. No methods after (possible, not done)

## must set API before rendering via
## header such as the following for http://127.0.0.1:8079
## <script
## src="http://maps.google.com/maps?file=api&v=2.x&key=ABQIAAAAYpRTbDoR3NFWvhN4JrY1ahS5eHnalTx_x--TpGz1e2ncErJceBS7FrNBqzV5DPxkpbheIzZ9nTJPsQ"
## type="text/javascript"></script>

## the key comes from google : http://code.google.com/apis/maps




ggooglemaps <- function(x, title = "",  type = c("map","panorama"), container, ...) {
  widget <- EXTComponent$new(toplevel=container$toplevel,
                             ..title = title,
                             ..gmapType = match.arg(type))
  class(widget) <- c("gGoogleMap",class(widget))
  widget$setValue(value = x)
  ## default is 0-row set of marks
  widget$setValues(value = data.frame(lat=0,long=0,title="")[0,])
  widget$..runCmds <- c()
  
  ## add in code from Ext blog
  widget$scripts <- function(.) {
   out <- String(sep = "\n")
#    +
#      (String() + 'AddLibrary("script","' +
#       'http://maps.google.com/maps?file=api&v=2.x&key=' +
#       .$APIKEY + '");')
    
    out <- out +
      paste('/**',
            '* http://extjs.com/blog/2008/07/01/integrating-google-maps-api-with-extjs/',
            '* author Shea Frederick -- says codes is "Lars - Free as a bird. Use it anywhere or any way you like"',
            '*/',
              '',
            'Ext.namespace("Ext.ux");',
            '/**',
            '*',
            '* @class GMaPanel',
            '* @extends Ext.Panel',
            '*/',
            'Ext.ux.GMapPanel = Ext.extend(Ext.Panel, {',
            'initComponent : function(){',
            'var defConfig = {',
            'plain: true,',
            'zoomLevel: 3,',
            'yaw: 180,',
            'pitch: 0,',
            'zoom: 0,',
            'gmapType: "map",',
            'border: false',
            '}',
            'Ext.applyIf(this,defConfig);',
                                        'Ext.ux.GMapPanel.superclass.initComponent.call(this);        ',
            '},',
            'afterRender : function(){',
            'var wh = this.ownerCt.getSize();',
            'Ext.applyIf(this, wh);',
            'Ext.ux.GMapPanel.superclass.afterRender.call(this);	',
            'if (this.gmapType === "map"){',
            'this.gmap = new GMap2(this.body.dom);',
            '}',
            'if (this.gmapType === "panorama"){',
            'this.gmap = new GStreetviewPanorama(this.body.dom);',
            '}',
            'if (typeof this.addControl === "object" && this.gmapType === "map") {',
            'this.gmap.addControl(this.addControl);',
            '}',
            'if (typeof this.setCenter === "object") {',
            'if (typeof this.setCenter.geoCodeAddr === "string"){',
            'this.geoCodeLookup(this.setCenter.geoCodeAddr);',
            '}else{',
            'if (this.gmapType === "map"){',
            'var point = new GLatLng(this.setCenter.lat,this.setCenter["long"]);',
            'this.gmap.setCenter(point, this.zoomLevel);	',
            '}',
            'if (typeof this.setCenter.marker === "object" && typeof point === "object"){',
            'this.addMarker(point,this.setCenter.marker,this.setCenter.marker.clear);',
            '}',
            '}',
            'if (this.gmapType === "panorama"){',
            'this.gmap.setLocationAndPOV(new GLatLng(this.setCenter.lat,this.setCenter["long"]), {yaw: this.yaw, pitch: this.pitch, zoom: this.zoom});',
            '}',
            '}',
            'var dt = new Ext.util.DelayedTask();',
            'dt.delay(300, function(){',
            'this.addMarkers(this.markers);',
            '}, this);',
            '},',
            'onResize : function(w, h){',
            'if (typeof this.gmap == "object") {',
            'this.gmap.checkResize();',
            '}',
            'Ext.ux.GMapPanel.superclass.onResize.call(this, w, h);',
            '},',
            'setSize : function(width, height, animate){',
            'if (typeof this.gmap == "object") {',
            'this.gmap.checkResize();',
            '}',
            'Ext.ux.GMapPanel.superclass.setSize.call(this, width, height, animate);',
            '},',
            'getMap: function(){',
            'return this.gmap;',
            '},',
            'addMarkers: function(markers) {',
            'if (Ext.isArray(markers)){',
            'for (var i = 0; i < markers.length; i++) {',
            'var mkr_point = new GLatLng(markers[i].lat,markers[i]["long"]);',
            'this.addMarker(mkr_point,markers[i].marker,false,markers[i].setCenter);',
            '}',
            '}',
            '',
            '},',
            'addMarker: function(point, marker, clear, center){',
            'Ext.applyIf(marker,G_DEFAULT_ICON);',
            'if (clear === true){',
            'this.gmap.clearOverlays();',
            '}',
            'if (center === true) {',
            'this.gmap.setCenter(point, this.zoomLevel);',
            '}',
            'var mark = new GMarker(point,marker);',
            'this.gmap.addOverlay(mark);',
            '},',
            'geoCodeLookup : function(addr) {',
            'this.geocoder = new GClientGeocoder();',
            'this.geocoder.getLocations(addr, this.addAddressToMap.createDelegate(this));',
            '},',
            'addAddressToMap : function(response) {',
            'if (!response || response.Status.code != 200) {',
            'Ext.MessageBox.alert("Error", "Code "+response.Status.code+" Error Returned");',
            '} else {',
            'place = response.Placemark[0];',
            'addressinfo = place.AddressDetails;',
            'accuracy = addressinfo.Accuracy;',
            'if (accuracy === 0) {',
            'Ext.MessageBox.alert("Unable to Locate Address", "Unable to Locate the Address you provided");',
            '}else{',
            'if (accuracy < 7) {',
            'Ext.MessageBox.alert("Address Accuracy", "The address provided has a low accuracy.<br><br>Level "+accuracy+" Accuracy (8 = Exact Match, 1 = Vague Match)");',
            '}else{',
            'point = new GLatLng(place.Point.coordinates[1], place.Point.coordinates[0]);',
            'if (typeof this.setCenter.marker === "object" && typeof point === "object"){',
            'this.addMarker(point,this.setCenter.marker,this.setCenter.marker.clear,true);',
            '}',
            '}',
            '}',
            '}',
            '',
            '}',
            '',
            '});',
            '',
            'Ext.reg("gmappanel",Ext.ux.GMapPanel);' ,
            sep = "\n")
            
    return(out)
  }

  ## svalue -- location of map
  ## [] -- extra markers specified by lat and long vector. Names of vector gives
  ##       title attribute


  ## markers are set with a data frame.
  ## [geoCodeAddr]
  ## [geoCodeAddr, title] ## first col is non-numeric
  ## [lat, long]
  ## [ lat, long, title]
 
widget$makeMarkers <- function(.) {
    values <- .$getValues()
    out <- String("")
    if(nrow(values) > 0) {
      if(ncol(values) == 1) {
        ## geoCodeAddr
        out <- paste("{ geoCodeAddr:",shQuoteEsc(values[,1]),
                     "}",
                     collapse=",")
      } else if(ncol(values) == 2) {
        if(!is.numeric(values[,1])) {
          ## geoCodeAddr, title
          out <- paste("{ geoCodeAddr:",shQuoteEsc(values[,1]),",",
                       "marker:{title:", shQuoteEsc(values[,2]),"}",
                       "}",
                       collapse=",")
        } else {
          ## no title, lat and long
          out <- paste("{ lat:",values[,1],",'long':",values[,2],
                       '}',
                       collapse=",")
        }
      } else {
        out <- paste("{ lat:",values[,1],",'long':",values[,2],",",
                     "marker:{title:", shQuoteEsc(values[,3]),"}",
                     "}",
                     collapse=",")
      }
      
    }
    return(String("[") + out + "]")
  }

  ## XXX set defaults for width and height -- doesn't like auto
  widget$..width <- 600
  widget$..height <- 400
  ## can override
  widget$..zoomLevel <- 14

  
  widget$makeMapCommands <- function(.) {
    lst <- list(xtype = "gmappanel",
                region = "center",
                zoomLevel = .$..zoomLevel,
                gmapType = .$..gmapType,
                width = .$..width,
                height = .$..height,
                addControl = String("new GSmallMapControl()")
                )
    val <- svalue(.)
    if(length(val) == 1) {
      lst[["setCenter"]] = list(
           geoCodeAddr = val,
           marker = list(title = .$..title)
           )
    } else {
      lst[["setCenter"]] = list(
           lat = val[1],
           long = val[2],
           marker = list(title = .$..title)
           )
    }

    if(length(.$getValues()) > 0)
      lst[["markers"]] <- .$makeMarkers()

    return(.$mapRtoObjectLiteral(lst))

  }
  widget$ExtConstructor <- "Ext.Panel"
  widget$ExtCfgOptions <- function(.) {
    out <- list(items = .$makeMapCommands())
                  
    return(out)
  }
  ## this is an exampe of a footer
  ## more API at http://code.google.com/apis/maps/documentation/reference.html#GMap2
  widget$footer <- function(.) {
    out <- String() +
      ## how to get the map
      .$setgmapID() +
        .$gmapID() + '.enableGoogleBar();' +
          .$gmapID() + '.enableScrollWheelZoom();'

    if(length(.$..runCmds)) {
      for(i in .$..runCmds) out <- out + i
    }

    return(out)
  }

  widget$setValueJS <- function(.) {
    value <- svalue(.)
    if(length(value) == 2)
      .$panTo(value)
  }
  widget$setValuesJS <- function(.,...) {
    values <- .$getValues()
    for(i in 1:nrow(values))
      widget$addMarker(values[i,1:2])

  }

  
  ## some non-gWidgets methods to access *some* of the google maps API

  ## return bounds of map
  ## write bounds in a transport function
  widget$setgmapID <- function(.) {
    out <- String() +
      'gmap' + .$ID +' = ' + 'o' + .$ID + '.getComponent(0).gmap;'
    return(out)
  }
  widget$gmapID <- function(.) {
    out <- String() +
      'gmap' + .$ID
    return(out)
  }

  ## bounds
  ## javascript transport to write bounds
  widget$transportBounds <- function(.) {
    out <- String() +
      .$setgmapID() +
        'var bounds = ' + .$gmapID() + '.getBounds();' +
          '_transportToR("' + .$ID + '.SouthWest",' +
            'bounds.getSouthWest().toString());' +
              '_transportToR("' + .$ID + '.NorthEast",' +
                'bounds.getNorthEast().toString());'
    return(out)
  }

  widget$getBounds <- function(.) {
    pat.sw = String(.$ID) + '.SouthWest'
    pat.ne = String(.$ID) + '.NorthEast'

    sw <- get(pat.sw,envir=.GlobalEnv)
    sw <- eval(parse(text = String("c") + sw), envir=.GlobalEnv)

    ne <- get(pat.ne,envir=.GlobalEnv)
    ne <- eval(parse(text = String("c") + ne), envir=.GlobalEnv)
    ## coerce

    ## return
    list(southwest = sw, northeast = ne)
  }

  ## set center
  ## latlng <- c(lat=xxx, lng = yyy)
  widget$panTo <- function(., latlng) {
    out <- String() +
      .$setgmapID() +
        .$gmapID() + '.panTo(' +
          'new GLatLng(' + latlng[1] + ',' + latlng[2] + '));'

    if(exists("..shown", envir=., inherits = FALSE))
      cat(out)
    else
      .$setValue(value = latlng)
  }
  ## zoom in or out
  widget$setZoom <- function(., zoom=14) {
    out <- String() +
      .$setgmapID() +
        .$gmapID() + '.setZoom(' + zoom + ');'

    if(exists("..shown", envir=., inherits = FALSE))
      cat(out)
    else
      .$..zoomLevel <- zoom

  }

  ## popup a message at a point
  widget$openInfoWindow <- function(., latlng, myHTML) {
    out <- String() +
      .$setgmapID() +
        'var point = new GLatLng(' + latlng[1] + ',' + latlng[2] + ');' +
          .$gmapID() + '.openInfoWindow(point,' +
            shQuoteEsc(myHTML) + ');'

    if(exists("..shown", envir=., inherits = FALSE))
      cat(out)
    else
      .$..runCmds <- c(.$..runCmds, out)
  }

  ## methods to add to map: marker, Polyline, Polygon
  
  ## addMarker
  widget$addMarker <- function(., latlng, title="", draggable = FALSE) {

    ## append to markers
    marks <- .$getValues()
    if(nrow(marks) == 0) {
      marks <- data.frame(latlng[1], latlng[2], title)
    } else {
      n <- nrow(marks)
      marks[n+1, 1:2] <- latlng
      if(ncol(marks) == 3)
        marks[n+1, 3] <- title
    }
    .$..values <- marks                 # bypass setValues, as it would recurse

    ## make JS
    lst <- list(draggable = draggable)
    if(title != "")
      lst[["title"]] <- title
    out <- String() +
      .$setgmapID() +
        'var point = new GLatLng(' + latlng[1] + ',' + latlng[2] + ');' +
          'var marker = new GMarker(point,' +
            .$mapRtoObjectLiteral(lst) +
              ');'
    if(draggable) {
      ##     ## add handlers
      out <- out +
        'GEvent.addListener(marker, "dragstart", function() {' +
          .$gmapID() + '.closeInfoWindow();' +
            '});'
      
      ## XXX dragend should also update marks position
      out <- out +
        'GEvent.addListener(marker, "dragend", function() {' +
          'myHtml = "new latitude and longitude:<br>" + this.getLatLng().toString();' +
            'this.openInfoWindowHtml(myHtml);' +
              '});'
    }

    out <- out +
      .$gmapID() + '.addOverlay(marker);'
    
    if(exists("..shown", envir=., inherits = FALSE))
      cat(out)
    else
      .$..runCmds <- c(.$..runCmds, out)
  }

  ## polyLine
  ## latlng matrix or data frame of lat and lng
  widget$addPolyline <- function(., latlng,
                                 color="#ff0000", pixel.width = 5, opacity=1) {
    if(missing(latlng))
      latlng <- .$getValues()
    if(! (is.matrix(latlng) || is.data.frame(latlng))) return()
    if(nrow(latlng) == 0) return()
    
    out <- String() +
      .$setgmapID() +
        'var polyline = new GPolyline(['
    tmp <- c()
    for(i in 1:nrow(latlng))
      tmp[i] <- String("new GLatLng(") +
        latlng[i,1] + ',' + latlng[i,2] + ')'

    out <- out + paste(tmp, collapse=", ") +
      '], ' + shQuote(color) + ',' + pixel.width + ',' + opacity +
        ', {clickable: true, geodesic: true}' +
        ');'
    ## add a handler to show length
     out <- out +
       'GEvent.addListener(polyline, "click", function(latlng) {' +
         'var dist = (this.getLength()/1000).toFixed(2);' +
           'myHtml = "length (meters):<br>" + dist.toString();' +
             .$gmapID() + '.openInfoWindowHtml(latlng, myHtml);' +
              '});'
    
    out <- out +
      .$gmapID() + '.addOverlay(polyline);'

    if(exists("..shown", envir=., inherits = FALSE))
      cat(out)
    else
      .$..runCmds <- c(.$..runCmds, out)
  }
    
        

  ## drawPolygon
  widget$addPolygon <- function(., latlng,
                                border.color="#ff0000", border.pixel.width = 5,
                                border.opacity = 1,
                                region.color = "#000000", region.opacity = .1
                                ) {

    if(missing(latlng))
      latlng <- .$getValues()
    if(! (is.matrix(latlng) || is.data.frame(latlng))) return()
    if(nrow(latlng) == 0) return()
    

    out <- String() +
      .$setgmapID() +
        'var polygon = new GPolygon(['
    tmp <- c()
    for(i in 1:nrow(latlng))
      tmp[i] <- String("new GLatLng(") +
        latlng[i,1] + ',' + latlng[i,2] + ')'
    ## terminate
    tmp[nrow(latlng) + 1] <- String("new GLatLng(") +
        latlng[1,1] + ',' + latlng[1,2] + ')'

    out <- out + paste(tmp, collapse=", ") +
      '], ' +
        shQuote(border.color) + ',' + border.pixel.width + ',' +
          border.opacity +
            ',' + shQuote(region.color) + ',' + region.opacity +
              ');'

    ## add a handler to show area
     out <- out +
       'GEvent.addListener(polygon, "click", function(latlng) {' +
         'var area = (this.getArea()/(1000*1000)).toFixed(2);' +
           'myHtml = "Area (sq. kms):<br>" + area.toString();' +
             .$gmapID() + '.openInfoWindowHtml(latlng, myHtml);' +
               '});'

    
    out <- out +
      .$gmapID() + '.addOverlay(polygon);'

    if(exists("..shown", envir=., inherits = FALSE))
      cat(out)
    else
      .$..runCmds <- c(.$..runCmds, out)
  }
    

  ## ???

  ## handlers -- these call back into R.
  ## this should work for click and dblclick
  widget$writeHandlerJS <- function(., signal, handler=NULL) {
    out <- String() +
      .$setgmapID() +
        'GEvent.addListener(' + .$gmapID() + ',' + shQuote(signal) +
          ',function(overlay, point) {' +
            ## transport bounds
            'var bounds = ' + .$gmapID() + '.getBounds();' +
              '_transportToR("' + .$ID + '.SouthWest",' +
                'bounds.getSouthWest().toString());' +
                  '_transportToR("' + .$ID + '.NorthEast",' +
                    'bounds.getNorthEast().toString());' +
                      'runHandlerJS(' + handler$handlerID +
                        ',"latlng","evalme c" + point.toString());' +
                          '});'
    return(out)
  }

  
  container$add(widget, ...)
  invisible(widget)

}
