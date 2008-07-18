## gedit
## svalue works
## svalue<- works
## autocomplete code not in Ext???
## addHandlerKeypress should work -- but no key passed in XXX
## change handler called after change and losing focus.
gedit <- function (text = "", width = 25, coerce.with = NULL,
                   handler = NULL,  action = NULL, container = NULL, ...) {
  
  widget <- EXTComponent$new(toplevel=container$toplevel,
                             ..width = width,
                           ..coerce.with=coerce.with)
  class(widget) <- c("gEdit",class(widget))
  widget$setValue(value=text)
  

  ## CSS

  ## Scripts

  ## methods
  widget$getValueJSMethod = "getValue"
  widget$setValueJSMethod = "setValue"
  widget$transportSignal <- "change" 
  widget$ExtConstructor <- "Ext.form.TextField"
  widget$ExtCfgOptions <- function(.) {
    list("value"= svalue(.),
         "enableKeyEvents"= TRUE
         )
  }



##   widget$show <- function(.) {
##     ## html
##     out <- String() +
##       '<span id=' + shQuote(.$ID) + '></span>\n'

##     ## javascript object
##     out <- out + '<script>' +
##       'o' + .$ID + '= new Ext.form.TextField({' +
##         'renderTo: ' + shQuote(.$ID) + ',' +
##           'value:' + shQuote(svalue(.)) + ',' +
##             'enableKeyEvents: true,' +
##               '});'

##     ## transport
##     out <- out +
##       'o' + .$ID + '.on("change", function() {' +
##         'var value = o' + .$ID + '.getValue();' +
##           '_transportToR(' + shQuote(.$ID) + ',value);' +
##             '});'

##     ## finish
##     out <- out + '</script>'
    
##     .$Cat(out)
##   }


  ## add after CSS, scripts defined
  container$add(widget,...)


  if(!is.null(handler))
    widget$addHandler("change",handler=handler,action=action)
  
  invisible(widget)
}

