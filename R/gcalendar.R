gcalendar <- function(text = "", format = "%Y-%m-%d",
                      handler=NULL, action=NULL, container = NULL, ... ) {


    widget <- EXTComponent$new(toplevel=container$toplevel,
                               ..text = text,
                               ..format = format)
    class(widget) <- c("gCalendar",class(widget))

    ## XXX put in today's date?
    widget$setValue(value="")           # no day
    widget$getValueJSMethod <- "getValue"
    widget$transportSignal <- c("change")
    ## coerceValues calls ..format
    widget$coerceValues <- function(., value) {
      ## Wed Jun 11 2008 00:00:00 GMT-0400 (EDT) -- ext format
      theDate = as.Date(value,"%a %b %d %Y %H:%M:%S")
      return(format(theDate,.$..format))
    }
      
      
    
    ## override writeConstructor of show method
    widget$writeConstructor <- function(.) {
      lst <- list(xtype = "datefield",
                  emptyText = .$..text,
                  id =  as.character(String(.$ID) + "date"))

      ## size doesn't work here, as we the style thing isn't
      ## applied to 
      if(exists("..width",envir = .,inherits=FALSE))
        lst[["width"]] <- .$..width
      else
        lst[["width"]] <- "auto"

      if(exists("..height",envir = .,inherits=FALSE))
        lst[["height"]] <- .$..height
      else
        lst[["height"]] <- "auto"
      
      out <- String() +
        'var o' + .$ID + 'date = new Ext.Panel({' +
          'id:' + shQuote(.$ID) + ',' +
            'renderTo:document.body,' +
              'items: [' +
                .$mapRtoObjectLiteral(lst) +
                  ']\n' +
                    '});'

      out <- out +
        'var o' + .$ID + ' = o' + .$ID + 'date' +
          '.getComponent(0);' + '\n'

      return(out)
    }
        

    container$add(widget,...)

      
    if(!is.null(handler))
      widget$addHandlerChanged(handler, action=action)
    
    
    invisible(widget)
  }

