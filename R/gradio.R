## gradio
## XXX no [<- method!!!
## size?,
## XXX transport is off the ranch -- is this necessary?


gradio <- function(items, selected = 1, horizontal=FALSE,
       handler = NULL, action = NULL, container = NULL, ...) {

  ## use a checkbox if only one item
  if(length(items) == 1) {
    out <- gcheckbox(items, checked = selected == 1, handler=handler, action=action, container = container, ...)
    return(out)
  }

  widget <- EXTComponentWithItems$new(toplevel=container$toplevel,
                                      ..selected = selected,
                                      ..horizontal = horizontal,
                                      ..handler = handler,
                                      ..action = action
                                      )
  class(widget) <- c("gRadio",class(widget))
  
  widget$setValue(value = selected) ## store the index
  widget$setValues(value = items)

  ## define methods
  ## we store values by index
  widget$getValue <- function(.,index=NULL ,drop=NULL,...) {
    ## we need to revers logic from AWidgtet$getValue
    out <- .$..data
    if(exists("..shown",envir=.,inherits=FALSE)) {
      ## get from widget ID
      out <- try(get(.$ID,envir=.GlobalEnv),silent=TRUE) ## XXX work in index here?
      if(!inherits(out,"try-error")) {
        out <- as.numeric(out)          # is character
      } else {
        out <- .$..data
      }
    }
    ## no index -- return values
    if(is.null(index)) index <- FALSE
    if(index)
      return(as.numeric(out))
    else
      return(.$..values[as.numeric(out)])
  }
  
  ## override setValue
  widget$setValue <- function(., index=NULL,..., value) {
    ## values can be set by index or character
    if(is.null(index) || !index) {
      ind <- which(value %in% .$getValues())
      if(length(ind) == 0) return()
      ind <- ind[1]
    } else {
      ind <- value
    }
     
    ## we store the index
    .$..data <- ind

    if(exists("..shown",envir=., inherits=FALSE))
      cat(.$setValueJS(index=ind))
  }

  widget$setValueJS <- function(.,..., index) {
    if(exists("..setValueJS", envir=., inherits=FALSE)) .$..setValueJS(...)
    
    out <- String() +
      .$asCharacter() + '.getComponent(' + as.character((index-1)) +
        ')' + '.setValue(true);'
    return(out)
  }
  
  ## to set values we a) remove old values b) add new ones c) handlers?
  ## XXX doesn't work!!!
##   widget$setValuesJS <- function(.) {
##     out <- String()

##     ## JS to remove values
##     out <- out +
##       'var n = ' + 'o' + .$ID + '.items.getCount();' +
##         'var i = 0;' +
##           'while(i < n) {' +
##             'var rb = ' + 'o' + .$ID + '.getComponent(n - i - 1);' +
##               'o' + .$ID + '.remove(rb);' +
##                 'i = i + 1;' +
##                   ' };' + '\n'

##     ## JS to add new ones
##     out <- out + .$makeRadioButtons()
##     out <- out + .$addRadioButtons()
##     out <- out + .$addRadioButtonHandlers()
##     return(out)
##   }

  widget$xtype <- "radio"
  widget$transportSignal <- "check"
  widget$checked <- function(.,i) (i == .$..selected)
  widget$ExtCfgOptions <- function(.) {
    out <- list(border = FALSE,
                hideBorders = TRUE,
                shim = FALSE,
                bodyStyle = list(padding = "5px"),
                items = .$makeItems()
                )
    if(.$..horizontal)
      out[['layout']] <- "column"
    return(out)
  }

  ## transport
  widget$transportValue <- function(.,...,i) {
    out <- String() +
      'if(checked === true) {' +
        '_transportToR(' + shQuote(.$ID) +
          ',' + i + ');}' + '\n'         # i passed into transportValue()!

    return(out)
  }

  widget$transportFUN <- function(.) return(String(""))

##   widget$makeRadioItems <- function(.) {
##     out <- String()

##     values <- .$getValues()
##     if((n <- length(values)) < 2)  return(out)

##     tmp <- list()                          # store items as String
##     for(i in 1:n) {
##       lst <- list(xtype = "radio",
##                   name = as.character(String() + .$ID + "radiogroup"),
##                   boxLabel = as.character(values[i]),
##                   checked = (i == .$..selected)
##                   )
##       tmp[[i]] <- .$mapRtoObjectLiteral(lst)
##     }

##     out <- out +
##       '[' + paste(tmp,collapse=",") + ']'
    
##     return(out)
##   }

##   widget$makeRadioButtons <- function(.) {
##     ## get ccmponents
##     values <- .$getValues()
##     n <- length(values)
##     out <- String()
##     for(i in 1:n) {
##       out <- out +
##         'var ' + .$ID + "radiobutton" + i +
##           ' = o' + .$ID + '.getComponent(' + as.character(i-1) + ');' + '\n'
##     }
##     return(out)
##   }

##   ## panel has regular id
##   widget$makeRadioPanel <- function(.) {
##     lst <- list(id = as.character(.$ID),
##                 renderTo =  String("document.body"),
##                 border = FALSE,
##                 bodyStyle = as.character('{padding: "5px"}')
##                 )
##     if(.$..horizontal)
##       lst[["layout"]] <- "column"

##     lst[["items"]] <-  .$makeRadioItems()


    
##     out <- String() +
##       'o' + .$ID + ' = new Ext.Panel(' + '\n' +
##         .$mapRtoObjectLiteral(lst) +
##           ');' + '\n'
##     return(out)
##   }

  ## do the transport function with handler 
  ## add handler (also does transport)

  ## override show method
##   widget$show <- function(.) {
##     out <- String() +
##         .$makeRadioPanel() +
##           .$makeRadioButtons() +
##             .$addRadioButtonHandlers() ## also does transport

##     .$Cat(out)
##   }


  ## add after CSS, scripts defined
  container$add(widget,...)


  ## add Handler
##   widget$addRadioButtonHandlers <- function(.) {
##     out <- String()
##     values <- .$getValues(); n <- length(values)
##     for(i in 1:n) {
##       out <- out +
##         .$ID + 'radiobutton' + i +
##           '.on("check",function(e,check) {' +
##             'if(check === true) {' +
##               ## do transport
##               '_transportToR(' + shQuote(.$ID) +
##                 ',' + i + ');' + '\n'
##       if(!is.null(.$..handler)) {
##         out <- out +
##           'runHandlerJS(' + .$..handlerID + ',\'""\', \'""\', true,this,{delay:100,buffer:100, single:false});'
##       }
##       out <- out +'}});' + '\n'
##     }
##     ## we need to add this handler for *each* radio button
##     ## we add transport to R and handler if present.
##     return(out)
##   }

##   widget$addHandler <- function(.,signal, handler, action=NULL,...) {
##     id <- get("addHandler",envir=EXTWidget, inherits=FALSE)(.,
##                                               signal=NULL, handler,action,...)
##     .$..handlerID <- id
##     invisible(id)
##   }
  

  ## we add handler regardless, as this introduces transport function
  id <- widget$addHandler(signal=NULL, handler, action)
  invisible(widget)

}
