
## no wrap argument in Ext
## font.attr not implemented -- use markup
## svalue
## svalue<-
## add (one line at end only)
## handlers?
gtext <- function(text = NULL, width = NULL, height = 300,
                  font.attr = NULL, wrap = TRUE,
                  handler = NULL, action = NULL, container = NULL,...,
                  resizable = FALSE     # gWidgetsWWW arg. Keep?
                  ) {

  if(!resizable) {
    widget <- EXTComponent$new(toplevel=container$toplevel,
                               ..width = width,..height=height,
                               ..wrap = wrap)
    class(widget) <- c("gText", class(widget))
  } else {
    widget <- EXTComponentResizable$new(toplevel=container$toplevel,
                                        ..width = width,..height=height,
                                        ..wrap = wrap)
    class(widget) <- c("gText","gWidgetResizable", class(widget))
    
  }
  widget$setValue(value=text)
  

  ## CSS

  ## Scripts

  ## methods
  widget$add <- function(.,child, ...) {
    ## warp around svalue<-
    svalue(.) <- c(svalue(.), child)
##     curValue <- String(svalue(.))
##     for(i in child) {
##       curValue <- curValue + i
##     }
##     svalue(.) <- curValue
  }

  
  ## methods
  ## getValue must escape the strings -- they are URL encoded by escape()
  widget$coerce.with <- function(., val) unescapeURL(val)
  
  
  widget$getValueJSMethod <- "getValue"
  widget$setValueJSMethod <- "setValue"

  ## lots of escapes for multiline stuff
  widget$setValueJS <- function(.,...) {
    if(exists("..setValueJS", envir=., inherits=FALSE)) .$..setValueJS(...)
    
    out <- String() +
      .$asCharacter() + '.setValue(' +
        shQuote(paste(.$..data,collapse="\\\\n")) + ');' + '\n'

    return(out)

  }

  widget$transportSignal <- "change"
  widget$transportValue <- function(.,...) {
    out <- String() +
      'var value = escape(' + .$asCharacter() + '.' +
        .$getValueJSMethod + '());' + '\n'
    return(out)
  }
  widget$ExtConstructor <- "Ext.form.TextArea"
  widget$ExtCfgOptions <- function(.) {
    out <- list("value"= svalue(.))
    if(!is.null(.$..width)) {
      out[["width"]] <- .$..width
    } else {
      out[["width"]] <-  "auto"
    }
    if(!is.null(.$..width)) {
       out[["height"]] <- .$..height
     } else {
       out[["height"]] <-  "auto"
     }
    out[["selectOnFocus"]] <- TRUE

    return(out)
  }

  ## add after CSS, scripts defined
  container$add(widget,...)


  if(!is.null(handler))
    widget$addHandler("change",handler=handler,action=action)
  
  invisible(widget)
}

