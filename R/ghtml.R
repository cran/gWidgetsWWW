## Show marked up text -- or show url
## svalue<- only works for urls, not for text
## pass object of S3 class URL if want url and not absolute  (eg. http:///)


ghtml <- function(x, container = NULL, ...) {
  ## x is a url or a character vector to show
  ## components

  widget <- EXTComponent$new(toplevel=container$toplevel)
  class(widget) <- c("gHtml",class(widget))
  widget$setValue(value=x)

  widget$setValueJS <- function(.,...) {
    val <- .$..data
    out <- String() + 'o' + .$ID
    if(isURL(val)) {
      out <- out +
        '.load(' + shQuote(val) + ');'
    } else {
      out <- out +
        '.body=' + shQuoteEsc(val) + ';'
    }
    return(out)
  }

  
  widget$ExtConstructor <- "Ext.Panel"
  widget$ExtCfgOptions <-  function(.) {
    out <- list()
    out[['border']] <- FALSE
    
    if(isURL(svalue(.)))
      out[['autoLoad']] <- svalue(.)
    else
      out[['html']] <- svalue(.)
    
    return(out)
  }
  
  
  
  ## add after CSS, scripts defined
  container$add(widget,...)
  invisible(widget)
}
