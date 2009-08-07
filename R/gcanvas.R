gcanvas <- function(f, width=480, height=400,
##                    handler = NULL, action = NULL,
                    container = NULL,...) {

  widget <- EXTComponent$new(toplevel=container$toplevel,
                             ..width=as.numeric(width),
                             ..height=as.numeric(height))
  
  class(widget) <- c("gCanvas",class(widget))
  if(!missing(f))
    widget$setValue(value=f)

  widget$ExtConstructor <- "Ext.Panel"
  widget$ExtCfgOptions <-  function(.) {
    out <- list()
    out[['border']] <- FALSE
    
    out[['html']] <- String() +
      '\'<canvas id="gWidgetsCanvas" width=' + .$..width + ' height=' + .$..height +
        '></canvas>\''
    
    return(out)
  }

  
  widget$footer <- function(.) {
    out <- String(sep="\n") +
      'var ctx = document.getElementById("gWidgetsCanvas").getContext("2d");' +
        'if(!ctx.fillText) {ctx.fillText =function() {};};' + '\n' +
          .$setValueJS() 
    return(out)
  }

  widget$setValueJS <- function(.,...) {
    if(exists("..data", envir=., inherits=FALSE)) {
      value <- .$..data
      out <- String()
      if(!is.null(value)) {
        ## clear out
        out <- out +
          paste(readLines(value)[-1], collapse="\n") +
            '\n'
      }
      return(out)
    } else {
      return("")
    }
  }

  
  ## add after CSS, scripts defined
  container$add(widget,...)
  invisible(widget)
  
}


## ggraphics is a pass through for gcanvas
ggraphics <- function(width = 480, height=400, container=NULL, ...) {
  gcanvas(width=width, height=height, container=container, ...)
}
