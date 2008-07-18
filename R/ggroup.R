### The container code in Ext is very similar.
## All use Ext.Panel with some different configuration options
## this code tries to take advantage of that  by introducing some sub"traits".
## the tricky thing is the call up to ExtCfgOptions from the inherited trait.
## just calling with .super didn't work

EXTPanel <- EXTContainer$new()
EXTPanel$ExtConstructor <- "Ext.Panel"



EXTGroup <- EXTPanel$new(children=list())
EXTGroup$ExtCfgOptions <-  function(.) {
  out <- list(
              border = FALSE,
              hideBorders=TRUE          # key to getting rid of blud
              )
  if(exists("..horizontal",envir=., inherits=FALSE)) {
    if(.$..horizontal) {
      orientation <- "column"
      out[['layout']] <- orientation
      if(exists("..use.scrollwindow", envir=., inherits=FALSE))
        out[['autoScroll']] <- .$..use.scrollwindow
    }
    ## no "row" layout, so we use the default.
  }

  spacing <- 10
  if(exists("..spacing", envir=., inherits=FALSE)) spacing <- .$..spacing
##  out[['spacing']] <- spacing
#  out[["bodyStyle"]] = String('{') + 'padding:"' + spacing + 'px"}'
    out[["bodyStyle"]] = list('padding' = String('"') + spacing + 'px"'  )
  return(out)
}
## these are not defined
EXTGroup$addSpring <- function(.) {}
EXTGroup$addSpace <- function(., value, horizontal=TRUE, ...) {}

ggroup <- function(horizontal=TRUE, spacing=5, use.scrollwindow = FALSE,
                    container,...) {
   ## a group
  cont <- EXTGroup$new(toplevel = container$toplevel,
                     ..horizontal = horizontal,
                     ..spacing = spacing,
                     ..use.scrollwindow = use.scrollwindow
                     )
   class(cont) <- c("gGroup",class(cont))
   container$add(cont,...)
   invisible(cont)
 }

EXTFrame <- EXTGroup$new(children=list())
EXTFrame$ExtCfgOptions <- function(.) {
  out <- EXTGroup[['ExtCfgOptions']](.)
  out[['title']] <- escapeHTML(svalue(.))
  return(out)
}
EXTFrame$setValueJSMethod = "setTitle"
     
gframe <- function(text = "", pos = 0, horizontal=TRUE, container=NULL,...) {

  cont <- EXTFrame$new(toplevel = container$toplevel,
                    ..horizontal = horizontal)
                    
  
  class(cont) <- c("gFrame",class(cont))
  cont$setValue(value=text)
  cont$..pos <- pos

  cont$..ExtCfgOptions <- function(.)
    list(border=TRUE)
  
  container$add(cont,...)  
  invisible(cont)
}


EXTExpandGroup <- EXTFrame$new(children=list())
EXTExpandGroup$ExtCfgOptions <- function(.) {
  out <- .super$ExtCfgOptions(.)
  out[['collapsible']] <- TRUE
  out[['titleCollapse']] <- TRUE
  return(out)
}
EXTExpandGroup$setVisibleJS <- function(.) {
  .$callExtMethod("expand","true")
}

gexpandgroup <- function(text = "", horizontal = TRUE,
                         handler = NULL, action=NULL,
                         container=NULL, ...) {

  cont <- EXTExpandGroup$new(toplevel=container$toplevel,
                            ..horizontal=horizontal)
                    

  class(cont) <- c("gExpandgroup",class(cont))
  cont$setValue(value=text)

  cont$..ExtCfgOptions <- function(.)
    list(border=TRUE)

  if(!is.null(handler))
    addHandlerClicked(cont, handler = handler, action=action)

  container$add(cont,...)    
  invisible(cont)
}
  
