### Not working!!!

gfile <- function(text="Choose a file",
                  type = c("open"),
                  filter = NULL, 
                  handler = NULL, action = NULL, container = NULL, ...) {

  widget <- EXTComponent$new(toplevel=container$toplevel,
                             ..text = text, ..type=type, ..filter=filter
                             )
  class(widget) <- c("gFile",class(widget))
  widget$setValue(value="")             # empty, set on fileselected
  widget$..width <- 400

  ## CSS
  
  widget$scripts <- function(.) {
    out <- String()

    f <- system.file("javascript","FileUploadField.js", package="gWidgetsWWW")
    out <- out + paste(readLines(f), collapse="\n")
    return(out)
  }
  
  ## methods
#  widget$getValueJSMethod <- "getValue"

#  widget$setValueJSMethod <- "setValue"
  widget$transportSignal <- c("fileselected")
  widget$transportValue <- function(.,...) {
    out <- String() +
      'var value = s;'
    return(out)
  }
  
  widget$ExtConstructor <- "Ext.FormPanel"
  widget$ExtCfgOptions <- function(.) {
    out <- list("fileupload"=TRUE,
                width=.$..width,
                defaults=list(
                  anchor="100%"
                  ),
                items=list(
                  xtype="fileuploadfield",
                  emptytext=.$..text),
                buttons = list(
                  list(
                       text="Save"
                       ),
                  list(
                       text="reset",
                       handler=String() + "function() {" + .$ID + ".getForm().reset();}"
                       )
                  )
                )
    
    return(out)
  }

  if(!is.null(handler))
    widget$addHandlerClicked(handler=handler,action=action)
  

  

  ## add after CSS, scripts defined
  container$add(widget,...)


  if(!is.null(handler))
    widget$addHandler("fileselected",handler=handler,action=action)
  
  invisible(widget)
}

