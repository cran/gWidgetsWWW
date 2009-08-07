## Works as of Ext-3.0
## why are there no arrows???

## others
gspinbutton <- function(from = 0, to = 100, by = 1, value = from,
                    handler = NULL, action = NULL, container = NULL, ...) {
  widget <- EXTComponent$new(toplevel=container$toplevel,
                             ..from = from, ..to = to, ..by=by
                             )
  class(widget) <- c("gSpinbutton",class(widget))
  widget$setValue(value=value)
  widget$..coerce.with="as.numeric"
  ## CSS
  ## XXX Get this css to work to get trigger icon for spin
  ##   widget$css <- function(.) {
##     out <- String()
##     f <- system.file("css","ext.ux.spinner.css", package="gWidgetsWWW")
##     out <- out + "\n" + paste(readLines(f), collapse="\n")
##     out
##   }
  widget$scripts <- function(.) {
    out <- String()

    ## These should be in a javascript directory on the web server,
    ## but this is easier (slower too as it doesn't cache)
    f <- system.file("javascript","ext.ux.spinner.js", package="gWidgetsWWW")
    out <- out + "\n" + paste(readLines(f), collapse="\n")
    
    f <- system.file("javascript","ext.ux.spinnerformfield.js", package="gWidgetsWWW")
    out <- out + "\n" + paste(readLines(f), collapse="\n")
    
    return(out)
  }
  
  ## methods
  widget$getValueJSMethod <- "getValue"

  widget$setValueJSMethod <- "setValue"
  widget$transportSignal <- c("spin")
  widget$ExtConstructor <- "Ext.ux.form.SpinnerField"
  widget$ExtCfgOptions <- function(.) {
    out <- list("value"= svalue(.),
                "minValue" =  .$..from,
                "maxValue" = .$..to,
                "allowDecimals"=TRUE,
                "decimalPrecision"=1,
                "accelerate"=TRUE,
                "incrementValue" = .$..by,
                "enableKeyEvents"=TRUE
                )

    return(out)
  }


  ## add after CSS, scripts defined
  container$add(widget,...)


  if(!is.null(handler))
    widget$addHandler("change",handler=handler,action=action)
  
  invisible(widget)
}

