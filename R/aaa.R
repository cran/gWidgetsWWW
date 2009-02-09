library(proto)
library(Rpad)

## Three main classes
## EXTWidget and its subclasses EXTComponent, EXTContainer


## The double dot is meant to indicate an instance variable/method as
## opposed to a "class" variable/method. It seems that proto does not
## propogate the double dot -- that is

## > a = proto(..test = TRUE, test = TRUE, new = function(.) .$proto())
## > b = a$new()
## > b$..test
## [1] TRUE
## > b$..test
## Error in get(x, env = this, inherits = inh) : 
##   variable "..test" was not found
##
## so we check for ..test using exists and inherits = FALSE

EXTWidget <-
  proto(new = function(.,...) {
    obj <- .$proto(...)
    class(obj) <- c("gWidget",class(obj))
    return(obj)
  },
        ## properties
        toplevel = NULL,                # stores top gwindow instance
        parent = NULL,                  # stores parent (sub)window
        ID = "",                        # what am i, IDXXX
        ..data = NULL,                  # for svalue
        ..values = NULL,                # for [
        ..enabled = TRUE,               # is enabled (or grayed out)
        ..visible = TRUE,               # logical, false if don't show
        ..shown = FALSE,                # is this object shown (rendered)?
        css = NULL,
        scripts = NULL,                 # javascript stuff per widget
        style = c(),                    # style options
        file = "",                      # for "cat"; in parent of widget
        ExtConstructor = NULL,          # eg. Ext.Button
        ExtCfgOptions = NULL,           # config options -- fn returns a list
        ..ExtCfgOptions = NULL,         # additional options per instance
        getValueJSMethod = NULL,        # name of method, eg. "getValue"
        setValueJSMethod = NULL,        # name of method, eg. "setValue"
        coerce.with = NULL,             # coerce FUN or string
        transportSignal = NULL          # possibly a vector
        )



## Some configuration

### methods

## used to display to STDOUT or a file for debugging
## XXX Should we use .$file <- stdout()???
## this isn't working for dialogs
EXTWidget$Cat <- function(.,...) cat(...,"\n", file=.$file, append=TRUE)
## Cat either a string or function
EXTWidget$showPart <- function(.,part) {
  ## part may be property of function. This checks
  if(!is.null(part))
    if(is.function(part))
      .$Cat(part())
    else
      .$Cat(part)
}

## instead of pasting "o" in front of ID
## we have Ext Object "o" + ID and DOM object "ID" to consider
## XXX This is misnamed (toString?)
EXTWidget$asCharacter <- function(.) {String('o') +  .$ID}

## simple function to call an Ext method on the corresponding object
EXTWidget$callExtMethod <- function(., methodname, args) {
  if(missing(args))
    args <- '();'
  else
    args <- String('(') + args + ');'
  out <- String() +
    .$asCharacter() +  methodname + args
  return(out)
}




## We have both S3 methods and their proto counterparts

## Here we have getValue, setValue for svalue, svalue<-
## svalue, svalue<-
EXTWidget$getValue <- function(., index=NULL,drop=NULL, ...) {
   if(exists("..shown",envir=.,inherits=FALSE)) {
     ## get from widget ID
     out <- try(get(.$ID,envir=.GlobalEnv),silent=TRUE) ## XXX work in index here?
     if(inherits(out,"try-error")) {
       out <- .$..data
     } else {
       .$..data <- out                  # update data
     }
   } else {
     if(is.null(index) || !index)
       out <- .$..data
     else
       out <- which(.$..data %in% .$..values)
    }

   out <- .$coerceValues(out)
   return(out)
 }

## have we shown the widget? if so, we set in document too
## We need to also assign to .$ID in GlobalEnv as otherwise
## we don't get the getValue right
EXTWidget$setValue <- function(., index=NULL, ..., value) {
  ## override locally if desired
  if(exists("..setValue",envir=., inherits=FALSE)) {
    .$..setValue(index=index, ..., value=value)
  } else {
    
    if(!is.null(index)) {
      items <- .$getValues();  d <- dim(items)
      if(is.null(d) || d == 1)
        newVal <- .$getValues()[value,1]
      else
        newVal <- .$getValues()[value,]
    } else {
      newVal <- value
    }
    .$..data <- newVal
    if(.$ID != "")
      assign(.$ID,newVal, envir=.GlobalEnv)
  }
  ## now process if shown
  if(exists("..shown",envir=., inherits=FALSE)) 
    cat(.$setValueJS(index=index,...),file=stdout())

 }
## create javascript code to write Javascript to set
## the value of a widget
## properties setValueMethod, 
EXTWidget$setValueJSAttribute = "innerHTML"

## this uses the DOM value -- not the Ext widget. EXTComponent
## overrides this.
EXTWidget$setValueJS <- function(.,...) {
  if(exists("..setValueJS", envir=., inherits=FALSE)) .$..setValueJS(...)

  
  ## default method to set the value using setValue
  value <- .$..data                     # don't use svalue here
  
  
  out <- String('var widget = ') +
    'EXT.get(' + shQuote(.$ID) + ');' +
      'widget.' + .$setValueJSAttribute + '= ' + shQuote(value) + ';\n'
  
  return(out)                              # to browser, not file
}


## function to coerce values using either coerce.with or ..coerce.with
EXTWidget$coerceValues <- function(.,values = .$..values) {
  coerce.with = NULL
  if(exists("..coerce.with", envir=., inherits=FALSE))
    coerce.with <- .$..coerce.with
  else if (exists("coerce.with", envir=., inherits=TRUE))
    coerce.with <- .$coerce.with
  
  if(is.null(coerce.with)) return(values)

  if(is.character(coerce.with))
    return(do.call(coerce.with, list(values)))
  else
    return(coerce.with(values))
}
  


## getValues = [; setValues = [<-
## [, [<-

EXTWidget$getValues <- function(., ...) .$..values
EXTWidget$setValues <- function(.,i,j,...,value) {
  ## XXX Need to include i,j
  .$..values <- value
  if(exists("..shown",envir=., inherits=FALSE))
    cat(.$setValuesJS(...))
}
EXTWidget$setValuesJS <- function(.,...) {
  if(exists("..setValuesJS", envir=., inherits=FALSE)) .$..setValuesJS(...)  
  return("")                               # cat javascript to set values
}

## length, dim -- issue with recursion if given full name
length.gWidget <- function(x) {. = x; .$.length()}
EXTWidget$.length <- function(.) {vals <- .$..values; length(vals)}
dim.gWidget <- function(x) {. = x; .$.dim()}
EXTWidget$.dim <- function(.) {vals <- .$..values; dim(vals)}

## names, names<-
EXTWidget$getNames <- function(.) .$names
EXTWidget$setNames <- function(.,value) {
  .$..names <- value
  if(exists("..shown",envir=., inherits=FALSE)) {
    cat(.$setNamesJS())
  }
}
EXTWidget$setNamesJS <- function(.) {}    # set names


## visible<-
EXTWidget$getVisible <- function(.) return(.$..visible )
EXTWidget$setVisible <- function(.,value) {
  .$..visible <- as.logical(value)
  if(exists("..shown",envir=., inherits=FALSE)) {
    cat(.$setVisibleJS())
  }
}
EXTWidget$setVisibleJS <- function(.) {
  if(exists("..setVisibleJS", envir=., inherits=FALSE))
    .$..SosetVisibleJS()
  
  value <- .$..visible
  if(as.logical(value))
    action = "show"
  else
    action = "hide"
  .$callExtMethod(action)
}

## enabled<- 
EXTWidget$getEnabled <- function(.) return(.$..enabled )
EXTWidget$setEnabled <- function(.,value) {
  .$..enabled <- as.logical(value)
  if(exists("..shown",envir=., inherits=FALSE)) 
    cat(.$setEnabledJS())
}
EXTWidget$setEnabledJS <- function(.) {
  if(exists("..enabled", envir=., inherits=FALSE))
    value <- as.logical(.$..enabled)
  else
    value <- TRUE

  ## which method
  method <- ifelse(value, "enable", "disable")

  out <- String() +
    'o' + .$ID + '.' + method + '();' + '\n'
  
##   ## uses DOM
##   out <- String() +
##     'var widget = ' +
##       'Ext.get(' + shQuote(.$ID) + ');\n' +
##        'widget.' + method + ';\n'
  return(out)
}


 ## ..style covers fonts, size, and others
 ## font uses this
 EXTWidget$setFont <- function(.,value) {
 }
## XXX integrate with setStylesJS
EXTWidget$setStyleJS <- function(.,styles = NULL) {
  ## styles
  if(is.null(styles)) {
    styles <- .$style
  }
  if(exists("..style",envir=., inherits=FALSE)) {
    for(i in names(.$..style))
      styles[i] <- .$..style[i]
  }
  
  if(length(styles) > 0) {
    out <- String() 
    
    for(i in names(styles)) {
      out <- out +
        'Ext.get(' + shQuote(.$ID) + ').setStyle(' + shQuote(i) + ',' +
          coerceToJSString(styles[i]) + ');\n'
    }
  } else {
    out <- String()
  }
  return(out)
}
  

 ## set size.
 ## calls setStyle
 EXTWidget$setSize <- function(., value) {
   ## fix size in ..style
   if(exists("..style",envir=., inherits=FALSE))
     curStyle <- .$..style
   else
     curStyle <- c()

   n <- length(value)
   if(n == 0) return()

   curStyle["width"] <- .$..width <- value[1]


   if(n > 1) {
     curStyle["height"] <- .$..height <- value[2]
   }

   .$..style <- curStyle
   return()
 }

EXTWidget$getSize <- function(.) {
  if(!exists("..style",envir=., inherits=FALSE))
    return(c(width=NULL,height=NULL))
  
  curStyle <- .$..style
  return(c(width=curStyle$width,height=curStyle$height))
}


## Methods to print out the javascript to create a widget
## using Ext. There are several stages that can be overridden. For
## example, see EXTComponentWithStore where writeConstructor is overridden.

## these options are inherited by others. Can be overridden
## by the widget
## Standard Configuration options
## also have ExtCfgOptions for each widget (class based)
## and ..ExtCfgOptions for instances

## The function mapRtoObjectLiteral pieces together the list.
## The one subtlety is that characters get quoted, String()'s do not.
## the function is recursive, as some options may be given as
## object literals in Ext

EXTWidget$ExtStdCfgOptions <- function(.) {
  out <- list("renderTo"=String("document.body"),
              "id"=.$ID
              )
  if(exists("..enabled",envir=., inherits = FALSE))
    if(!.$..enabled)
      out[['disabled']] <- !.$..enabled   # in Ext.Component
  if(exists("..visible",envir=., inherits = FALSE))
    if(!.$..visible)
      out[['hidden']] <- !.$..visible   # in Ext.Component

  if(exists("..tpl", envir=., inherits=FALSE)) {
    out[['tpl']] <- .$..tpl()
  } else if(exists("tpl", envir=., inherits =FALSE)) {
    out[['tpl']] <- .$tpl()
  }
  
  ## XXX how to integrate styles into this?
  return(out)
}


       
## method to coerce ExtCfgOptions into a string
## ExtCfgOptions is a list. The names are passed as keys and the values
## are assigned.
## Object Literals in Ext are like passing lists through the ... argument
## characters are quoted, String()s are not. Sometimes
## as.character(String() + "...") is useful.
## This function recurses when the value is a list
## method coerceToJSString is in common.R

EXTWidget$mapRtoObjectLiteral <- function(.,values,doBraces=TRUE) {
  
  if(missing(values)) {
    ## pull in from std, class configuration, instance values
    values <- .$ExtStdCfgOptions()

    if(exists("ExtCfgOptions", envir=., inherits=TRUE) &&
       !is.null(.$ExtCfgOptions)) {

      cfgValues <- .$ExtCfgOptions()
      for(i in names(cfgValues))
        values[[i]] <- cfgValues[[i]]
    }
    ## add to values if some there
    if(exists("..ExtCfgOptions", envir=., inherits=FALSE)) {
      instanceValues <- .$..ExtCfgOptions()
      for(i in names(instanceValues)) {
        if(!is.null(instanceValues[[i]]))
          values[[i]] <- instanceValues[[i]]
      }
    }
  }
  

  ## values is a list, we need to make a vector of strings
  out <- c()
  for(i in names(values)) {
    if(!is.null(values[[i]])) {
      ## recurse if a list
      if(is.list(values[[i]])) {
        out[i] <- .$mapRtoObjectLiteral(values[[i]], doBraces=TRUE)
      } else {
        out[i] <- coerceToJSString(values[[i]])
      }
    }
  }

  res <- paste(names(out), out, sep=": ", collapse=",\n")
  if(doBraces)
    res <- String('{') + res + '}\n'

  return(res)
}

## Basic template for a EXT widget.
## There are several stages.
## header and footer are used to wrap the object if desired

## The header and footer are blank
EXTWidget$header <- EXTWidget$footer <- EXTWidget$separator <- function(.) return("")

EXTWidget$writeConstructor <- function(.) {
  out <- String() +
### var creates a *local* variable -- issues with safari here
###    'var o' + .$ID +
    'o' + .$ID +
      ' = new ' +.$ExtConstructor + '(\n' +
        .$mapRtoObjectLiteral() +
          ');\n'
  ## write out x-hidden unless requested not to.
  ## x-hidden causes the widget not to display until added to parent
  if(!exists("no.x.hidden",envir=., inherits=FALSE))
    out <- out +
      'Ext.get(' + shQuote(.$ID) +').addClass("x-hidden");\n'
  
  return(out)
}

## For controls whose value may be changed by the GUI, we write out changes
## immediately back to R so that R handlers will be aware of the changes. We
## call this transport.

## code to write out transport function
## called in writeHandlers
EXTWidget$transportValue <- function(.,...) {
  out <- String() +
    'var value = o' + .$ID + '.' +
      .$getValueJSMethod + '();\n'
  return(out)
}
EXTWidget$transportFUN <- function(.) {
  out <- String() +
    '_transportToR(' + shQuote(.$ID) +', value);'
  return(out)
}

EXTWidget$writeTransport <- function(.,ext="",signal=NULL) {
  ## transport to R
  if(!is.null(.$transportSignal)) {
    out <- String() +
      .$transportValue(i = ext,signal=signal) + # (For EXTComponentWithItems)
        .$transportFUN() +
          '\n'
  } else {
    out <- String("")
  }
  return(out)
}

## writes tooltip. Tooltips are added with tooltip<- function
## value can be a URL (isURL == TRUE) or a string or a character vector which
## gets pasted together to be a string
EXTWidget$tooltipWidth <- 200
EXTWidget$tooltipAutoHide <- TRUE # override to 
EXTWidget$writeTooltip <- function(.) {
  out <- String()
  ## tooltip
  if(exists("..tooltip", envir=., inherits=FALSE)) {
    lst <- list(target=.$ID,
                showDelay=100,
                hideDelay=50,
                autoHide = .$tooltipAutoHide,
                trackMouse = TRUE,
                width = .$tooltipWidth)            # default size?

    if(isURL(.$..tooltip)) {
      lst[["autoLoad"]] <- String('{url:') + shQuote(.$..tooltip) + '}'
    } else {
      ## ..tooltip can be a) a string, b) a character vector of c) a list with components title and message
      if(is.list(.$..tooltip)) {
        lst[['title']] <- .$..tooltip$title
        message <- .$..tooltip$message
      } else {
        message <- .$..tooltip
      }
      lst[["html"]] <- paste(escapeQuotes(message), collapse="<BR>")
    }

    if(!.$tooltipAutoHide) {
      lst[["closable"]] <- TRUE
      lst[["draggable"]] <- TRUE
    }
    out <- out +
      'var tooltip' + .$ID + '= new Ext.ToolTip(' +
        +  .$mapRtoObjectLiteral(lst) + ');' + '\n'
  }
  return(out)
}

## show object, including catting it
## called by Show
EXTWidget$show <- function(.) {
  out <- String() +
    .$writeConstructor() +
      .$setStyleJS(styles=.$style) +
          .$writeTooltip() +
            .$writeHandlersJS()           # includes transport
  
  .$Cat(out)
}


##################################################
## Some "subclasses" of EXTWidget defined below

## EXT Component -- for controls, etc

EXTComponent <- EXTWidget$new()



## public API. Call Show which wraps show withing header, tooltip,
## separators, footer,
## A widget will show
## header
## css (gwindow)
## scripts (gwindow)
## tooltip
## .$show()
## setHandlers (gwindow)
## footer

## unlike a Container, these have no children

EXTComponent$Show <- function(.,...) {        # wraps Show
  ## add in any instance specific scripts
  ## component specific scripts written out once in gwindow
  if(exists("..scripts",envir=., inherits=FALSE)) 
    .$showPart(.$..scripts)
  
  ## make an object with method?
  if(exists("..header",envir=.,inherits=FALSE))  .$showPart(.$..header)
  .$showPart(.$header)

  
  .$show()                      # show self
  .$..shown <- TRUE             # set shown (rendered)
  
  if(exists("..footer",envir=.,inherits=FALSE))  .$showPart(.$..footer)
  .$showPart(.$footer)
}



### Methods have two parts
### * one for first showing (sets value in R)
### * one after shown -- returns string with javascript

EXTComponent$setValueJSAttribute <- "value"
EXTComponent$setValueJSMethod = "setValue"  # oID.method()
EXTComponent$setValueJS <- function(.,...) {
  if(exists("..setValueJS", envir=., inherits=FALSE)) .$..setValueJS(...)
   ## default method to set the value using setValue
   value <- .$..data                     # don't use svalue here


   ## use Ext object and a method directly -- no DOM
   out <- String() +
     'o' + .$ID +'.' + .$setValueJSMethod +
       '(' + toJS(.$..data) + ');' + '\n'
   
   return(out)                              # to browser, not file
 }

 EXTComponent$setValuesJS <- function(.,...) {
   if(exists("..setValuesJS", envir=., inherits=FALSE)) .$..setValuesJS(...)  
   return("")
 }


EXTComponent$setNamesJS <- function(.) {}    # set names
## get from EXTWidget
## EXTComponent$setVisibleJS <- function(.) {
##   if(exists("..setVisibleJS", envir=., inherits=FALSE))
##     .$..setVisibleJS()
  
##   if(.$..visible)
##     method = "hide"
##   else
##     method = "show"
##   .$callExtMethod(method)
## }


## Different components

EXTComponentResizable <- EXTComponent$new()

## footer adds in a resizable conainer -- notw orking?
EXTComponentResizable$footer <- function(.) {
  lst <- list(id  = as.character(.$ID + 'resizer'),
              wrap = TRUE,
              pinned = TRUE)
  if(inherits(.,"gImage"))
    lst[['preserveRatio']] <- TRUE

  out <- String() +
    'new Ext.Resizable(' + shQuote(.$ID) + ',' +
      .$mapRtoObjectLiteral(lst) + ');\n'

  return(out)
  }



##
##
###########

## Container Trait. Main methods
## are:
## newID -- ID for a widget generated when added to a container.
##   This also copies over stuff to
## add -- adds to list for later rendering
## Show -- to show object -- loops over children

EXTContainer <- EXTWidget$new(children = list(),
                              width = "auto",
                              height = "auto",
                              makeItemsFixedItems = "" # to make items[]
                              )
## each object gets an ID.
EXTContainer$newID <- function(.) {
  IDS <- .$toplevel$..IDS
  n <- length(IDS)
  newID <- paste("gWidgetID",n+1,sep="")
  .$toplevel$..IDS <- c(IDS,newID)
  return(newID)
}
## add for a container does several things:
## * set toplevel for each child
## * add css to toplevel if applicable
## * add javascript to toplevel if applicable
## * set object into child
## ... is not used XXX should fix this.
EXTContainer$add <- function(.,child,...) {

   ## add an ID
   child$ID <- .$newID()

   ## add parent to child for traversal
   child$parent = .
   child$toplevel = .$toplevel         # pass in toplevel window

   ## pass along parent properties
   child$file <- .$file
   child$titlename <- .$titlename

   ## Move scripts, css to toplevel
   if(!is.null(child$css)) {
     css <- .$toplevel$css
     if(is.function(child$css))
       css[[class(child)[1]]] <-
         list(obj = child, FUN = get("css",child))
     else if(is.character(child$css))
       css[[class(child)[1]]] <- child$css

     .$toplevel$css <- css
   }

   ## scripts
   if(!is.null(child$scripts)) {
     scripts <- .$toplevel$scripts
     if(is.null(scripts[[class(child)[1]]])) {
       ## not show, add
       if(is.function(child$scripts))
         scripts[[class(child)[1]]] <-
           list(obj = child, FUN = get("scripts",child))
       else if(is.character(child$scripts))
         scripts[[class(child)[1]]] <- child$scripts
       
       .$toplevel$scripts <- scripts

       
       if(exists("..shown", envir=.$toplevel, inherits=FALSE) && .$toplevel$..shown) {
         ## need to cat this script out now,
         ## This prevents things being defined in subwindows
         ## for first time
         i <- scripts[[class(child)[1]]]
         if(is.list(i))
           cat(i$FUN(i$obj))
         else if(is.character(i))
           cat(i)
        }
     }
   }
   

   
   ## add to children
   lst <- .$children
   .$children <- c(lst, child)

   if(exists("..shown",envir=., inherits=FALSE)) {
     if(!inherits(child,"gSubwindow")) {
       child$Show()
       cat(.$addJS(child))
     }
   }
   
 }
## write out JS to add child after things have been shown
## this is likely not perfect
EXTContainer$addJS <- function(.,child) {
  out <- String() +
    .$asCharacter() + '.add(' +
      child$asCharacter() + ');'

  return(out)
}

## for containers width and height are properties, not in .style
EXTContainer$setSize <- function(., value) {
  .$width <- value[1]
  if(length(value) > 1)
    .$height <- value[2]
}
EXTContainer$getSize <- function(.) c(width=.$width,height=.$height)

EXTContainer$ExtStdCfgOptions <- function(.) {
  out <- get("ExtStdCfgOptions",EXTWidget)(.)
  out[['width']] <- .$width
  out[['height']] <- .$height
  
  ## menubar, toolbar, statusbar
  if(exists("..menuBar",envir=., inherits=FALSE)) {
    out[['tbar']] <- .$..menuBar$writeMenu()
    .$..menuBar$..shown <- TRUE
  }
  
  if(exists("..statusBar",envir=., inherits=FALSE)) {
    sbText <- String() +
      'new Ext.StatusBar({' +
        'id: "' + .$ID + 'statusBar",' +
          'defaultText: "",' +
            'text:' + shQuote(.$..statusBarText) +
              '})'
    out[['bbar']] <- sbText
    .$..statusBar$..shown <- TRUE
    
  }
    
  return(out)
}



## Containers have children to show too.
## also a separator is possible to display between the children,
## although this should go
EXTContainer$Show <- function(.) {
  
  ## scripts
  if(exists("scripts", envir=., inherits=FALSE)) {
    out <- String() 
    for(i in .$scripts) {
      if(is.list(i))
        out <- out + i$FUN(i$obj)
      else if(is.character(i))
        out <- out + i
    }
    .$Cat(out)
  }

  ## css -- use createStyleSheet method of Ext JS to write out
  if(exists("css",envir=., inherits=FALSE)) {
    out <- String() 
    for(i in .$css) {
      if(is.list(i))
        out <- out + i$FUN(i$obj)
      else if(is.character(i))
        out <- out + i
    }
    ## wrap in EXT JS function
    out <- String('Ext.util.CSS.createStyleSheet("') + out + '");'
    
    .$Cat(out)
  }


  ## now show container
  if(exists("..header",envir=.,inherits=FALSE))  .$showPart(.$..header)
  .$showPart(.$header)

  
  ## write out actions if present
  if(exists("..actions", envir = ., inherits = FALSE)) {
    if(length(.$..actions) > 0) {
      for(i in .$..actions) {
        i$Show();
        i$..shown <- TRUE
      }
    }
  }

  

  children <- .$children
  if((n <- length(children)) > 0) {
    for(i in 1:n) {
      children[[i]]$Show()              # Show children
      if(i < n) {
        if(exists("..separator",envir=.,inherits=FALSE))
          .$showPart(.$..separator)       # widget specific
        .$showPart(.$separator)
      }
    }
  }

  .$show()                      # show self
  .$..shown <- TRUE                     # set shown

  ## handlers ## gwindow only
  if(exists("..setHandlers",envir=.,inherits=FALSE)) # gwindow only
     .$showPart(.$..setHandlers)

  if(exists("..footer",envir=.,inherits=FALSE))  .$showPart(.$..footer)
  .$showPart(.$footer)

}

## items are how containers refer to their children

## this will be overridden more than likely
EXTContainer$makeItems <- function(.) {
  childIDs <- sapply(.$children, function(i) i$ID)
  isResizable <- sapply(.$children, function(i)
                        inherits(i,"gWidgetResizable"))
  if(any(isResizable))
    childIDs[isResizable] <- paste(childIDs[isResizable],"resizer",sep="")

  
  n <- length(.$children)
  if(n == 0)
    return("{}")
  
  ## contentEl specifies where to get values
  contentEls <- paste('contentEl:', shQuote(childIDs), sep="")
  ## labels are for notebooks
  theLabels <- character(n)
  for(i in 1:n) {
    if(exists("..label",envir=.$children[[i]],inherits=FALSE))
      theLabels[i] <- String("title:") + shQuoteEsc(.$children[[i]]$..label)
  }
  ## tabTooltips are for notebooks
  tabTooltips <- character(n)
  for(i in 1:n) {
    if(exists("..tabTooltip",envir=.$children[[i]],inherits=FALSE))
      tabTooltips[i] <- String("tabTip:") + shQuoteEsc(.$children[[i]]$..tabTooltip)
  }

  itemContent <- character(n)
  for(i in 1:n) {
    if(theLabels[i] == "" && tabTooltips[i] == "")
      itemContent[i] <- paste(contentEls[i],sep=",")
    else if(theLabels[i] == "") 
      itemContent[i] <- paste(contentEls[i],tabTooltips[i],sep=",")
    else if(tabTooltips[i] == "")
      itemContent[i] <- paste(contentEls[i],theLabels[i],sep=",")
    else
      itemContent[i] <- paste(contentEls[i],theLabels[i],tabTooltips[i],sep=",")
  }
  if(!exists("makeItemsFixedItems",.,inherits=FALSE))
    .$..fixedItems <-  ""               # ends with ",".


  
  tmp <- String('{') +  .$makeItemsFixedItems

  items <- paste(paste(tmp,itemContent,'}',
                  sep=""),
            collapse=",")
  return(items)
}

## overritde EXTWidget$show, as
## unlike a EXTComponent, here we need to add in the items too
EXTContainer$show <- function(.) {
  out <- String() +
##XX    '<script>\n' +
      'o' + .$ID + '= new ' + .$ExtConstructor + '({' + '\n' +
        .$mapRtoObjectLiteral(doBraces=FALSE) +
          ',' + '\n' +
            'items:[' +.$makeItems() +
              ']\n' + '});' 
##XX  out <- out +'</script>'
  .$Cat(out)
}

##################################################
##
## Some widget have a data store associated with them
## eg. gcombobox, gtable, gdf 

EXTStore <- proto()
EXTStore$new <- function(.) {
  obj <- .$proto()
  class(obj) <- c("gWidgetStore",class(obj))
  invisible(obj)
}
## properties
## the data frame (or vector)
EXTStore$ID <- NULL                      # get from newID
EXTStore$data <- NULL
EXTStore$chosenCol <- NULL               # selected column

## methods
## coerce val to a JS array
EXTStore$setData <- function(.,d) .$data <- d
EXTStore$getData <- function(.) .$data
EXTStore$setChosenCol <- function(.,value).$chosenCol <- value
EXTStore$getChosenCol <- function(.).$chosenCol

EXTStore$asJSArray <- function(.,val) {
  if(missing(val)) val <- .$data
  toJSArray(val)
}
EXTStore$asCharacter <- function(.) String('o') + .$ID + 'store'
EXTStore$displayField <- function(.) .$chosenCol

EXTStore$fieldNames <- function(.) {names(.$data)}
## for combo tihs is just an array, 
## for a grid object it is more work
EXTStore$makeFields <- function(.) {
  .$asJSArray(.$fieldNames())
}
EXTStore$show <- function(.) {
  out <- .$asCharacter() + '= new Ext.data.SimpleStore({' +
    'fields:  ' + .$makeFields() + ',' + '\n' +
      'data: ' + .$asJSArray() +
        '\n' + '})' + '\n'
  return(out)
}

## replace the store with this data frame
EXTStore$replaceStore <- function(., data) {
  if(!missing(data)) .$data <- data
  out <- String() +
    .$asCharacter() + '.removeAll();' +
      .$asCharacter() + '.loadData(' +
        .$asJSArray() + ');'

  return(out)
}
## XXX need more granular approach


## extend Component to handle a data store
EXTComponentWithStore <- EXTComponent$new()

## additional properties

## store -- holds an EXTStore instance
EXTComponentWithStore$..store <- NULL

## methods
EXTComponentWithStore$getValues <- function(., ...) .$..store$data
EXTComponentWithStore$getLength <- function(.)
  length(.$getValues())
EXTComponentWithStore$getNames <- function(.)
  names(.$getValues())
## XXX names<- not defined
## getValues needs to refer to the store
EXTComponentWithStore$getValues <- function(.,...) 
  .$..store$data
EXTComponentWithStore$setValues <- function(.,i,j,...,value) {
  ## XXX need to include i,j stuff
  .$..store$data <- value
  if(exists("..shown",envir=., inherits=FALSE))
    cat(.$setValuesJS(...))
}
EXTComponentWithStore$setValuesJS <- function(.) {
  if(exists("..setValuesJS", envir=., inherits=FALSE)) .$..setValuesJS(...)
  
  out <- String() +
    .$asCharacter() + '.removeAll();' +
      .$asCharacter() + '.loadData(' +
        .$asJSArray(.$getValues()) +');'

  return(out)
}


## turn an object into an array
EXTComponentWithStore$asJSArray <- function(.,...) {
  .$..store$asJSArray(...)
}
## show needs to show store and component
EXTComponentWithStore$show <- function(.) {
  .$Cat(.$..store$show())
  get("show",EXTComponent)(.)       # call up
}
## get name of store to paste into JS code
EXTComponentWithStore$asCharacter <- function(.) {
  .$..store$asCharacter()               # call into store
}


### Some widgets render better in a panel
## This overrides the writeConstructor method to show the object
## in ExtStdCfgOptions use an xtype and override renderTo with NULL
## see gcheckbox for an example
EXTComponentInPanel <- EXTComponent$new()
EXTComponentInPanel$getItemID <- function(.) String(.$ID) + 'item'
EXTComponentInPanel$writeConstructor <- function(.) {
  lst <- list(id = as.character(.$ID),
              xtype = "panel",
              layout = "fit",
              border = FALSE,
              hideBorders = TRUE,
              width = ifelse(exists("..width", ., inherits=FALSE),
                .$..width,"auto"),
              renderTo = String("Ext.getBody()"),
              items = String("[") + .$mapRtoObjectLiteral() + ']'
              )
  out <- String() +
    'o' + .$ID + 'panel = new Ext.Panel(' + # no var -- global
      .$mapRtoObjectLiteral(lst) +
        ');' + '\n'
  ## get component from first child object
  out <- out +
    'o' + .$ID + ' = ' +                # no var -- global
      'o' + .$ID + 'panel.getComponent("' + .$getItemID() + '");' + '\n'
  
  return(out)
}

## a component with items like gradio and gcheckboxgroup
## we use a panel and use the items to store the values
## the handlers need to be assigned to each 
EXTComponentWithItems <- EXTComponent$new()
EXTComponentWithItems$xtype <- ""       # eg "checkbox", "radio"
EXTComponentWithItems$itemname <- "item"
EXTComponentWithItems$ExtConstructor <- "Ext.Panel"
EXTComponentWithItems$no.x.hidden <- TRUE
EXTComponentWithItems$checked <- function(.,i) {
  ## return TRUE if checked o/w false
}
## make the items
EXTComponentWithItems$makeItems <- function(.) {
  out <- String()
  
  values <- .$getValues()
  if((n <- length(values)) < 2)  return(out)
    
  tmp <- list()                          # store items as String
  for(i in 1:n) {
    lst <- list(xtype = .$xtype,
                name = as.character(String() + .$ID + .$itemname),
                boxLabel = as.character(values[i]),
                checked = .$checked(i)
                )
    tmp[[i]] <- .$mapRtoObjectLiteral(lst)
  }

  out <- out +
    '[' + paste(tmp,collapse=",") + ']'
  
  return(out)
}

## Modified from orignial to loop over all items
EXTComponentWithItems$writeHandlersJS <- function(.) {
  if(exists("..handlers", envir=., inherits=FALSE))
    allHandlers <- .$..handlers
  else
    allHandlers <- list()

  ## get all signals
  signals <- c()
  if(!is.null(.$transportSignal))
    signals <- .$transportSignal
  if(length(allHandlers) > 0)
    signals <- union(signals, names(allHandlers))

  if(length(signals) == 0) return(String(""))     # nothing to do
  
  out <- String()
  for(sig in signals) {
    for(i in 1:(n <- length(.))) {
      out <- out +
        'var widget = ' + .$asCharacter() + '.getComponent(' +
          as.character(i - 1) + ');' +
            'widget.on(' +
              ## XXX transport args needs to be siganl dependent!!
              shQuote(sig) + ', function(' + .$handlerArguments(sig) + ') {\n'

      ## write out transport if necessary
      ## XXX code to pass values createDelegate ....
      if(!is.null(.$transportSignal) && sig %in% .$transportSignal) {
        out <- out + .$writeTransport(ext = shQuote(i), signal=sig) # ## pass this in
      }
      
      ## write out handler if needed
      if(!is.null(allHandlers[[sig]])) {
        handler <- allHandlers[[sig]]
        out <- out +
          'runHandlerJS(' + handler$handlerID  +
            handler$handlerExtraParameters + ');' + '\n' +
              'true;' + '\n'
      }
    
      out <- out +
        '},this, {delay:100,buffer:100, single:false});' + '\n'
    }
  }

  return(out)
}






##############################
## gwidget methods
svalue <- function(obj,index=NULL, drop=NULL,...) UseMethod("svalue")
svalue.gWidget <- function(obj,index=NULL, drop=NULL,...) {
  obj$getValue(index=index,drop=drop,...)
}

"svalue<-" <- function(obj,index=NULL, ...,value) UseMethod("svalue<-")
"svalue<-.gWidget" <- function(obj,index=NULL, ..., value) {
  obj$setValue(index=index,..., value=value)
  return(obj)
}

## add is used by gtext atleast. $Add implicitly used by contaienrs
"add" <- function(obj,value,...) UseMethod("add")
"add.gWidget" <- function(obj, value, ...) {
  if(exists("add",envir=obj, inherits=TRUE))
    obj$add(child=value,...)
}

## insert is new name for add for gtext
"insert" <- function(obj, value, where = c("end","beginning","at.cursor"),
                     font.attr = NULL,
                     do.newline = TRUE, ...) UseMethod("insert")
"insert.gWidget" <- function(obj, value, where = c("end","beginning","at.cursor"),
                             font.attr = NULL,
                             do.newline = TRUE, ...) {
  where = match.arg(where)
  add(obj, value, where=where, font.attr=font.attr, do.newline=do.newline,...)
}
                       

## toggle whether widget can receive input
"enabled" <- function(obj) UseMethod("enabled")
"enabled.gWidget" <- function(obj) {
  . <- obj
  if(exists("..enabled", envir=., inherits =FALSE))
    return(.$..enabled)
  else
    return(TRUE)
}
"enabled<-" <- function(obj,...,value) UseMethod("enabled<-")
"enabled<-.gWidget" <- function(obj,..., value) {
  . <- obj
  if(missing(value)) value <- TRUE
  .$..enabled <- as.logical(value)

  if(exists("..shown", envir=., inherits=FALSE))
    cat(obj$setEnabledJS())
  
  return(obj)
}


## dispose of widget. We simply hide it here
## no method until created
"dispose" <- function(obj,...) UseMethod("dispose")
"dispose.gWidget" <- function(obj,...) {
  . = obj

  if(exists("dispose", envir=.)) {
    .$dispose()
  } else if(exists("..shown",envir=., inherits=FALSE)) {
    cat(.$callExtMethod("hide"))
  }
}

 ## focus
"focus<-" <- function(obj,...,value) UseMethod("focus<-")
"focus<-.gWidget" <- function(obj,..., value) {
  . = obj
  value <- as.logical(value)
  ## set ..focus attribute -- not implemented
  if(value) .$..focus <- TRUE
  
  if(exists("..shown",envir=., inherits=FALSE) && value)
    cat(.$callExtMethod("focus",tolower(as.character(value))))

  return(obj)
}

 ## visible, visible<-
visible <- function(obj) UseMethod("visible")
visible.gWidget <- function(obj) obj$getVisible()
"visible<-" <- function(obj,...,value) UseMethod("visible<-")
"visible<-.gWidget" <- function(obj,..., value) {
  obj$setVisible(value)
  return(obj)
}



"[.gWidget" <- function(x,i,j,drop = TRUE) {
##  if (missing(i)) TRUE else length(cols) == 1) {
  . = x
  values <- .$getValues()
  
  if(missing(i)) {
    if(is.null(dim(values)))
      return(values)
    else if(missing(j))
      return(values[,,drop=drop])
    else
      return(values[,j,drop=drop])
  } else {
    if(is.null(dim(values)))
      return(values[i])
    else if(missing(j))
      return(values[i,,drop=drop])
    else
      return(values[i,j,drop=drop])
  }
}

"[<-.gWidget" <- function(x,i,j,...,value) {
  . = x
  if(missing(i) && missing(j))
    .$setValues(..., value=value)
  else if(missing(i))
    .$setValues(j =j,..., value=value)
  else if(missing(j))
    .$setValues(i = i,..., value=value)
  else
    .$setValues(i = i, j =j,..., value=value)
  return(x)
 }

## names
"names.gWidget" <- function(x) {
  . <-  x
  .$getNames()
}
"names<-.gWidget" <- function(x,value) {
  . = x
  .$setNames(value)
  return(x)
}


## size of widget
"size" <- function(obj) UseMethod("size")
"size.gWidget" <- function(obj) {
  obj$getSize()
}
"size<-" <- function(obj,value) UseMethod("size<-")
"size<-.gWidget" <- function(obj,value) {
  obj$setSize(value)
  return(obj)
}

## set font -- use stylesheet
## eg:
## font(l) <- c("font-family"="Verdana, Arial, Helvetica, sans-serif",
## 	   "font-size" = "large",
## 	   "font-style" = "italic",
## 	   "font-weight" = "bold")
## cf: http://www.yourhtmlsource.com/stylesheets/csstext.html
"font<-" <- function(obj,value) UseMethod("font<-")
"font<-.gWidget" <- function(obj,value) {
  . <- obj
  ## gWidgets names are family, size, style, weigth, ala X11
  changeThese <- c("family","size","style","weight")
  
  vals <- intersect(names(value), changeThese)
  for(i in vals)
    names(value)[which(names(value) == i)] <- paste("font-",i,sep="")

  if(!exists("..style",envir=., inherits=FALSE))
    .$..style <- value
  else
    .$..style <- c(.$..style,value)
  
  if(exists("..shown",., inherits=FALSE)) {
    cat(.$setStyleJS())
   }
  
  return(obj)
}

## we use print for gwindow and gsubwindow as a more natural alias
## than $Show() i.e. after storing objects into the toplevel window or
## its subwindos, we need to be able to show the contents to the
## brower. This is done with th eproto method $#Show(), which here is
## aliased to theprint method of the proto objects.
print.gWindow <- print.gSubwindow <- function(x,...) {
  . = x; .$Show()
}
  


## Method to set a tooltip on an object
## if isURL(value) == TRUE, then loads from website
## value can be a list with title, message or just a message
"tooltip<-" <- function(obj,value) UseMethod("tooltip<-")
"tooltip<-.gWidget" <- function(obj,value) {
  if(isURL(value)) {
    obj$..tooltip <- value
  } else {
      obj$..tooltip <- value
  }
  return(obj)
}

## addSpring and addSpace are used to align children within the group
## containers. These are not defined, but should be in ggroup.R
addSpace <- function(obj, value, horizontal=TRUE, ...) UseMethod("addSpace")
addSpace.gWidget <- function(obj, value, horizontal=TRUE, ...) 
  obj$addSpace(value, horizontal = horizontal, ...)

addSpring <- function(obj, ...) UseMethod("addSpring")
addSpring.gWidget <- function(obj,  ...) 
  obj$addSpring(...)



## DND -- XXX not defined
addDropSource <- function(obj, targetType = "text", handler = NULL, action = NULL, ...) UseMethod("addDropSource")
addDropMotion <- function(obj, handler = NULL, action = NULL, ...) UseMethod("addDropMotion")
addDropTarget <- function(obj, targetType = "text", handler = NULL, action = NULL,  ...) UseMethod("addDropTarget")



##################################################
## Javascript handlers -- not submit handlers for buttons etc.


## Handler code

## code to run a handler.
## This must be exported.
## Called from www page
## worry about scope!! XXX -- doesn't seem to work

runHandler <- function(obj, id,key,value) {
  obj <- get(obj, envir = .GlobalEnv)
  lst <- obj$jscriptHandlers[[as.numeric(id)]]
  h <- list(obj=lst$obj, action = lst$action)
  if(!missing(key) && key != "") {
    value <- unescapeURL(value)
    assign("test", value,envir=.GlobalEnv)
    ## eval things that say ... eval:
    if(length(grep("^evalme", value)) > 0) {
      cmd <- gsub("^evalme","",value)
      value <- eval(parse(text = cmd), envir=.GlobalEnv)
    }
    h[[key]] <- value       # escaped text. See dialogs example
  }
##  with(lst$scope,lst$handler(h))
  return(lst$handler(h))
}


## code to write out JS for the handlers on a object
## tricky part is that handlers must also be called for
## transport signals

## can override this per widget if desired!
## These were cherry picked from the Ext docs. Some may be missing,
## many are only available through the addHandler() method.

EXTWidget$handlerArgumentsList <-
  list(afteredit = "e",                 # for gdf cell editing
       blur="w",                        # w = "this"
       bodyresize = "w, width, height",
       bodyscroll = "scrollLeft, scrollRight",
       cellcontextmenu = "w, rowIndex, cellIndex, e",
       cellclick = "w, rowIndex, columnIndex, e", # grid
       celldblclick = "w, rowIndex, columnIndex, e", # grid
       cellmousedown = "w, rowIndex, columnIndex, e", # grid
       change="w, newValue, oldValue", beforechange = "w, newValue, oldValue",
       check = "w, checked",
       collapse = "w",                  # combobox
       columnmove = "oldIndex, newIndex",
       columnresize = "columnIndex, newSize",
       dblclick = "e",                  # grid -- not celldblclick
       destroy="w", beforedestroy = "w",
       disable="w",
       drag = "w, e", dragend = "w,e", dragstart = "w,e",
       enable = "w",
       expand = "w",                    # combobox
       focus = "w",
       headerclick = "w, columnIndex, e", # grid
       headercontextmenu = "w, columnIndex, e", # grid
       headerdblclick = "w, columnIndex, e", # grid
       headermousedown = "w, columnIndex, e", # grid       
       hide = "w", beforehide = "w",
       invalid = "w",
       keydown = "w,e",                 # e Ext.EventObject
       keypress = "w,e",
       keyup = "w,e",
       move = "w, x, y",
       render = "w", beforerender = "w",
       resize = "w, adjWidth, adjHeight, rawWidth, rawHeight",
       rowclick = "w, rowIndex, e", # grid
       rowcontextmenu = "w, rowIndex, e", # grid
       rowdblclick = "w, rowIndex, e", # grid
       rowmousedown = "w, rowIndex, e", # grid       
       select = "w,record,index", beforeselect = "w, record, index", 
       show = "w", beforeshow = "w", 
       specialkey = "w, e",
       valid = "w")

## process the list above allowing for local overrides                                       
EXTWidget$handlerArguments <- function(.,signal) {
  out <- .$handlerArgumentsList
  if(exists("..handlerArgumentsList", envir=., inherits = FALSE)) {
    for(i in names(.$..handlerlArgumentsList))
      out[[i]] <- .$..handlerlArgumentsList[[i]]
  }
  val <- ifelse(is.null(out[[signal]]), "", out[[signal]])
  return(val)
}

## write out a single handler passed as a list
EXTWidget$writeHandlerJS <- function(.,signal,handler=NULL) {
  out <- String() +
    'o' + .$ID + '.on(' +
      ## XXX transport args needs to be siganl dependent!!
      shQuote(signal) + ', function(' + .$handlerArguments(signal) + ') {\n' 
  
  ## write out transport if necessary
  ## XXX code to pass values createDelegate ....
  if(!is.null(.$transportSignal) && signal %in% .$transportSignal) {
    out <- out + .$writeTransport(signal = signal)
  }
              

  ## write out handler if needed
  if(!is.null(handler)) {
    out <- out +
      'runHandlerJS(' + handler$handlerID  +
        handler$handlerExtraParameters + ');' + '\n' +
          ##            'true;' +  ## return value needed, desired?
              '\n'
  }
  
  out <- out +
      '},this, {delay:100,buffer:100, single:false});' + '\n'
  
  return(out)
}
EXTWidget$writeHandlersJS <- function(.) {
  if(exists("..handlers", envir=., inherits=FALSE))
    allHandlers <- .$..handlers
  else
    allHandlers <- list()

  ## get all signals
  signals <- c()
  if(!is.null(.$transportSignal))
    signals <- .$transportSignal
  if(length(allHandlers) > 0)
    signals <- union(signals, names(allHandlers))

  if(length(signals) == 0) return(String(""))     # nothing to do
  
  out <- String()
  for(sig in signals) {
    out <- out + .$writeHandlerJS(sig, allHandlers[[sig]])
  }

  return(out)
}
      
## extra parameters fill in h$key = value
## if the value is preceeded by "^evalme" it gets eval-parsed
EXTWidget$addHandler <- function(., signal, handler, action=NULL,
                                 handlerArguments="w",
#                                 handlerExtraParameters=",'\"\"\',\'\"\"'",
                                 handlerExtraParameters=",'',''",
                                 handlerValue = NULL
                                 ) {
  lst <- list(obj = .,
              signal=signal,
              handler=handler,
              action=action,
              scope = parent.frame(),
              handlerArguments = handlerArguments, # eg "widget,evt"
              handlerExtraParameters = handlerExtraParameters, # eg ", keypress=evt.getKey()" -- with comma
              handlerValue = handlerValue                      # eg "var value = rowIndex -1" for GridPanel instances
              )

  ## we put handlers into parent widget
  ## regardless of whether we have shown the object or not
  parent <- .$toplevel
  curHandlers <- parent$jscriptHandlers
  n <- length(curHandlers)
  lst$handlerID <- n + 1
  if(n > 0) {
    parent$jscriptHandlers[[n+1]] <- lst
  } else {
    parent$jscriptHandlers <- list(lst)
  }
  
  ## add handler to list of handlers in object
  if(!is.null(signal)) {
    if(exists("..handlers", envir=., inherits = FALSE))
      curHandlers <- .$..handlers
    else
      curHandlers <- list()
    ## add handler
    curHandlers[[signal]] <- lst
    ## add back to object
    .$..handlers <- curHandlers
  }

  
  ## there are, as with other cases, two states
  ## if widget is not shown, then we
  ## a) add handler to parent so that it cna be written out.
  ## b) the actual handler code will be written out in gwindow
  ## If the widget is shown, then we need to
  ## a) store the handler in the global variable w$titlename
  ## b) write out the Javascript to
  ##    1) set the transport function (if necessary)
  ##    2) write the handler

  if(!exists("..shown", envir=., inherits = FALSE)) {
    ## all done here
  } else {
    ## need to write out the JS to show the handler
    cat(.$writeHandlerJS(signal, lst))          # a single handler
  }
    
  ## we return the ID
  return(invisible(lst$handlerID))
}

"addHandler" <- function(obj,signal, handler, action=NULL,...)
  UseMethod("addHandler")
addHandler.gWidget <- function(obj,signal,handler, action=NULL,...)
  obj$addHandler(signal, handler, action,...)


## instances
## addHandlerBlur
EXTWidget$addHandlerBlur <- function(., handler, action=NULL) {
  .$addHandler(signal="blur",handler, action)
 }

"addHandlerBlur" <- function(obj, handler, action=NULL)
  UseMethod("addHandlerBlur")
addHandlerBlur.gWidget <- function(obj,handler, action=NULL)
  obj$addHandlerBlur(handler, action)


## addHandlerChanged
 EXTWidget$addHandlerChanged <- function(., handler, action=NULL) {
   .$addHandler(signal="change",handler, action)
 }

 "addHandlerChanged" <- function(obj, handler, action=NULL)
   UseMethod("addHandlerChanged")
 addHandlerChanged.gWidget <- function(obj,handler, action=NULL)
   obj$addHandlerChanged(handler, action)

 ## addHandlerClicked
 EXTWidget$addHandlerClicked <- function(., handler, action=NULL) {
   .$addHandler(signal="click",handler, action)
 }

 "addHandlerClicked" <- function(obj, handler, action=NULL)
   UseMethod("addHandlerClicked")
 addHandlerClicked.gWidget <- function(obj,handler, action=NULL)
   obj$addHandlerClicked(handler, action)

 ## addHandlerDoubleclick
 EXTWidget$addHandlerDoubleclick <- function(., handler, action=NULL) {
   .$addHandler(signal="dblclick",handler, action)
 }

 "addHandlerDoubleclick" <- function(obj, handler, action=NULL)
   UseMethod("addHandlerDoubleclick")
 addHandlerDoubleclick.gWidget <- function(obj,handler, action=NULL)
   obj$addHandlerDoubleclick(handler, action)


 ## addHandlerMouseclick
 EXTWidget$addHandlerMouseclick <- function(., handler, action=NULL) {
   .$addHandler(signal="mousedown",handler, action)
 }

 "addHandlerMouseclick" <- function(obj, handler, action=NULL)
   UseMethod("addHandlerMouseclick")
 addHandlerMouseclick.gWidget <- function(obj,handler, action=NULL)
   obj$addHandlerMouseclick(handler, action)

## addHandlerKeystroke
## This shows how to pass in 1 argument to h using the extraparameters
## notice the excessive quoting involved for the keyword
 EXTWidget$addHandlerKeystroke <- function(., handler, action=NULL) {
   .$addHandler(signal="keyup",handler, action,
                handlerArguments="b,e",
                handlerExtraParameters = ",'key', e.getKey()"
                )
 }


 "addHandlerKeystroke" <- function(obj, handler, action=NULL)
   UseMethod("addHandlerKeystroke")
 addHandlerKeystroke.gWidget <- function(obj,handler, action=NULL)
   obj$addHandlerKeystroke(handler, action)

 ## addHandlerSelect
 EXTWidget$addHandlerSelect <- function(., handler, action=NULL) {
   .$addHandler(signal="onselect",handler, action)
 }

 "addHandlerSelect" <- function(obj, handler, action=NULL)
   UseMethod("addHandlerSelect")
 addHandlerSelect.gWidget <- function(obj,handler, action=NULL)
   obj$addHandlerSelect(handler, action)


 ## addHandlerDestroy
 EXTWidget$addHandlerDestroy <- function(., handler, action=NULL) {
   .$addHandler(signal="onunload",handler, action)
 }

 "addHandlerDestroy" <- function(obj, handler, action=NULL)
   UseMethod("addHandlerDestroy")
 addHandlerDestroy.gWidget <- function(obj,handler, action=NULL)
   obj$addHandlerDestroy(handler, action)

 ## addHandlerExposed
 EXTWidget$addHandlerExposed <- function(., handler, action=NULL) {
   .$addHandler(signal="onLoad",handler, action)
 }

 "addHandlerExposed" <- function(obj, handler, action=NULL)
   UseMethod("addHandlerExposed")
 addHandlerExposed.gWidget <- function(obj,handler, action=NULL)
   obj$addHandlerExposed(handler, action)

 ## addHandlerMouseMotion
 EXTWidget$addHandlerMouseMotion <- function(., handler, action=NULL) {
   .$addHandler(signal="onmouseover",handler, action)
 }

"addHandlerMouseMotion" <- function(obj, handler, action=NULL)
  UseMethod("addHandlerMouseMotion")
addHandlerMouseMotion.gWidget <- function(obj,handler, action=NULL)
  obj$addHandlerMouseMotion(handler, action)







