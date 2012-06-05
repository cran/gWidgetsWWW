##  Copyright (C) 2010 John Verzani
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; either version 2 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  A copy of the GNU General Public License is available at
##  http://www.r-project.org/Licenses/

## Simple interface to ace editor

gace <- function(x, container, ...) {
  
  widget <- EXTComponent$new(toplevel=container$toplevel,
                             ..title = title
                             )
  class(widget) <- c("gAce",class(widget))

  
  if(!missing(x))
    widget$setValue(value = x)

   widget$scripts <- function(.) {
     f <- system.file("javascript","GMapPanel.js", package="gWidgetsWWW")
     out <- paste(readLines(f), collapse="\n")
     
     return(out)
   }

  widget$ExtConstructor <- "Ext.Panel"

  widget$ExtCfgOptions <- function(.) {
    out <- list()
                  
    return(out)
  }

  widget$setValueJS <- function(., ...) {
    value <- svalue(.)
    
  }

  
  container$add(widget, ...)
  invisible(widget)

}
