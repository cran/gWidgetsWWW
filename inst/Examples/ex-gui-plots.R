tempFile <- function(ext="") {
  ## just increment
  ctr <- e$fileCtr
  e$fileCtr <- e$fileCtr + 1
  paste("tmp/file", ctr, ".",ext, sep="")
}

showLattice <- function(lat,w, title="") {
  w1 <- gwindow(paste("Plot:  ",title,sep=" "), parent = w, width=600, height = 600)
  g <- ggroup(cont = w1, horizontal=FALSE)

  file <- tempFile("png")
  png(file = file, width = 550, height = 500)
  print(lat)
  dev.off()

  im <- gimage(file, cont = g)
  gseparator(cont = g)
  gbutton("dismiss", cont = g, handler = function(h,...) dispose(w1))
  visible(w1) <- TRUE
  
}

## make a histogram
plotHist <- function(w) {
  width <- 500
  histogramPlot <- list(type = "ggroup",
                     horizontal = FALSE,
                     children = list(
                       list(type="fieldset",
                            columns = 2,
                            height = 125,
                            width =  width,
                            label = "Variable(s)",
                            label.pos = "top",
                            label.font = c(weight="bold"),
                            children = list(
                              list(name = "x",
                                   label = "~ x",
                                   type = "gcombobox",
                                   editable=TRUE,                              
                                   items = call("listVariableNames")
                                   ),
                              list(name = "f",
                                   label = "| f",
                                   type = "gcombobox",
                                   editable=TRUE,
                                   items = call("listVariableNames"),
                                   depends.on = "x",
                                   depends.FUN = function(value) nchar(value) > 0,
                                   depends.signal = "addHandlerChanged"
                                   )
                              )
                            ),
                       list(type = "fieldset",
                            label = "Arguments",
                            width = width,
                            height = 100,
                            columns = 2,
                            children = list(
                              list(name = "type",
                                   type = "gcombobox",                            
                                   label = "type",
                                   items= c("percent", "count", "density")
                                   )
                              )
                            )
                       )
                     )
  
  w1 <- gwindow("Histogram", parent = w, width=width + 50, height=400)
  g <- ggroup(cont = w1, horiz = FALSE)
  
  f <- gformlayout(histogramPlot, cont = g)
  gseparator(cont = g)
  bg <- ggroup(cont = g)
  gbutton("dismiss", cont = bg, handler = function(h,...) dispose(w1))
  gbutton("ok", cont = bg, handler = function(h,...) {
    ## handler will produce a string for eval parse
    ## all values
    out <- svalue(f)

    
    ## get main variable
    cmd <- paste("~ ", out$x, sep="")
    if(out$f != "") {
      cmd <- paste(cmd, out$f, sep=" | ")
    }
    ## data argument for formula
    df <- e$currentData
    if(df != "" || df != ".GlobalEnv")
      out$data <- df
    
    out$x <- out$f <- NULL

    ## fix characters that need quoting
    out$type <- paste("\"",out$type,"\"", sep="")

    ## put arguments together
    if(length(out)) {
      nms <- names(out)
      args <- paste(nms, out, sep="=", collapse=", ")
      cmd <- paste(cmd, ", " , args, sep = "")
    }
    cmd <- paste("histogram(", cmd, ")", sep="")

    out <- try(eval(parse(text=cmd),envir=.GlobalEnv), silent=TRUE)
    if(inherits(out,"try-error")) {
      galert(paste("Error", paste(out, collapse=" "), sep=" "), parent=w)
    } else {
      dispose(w1)
      showLattice(out, w, title = cmd)
    }
  })
  visible(w1) <- TRUE
}
        
plotBoxplot <- function(w) {
  width <- 500
  boxplotPlot <- list(type = "ggroup",
                     horizontal = FALSE,
                     children = list(
                       list(type="fieldset",
                            columns = 2,
                            height = 125,
                            width =  width,
                            label = "Variable(s)",
                            label.pos = "top",
                            label.font = c(weight="bold"),
                            children = list(
                              list(name = "x",
                                   label = "~ x",
                                   type = "gcombobox",
                                   editable=TRUE,                              
                                   items = call("listVariableNames")
                                   ),
                              list(name = "f",
                                   label = "| f",
                                   type = "gcombobox",
                                   editable=TRUE,
                                   items = call("listVariableNames"),
                                   depends.on = "x",
                                   depends.FUN = function(value) nchar(value) > 0,
                                   depends.signal = "addHandlerChanged"
                                   )
                              )
                            ),
                       list(type = "fieldset",
                            label = "Arguments",
                            width = width,
                            height = 100,
                            columns = 2,
                            children = list(
                              list(name = "type",
                                   type = "gcombobox",                            
                                   label = "type",
                                   items= c("percent", "count", "density")
                                   )
                              )
                            )
                       )
                     )
  
  w1 <- gwindow("Boxplot", parent = w, width=width + 50, height=400)
  g <- ggroup(cont = w1, horiz = FALSE)
  
  f <- gformlayout(boxplotPlot, cont = g)
  gseparator(cont = g)
  bg <- ggroup(cont = g)
  gbutton("dismiss", cont = bg, handler = function(h,...) dispose(w1))
  gbutton("ok", cont = bg, handler = function(h,...) {
    ## handler will produce a string for eval parse
    ## all values
    out <- svalue(f)

    
    ## get main variable
    cmd <- paste("~ ", out$x, sep="")
    if(out$f != "") {
      cmd <- paste(cmd, out$f, sep=" | ")
    }
    ## data argument for formula
    df <- e$currentData
    if(df != "" || df != ".GlobalEnv")
      out$data <- df
    out$x <- out$f <- NULL

    ## fix characters that need quoting
    out$type <- paste("\"",out$type,"\"", sep="")

    ## put arguments together
    if(length(out)) {
      nms <- names(out)
      args <- paste(nms, out, sep="=", collapse=", ")
      cmd <- paste(cmd, ", " , args, sep = "")
    }
    cmd <- paste("bwplot", "(", cmd, ")", sep="")

    out <- try(eval(parse(text=cmd),envir=.GlobalEnv), silent=TRUE)
    if(inherits(out,"try-error")) {
      galert(paste("Error", paste(out, collapse=" "), sep=" "), parent=w)
    } else {
      dispose(w1)
      showLattice(out, w, title = cmd)
    }
  })
  visible(w1) <- TRUE
}
        
plotXYplot <- function(w) {
  width <- 750

  xyPlot <- list(type = "ggroup",
                     horizontal = FALSE,
                     children = list(
                       list(type="fieldset",
                            columns = 3,
                            height = 125,
                            width =  width,
                            label = "Variable(s)",
                            label.pos = "top",
                            label.font = c(weight="bold"),
                            children = list(
                              list(name = "y",
                                   label = "y",
                                   type = "gcombobox",
                                   editable=TRUE,                              
                                   items = call("listVariableNames")
                                   ),
                              list(name = "x",
                                   label = "~ x",
                                   type = "gcombobox",
                                   editable=TRUE,                              
                                   items = call("listVariableNames")
                                   ),
                              list(name = "f",
                                   label = "| f",
                                   type = "gcombobox",
                                   editable=TRUE,
                                   items = call("listVariableNames"),
                                   depends.on = "x",
                                   depends.FUN = function(value) nchar(value) > 0,
                                   depends.signal = "addHandlerChanged"
                                   )
                              )
                            ),
                       list(type = "fieldset",
                            label = "Arguments",
                            width = width,
                            height = 100,
                            columns = 2,
                            children = list(
                              list(name = "aspect",
                                   type = "gcombobox",                            
                                   label = "aspect",
                                   items= c("fill", "xy", "iso")
                                   ),
                              list(name = "auto.key",
                                   label = "auto.key",
                                   depends.on = "f",
                                   depends.FUN = function(value) nchar(value) > 0,
                                   depends.signal = "addHandlerChanged",
                                   type = "gcheckbox",
                                   text = "auto key",
                                   checked = FALSE
                                   )
                              )
                            )
                       )
                 )
  
  w1 <- gwindow("Scatterplot", parent = w, width=width + 50, height=400)
  g <- ggroup(cont = w1, horiz = FALSE)
  
  f <- gformlayout(xyPlot, cont = g)
  gseparator(cont = g)
  bg <- ggroup(cont = g)
  gbutton("dismiss", cont = bg, handler = function(h,...) dispose(w1))
  gbutton("ok", cont = bg, handler = function(h,...) {
    ## handler will produce a string for eval parse
    ## all values
    out <- svalue(f)

    
    ## get main variable
    cmd <- paste(out$y, "~ ", out$x, sep="")
    if(out$f != "") {
      cmd <- paste(cmd, out$f, sep=" | ")
    }
    out$x <- out$y <- out$f <- NULL
    ## data argument for formula
    df <- e$currentData
    if(df != "" || df != ".GlobalEnv")
      out$data <- df

    ## fix characters that need quoting
    out$aspect <- paste("\"",out$aspect,"\"", sep="")
    
    ## put arguments together
    if(length(out)) {
      nms <- names(out)
      args <- paste(nms, out, sep="=", collapse=", ")
      cmd <- paste(cmd, ", " , args, sep = "")
    }
    cmd <- paste("xyplot", "(", cmd, ")", sep="")

    out <- try(eval(parse(text=cmd),envir=.GlobalEnv), silent=TRUE)
    if(inherits(out,"try-error")) {
      galert(paste("Error", paste(out, collapse=" "), sep=" "), parent=w)
    } else {
      dispose(w1)
      showLattice(out, w, title = cmd)
    }
  })
  visible(w1) <- TRUE
}
        
