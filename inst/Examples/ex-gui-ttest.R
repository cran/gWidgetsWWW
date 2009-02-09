## sample GUI using gformlayou
list2levelFactors <- function() {
  listVariableNames(cond = function(x) {
    y <- factor(x)
    length(levels(y)) == 2
  })
}

tTest <- function(w) {

                      
    
  tTest <- list(type = "ggroup",
                horizontal = FALSE,
                children = list(
                  list(type="fieldset",
                       columns = 2,
                       height = 125,
                       width =  600,
                       label = "Variable(s)",
                       label.pos = "top",
                       label.font = c(weight="bold"),
                       children = list(
                         list(name = "x",
                              label = "x",
                              type = "gcombobox",
                              editable=TRUE,                              
                              items = call("listVariableNames")
                              ),
                         list(name = "f",
                              label = "~ f",
                              type = "gcombobox",
                              editable=TRUE,
                              items = call("list2levelFactors"),
                              depends.on = "x",
                              depends.FUN = function(value) nchar(value) > 0,
                              depends.signal = "addHandlerChanged"
                              )
                         )
                       ),
                  list(type = "fieldset",
                       label = "Arguments",
                       width = 600,
                       height = 200,
                       columns = 2, 
                       children = list(
                          list(name = "mu",
                               type = "gedit",                            
                               label = "Ho: mu=",
                               text = "0",
                               coerce.with = "as.numeric"),
                          list(name = "alternative",
                               type="gcombobox",
                               label = "HA: ",
                               items = c("two.sided","less","greater")
                               ),
                         list(
#                              depends.on = "f",
#                              depends.FUN = function(value) nchar(value) > 0,
#                              depends.signal = "addHandlerChanged",                     
                              name = "paired",
                              label = "paired samples",
                              type = "gcombobox",
                              items = c(FALSE, TRUE)
                              ),
                         list(
                              depends.on = "f",
                              depends.FUN = function(value) nchar(value) > 0,
                              depends.signal = "addHandlerChanged",                     
                              name = "var.equal",
                              label = "assume equal var",
                              type = "gcombobox",
                              items = c(FALSE, TRUE)
                              ),
                         list(name = "conf.level",
                              label = "confidence level",
                              type = "gedit",
                              text = "0.95"
                              )
                         )
                       )
                  )
                )

  w1 <- gwindow("t-test", parent = w, width=650, height=450)
  g <- ggroup(cont = w1, horizontal=FALSE)
  f <- gformlayout(tTest, cont = g)
  gseparator(cont = g)
  bg <- ggroup(cont = g)
  gbutton("dismiss", cont = bg, handler = function(h,...) dispose(w1))
  gbutton("ok", cont = bg, handler = function(h,...) {
    ## handler will produce a string for eval parse

    ## all values
    out <- svalue(f)

    ## get main variable
    cmd <- out$x
    if(out$f != "") {
      cmd <- paste(cmd,out$f, sep="~")
      ## data argument for formula
      df <- e$currentData
      if(df != "" || df != ".GlobalEnv")
        out$data <- df
    }
    out$x <- out$f <- NULL

    ## fix characters that need quoting
    out$alternative <- paste("\"",out$alternative,"\"", sep="")

    ## put arguments together
    if(length(out)) {
      nms <- names(out)
      args <- paste(nms, out, sep="=", collapse=", ")
      cmd <- paste(cmd, "," , args, sep = "")
    }
    cmd <- paste("t.test(", cmd, ")", sep="")

    out <- try(capture.output(eval(parse(text=cmd),envir=.GlobalEnv)), silent=TRUE)
    if(inherits(out,"try-error")) {
      galert(paste("Error", paste(out, collapse=" "), sep=" "), parent=w)
    } else {
      svalue(outArea) <- out
      dispose(w1)
    }
  })
  
  visible(w1) <- TRUE
}
  
