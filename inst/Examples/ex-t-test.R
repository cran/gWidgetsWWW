## Build a t-test gui
require(Cairo) ## no X11 dependence

w <- gwindow("t-test example")
g <- ggroup(cont = w, horizontal = FALSE)
data(Cars93, package="MASS")


dataSets <- c("mtcars","Cars93")
alternatives <- data.frame(value=c("two.sided","less","greater"),
                           label = c("two sided", "less than", "greater than"))

tbl <- glayout(cont=g)
tbl[1,1] <- glabel("Selecte a data frame:", cont=tbl)
tbl[1,2] <- (selData <- gcombobox(dataSets, selected=0, cont=tbl, editable=TRUE))

tbl[2,1] <- glabel("Select a variable", cont=tbl)
tbl[2,2] <- (selVariable <- gcombobox(c("",""),cont = tbl, editable = TRUE))

tbl[3,1] <- glabel("mu:", cont=tbl)
tbl[3,2] <- (selMu <- gedit(0, cont=tbl))

tbl[4,1] <- glabel("alternative:", cont=tbl)
tbl[4,2] <- (selAlt <- gcombobox(alternatives, cont=tbl))

## doesn't work
tbl[5,2] <- (buttonGroup <- ggroup(cont=tbl))

doButton <- gbutton("run t-test", cont=buttonGroup)
doGraph <- gbutton("EDA plot", cont=buttonGroup)

outputArea <- gtext("", cont = g); size(outputArea) <- c(width=500,height=200)


## blank out until ready
enabled(selVariable) <- FALSE
enabled(selMu) <- FALSE
enabled(selAlt) <- FALSE
enabled(buttonGroup) <- FALSE

## handlers
addHandlerChanged(selData, handler = function(h,...) {
  enabled(selVariable) <- TRUE
  df <- svalue(selData)
  df <- get(df, envir=.GlobalEnv)                # string to object
  selVariable[] <- data.frame(names(df),names(df))
})

addHandlerChanged(selVariable, handler = function(h,...) {
  enabled(selMu) <- TRUE
  enabled(selAlt) <- TRUE
  enabled(buttonGroup) <- TRUE
})
                  
addHandlerClicked(doButton, handler = function(h,...) {
  df <- svalue(selData)
  var <- svalue(selVariable)
  mu <- svalue(selMu)
  alt <- svalue(selAlt)
  
  out <- String() +
    "with(" + df + ', t.test(' + var +
      ', mu = ' + mu +
        ', alt = ' + shQuote(alt) +
          '))'

  val <- capture.output(eval(parse(text=out),envir=.GlobalEnv))
  svalue(outputArea) <- paste(val,collapse="\\\\n") # lots of escapes
})

addHandlerClicked(doGraph, handler = function(h,...) {
  df <- svalue(selData)
  var <- svalue(selVariable)
  tmp <- tempfile(tmpdir=""); tmp <- gsub("^/","",tmp)

  Cairo(file=paste(basedir,tmp,sep=""), width=400, height=400)
  eval(parse(text=String() + "with(" + df + ', hist(' + var +  '));'), envir=.GlobalEnv)
  dev.off()
  dev.off()
  
  w1 <- gwindow("EDA", parent = w, width=410, height=500)
  g1 <- ggroup(horizontal=FALSE, cont=w1, use.scrollwindow=TRUE)
  gimage(tmp, cont=g1)
  gseparator(cont = g1)
  gbutton("dismiss", cont=g1, handler = function(h,...) dispose(w1))
  ## show page
  visible(w1) <- TRUE
})
  
## show w
##visible(w) <- TRUE
                  
