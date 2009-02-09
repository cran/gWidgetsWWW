## Sample GUI using gWidgetsWWW for students, say, to do simpe
## exploring
require(lattice)

w <- gwindow("Simple GUI with gWidgetsWWW")

## these depend on having global w defined
source("ex-gui-helpers.R")
source("ex-gui-variables.R")
source("ex-gui-ttest.R")
source("ex-gui-data.R")
source("ex-gui-plots.R")
source("ex-gui-help.R")


e <- proto()                            # globals go here
e$currentData <- ".GlobalEnv"             # data frame with current
e$formulaList <- list()
e$fileCtr <- 1                           # which image
e$tmp <- tempfile()

## Actions

loadDataSetAction <- gaction("Load built-in data set...", parent=w, handler = function(...) loadDataSet(w))
editDataSetAction <- gaction("Edit current data set...", parent=w, handler = function(...) editDataSet(w))
readCSVAction <- gaction("Read CSV data...", parent=w, handler = function(...) guiReadCSV(w))
readTableAction <- gaction("Read table data...", parent=w, handler = function(...) guiReadTable(w))

ttestAction <- gaction("t-test", parent=w, handler= function(...)  tTest(w))


plotHistAction <- gaction("Plot histogram...", parent=w, handler = function(...) plotHist(w))
plotBoxAction <- gaction("Plot boxplot...", parent=w, handler = function(...) plotBoxplot(w))
plotXYAction <- gaction("Plot scatterplot...", parent=w, handler = function(...) plotXYplot(w))

aboutAction<- gaction("About...", parent=w, handler = function(...) guiAbout(w))

## menuList
menuList <- list()
menuList$Data <- list(loadData=loadDataSetAction,
                      editData=editDataSetAction,
                      readCSV=readCSVAction,
                      readTable=readTableAction
                      )

menuList$Tests <- list(ttest=ttestAction
                       )

menuList$Plots <- list(hist = plotHistAction,
                       bwp = plotBoxAction,
                       xy = plotXYAction
                       )

menuList$Help <- list(about = aboutAction)

gmenu(menuList, cont = w)
g <- ggroup(cont = w, horizontal=FALSE)

## command line, data set
tbl <- glayout(cont = g)
## data
tbl[1,1] <- glabel("Current data set:", cont = tbl)
tbl[1,2] <- (e$dataSetSelector <- gcombobox(listDataFrames(), editable=TRUE, cont = tbl,
                               handler = function(h,...) {
                                 e$currentData <- svalue(h$obj)
                               })
             )
tbl[1,3] <- gbutton(action=editDataSetAction, cont = tbl)

updateDataSelector <- function() {
  tmp <- e$dataSetSelector
  tmp[] <- listDataFrames()
}

## command line
tbl[2,1] <- glabel("Command:", cont = tbl)
tbl[2,2:3] <- (commandLine <- gedit("", width=100, cont = tbl))
addHandlerKeystroke(commandLine,  handler = function(h,...) {
  if(h$key == 13) {
    val <- svalue(commandLine)
    out <- try(capture.output(eval(parse(text=val), envir=.GlobalEnv)), silent=TRUE)
    if(!inherits(out, "try-error")) {
      svalue(outArea) <- out
      updateDataSelector()
    } else {
      galert("Error with command", parent = w)
    }
  }
})
size(commandLine) <- c(590,-1)

## command output
g1 <- ggroup(cont = g)
g2 <- gframe("Output:", cont = g1, width=720, height=400)
outArea <- gtext("", cont = g2, width=700, height=400)


visible(w) <- TRUE
