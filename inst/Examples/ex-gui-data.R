## provide


guiReadCSV <- function(w) {
  w1 <- gwindow("Input CSV data", parent = w)
  g <- ggroup(cont = w1, horizontal=FALSE)

  g1 <- ggroup(cont=g)
  glabel("Data frame name: ", cont = g1)
  varname <- gedit("x", cont = g1)
  
  glabel("Data:", cont = g)
  t <- gtext("", cont = g )
  size(t) <- c(300,150)

  g1 <- ggroup(cont = g)
  header <- gcheckbox("header", checked=TRUE, cont = g1)
  sAsF <- gcheckbox("stringsAsFactors", checked=TRUE, cont = g1)

  bg <- ggroup(cont = g)
  gbutton("dismiss", cont = bg, handler = function(...) {
    dispose(w1)
  })
  okButton <- gbutton("ok", cont = bg, handler = function(...) {
    tmp <- tempfile()
    val <- svalue(t)
    cat(val[1], file=tmp)
    if(length(val) > 1)
      for(i in 2:length(val)) cat(val[i], file=tmp, append=TRUE)
    cat("\n", file=tmp, append=TRUE)
    out <- try(read.csv(tmp, header=svalue(header), stringsAsFactors=svalue(sAsF)), silent=TRUE)
    if(inherits(out,"try-error")) {
      galert("Error reading file", parent = w1)
    } else {
      assign(svalue(varname), out, envir=.GlobalEnv)
      updateDataSelector()
      dispose(w1) 
    }
    unlink(tmp)                         # tidy up
  })
  visible(w1) <- TRUE
}

## use read.table
guiReadTable <- function(w) {
  w1 <- gwindow("Input data for reac.table", parent = w)
  g <- ggroup(cont = w1, horizontal=FALSE)

  g1 <- ggroup(cont=g)
  glabel("Data frame name: ", cont = g1)
  varname <- gedit("x", cont = g1)
  
  glabel("Data:", cont = g)
  t <- gtext("", cont = g )
  size(t) <- c(300,150)

  g1 <- ggroup(cont = g)
  header <- gcheckbox("header", checked=TRUE, cont = g1)
  sAsF <- gcheckbox("stringsAsFactors", checked=TRUE, cont = g1)

  bg <- ggroup(cont = g)
  gbutton("dismiss", cont = bg, handler = function(...) {
    dispose(w1)
  })
  okButton <- gbutton("ok", cont = bg, handler = function(...) {
    tmp <- tempfile()
    val <- svalue(t)
    cat(val[1], file=tmp)
    if(length(val) > 1)
      for(i in 2:length(val)) cat(val[i], file=tmp, append=TRUE)
    cat("\n", file=tmp, append=TRUE)
    out <- try(read.table(tmp, header=svalue(header), stringsAsFactors=svalue(sAsF)), silent=TRUE)
    if(inherits(out,"try-error")) {
      galert("Error reading file", parent = w1)
    } else {
      assign(svalue(varname), out, envir=.GlobalEnv)
      updateDataSelector()
      dispose(w1) 
    }
    unlink(tmp)                         # tidy up
  })
  visible(w1) <- TRUE

}

## use scan()
guiScan <- function(w, varname) {


}


loadDataSet <- function(w) {
  ## load a built in data set
  allDataSets <- data(package = .packages(all.available = TRUE))
  df <- allDataSets$results[,c(3,1)]
  df <- as.data.frame(df, stringsAsFactors=FALSE)
  
  w1 <- gwindow("Double click to load data set", parent = w, width=300)
  g <- ggroup(horizontal=FALSE, cont = w1)
  tbl <- gtable(df, cont = g, handler = function(h,...) {
    tmp <- svalue(h$obj, drop=FALSE)
    data(list=tmp[[1]], package=tmp[[2]])
    svalue(sb) <- paste("Loaded", tmp[[1]])
    updateDataSelector()
  })
  size(tbl) <- c(300,400)
  gbutton("dismiss", cont = g, handler = function(h,...) {
    dispose(w1) 
  })
  sb <- gstatusbar("", cont = w1)
  visible(w1) <- TRUE
}



editDataSet <- function(w) {
  ds <- e$currentData
  if(ds == ".GlobalEnv")
    return()
  
  dataset <- try(get(ds), silent=TRUE)
  if(inherits(dataset, "try-error")) {
    gmessage(paste("Can't edit this data set", ds, sep=" "), parent = w)
    return()
  }
  
  w2 <- gwindow("Edit current data set", parent=w)
  g <- ggroup(cont = w2, horizontal=FALSE)
  glabel(ds, cont = g)
  df <- gdf(dataset, cont = g)
  size(df) <- c(500,200)
  bg <- ggroup(cont = g)
  gbutton("dismiss", cont = bg, handler = function(h,...) dispose(w2))
  gbutton("save", cont = bg, handler = function(h,...) {
    assign(ds, df[,], envir=.GlobalEnv)
    dispose(w2)
  })
  visible(w2) <- TRUE
}
