

makePlot <- function(fileName, width, height) {
  require(Cairo, quietly=TRUE, warn=FALSE)
  require(lattice, quietly=TRUE, warn=FALSE)
  Cairo(file=fileName, width=width, height=height)
  print(histogram(rnorm(100)))
  dev.off()
}

fileName <- getStaticTmpFile(ext=".png")
width <- 500; height <- 300


w <- gwindow("Using Cairo plus gimage")
g <- ggroup(horizontal = FALSE, cont = w)
b <- gbutton("Make new plot", cont = g, handler = function(h,...) {
  if(file.exists(fileName))
    unlink(fileName)
  fileName <<- getStaticTmpFile(ext="png")
  makePlot(fileName, width,height)
  svalue(img) <- convertStaticFileToUrl(fileName)
})

makePlot(fileName, width, height)
img <- gimage(convertStaticFileToUrl(fileName), cont = g)

gstatusbar("Powered by RApache and gWidgetsWWW", cont = w)
visible(w) <- TRUE
