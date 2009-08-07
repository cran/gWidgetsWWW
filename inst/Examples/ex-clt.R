f <- tempfile()

w <- gwindow("CLT example")
g <- ggroup(horizontal=FALSE, cont=w)
l <- glabel("", cont = g)
p <- gcanvas(width=500, height=400, cont = g)

tbl <- glayout(cont = g)
tbl[1,1] <- "n (sample size)"
tbl[1,2] <- (sampleSize <- gedit("10", cont = tbl, coerce.with="as.numeric"))

tbl[2,1] <- "Population:"
tbl[2,2] <- (population <- gcombobox(c("rnorm(n,0,1)","rexp(n)", "rt(n, df=3)",
                                       "rt(n, df=30)", "runif(n)","rlnorm(n)"),
                                     editable=TRUE, cont=tbl))
tbl[3,1] <- "Replicates"
tbl[3,2] <- (replicates <- gslider(from=10, to=200, by=1, value=50, cont=tbl))

makePlot <- function(h,...) {
  require(canvas, quietly=TRUE, warn=FALSE)
  n <- max(as.numeric(svalue(sampleSize)), 500)
  m <- max(as.numeric(svalue(replicates)), 500)
  cmd <- svalue(population)
  ## try cmd
  out <- try(as.numeric(eval(parse(text=cmd))), silent=TRUE)
  if(inherits(out,"try-error")) {
    galert(paste("Error with", cmd, sep=" "), parent=w)
    return()
  }
  x <- replicate(m, eval(parse(text=cmd)))
  xbars <- apply(x, 2, function(x) mean(x))
  canvas(f, width=500, height=400, bg="#ffffff")
  plot(density(xbars))
  rug(xbars)
  dev.off()
  svalue(p) <- f
  rm(c("m","x"))                        # clear out data
}
makePlot(1)

g1 <- ggroup(cont = g)
b <- gbutton("plot", cont = g1, handler=makePlot)
helpButton <- gbutton("help", cont = g1, handler=function(h,...) {
  w1 <- gwindow("Help with CLT demo", parent=w)
  g <- ggroup(cont = w1, horizontal=FALSE)
  ghtml(paste("<h1>CLT Example</h1>",
              "Demonstrates the Central Limit Theorem, which says the CDF of the sample mean",
              "of a random sample",
              "of <i>n</i> data points converges to that of the normal distribution as",
              "<i>n</i> goes to infinity. This shows a density estimate, so it is not quite the same for discrete",
              "populations, but still illustrates the point.",
              "<p>",
              "<UL>",
              "<li>Set the <b>sample size</b> by adjusting the value of <i>n</i>.</li>",
              "<li>Set the <b>population</b> from one of the set ones, or enter",
              "an R command that produces a sample from the population.",
              "The value <i>n</i>can be used as a parameter.</li>",
              "<li>Adjust the number of <b>replicates</b> if desired.</li>",
              "</UL>",
              "Click the <b>plot</b> button to update the plot",
              sep=" "), cont = g, expand=TRUE)
  gseparator(cont = g)
  gbutton("dismiss", cont = g, handler=function(h,...) {
    dispose(w1)
  })
  visible(w1) <- TRUE
})

gstatusbar("Powered by RApache and gWidgetsWWW", cont = w)
visible(w) <- TRUE