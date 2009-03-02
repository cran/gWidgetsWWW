w <- gwindow("Windows example")
g <- ggroup(cont = w, horizontal = FALSE)


g1 <- gexpandgroup("Modal dialogs. Handlers for gconfirm, ginput don't really work!!!",cont=g)

b1 <- gbutton("galert", cont=g1, handler = function(h,...) {
  galert("for quick transient messages", title="galert dialog")
})


b2 <- gbutton("gmessage", cont=g1, handler = function(h,...) {
  ## parent needed -- dialog animation comes from parent.
  gmessage("hi there", parent = b2)
})

b3 <- gbutton("gconfirm", cont=g1, handler = function(h,...) {
  gconfirm("Unfortunately the handler doesn't work without an intrusive hack.", parent = b3,
           handler = function(h,...) {
             gmessage("you clicked ok", parent = b3)
           })
})

b4 <- gbutton("ginput", cont=g1, handler = function(h,...) {
  ginput("Unfortunately the handler doesn't work without an intrusive hack.",
         parent = b4, handler = function(h,...) {
           ##   cat("alert('hack to make handler work');")
           gmessage(String("you clicked ok") + h$input, parent = b4)
         })
})


g1 <- gexpandgroup("A subwindow allowing interactivity",cont=g)


b5 <- gbutton("gwindow" ,cont=g1, handler = function(h,...) {
  w1 <- gwindow("subwindow", parent=w)
  g1 <- ggroup(cont = w1, horizontal=FALSE)
  ## need the gimage code first!!!
  glabel("subwindow text", cont = g1)
  gimage("ex-graph.png", cont=g1)
  gseparator(cont = g1)
  gbutton("dismiss", cont = g1, handler = function(h,...) dispose(w1))
  visible(w1) <- TRUE #show
})

##visible(w) <- TRUE
