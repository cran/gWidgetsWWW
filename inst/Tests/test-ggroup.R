w <- gwindow("Tests for ggroup, gframe, gexpandgroup")
g <- ggroup(cont=w, horizontal=FALSE)

## horizontal
g1 <- gframe("horizontal", cont=g, horizontal=TRUE)
for(i in 1:5) gbutton(state.name[i], cont=g1)


## vertical
g1 <- gframe("horizontal=FALSE", cont=g, horizontal=FALSE)
for(i in 1:5) gbutton(state.name[i], cont=g1)

## nested
g1 <- gframe("Nested containers", cont=g)
g2 <- ggroup(cont=g1, horizontal=TRUE)
gbutton("left right", cont=g2)
g3 <- ggroup(cont=g2, spacing=0, horizontal=FALSE)
gbutton("top", cont=g3)
gbutton("bottom", cont=g3)


## add/subtract
g2 <- gframe("Swap out", cont=g, horizontal=FALSE)
ig <- ggroup(cont=g2);  glabel(as.character(rnorm(1)), cont=ig)
changeLabel <- function(...) {
  delete(g2, ig)
  ig <<- ggroup(cont=g2)
  glabel(as.character(rnorm(1)), cont=ig)
}
gbutton("swap", cont=g, handler=changeLabel)




## test with other widget
g3 <- gframe("Share the road with tables", cont=g, horizontal=TRUE)

tbl <- gtable(mtcars[1:3, 1:4], cont=g3)
size(tbl) <- c(400,200)

tbl <- gtable(mtcars[1:3, 1:4], cont=g3)
size(tbl) <- c(400,200)

g4 <- ggroup(cont=g3, horizontal=FALSE)
for(i in 1:5) gbutton(state.name[i], cont=g4)



## set up debug
old <- getOption("gWidgetsWWW_debug")
options(gWidgetsWWW_debug=TRUE)
on.exit(options(gWidgetsWWW_debug=old))
        
g4 <- gframe("Issue with groups expanding to right", cont=g, horizontal=FALSE)

glabel("Fails, no size", cont=g4)
g <- ggroup(cont=g4, horizontal=TRUE)
lg <- ggroup(cont=g); gbutton("one", cont=lg);gbutton("two", cont=lg)
rg <- ggroup(cont=g); gbutton("three", cont=rg)

glabel("Works, set size", cont=g4)
g <- ggroup(cont=g4, horizontal=TRUE)
lg <- ggroup(cont=g); gbutton("one", cont=lg);gbutton("two", cont=lg)
size(lg) <- c(200)
rg <- ggroup(cont=g); gbutton("three", cont=rg)



sb <- gstatusbar("Tests for ggroup, gframe, gexpandgroup", cont=w)
visible(w) <- TRUE
