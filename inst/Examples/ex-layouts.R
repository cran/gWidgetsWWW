w <- gwindow("Test of different layouts")

## ggroup
## ggroup horizontal works -- if -- we specify size for frames, ...
g <- ggroup(cont=w, horizontal=FALSE)

l <- glabel("Test of layouts: ggroup, gframe, gexpandgroup, gnotebook and glayout", cont=g)
## horizontal group
f <- ggroup(cont=g, horizontal = TRUE, width=width)
b <- gbutton("buttonhg", cont=f)
addHandlerClicked(b, handler = function(h,...) gmessage("clicked",parent=b))
b1 <- gbutton("button one hg", cont=f)


## frame
f1 <- gframe("title",cont=g, horizontal=TRUE, width=width)
l <- glabel("buttong", cont=f1)
font(l) <- c("font-weight"="bold")
b1 <- gbutton("button one g", cont=f1)

## gexpandgroup
f <- gexpandgroup("title",cont=g, horizontal=TRUE, width=width)
b <- gbutton("button exp", cont=f)
b1 <- gbutton("button one exp", cont=f)

## gnotebook
f <- gnotebook(cont=g,horizontal=TRUE)
size(f) <- c(width,200)
gbutton("button", cont = f, label = "tab 1")
gbutton("Another button", cont = f, label = "tab 2")
## tooltips when tabpos not on bottom (=1)
gbutton("button with tooltip", cont=f, label="one",tooltip="hover")


## glayout
f <- glayout(cont = g)
f[1,1] <- (tb = gbutton("hi", cont=f))
f[1,2:3,anchor=c(1,0)] <- (tl = glabel("label",cont=f))
## set fonts
font(tl) <- c("weight" = "bold","background-color"="red","color"="blue")
f[2,2] <- glabel("missing 1,3 in this row", cont=f)
addHandlerClicked(tb,handler=function(h,...) svalue(tl) = "new label")

visible(w) <- TRUE
