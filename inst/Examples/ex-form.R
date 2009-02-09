w <- gwindow("An example form")
g <- ggroup(horiz=FALSE, cont = w)

lst <- list(type = "ggroup",
            horizontal=FALSE,
            children = list(
              list(type="fieldset",
                   label = "argument",
                   width = 500,
                   children=list(
                     list(type="gcombobox",
                          label = "combo",
                          items=letters),
                     list(type = "gslider",
                          label="slider"),
                     list(type = "gedit",
                          label = "edit",
                          text = "edit this",
                          width=200)
                     )
                   )
              )
            )


f <- gformlayout(lst, cont = g)

bg <- ggroup(cont = g)
addSpring(bg)
gbutton("ok", cont = bg, handler = function(h,...) {
  vals <- svalue(f)
})

## show top level
visible(w) <- TRUE

