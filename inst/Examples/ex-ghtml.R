w <- gwindow("Test of ghtml widget")

g1 <- ggroup(cont=w, horizontal=FALSE)

g <- gexpandgroup("Using a url", cont=g1, horizontal=FALSE)
glabel("URL's are defined by a leading http:// *or* local references can be wrapped in the asURL() function", cont=g)
urlText <- ghtml(asURL("ex-ghtml.txt"), cont=g)



g <- gexpandgroup("Using markup: within HTML script", cont=g1, horizontal=FALSE)
glabel("Markup can be used here", cont=g)
x <- "<b>bold text</b>"
markupText <- ghtml(x, cont=g)

g <- gexpandgroup("Using Rpad utils", cont=g1)
glabel("Rpad has some HTML formatting utilities", cont=g)
x <- Html(head(iris))
RpadText <- ghtml(x, cont=g)

g <- gexpandgroup("Using xtable, say", cont=g1)
glabel("xtable can be used to produce the markup. Use a file though", cont=g)
library(MASS); library(xtable);
file <- tempfile()
print(xtable(xtabs(~ Origin + Cylinders,Cars93)),file=file,type="html")
x <- paste(readLines(file), collapse="")
unlink(file)
xtableText <- ghtml(x, cont=g)


visible(w) <- TRUE


