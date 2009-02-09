## show ghtml widget

guiAbout <- function(w) {
  w1 <- gwindow("About gWidgetsWWW GUI", parent = w)
  g <- ggroup(cont = w1, horizontal=FALSE)

  ghtml(paste("<h2>Simple GUI for gWidgetsWWW</h2>",
              "To use GUI:",
              "<UL>",
              "<LI>Load a data set using the Data menu",
              "<LI>Select the current data set, as desired",
              "<LI>Use the Tests of Plots menu to perform your analysis",
              "</UL>",
              sep = " "),
        cont = g)
  gseparator(cont = g)
  gbutton("dismiss", cont = g, handler = function(h,...) {
    dispose(w1)
  })

  visible(w1) <- TRUE
}
