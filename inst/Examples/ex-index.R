w <- gwindow("gWidgetsWWW")
g <- ggroup(cont = w, horizontal=FALSE)
ghtml("<h1>gWidgetsWWW</h1>", cont = g)

width <- 800

f <- gframe("Introduction", cont = g, width=width)

str <- paste(
"This package provides an implementation of the gWidgets API for use on",
"the web.",
"<p></p>",
"This allows the R programmer to easily create dynamic web pages within R itself without the need to understand HTML, ajax, cgi, or other bits of internet programming.",
"<p></p>",
"It uses the <A href=www.rpad.org>Rpad</A> package for R and the",
"freely available <A href=www.extjs.com>EXTJS Libraries</A>",
"for javascript.",
"<P></P>",
"Rpad can be installed in local mode or server mode.",
"The latter allows other users access to the GUIs.",
sep=" ")
ghtml(str, cont = f)



f <- gframe("Examples", cont = g, width=width)

str <- paste(
"Several examples are available here. These files also sit in the",
"Examples directory after the package is installed within R.",
"",
"<UL>",
"  <LI>A <A href=ex-gui.html>simple GUI</A> which could easily be extended  </LI>",
"  <LI>A demonstration <A href=ex-widgets.html>of the widgets</A></LI>",
"  <LI>A demo of <A href=ex-windows.html>windows</A> widgets</LI>",
"  <LI>A demo of <A href=ex-layouts.html>container</A> widgets</LI>",
"  <LI>A demo of <A href=ex-gcombobox.html>the combobox</A> widget</LI>",
"  <LI>A demo <A href=ex-form.html>simple form</A> using gfromlayout example  </LI>",
" <LI> A demo of <A href=ex-gprocessingjs.html>gprocessingjs</A> for javascript-based graphics.</LI>",
" <LI> A demo of <A href=ex-image.html>gimage</A> based graphics.</LI>",
             " <LI> A demo showing how to edit a  <A href=ex-editdata.html>data set</A>.</LI>",             
"  <LI>A  <A href=ex-t-test.html>t-test form</A> example  </LI>",
"  <LI>An illustration of <A href=ex-actions.html>action objects</A></LI>",
"  <LI>An illustration of <A href=ex-buttons.html>button icons</A></LI>",
"  <LI>An illustration of using <A href=ex-ggooglemaps.html>Google maps</A>(not working except local version)</LI>",
"</UL>",
sep=" ")
ghtml(str, cont = f)

             
f <- gframe("Documentation", cont = g, width=width)

str <- paste(
"The main source for documentation is in the package",
"gWidgets. Differences from that API spelled out there in the help",
"files are documented (hopefully) in the help page for gWidgetsWWW",
"which can be read with ?gWidgetsWWW The gWidgets API is in need of",
"fine tuning, but for the most part is spelled out in these pages.",
"<P></P>",
"",
"Several examples of gWidgets are available <A href=http://www.math.csi.cuny.edu/pmg/gWidgets/Examples>here</A>.",
"<P></P>",
"  ",
"The gWidgets package also has a vignette with many examples. As of",
"version 0.0-29 this is in need of updating, so some examples may not",
"work.",
"<P></P>",
sep=" ")
ghtml(str, cont = f)

f <- gexpandgroup("Quirks", cont = g, width=width); visible(f) <- FALSE
str <- paste(
             "This package has a few quirks:",
             "<UL>",
             "<LI>Sometimes things need to be escaped. escapeHTML, shQuote can help here</LI>",
             "<LI>Do not use setwd() with the server version</LI>",
             "<LI>There are a lot of small calls back into R to sychronize the GUI with R</LI>",
             "<LI>Some features are found through proto method calls: obj$method(...)</LI>",
             "<LI>Debugging can be a chore, as the browser doesn't give good error messages. Firebug is sometimes useful.</LI>",
sep=" ")
ghtml(str, cont = f)
f <- gexpandgroup("Installation", cont = g, width=width); visible(f) <- FALSE

str <- paste(
"Installation involves: installing and configuring Rpad, installing the",
"extjs libraries, installing and configuring gWidgetsWWW.",
"<P></p>",
"",
"Installation The Rpad package installs both a local version and",
"optionally a server version. The server version allows one to share a",
"GUI with other users, but should be installed with care, as there is a",
"potential security issue if steps are not taken. Using a virtual",
"appliance, such as through <A href=www.vmware.com>vmware</A> can be used here,",
"or pages can be password protected.",
"<p></p>",
"",
"The EXTJS libraries are installed by a) downloading them and b)",
"placing them in the proper place on your website. If you are using the",
"local version, then they can be placed in the directory where you will",
"run Rpad. If using the server version, they can be installed along",
"with the Rpad server files, say var/www/Rpad/ext (which is where the",
"examples find them).",
"<P></P>",
"",
"Installing gWidgetsWWW can be done through install.packages. The stock",
"icons must be installed so that the web browser can find them in '/images', say",
"/var/www/images.  The stock icons (which need redoing) are in the",
"images directory of the package. This directory needs to be copied to",
"the images directory as referred to in a url. For the local version of",
"Rpad, this is the directory from which it is run.",
"<P></P>",
"",
"This package  has only been tested under Linux and Mac OS X.  ",
sep=" ")
ghtml(str, cont = f)

f <- gframe("Debugging", cont = g, width=width)

str <- paste(
"Error messages are pretty uninformative. The message: <b>DEBUG:",
"XMLHttpTransport error callback failed: ReferenceError: props is not",
"defined</b>. appears when an error occurs. This may be due to",
"<UL>",
"<LI> a typo in your R commands. Try to run them from the R process from which Rpad is running (running locally)",
"<LI> A lack of HTML escaping, such as using a quote or &lt; symbol. These are supposed to be programmed around in the package. If you find this error, please email. A quote can be escaped with two slashes.",
"<LI> a bug in gWidgetsWWW. There are probably many. Please email. ",
"</UL>",
             "Running Rpad locally allows one to see if the file sources properly.",
             "Using the firebug add-on for firefox can help identify errors too.",
sep=" ")
ghtml(str, cont = f)


##visible(w) <- TRUE
