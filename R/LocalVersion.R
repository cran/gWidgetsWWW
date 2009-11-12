
# Rpad utility functions for running Rpad locally.
# Here we use a local Tcl httpd server to receive Rpad commands.

getBaseObjectFromSessionID <- function(sessionID) {
  ## get the gWindow instance matching session ID
  ## return object or NULL
  vars <- ls(envir=.GlobalEnv)
  gWindowObjects <- vars[ sapply(vars, function(i) inherits(get(i, envir=.GlobalEnv), "gWindow")) ]
  if(length(gWindowObjects)) {
    for(i in gWindowObjects) {
      w <- get(i, envir=.GlobalEnv)
      if(w$sessionID == sessionID) {
        return(w)
      }
    }
  }
  return(NULL)
}

escapeBrackets <- function(x) gsub("(\\{|\\})", "\\\\\\1", x)

## The Tcl variable RpadTclResults is set to pass information back into the
## web server
## This is supposed to signal an error, but doesn't seem to be correct.
RpadAssignValue <- function(id, value, sessionID) {
  e <- getBaseObjectFromSessionID(sessionID)
  retval <- "419"                         # expectation failed
  if(is.null(e)) {
    cat("Error: can't find session for", sessionID, "\n")
  } else {
    out <- fromJSON(value)
    if(is.list(out)) {
      tmp <- try(assign(id, out$value, envir=e), silent=TRUE)
      if(!inherits(tmp, "try-error"))
        retval <- "200"                   # all good
    }
  }
  .Tcl(paste("set RpadTclResults {", escapeBrackets(retval), "}", sep=""))
  return("")
}



RpadRunHandler <- function(id, context="", sessionID) {
  e <- getBaseObjectFromSessionID(sessionID)
  if(is.null(e)) {
    .Tcl(paste("set RpadTclResults {alert('session has expired');}", sep="")) # return "" if fails
  } else {
    results <- tryCatch({
      tc <- textConnection("textfromconnection", open="w")
      sink(file=tc)
      if(nchar(context))
        e$runHandler(id, fromJSON(context))
      else
        e$runHandler(id)
      sink()
      close(tc)
      results <- paste(textfromconnection,sep="",collapse="")
#      results <- gsub("\n","\\n",results)
      .Tcl(paste("set RpadTclResults {", escapeBrackets(results), "}", sep=""))
    }, error=function(e) {
      sink()
      close(tc)
      cat('ERROR1: ')
      cat(paste(paste(textfromconnection, "\n", collapse=""), '\n', e),"\n")
      .Tcl("set RpadTclResults {}") # return "" if fails
    }, finally= {})
  }
}

RpadSourceScript <- function(file) {
  file <- gsub("\\.\\.","", file)
  if(!length(grep("R$",file)))
    file <- paste(file, ".R", sep="")
  if(file.exists(paste(getwd(),file, sep=.Platform$file.sep))) {
    results <- tryCatch({
      tc <- textConnection("textfromconnection",open="w")
      sink(file=tc)
      source(file)
      sink()
      close(tc)
      formattedresults <- paste(textfromconnection,"\n",sep="",collapse="")
      ## Now we add some stuff to formatted results
      ## header
      out <-  String() + makegWidgetsWWWPageHeader()
      if(!is.null(getOption("gWidgetsWWWGoogleAPI"))) {
        out <- out +
          paste("<script type='text/javascript' src=http://maps.google.com/maps?file=api&v=2&key=",
                getOption("gWidgetsWWWGoogleAPI"),
                "&sensor=false></script>", sep="")
      }
      out <- out + "<script type='text/javascript'>" +
        formattedresults +
          "</script>"
      options("gWidgetsWWWGoogleAPI"=NULL) # must set in each script
      .Tcl(paste("set RpadTclResults {", escapeBrackets(out), "}", sep=""))
    }, error=function(e) {
      sink()
      close(tc)
      cat('ERROR1: ')
      cat(paste(paste(textfromconnection, "\n", collapse=""), '\n', e),"\n")
      .Tcl(paste("set RpadTclResults {}", sep="")) # return "" if fails
    }, finally= {})
  } else {
    ## can't find the file
    cat("Error1: can't find file", file, "in directory", getwd())
    .Tcl("set RpadTclResults {}")
  }
}

## find file and run from package
## This one requires us to put in headers. This allows
## files other than .R files to be served
mimeTypes <- function(ext) {
  switch(ext,
         "R","text/javascript",
         "txt"="text/plain",
         "htm"="text/html",
         "html"="text/html",
         "gif"="image/gif",
         "jpg"="image/jpeg",
         "png"="image/png",
         "svg"="image/svg+xml",
         "xbm"="image/x-xbitmap",
         "css"="text/css",
         "js "="application/x-javascript",
         "htc"="text/x-component",
         "xml"="text/xml",
         "pdf"="application/pdf",
         "eps"="application/postscript",
         "ps"="application/postscript",
         "text/html"
         )
}

makegWidgetsWWWPageHeader <- function() {
  out <- paste(
               "<!DOCTYPE html PUBLIC '-//W3C//DTD XHTML 1.0 Strict//EN' 'http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd'>",
               "<html xmlns='http://www.w3.org/1999/xhtml' xmlns:v='urn:schemas-microsoft-com:vml'>",
               "<head>",
               "<meta http-equiv='Content-Type' content='text/html; charset=UTF-8'>",
               "<!-- Call in Ext style sheet -->",
               "<link rel='stylesheet' type='text/css' href='/ext/resources/css/ext-all.css'>",
               "</head>",
               "<body>",
               "<!-- Call in Ext files -->",               
               "<div id='loading'>",
               "<div class='loading-indicator'>",
               "<img src='/images/extanim32.gif' width='32' height='32' style='margin-right:8px;float:left;vertical-align:top;'/>",
               "gWidgetsWWW<br /><span id='loading-msg'>Loading styles and images...</span></div>",
               "</div>",
               "<span id='loading-msg'></span></div>",
               "<script type='text/javascript'>document.getElementById('loading-msg').innerHTML = 'Loading Core API...';</script>",
               "<script type='text/javascript' src='/ext/adapter/ext/ext-base.js'></script>",
               "<script type='text/javascript'>document.getElementById('loading-msg').innerHTML = 'Loading UI Components...';</script>",

               "<script type='text/javascript' src='/ext/ext-all.js'></script>",
               "<script type='text/javascript'>document.getElementById('loading-msg').innerHTML = 'Loading gWidgetsWWW...';</script>",               
               "<script type='text/javascript' src='/gWidgetsWWW.js'></script>",
               "<script type='text/javascript'>Ext.onReady(function(){Ext.get('loading').remove();});</script>",
               sep="")
  return(out)
}

makegWidgetsWWWpage <- function(results, script=TRUE) {
  out <- makegWidgetsWWWPageHeader()
  out <-  paste(out,
               if(script) {
                 "<script type='text/javascript'>"
               },
               results,
               if(script) {
                 "</script>"
               },
               "</body>",
               "</html>",
               sep="\n")
  return(out)
}

RpadRunFromPackage <- function(file, package) {
  filename <- system.file(file, package=package)
  if(!file.exists(filename)) {
    ## return error
    out <- paste("HTTP/1.1 404 File Not Found","\n",
                 "Date:", date(),
                 "Connection: close" , "\n\n", sep="")
  } else {
    ## modify this, and put text into results
    output <- list(header="HTTP/1.1 200 Data follows",
                   contentType="text/html")
    
    ## is it a directory?
    if(file.info(filename)$isdir) {
      files <- list.files(filename, pattern="R$")
      
      results <- "Choose a file<br><UL>"
      results <- paste(results,
                       paste("<LI><A href=/gWidgetsWWWRunFromPackage/",
                             paste(file,files,sep="/"),
                             "?package=",package,
                             ">",files,"</A></LI>",
                             sep="", collapse=""),
                       "</UL>", sep="")
      results <- makegWidgetsWWWpage(results, script=FALSE)
    } else {
      ## it's a file
      baseFile <- basename(filename)
      ext <- rev(unlist(strsplit(baseFile,"\\.")))[1]
      
      ## we need to treat some types of files differently
      if(ext == "R") {
        ## source, then output as javascript
        results <- tryCatch({
          tc <- textConnection("textfromconnection",open="w")
          sink(file=tc)
          source(filename)
          sink()
          close(tc)
          formattedresults <- paste(textfromconnection,"\n",sep="",collapse="")
          makegWidgetsWWWpage(formattedresults)
        }, error=function(e) {
          sink()
          close(tc)
          cat('ERROR1: ')
          cat(paste(paste(textfromconnection, "\n", collapse=""), '\n', e),"\n")
        }, finally= {})
      } else {
        ## just change contentType
        output$contentType <- mimeTypes(ext)
        results <- paste(readLines(filename), collapse="\n")
      }
    }
    out <- paste(output$header,
                 output$contentType,
                 "",
                 results,
                 sep="\n")
    
    
    ## set the variable then return
    .Tcl(paste("set RpadTclResults {",
               escapeBrackets(paste(out, collapse="\n"))
               , "}", sep=""))
    return()
  }
}
  

## "processRpadCommands" <-
## function() {
##   require("tcltk")
##   commands <- tclvalue(.Tcl("set user(R_commands)"))
##   textcommands <- textConnection(commands)

##   results <- tryCatch({
##     tc <- textConnection("textfromconnection",open="w")
##     sink(file=tc)
##     guiSource(textcommands)
##     sink()
##     close(tc)
##     textfromconnection
##   }, error=function(e) {
##     sink()
##     close(tc)
##     cat('ERROR1: ')
##     paste(paste(textfromconnection, "\n", collapse=""), '\n', e)},
##                       finally=close(textcommands))
##   formattedresults <- paste(results,"\n",sep="",collapse="")
##   .Tcl(paste("set RpadTclResults {", escapeBrackets(formattedresults), "}", sep=""))
## }

##################################################
## functions to start/stop the server

"Rpad" <-
function(file = "", defaultfile = "LocalDefault.Rpad", port = 8079) {
    startRpadServer(defaultfile, port)
    if(file=="")
      file <- "/gWidgetsWWWRunFromPackage/basehtml/makeIndex.R?package=gWidgetsServer"
    browseURL(paste("http://127.0.0.1:", port, file, sep = ""))
}




"startRpadServer" <-
function(defaultfile = "index.gWWW", port = 8079) {
    require("tcltk")
    ## This is the main function that starts the server
    ## This function implements a basic http server on 'port'
    ## The server is written in Tcl.
    ## This way it is not blocking the R command-line!

    if (!require("tcltk")) stop("package tcltk required for the local Rpad http server")

    ## Need to set some variables
    ## we set this values, as they are used by the scripts
    ## These files are under basehtml in the main directory

    getOptionWithDefault <- function(x, default) {
      x <- getOption(x)
      if(is.null(x))
        x <- default
      return(x)
    }
    ## TODO(JV): Make this smarter, for now gWidgetsWWW server uses global variables
    ## so this must too
    assignOption <- function(x, default) {
      a <- getOptionWithDefault(x, default)
      assign(x, a, envir=.GlobalEnv)
    }
    assignOption("extjsBaseUrl",'/ext')
    assignOption("gWidgetsWWWimageUrl",'/images/')
    
    ## directory and baseurl for static html files, images, svg graphics, ...
    ## This needs to be writeable by the web server process
    ## May need to unlink files that accumulate here!
    assignOption("gWidgetsWWWStaticDir", paste(getwd(),"/",sep=""))
    assignOption("gWidgetsWWWStaticUrlBase","/")

    ## set in key= prop of ggoglemaps
#    options("gWidgetsWWWGoogleAPI"="ABQIAAAAYpRTbDoR3NFWvhN4JrY1ahS5eHnalTx_x--TpGz1e2ncErJceBS7FrNBqzV5DPxkpbheIzZ9nTJPsQ") ## key for 127.0.0.1:8079
    options("gWidgetsWWWGoogleAPI"=NULL)
    assignOption("gWidgetsWWWGoogleAPI","NULL")
    assignOption("gWidgetsWWWRunGoogle","FALSE") # ## Set to TRUE to show google
    
    ## Load in session code -- not needed for local server
    assignOption("sessionSecretKey", "abcdefg")
    
    ## gWidgets AJAX setup
    ## this is needed to handle www <--> R interface
    assignOption("gWidgetsWWWAJAXurl", "/gWidgetsWWWAJAX")
    

    ## Rpad uses environment to keep values, We place in global environment
    e <- new.env()
    assign(".RpadEnv", e, envir=.GlobalEnv)
    assign("RpadLocal", TRUE, envir = e)
    assign("RpadDir",   ".",  envir = e)
    assign("RpadPort",  port, envir = e)

    tclfile <- system.file( "tcl", "mini1.1.tcl", package = "gWidgetsWWW")
    htmlroot <- system.file("basehtml",package = "gWidgetsWWW")
    tcl("source", tclfile)
    tcl("Httpd_Server", htmlroot, port, defaultfile)
    return(TRUE)
}

"stopRpadServer" <-
function() {
    require("tcltk")
    e <- .RpadEnv
    assign("RpadLocal", FALSE, envir = e)
    assign("RpadDir",   NULL,  envir = e)
    assign("RpadPort",  NULL, envir = e)

    .Tcl("close $Httpd(listen)")
    .Tcl("unset Httpd")
}

"restartRpadServer" <-
function() {
  stopRpadServer()
  startRpadServer()
}

##################################################
## These are the exported files
localServerStart <- function(file="", port=8079, package=NULL) {
  startRpadServer("index.gWWW", port)   # just to keep it quiet
  if(file == "" && is.null(package)) {
    file <- "basehtml/makeIndex.R"
    package <- "gWidgetsWWW"
  }
  localServerOpen(file, package)
}
localServerStop <- stopRpadServer
localServerRestart <- restartRpadServer

localServerOpen <- function(file, package) {
  ## open file
  ## if package, then open from package found through
  ## system.file(file, package=package)
  ## if file matches R$, then use gWidgetsWWWrun
  ## else pass through
  if(!missing(package) | !is.null(package)) 
    file <- paste("gWidgetsWWWRunFromPackage/",file, "?package=",package, sep="")
  
  if(length(grep("R$", file)))
    file <- paste("gWidgetsWWWrun/", file)

  port <- get("RpadPort", envir = .RpadEnv)
  browseURL(paste("http://127.0.0.1:", port,"/", file, sep = ""))
}

gWidgetsWWWIsLocal <- function() {
  exists(".RpadEnv", envir=.GlobalEnv) && get("RpadLocal", envir=.RpadEnv)
}
