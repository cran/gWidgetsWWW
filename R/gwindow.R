## XXX would like to integrate layout manager into this
## Should be a big panel with menubar and toolbar and statusbar areas
## ... contains width=, height= for gsubwindow call
## if container is not null, then a subwindow is made
## handler is not used
gwindow <- function(title="title",file="",visible=TRUE,
                    name=title,
                    width = NULL, height = NULL, parent = NULL,
                    handler=NULL, action=NULL,...) {

  ## width, height  for subwindows
  ## changed container argument to  parent to match gWidgets
  container <- parent
  
   ## make a subwindow?
   if(!is.null(container))
     return(.gsubwindow(title=title,handler=handler, action=action,
                        visible=visible,
                        width = width, height = height,
                       container=container,...))

   w <- EXTContainer$new(file=file,
                         visible=visible,
                         ..actions = list())
   class(w) <- c("gWindow",class(w))

   ## no parent container -- so no ID. We fix this
  w$ID <- "gWidgetID0"

##  w$..visible <- FALSE
  
   w$setValue(value=title)
   ## XXX handle cat to STDOUT
   w$file <- file; unlink(file)

   w$jscriptHandlers = list()        # handlers in parent winoow
   w$toplevel <- w
   w$..IDS <- c()



   ## store name in title for handlers.
   w$titlename <- make.names(title)
  assign(w$titlename,w, envir=.GlobalEnv)

   ## Some properties that can be configured later
   w$ExtBaseURL = "http://localhost:8079/" # with trailing slash

   
   ## methods
  ## for top-level window visible same as print
  w$setVisible <- function(., value) {
    ## same as print
    if(value) {
      .$Show()
    } else {
      cat("can't hide otp-level window\n")
    }
  }

  w$addAction <- function(., action) 
    .$..actions <- c(.$..actions, action)
  
  ## set title
  w$setValueJS <- function(.) {
    if(exists("..setValueJS", envir=., inherits=FALSE)) .$..setValueJS(...)
     
     out <- String() +
       'document.title = ' + shQuote(.$..data) +';'
     return(out)
   }
   

   ## The method makeTemplate will make the website template for this instance
   w$templateHeader <- function(.) {
     out <- String() +
       '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"' +
         '"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">' +
           '<html xmlns="http://www.w3.org/1999/xhtml" xmlns:v="urn:schemas-microsoft-com:vml">' +
             '<html>'+
               '<head>' + 
                 '<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">'+
                   '<title>' + .$getValue() +'</title>' 
     
     ## turn on debug; load RPad
     out <- out +
       '<script type="text/javascript">' +
         'djConfig = {isDebug: true};' +
           'rpadConfig = {rpadHideSource: true,rpadRun: "all"};' +
             '</script>\n' 
               ## CSS for rpad results
     out <- out +
       '<style type="text/css">\n' +
         '.Rpad_results {background-color: #eeeeee;font-family: georgia}' +
           '</style>\n'
     ## Rpad javascript stuff
     out <- out +
       '<script type="text/javascript" src="gui/dojo.js"></script>\n' +
         '<script type="text/javascript" src="gui/Rpad_main.js"></script>\n'
     ## Ext files
     out <- out +
       '<script type="text/javascript" src="' + .$ExtBaseURL +'ext-2.1/adapter/ext/ext-base.js"></script>' +
         '<script type="text/javascript" src="' + .$ExtBaseURL +'ext-2.1/ext-all.js"></script>' +
           '<link rel="stylesheet" type="text/css" href="' + .$ExtBaseURL + 'ext-2.1/resources/css/ext-all.css">'
     ## finish up
     out <- out +
       '</head>' +
         '<body class=' + shQuote(.$defaultCSS) + '>\n'
     out <- out +
       '<div dojoType="Rpad" rpadType="R"></div> <!-- oddity -->\n' +
         '<pre dojoType="Rpad" rpadType="R" rpadOutput="javascript" rpadRun="init">'
     
     .$Cat(out)
   }

   w$templateFooter <- function(.) {
     ## put in footer
     if(.$file == "") return("")
     out <- String() + '</pre>' +'</body>' + '</html>\n'
     .$Cat(out)
   }
   w$makeTemplate <- function(.) {
     .$templateHeader()
     .$Cat("<!-- PUT CODE HERE -->")
     .$templateFooter()
   }


   ## Now for the real stuff
   
   ## css and scripts are different for gwindow instances, as these
   ## are the toplevel instances -- sub classes place there values into
   ## these lists.
   ## css is a list keyed by class.
   w$css <- list("gWindow"=list(obj=w,FUN=function(.) {return("")}))
   
   w$scripts <-
     list("gWindow" = list(obj=w,
            FUN = function(.) {
              ## Scripts, this gets placed into a
              ## list keyed by the class of the object
              
              out <- String()

              ## add Library and Style Sheets
              ## use "script" to add library
              ## use "link" to add style sheet for type
              out <- out +
                'AddLibrary = function(type, file){' +
                  'var NewNode=document.createElement(type);' +
                    'NewNode.src=file;' +
                       'document.body.appendChild(NewNode);' +
                         '};' + '\n'
              

              
              ## runHandlerJS is key to linking in R with the web page
              ## we pass in an ID and optionally some values with keys.
              out <- out +
                'runHandlerJS = function(id,key,extra) {' +
                  'var value = "runHandler(\'' + .$titlename +
                    '\'," + id + ",\'" + key + "\',\'" + escape(extra) + "\')";'  +
                  'rpad.script.run("R",' +
                    'value,' +
                      '"javascript",' +
                        'document,' +
                          '"djConfig.isDebug = false");};\n'
              
              
              ## transportToR copies data in widget back into R
              ## using a global variable IDXXX (as does Rpad)
              ## This shouldn't be necessary, but Rpad runs flaky from
              ## within a script.
              
              out <- out +
                '_transportToR = function(id, val) {' +
                  ' R("assign(\'" + id.toString()+ "\',\'" + val.toString() + "\', envir=.GlobalEnv)");' +
                    '};\n'
              
              ## this is for tooltips in, say, gnotebook tabs.
              out <- out +
                "Ext.QuickTips.init();\n"

              ## ext message for galert
              galertString <-
                paste('Ext.example = function(){' ,
                      'var msgCt;' ,
                        'function createBox(t, s){' ,
                        'return ["<div class=\'msg\'>",' ,
                        '"<div class=\'x-box-tl\'><div class=\'x-box-tr\'><div class=\'x-box-tc\'></div></div></div>",' ,
                        '"<div class=\'x-box-ml\'><div class=\'x-box-mr\'><div class=\'x-box-mc\'><h3>", t, "</h3>", s, "</div></div></div>",' ,
                        '"<div class=\'x-box-bl\'><div class=\'x-box-br\'><div class=\'x-box-bc\'></div></div></div>",' ,
                        '"</div>"].join("");' ,
                        '};' ,
                        'return {' ,
                        'msg : function(title, format, delay){' ,
                        'if(!msgCt){' ,
                        'msgCt = Ext.DomHelper.insertFirst(document.body, {id:"msg-div"}, true);' ,
                        '}' ,
                        'msgCt.alignTo(document, "t-t");' ,
                        'var s = String.format.apply(String, Array.prototype.slice.call(arguments, 1));' ,
                        'var m = Ext.DomHelper.append(msgCt, {html:createBox(title, s)}, true);' ,
                        'm.slideIn("t").pause(delay).ghost("t", {remove:true});' ,
                        '},' ,
                        '' ,
                        'init : function(){' ,
                        'var t = Ext.get("exttheme");' ,
                        'if(!t){ ' ,
                                                        'return;' ,
                        '}' ,
                        'var theme =  "aero";' ,
                                                            'if(theme){' ,
                        't.dom.value = theme;' ,
                        'Ext.getBody().addClass("x-",theme);' ,
                        '};' ,
                        't.on("change", function(){' ,
                        'setTimeout(function(){' ,
                        'window.location.reload();' ,
                        '}, 250);' ,
                        '});' ,
                        'var lb = Ext.get("lib-bar");' ,
                        'if(lb){' ,
                        'lb.show();' ,
                        '}' ,
                        '}' ,
                        '};' ,
                        '}();',
                        collapse="\n")
              
              
              out <- out + galertString
              return(out)
            })
          )

   w$ExtConstructor <- "Ext.Panel" ## inherits
   w$ExtCfgOptions <- function(.) { ## ih
     out <- list(
                 border = TRUE,
#                 bodyBorder = FALSE,
                 hideBorders = FALSE,
                 autoScroll = TRUE
                 )
     
     return(out)
   }

   ## code to set up iconclasses for use with buttons, toolbar, menubar
   w$iconDir <- ""
   w$makeIconClasses <- function(.) {
     out <- String()
     x <- getStockIcons();
     nms <- names(x)
     for(i in 1:length(x)) {
       out <- out +
         'Ext.util.CSS.createStyleSheet("' +
           paste("button.",nms[i], "{background-image:url(",x[i],")};", sep="",
                 collapse="") +
           '");' + '\n'
     }
     return(out)
   }
   
   w$header <- function(.) {
     out <- String() + .$makeIconClasses()

     .$Cat(out)
   }


     
   ## This didn't work. Tried to make finding Ext configurable
   ##     w$header <- function(.) {
   ##       out <- String() +
   ## ##        'AddLibrary("script","/gui/dojo.js");' +
   ## ##          'AddLibrary("script","/gui/Rpad_main.js");' +
   ##             'AddLibrary("script","/ext-2.1/adapter/ext/ext-base.js");' +
   ##               'AddLibrary("script","/ext-2.1/ext-all.js");' +
   ##                 'AddLibrary("link","/ext-2.1/resources/css/ext-all.css");'
   ##       .$Cat(out)
   ##     }
   
w$footer <- function(.) {
  
     ## clear out any old IDS
     remove(list=ls(pat="^gWidgetID",envir=.GlobalEnv),envir=.GlobalEnv)

     ## doLayout()
     out <- String() +
       .$asCharacter() + '.doLayout();' + '\n'

     ## set title 
     out <- out +
       'document.title =' +shQuote(.$getValue()) + ';\n' 

     ## finish Ext.onReady
#     out <- out + '})\n'
     .$Cat(out)
   }
   

   w$..setHandlers <- function(.) {
     ## deprecated, see addHandler now
     return("")
   }

   ## unload handler
   if(!is.null(handler)) 
  w$addHandler("onunload",handler, action=action)

   
    invisible(w)
 }

## gsubwindow
## a subwindow appears on top of a regular window
## style properties width, height (size) and x, y are used
## should be draggable -- but doesn't seem to work
##
## svalue<- sets title
## visible(obj) TRUE is showing
## visible(obj) <- FALSE/TRUE hides/shows
.gsubwindow <- function(title="Subwindow title", visible=TRUE,
                        width=500, height=300,
handler = NULL, action=NULL, container=NULL,...) {

  widget <- EXTContainer$new(toplevel=container$toplevel,
                           ..visible = as.logical(visible)
                           ) 
  class(widget) <- c("gSubwindow",class(widget))
  widget$setValue(value=title)

  if(is.null(width)) width <- 500
  if(is.null(height)) height <- 300
  widget$..style <- c("width"=width, height=height)
  ## methods
  widget$addAction <- function(., action) 
     .$..actions <- c(.$..actions, action)

  ## visible
  widget$setVisible <- function(., value) {
    .$..visible <- as.logical(value)
    if(exists("..shown",envir=., inherits=FALSE)) {
      cat(.$setVisibleJS())
    } else {
      if(as.logical(value))
        print(.)
    }
  }

  widget$dispose <- function(.) visible(.) <- FALSE

  ## set title
  widget$setValueJS <- function(.) {
  if(exists("..setValueJS", envir=., inherits=FALSE)) .$..setValueJS(...)
  
    out <-  String() +
      'o' + .$ID + '.setTitle(' + shQuote(.$..data) + ');' + '\n'
    cat(out)
  }
  ## odd inheritance here
  widget$setVisibleJS <- function(.) {
    if(exists("..setVisibleJS", envir=., inherits=FALSE))
      .$..setVisibleJS()
    
    ## opposite -- we already changed if we got here
    if(.$..visible)
      method = "show"
    else
      method = "hide"
    
    out <- String() + 
      'o' + .$ID + '.' + method + '();'
    cat(out)
  }


  widget$ExtConstructor = "Ext.Window"
  widget$ExtCfgOptions <- function(.) {
    style <- .$..style
    width <- style['width']
    height <- style['height']
    
    out <- list(
                'title' = .$..data,
                'layout' = "fit",
                'width' = as.numeric(width),
                'height' =  as.numeric(height),
                'closeAction' = "hide",
                'autoScroll' = TRUE,
                'plain' = TRUE,
                'button' = String('[{text: "Close", handler: function(){') +
                'o' + .$ID + '.hide();}}]'
                )
    ## statusbar. Menu? Tool?
    if(exists("..statusBar",envir=., inherits=FALSE)) {
      sbText <- String() +
        'new Ext.StatusBar({' +
          'id: "' + .$ID + 'statusBar",' +
            'defaultText: "",' +
              'text:' + shQuote(.$..statusBarText) +
                '})'
      out[['bbar']] <- sbText
      .$..statusBar$..shown <- TRUE
    }
    
    return(out)
  }

  

  widget$header <- function(.) {}
  widget$footer <- function(.) {
    ## render and show
    out <- String()

    out <-  out + 'o' + .$ID + '.render();' + '\n'
    if(visible(.))
      out <-  out + 'o' + .$ID + '.show();' + '\n'

    .$Cat(out)
  }

  ## we don't add. The subwindow prints out
  container$add(widget, ...)


  ## return
  invisible(widget)
}
