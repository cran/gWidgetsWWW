## Right way is to extend EXT.Component
gimage <- function(filename = "", dirname = "",  size = "",
                   handler = NULL, action = NULL, container = NULL,...,
                   resizable =FALSE     # WWW option. Keep?
                   ) {

  if(!resizable) {
    widget <- EXTComponent$new(toplevel=container$toplevel)
    class(widget) <- c("gImage", class(widget))
  } else {
    widget <- EXTComponentResizable$new(toplevel=container$toplevel)
    class(widget) <- c("gImage","gWidgetResizable", class(widget))
  }

  ## append dirname if non empty
  if(dirname != "")
    filename <- String(dirname) + filename
  widget$setValue(value=filename)


  widget$scripts <- 
    String(sep="\n") +
      'Ext.ux.imageBox = Ext.extend(Ext.Component, {' + 
	'value: null,' + 
          'initComponent:function() {' + 
	    'Ext.ux.imageBox.superclass.initComponent.call(this);' + 
              '},' + 
                'onRender:function(ct, position) {' + 
                  'this.el = document.createElement("img");' + 
                    'this.el.id = this.getId();' + 
                      'this.el.src = this.value;' + 
                        'Ext.ux.imageBox.superclass.onRender.call(this,ct,position);' + 
                          '},' + 
                            'getValue: function() {' + 
                              'return this.value;' + 
                                '},' + 
                                  'setValue: function(value) {' + 
                                    'this.value = value;' + 
                                      'document.getElementById(this.id).src = value;' + 
                                        '}' + 
                                          '});' +
                                            'Ext.reg("imagebox", Ext.ux.imageBox);' 

  
  widget$setValueJSMethod = "setValue"
  widget$getValueJSMethod = "setValue"
  widget$ExtConstructor <- "Ext.ux.imageBox"
  widget$ExtCfgOptions <-  function(.) {
    out <- list()
    out[["value"]] = svalue(.)
    return(out)
  }
  if(size != "") size(widget) <- size
  
  ## add after CSS, scripts defined
  container$add(widget,...)

  ## no handler
  
  invisible(widget)
}
