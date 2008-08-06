  
## others
## propert ..length is added
gslider <- function(from = 0, to = 100, by = 1, value = from,
                    horizontal = TRUE,
                    handler = NULL, action = NULL, container = NULL, ...) {
  widget <- EXTComponent$new(toplevel=container$toplevel,
                             ..from = from, ..to = to, ..by=by,
                             ..horizontal=horizontal
                             )
  class(widget) <- c("gSlider",class(widget))
  widget$setValue(value=value)
  widget$..coerce.with="as.numeric"
  widget$..length = if(horizontal) 300 else 100
  ## CSS
  
  widget$scripts <- function(.) {
    ## from main example page 
    out <- String() +
    '/**' +
      '* @class Ext.ux.SliderTip' +
        '* @extends Ext.Tip' +
          '* Simple plugin for using an Ext.Tip with a slider to show the slider value' +
            '*/'
    out <- out +
      'Ext.ux.SliderTip = Ext.extend(Ext.Tip, {' +
        'minWidth: 10,' +
          'offsets : [0, -10],' +
            'init : function(slider){' +
              'slider.on("dragstart", this.onSlide, this);' +
                'slider.on("drag", this.onSlide, this);' +
                  'slider.on("dragend", this.hide, this);' +
                    'slider.on("destroy", this.destroy, this);' +
                      '},' +
                        ''
    out <- out +
      
      'onSlide : function(slider){' +
        'this.show();' +
          'this.body.update(this.getText(slider));' +
            'this.doAutoWidth();' +
              'this.el.alignTo(slider.thumb, "b-t?", this.offsets);' +
                '},' +
                  '' +
                    'getText : function(slider){' +
                      'return slider.getValue();' +
                        '}' +
                          '});'
    out <- out +
      'var tip = new Ext.ux.SliderTip({' +
        'getText: function(slider){' +
          'return String.format("<b>{0}% complete</b>", slider.getValue());' +
            '}' +
              '});'
    return(out)
  }
  
  ## methods
  widget$getValueJSMethod <- "getValue"
  widget$setValueJSMethod <- "setValue"
  widget$transportSignal <- "change"
  widget$transportValue <- function(.,...) {
    out <- String() +
      'var value = newValue;'
    return(out)
  }
  widget$ExtConstructor <- "Ext.Slider"
  widget$ExtCfgOptions <- function(.) {
    out <- list("value"= svalue(.),
                "increment" = .$..by,
                "minValue" =  .$..from,
                "maxValue" = .$..to,
                "enableKeyEvents"=TRUE,
                "vertical"= !.$..horizontal,
                plugins = String("new Ext.ux.SliderTip()")
                )
    if(.$..horizontal)
      out[['width']] <- .$..length
    else
      out[['height']] <- .$..length

    return(out)
  }


  ## add after CSS, scripts defined
  container$add(widget,...)


  if(!is.null(handler))
    widget$addHandler("change",handler=handler,action=action)
  
  invisible(widget)
}

gspinbutton <- function(...) {stop("No spinbutton widget in ext. The gslider widget is an alternative")}
