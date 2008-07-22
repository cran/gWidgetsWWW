## use GridView to show a table

## working

## svalue
## transport
## click and double click handler
## icon fun: use class(icon) to mark
## multiple is working
## [<- : needs to have data frame with same column types, no. of columns

## get working:
## names<-, names for headers
## integrate -- filter fun: maybe never

gtable <- function(items, multiple = FALSE, chosencol = 1,
                   icon.FUN = NULL,
                   filter.column = NULL, filter.labels = NULL,
                   filter.FUN = NULL, handler = NULL, action = NULL,
                   container = NULL, ...) {

  widget <- EXTComponentWithStore$new(toplevel=container$toplevel,
                                      ..multiple = multiple,
                                      ..icon.FUN = icon.FUN
                                      )

  class(widget) <- c("gTable",class(widget))

  ## set up store
  store <- EXTStore$new()
  store$ID <- container$newID()       # set ID

  ## load in items
  if(!is.data.frame(items)) items <- as.data.frame(items)

  store$chosenCol <- chosencol
  if(!is.null(icon.FUN)) {
    n <- length(items)
    icons <- icon.FUN(items)
    class(icons) <- c("icon",class(icons)) # to render as icon
    items$..icons <- icons
    items <- items[,c(n+1,1:n)]
    ## must up the chosen col by 1
    store$chosenCol <- store$chosenCol + 1
  }
  store$data <- items
  widget$..store <- store

  ## set up widget
  widget$setValue(value = 1)            # first column is selected on startup
  widget$getValue <- function(.,index=NULL ,drop=NULL,...) {
    ## we store value as an index
    out <- .$..data
    values <- .$..store$data
    
    if(exists("..shown",envir=.,inherits=FALSE)) {
      ## get from widget ID
      out <- try(get(.$ID,envir=.GlobalEnv),silent=TRUE) ## XXX work in index here?
      if(!inherits(out,"try-error")) {
        ## For multiple, this is stored as a vector that needs to be eval'ed
        if(.$..multiple) {
          out <- eval(parse(text=out))
        } else {
          out <- as.numeric(out)          # is character
        }
        .$..data <- out                 # update data
      } else {
        out <- .$..data
      }
    }
    ## no index -- return values
    if(is.null(index)) index <- FALSE
    if(index) {
      return(as.numeric(out))
    } else {
      ## depends on drop
      if(is.null(drop) || drop) {
        return(values[as.numeric(out),.$..store$chosenCol,drop=TRUE])
      } else {
        return(values[as.numeric(out),])
      }
    }      
  }

  ## setValues need to add in icons.
  widget$setValues <- function(.,i,j,...,value) {
    ## XXX need to include i,j stuff
    ## XXX value must be a data frame of the same size as original
    ## add in icons if present
    items <- value
    if(exists("..icon.FUN", envir=., inherits=FALSE)) {
      if(!is.null(.$..icon.FUN)) {        # adjust icons
        icon.FUN <- get("..icon.FUN",.)
        icons <- icon.FUN(items)
        class(icons) <- c("icon",class(icons)) # to render as icon
        n <- length(items)
        items$..icons <- icons
        items <- items[,c(n+1,1:n)]
      }
    }
    .$..store$data <- items

    if(exists("..shown",envir=., inherits=FALSE))
      cat(.$setValuesJS(...))
  }
  
  widget$transportSignal <- c("cellclick")
  widget$transportValue <- function(.,...) {
    if(.$..multiple) {
      ## work a bit to get the value
      out <- String() +
        'store = w.getStore();' +
          'selModel = w.getSelectionModel();' +
            'var values = selModel.getSelections();' +
              'value = "c(";' +
                'for(var i = 0, len = values.length; i < len; i++){;' +
                  'var  ind = store.indexOf(values[i]) + 1;' +
                    'value = value + ind + ",";' +
                    '};' +
                      're = /,$/;' + 'value = value.replace(re,"");' +
                      'value = value + ")";'
    } else {
      out <- "var value = rowIndex + 1;"
    }
    
    return(out)
  }

  widget$ExtConstructor <- "Ext.grid.GridPanel"
  widget$ExtCfgOptions <- function(.) {
    out <- list(store = String(.$..store$asCharacter()),
                columns = String(.$makeColumnModel()),
                stripeRows = TRUE,
                frame = FALSE
                ) ## also autoExpandColumn, XXX

    ## paging -- doesn't work -- doesn't get no. of fields from store
##     out[['bbar']] = String() +
##       paste("new Ext.PagingToolbar({",
##             "pageSize: 10,",
##             (String("store:") + .$..store$asCharacter() + ','),
##             "displayInfo: true,",
##             "displayMsg: 'Displaying topics {0} - {1} of {2}',",
##             "emptyMsg: 'No topics to display',",
##             "items:[",
##             "'-', {",
##             "pressed: true,",
##             "enableToggle:true,",
##             "text: 'Show Preview',",
##             "cls: 'x-btn-text-icon details',",
##             "toggleHandler: function(btn, pressed){",
##             (String("var view =") + .$asCharacter() + ".getView();"),
##             "view.showPreview = pressed;",
##             "view.refresh();",
##             "}",
##           "}]",
##           "})",
##             collapse="\n")

    
    ## XXX Need to get multiple working on other end via transport function
    if(.$..multiple) {
      out[["sm"]] <- String() +
        'new Ext.grid.RowSelectionModel({singleSelect:false})'
    } else {
      out[["sm"]] <- String() +
        'new Ext.grid.RowSelectionModel({singleSelect:true})'
    }

    ## size in panel config, not setStyle
    if(exists("..width",envir = .,inherits=FALSE))
      out[["width"]] <- .$..width
    else
      out[["width"]] <- "auto"
    
    if(exists("..height",envir = .,inherits=FALSE))
      out[["height"]] <- .$..height
    else
        out[["height"]] <- "auto"
    
    return(out)
  }

  ## make some renderes
  widget$scripts <- function(.) {
    out <- String()
    ## text is red or black depending
    out <- out +
      'gtableNumeric = function(val) { ' +
        'return \'<span style="color:red">\' + val + \'</span>\';' +
          '};' + '\n'
    out <- out +
      'gtableInteger = function(val) { ' +
        'return \'<span style="color:red">\' + val + \'</span>\';' +
          '};' + '\n'
    out <- out +
      'gtableLogical = function(val) { ' +
        'return \'<span style="color:black">\' + val + \'</span>\';' +
          '};' + '\n'
    out <- out +
      'gtableIcon = function(val) { ' +
        'return \'<img src="\' + val + \'"/>\';' +
          '};' + '\n'

    return(out)
  }

  
  widget$makeColumnModel <- function(.) {
    ## return array for columns
    ## id, header, sortable, renderer, dataIndex, tooltip
##     columns: [
##               {id:'company',header: "Company", sortable: true, dataIndex: 'company'},
##               {header: "Price",  sortable: true, renderer: 'usMoney', dataIndex: 'price'},
##               {header: "Change", sortable: true, renderer: change, dataIndex: 'change'},
##               {header: "% Change", sortable: true, renderer: pctChange, dataIndex: 'pctChange'},
##               {header: "Last Updated", sortable: true, renderer: Ext.util.Format.dateRenderer('m/d/Y'), dataIndex: 'lastChange'}
##               ],

    mapRenderer <- function(type) {
      switch(type,
             "character"="",
             "String" = "",
             "integer" = ",renderer:gtableInteger",
             "numeric" = ",renderer:gtableNumeric",
             "logical" = ",renderer:gtableLogical",
             "factor" = "",
             "icon" = ",width: 16,renderer:gtableIcon",              # for icons(we create this)
             "date" = "",               # we create this?
             "")
    }

    df <- .$..store$data
    renderers <- sapply(df, function(i) mapRenderer(class(i)[1]))
    colNames <- names(df)
    colNames <- shQuoteEsc(colNames)

    ## didn't work for header:
    trimDD <- function(x) {
      ind <- grep("^..", x)
      if(length(ind) > 0)
        x[ind] <- "''"
      return(x)
    }

    
    tmp <- paste('{',
                 'id:',colNames,
                 ', header:',colNames,
                 ', sortable:true',
                 ', dataIndex:',colNames,
                 renderers,
                 '}',
                 sep="")
    out <- paste('[\n', paste(tmp,collapse=",\n"), ']', collapse="")

    return(out)
  }
  widget$makeFields <- function(.) {
    ## return something like this with name, type
    ##     fields: [
    ##            {name: 'company'},
    ##            {name: 'price', type: 'float'},
    ##            {name: 'change', type: 'float'},
    ##            {name: 'pctChange', type: 'float'},
    ##            {name: 'lastChange', type: 'date', dateFormat: 'n/j h:ia'}
    ##         ]
    ## types in DataField.js
    mapTypes <- function(type) {
      switch(type,
             "character"="",
             "String" = ",type: 'string'",
             "integer" = ",type: 'int'",
             "numeric" = ",type: 'float'",
             "logical" = ",type: 'boolean'",
             "factor"  = "",
             "date" = ",type:date",
             "")
    }
    df <- .$..store$data
    types <- sapply(df, function(i) mapTypes(class(i)[1]))
    colNames <- shQuoteEsc(names(df))
    tmp <- paste("{name:", colNames, types, "}", sep="")
    out <- paste("[",tmp,"]", collapse="\n")

    return(out)
  }

  widget$footer <- function(.) {
    out <- String() +
      'o' + .$ID + '.getSelectionModel().selectFirstRow();'

    .$Cat(out)
  }
  
  
  ###
  container$add(widget,...)

  ## changed = clicked
  widget$addHandlerClicked <- function(.,handler, action=NULL, ...) {

    ## we need to set up some stuff
    .$addHandler(signal="cellclick",
                 handler = handler,
                 action = action,
                 handlerArguments = "grid, rowIndex, columnIndex, e",
                 handlerValue = "var value = rowIndex + 1;"
                 )
  }

  ## double click is default
  widget$addHandlerDoubleclick <- widget$addHandlerChanged <- function(.,handler, action=NULL, ...) {
    ## we need to set up some stuff
    .$addHandler(signal="celldblclick",
                 handler = handler,
                 action = action,
                 handlerArguments = "grid, rowIndex, columnIndex, e")
  }
  
  if(!is.null(handler))
    widget$addHandlerChanged(handler, action=action)
  
  
  invisible(widget)
  
}
