## hack to add separator
gseparator <- function(horizontal = TRUE, container = NULL, ...)  {
  if(horizontal)
    return(ghtml("<hr>",cont=container))
  else
    return()
}

