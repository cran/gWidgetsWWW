## list data frame
data(mtcars)
listDataFrames <- function() {
  vars <- ls(envir=.GlobalEnv)
  ind <- sapply(vars, function(i) is(get(i, envir=.GlobalEnv), "data.frame"))
  if(length(ind) > 0)
    return(c("",vars[ind]))
  else
    return(c(""))
}

## list names of variables in data frame
## cond is a function of a variable that returns TRUE to keep, FALSE to filer
listVariableNames <- function(cond, blank=TRUE) {
  df <- e$currentData
  if(missing(cond)) cond <- function(x) TRUE
  if(df == ".GlobalEnv" || df =="") {
    vars <- ls(envir=.GlobalEnv)
    ind <- sapply(vars, function(i) {
      x <- get(i, envir=.GlobalEnv)
      tmp <- is(x,"numeric") || is(x,"integer") || is(x,"character") || is(x, "logical") || is(x,"factor")
      tmp <- tmp && cond(x)
      tmp
    })
    out <- vars[ind]
  } else {
    df <- get(df, envir=.GlobalEnv)
    out <- try(names(df), silent=TRUE)
    if(!inherits(out,"try-error")) {
      if(!missing(cond)) {
        ind <- sapply(out, function(x) cond(df[,x]))
        out <- out[ind]
      }
    } else {
      out <- c("")
    }
  }
  if(blank)
    out <- c("", out)
  return(out)
}

getVarValue <- function(var) {
  df <- e$currentData
  if(df == "" || df == ".GlobalEnv") {
    out <- try(get(var, envir = .GlobalEnv), silent=TRUE)
  } else {
    df <- try(get(df, envir=.GlobalEnv), silent=TRUE)
    out <- df[,var, drop=TRUE]
  }
  if(!inherits(out, "try-error"))
    return(out)
  else
    return(NA)
}
