lapply.to.class <- function(X, FUN, Class, ...){
  res.list <- lapply(X = X, FUN = FUN, ...)
  res <- as(object = res.list, Class = Class)
  return(res)
}
