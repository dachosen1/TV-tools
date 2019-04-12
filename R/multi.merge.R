## Merges together all objects (in ordered list format)
multi.merge <- function(objects, by = NULL, by.x = NULL, by.y = NULL, all = FALSE, all.x = all,
                        all.y = all, sort = TRUE, suffixes = c(".x", ".y"), no.dups = TRUE,
                        allow.cartesian=getOption("datatable.allow.cartesian"), ...){

  require(data.table)

  num.objects <- length(objects)

  if(!is.null(by)){
    by.x <- by
    by.y <- by
  }

  if(length(by.x) == 1){
    by.x <- rep.int(x = by.x, times = num.objects - 1)
  }
  if(length(by.y) == 1){
    by.y <- rep.int(x = by.y, times = num.objects - 1)
  }
  if(length(all.x) == 1){
    all.x <- rep.int(x = all.x, times = num.objects - 1)
  }
  if(length(all.y) == 1){
    all.y <- rep.int(x = all.y, times = num.objects - 1)
  }

  res <- setDT(objects[[1]])

  for(i in 2:num.objects){
    res <- merge(x = res, y = setDT(objects[[i]]), by.x = by.x[i-1], by.y = by.y[i-1],
                 all.x = all.x[i-1], all.y = all.y[i-1], sort = sort, suffixes = suffixes,
                 allow.cartesian = allow.cartesian, ...)
  }

  return(res)

}
