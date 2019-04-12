## Compute a function on the designated columns split by the designated variables.  Then add an additional row that shows the total results (unsegmented).  Creates a data.table object.

segments.and.totals <- function(dat, function.name, .SDcols, by.name, total.name = "All", ...){
 library(data.table)
 dat <- as.data.table(x = dat)

 segmented <- dat[, lapply(X = .SD, FUN = get(function.name), ... = ...), .SDcols = .SDcols, by = by.name]
 total <- dat[, lapply(X = .SD, FUN = get(function.name), ... = ...), .SDcols = .SDcols]
 total[, eval(by.name) := total.name]

 res <- rbindlist(l = list(segmented, total), fill = TRUE)
 return(res)
}
