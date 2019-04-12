mean.segments.and.totals <- function(dat, SDcols, by, digits = 1){
 library(data.table)

 sd.name <- "SD"
 se.name <- "SE"
 n.segmented <- dat[, lapply(X = .SD, FUN = "get.sample.size"), .SDcols = SDcols, by = by]

 n.segmented[, eval(by) := sprintf("%s - n", get(by))]

 mean.segmented <- dat[, lapply(X = .SD, FUN = "round.mean", digits = digits), .SDcols = SDcols, by = by]

 mean.segmented[, eval(by) := sprintf("%s - Mean", get(by))]

 sd.segmented <- dat[, lapply(X = .SD, FUN = "round.sd", digits = digits), .SDcols = SDcols, by = by]

 sd.segmented[, eval(by) := sprintf("%s - %s", get(by), sd.name)]

 se.segmented <- dat[, lapply(X = .SD, FUN = "round.se", digits = digits), .SDcols = SDcols, by = by]

 se.segmented[, eval(by) := sprintf("%s - %s", get(by), se.name)]

 tvalues <- dat[, lapply(X = .SD, FUN = "comparison.test.statistic", splitter = get(by)), .SDcols = SDcols]
 tvalues[, eval(by) := "test stat"]

 dfvalues <- dat[, lapply(X = .SD, FUN = "comparison.test.df", splitter = get(by)), .SDcols = SDcols]
 dfvalues[, eval(by) := "df"]

 pvalues <- dat[, lapply(X = .SD, FUN = "comparison.test.p.value", splitter = get(by)), .SDcols = SDcols]
 pvalues[, eval(by) := "p"]
 n.all <- dat[, lapply(X = .SD, FUN = "get.sample.size"), .SDcols = SDcols]
 n.all[, eval(by) := "All - n"]
 mean.all <- dat[, lapply(X = .SD, FUN = "round.mean", digits = digits), .SDcols = SDcols]
 mean.all[, eval(by) := "All - Mean"]
 se.all <- dat[, lapply(X = .SD, FUN = "round.se", digits = digits), .SDcols = SDcols]
 se.all[, eval(by) := sprintf("All - %s", se.name)]

 sd.all <- dat[, lapply(X = .SD, FUN = "round.sd", digits = digits), .SDcols = SDcols]
 sd.all[, eval(by) := sprintf("All - %s", sd.name)]

 all.rows <- rbindlist(l = list(n.segmented, mean.segmented, sd.segmented, se.segmented, tvalues, dfvalues, pvalues, n.all, mean.all, sd.all, se.all), fill = TRUE)
 by.values <- all.rows[, get(by)]

 results <- all.rows[, data.table(t(.SD), keep.rownames=TRUE), .SDcols=-by]
 setnames(x = results, old = names(results), new = c("Trait", by.values))

 test.names <- as.data.table(t(dat[, lapply(X = .SD, FUN = "comparison.test.names", splitter = get(by)), .SDcols = SDcols]), keep.rownames = TRUE)
 setnames(x = test.names, old = names(test.names), new = c("Trait", "Test"))

 results <- merge(x = results, y = test.names, by = "Trait")

 setcolorder(x = results, neworder = c("Trait", "Test", names(results)[names(results) %nin% c("Trait", "Test")]))

 return(results)
}
