my.barplot <- function(the.table, rate.name, val.name, main = "", width = 1, space = 0.2, cex = 0.7, the.order = -1, col = "cadetblue1", ylim = NA, digits = 2, las = 2, xlab = NULL, ylab = NULL){
  require(data.table)
  setDT(the.table)
  
  if(!is.na(the.order[1])){
    setorderv(x = the.table, cols = rate.name, order = the.order)
  }
  if(is.na(ylim[1])){
    ylim <- c(0, 1.5 * max(the.table[, get(rate.name)]))
  }
  
  if(length(val.name) == 0){
    val.name <- "this.name.is.made.up.123456789"
    the.table[, eval(val.name) := ""]
  }
  if(!val.name %in% names(the.table)){
    val.name <- "this.name.is.made.up.123456789"
    the.table[, eval(val.name) := ""]
  }
  
  barplot(height = the.table[, get(rate.name)], width = width, space = space, names.arg = the.table[, get(val.name)], col = col, las = las, main = main, ylim = ylim, cex.names = cex, cex.axis = cex, xlab = xlab, ylab = ylab)
  text(x = -1 * (width/2) + (1+space) * 1:the.table[, .N], y = the.table[, get(rate.name)], labels = round(x = the.table[, get(rate.name)], digits = digits), cex = cex, pos = 3)
}
