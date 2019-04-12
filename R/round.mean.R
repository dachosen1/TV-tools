round.mean <- function(x, digits = 1){
 y <- round(x = mean(x, na.rm = TRUE), digits = digits)
 return(y)
}
