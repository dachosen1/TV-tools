round.sd <- function(x, digits = 1){
 y <- round(x = sd(x, na.rm = TRUE), digits = digits)
 return(y)
}
