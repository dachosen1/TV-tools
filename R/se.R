se <- function(x, na.rm = TRUE){
 return(sd(x, na.rm = na.rm)/sqrt(sum(!is.na(x))))
}
