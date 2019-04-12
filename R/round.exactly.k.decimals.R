round.exactly.k.digits <- function(x, k, decimal = "."){
  
  y <- round(x = x, digits = k)
  
  if(y == floor(y)){
    if(k <= 0){
      res <- as.character(y)
    }
    if(k > 0){
      res <- sprintf("%d.%s", x, paste(rep.int(x = 0, times = k), collapse = ""))
    }
  }
  if(y != floor(y)){
    the.pieces <- strsplit(x = as.character(y), split = decimal, fixed = TRUE)[[1]]
    
    nc <- nchar(the.pieces[2])
    
    if(nc == k){
      res <- as.character(y)
    }
    if(nc < k){
      res <- sprintf("%s%s", y, paste(rep.int(x = 0, times = k - nc), collapse = ""))
    }
  }
  return(res)
}