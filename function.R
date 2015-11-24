timestrip <- function(time) {
  
  for (i in 1:nrow(docs)) {
    
    stripped_date = data.frame(as.Date(as.POSIXct(strptime(docs$date, "%Y-%m-%d %H:%M:%S"))))
    sdc <- stripped_date
    sdc[complete.cases(sdc),]
  }
  return(x)
}
