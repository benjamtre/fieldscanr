mirror.data <- function(DT, plane='x'){
  DT2 <- DT[get(plane)!=0]
  DT2[, c(plane):= -get(plane)]
  DT <- rbindlist(list(DT,DT2))
  setkey(DT,x,z)
  return(DT)
}
