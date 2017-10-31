load.data <- function(xplane='y',
                     yplane='z',
                     pitch=pi,
                     fseq=NA,
                     mirrorx=F,
                     mirrory=F, ...){

  DT <- read.xyz(...)
  if(is.na(fseq)==F) DT <- DT[freq %in% fseq]
  if(mirrorx==T) DT <- mirror.data(DT, xplane)
  if(mirrory==T) DT <- mirror.data(DT, yplane)


  diffx<- min(DT[,.(get(xplane))])+max(DT[,.(get(xplane))])
  diffy<- min(DT[,.(get(yplane))])+max(DT[,.(get(yplane))])

  DT[, c(xplane):=get(xplane)-diffx/2]
  DT[, c(yplane):=get(yplane)-diffy/2]

  return(DT)
}
