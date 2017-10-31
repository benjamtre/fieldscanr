plot.seq <- function(DT, fseq, direc='Field Maps', ...){

  dir.create(direc)
  options("scipen"=100, "digits"=3)
  for(f in fseq){
    DTf <- DT[freq==f]
    tit<- f/1e9
    p <- plot.map(DTf, xyfix=T, xtitle = 'x', ytitle='y', ztitle='Electric Field',...)
    ggsave(paste(direc, '/f=', f, 'GHz.png', sep=''), p, width=7, height=7)
  }

}
