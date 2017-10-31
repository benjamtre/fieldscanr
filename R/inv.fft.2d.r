inv.fft.2d <- function(DT, xx='kx', yy='ky', pitch=pi){
  frequencies <- unique(DT$freq)
  FT2D <- function(f){
    z <- DT[freq==f, .(get(xx),get(yy),ft)]
    z <- reshape2::acast(z, V1~V2)

    kz <- stats::fft(z, inverse=T)
    kz <- reshape2::melt(kz)

    ft <- data.table(freq=f,
                     kx=pitch*kz$Var1*(length(unique(kz$Var1))-1)/(2*1e-3*max(abs(kz$Var1))^2),
                     ky=pitch*kz$Var2*(length(unique(kz$Var2))-1)/(2*1e-3*max(abs(kz$Var2))^2),
                     ft=Mod(kz$value))

    ft[,ft:=SynchWave::fftshift(ft),ky]
    ft[,ft:=SynchWave::fftshift(ft),kx]
    #ft$ft <- ifftshift(ft$ft)
    return(ft)
  }

  l <- lapply(frequencies, FT2D)
  FTDT <- rbindlist(l, use.names = F)
  return(FTDT)
}
