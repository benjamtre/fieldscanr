fft.1d <- function(DT, xx='x', pitch=pi){

  frequencies <- unique(DT$freq)
  FT1D <- function(f){
    z <- DT[freq==f, .(get(xx),complex(modulus=EMag, argument=EPhase))]

    kz <- stats::fft(z$V2)
    kz <- c(kz[((length(kz)+1)/2):length(kz)], kz[1:((length(kz)-1)/2)])
    FT <- data.table(freq=f,
                     kx=pitch*z$V1*(length(unique(z$V1))-1)/(2*1e-3*max(abs(z$V1))^2),
                     ft=Mod(kz))

    # FT[,ft:=fftshift(ft),kx]
    #ft$ft <- ifftshift(ft$ft)
    return(FT)
  }

  l <- lapply(frequencies, FT1D)
  FTDT <- rbindlist(l, use.names = F)
  return(FTDT)
}
