## Draws the Brillouin Zone on a plot
draw.bz <- function(plt=p, u=5, v=2, alpha=40*pi/180, phi=0, N=2, i=seq(-N,N,1), j=i){

  ux<- u*cos(phi)
  uy<- u*sin(phi)
  vx<- v*cos(phi+alpha)
  vy<- v*sin(phi+alpha)

  ku <- (2*pi)/(u*cos((pi/2)+alpha))
  kv <- (2*pi)/(v*cos((pi/2)-alpha))
  kphi <- alpha-(pi/2)+phi
  kalpha <- alpha+2*phi

  kux<- ku*cos(kphi)
  kuy<- ku*sin(kphi)
  kvx<- kv*cos(kphi+kalpha)
  kvy<- kv*sin(kphi+kalpha)


  for(m in i){
    for(n in j){
      kxN<- i*kux + j*kvx
      kyN<- i*kuy + j*kvy

      gradient <- -kxN/kyN
      int <- (kyN/2)+((kxN^2)/(2*kyN))
      plt <- plt + geom_abline(intercept=int, slope=gradient, colour='blue', linetype='dashed') + geom_point(x=kxN,y=kyN, colour='red')
    }
  }

  return(plt)
}
