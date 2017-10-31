## Plots a map of zz over an area dictated by xx yy
plot.map <- function(DT, xx='y', yy='x', zz='EReal',
                     width.col=NA, title='',
                     norm=T, symm.fill=F, xyfix=F,
                     ztitle='z', ytitle='y', xtitle='x',...){

  maximum <- max(abs(DT[,.(get(zz))]))
  minimum <- min(abs(DT[,.(get(zz))]))
  if(norm==T){
    DT[,c(zz):=get(zz)/maximum]
  }
  maximum <- max(abs(DT[,.(get(zz))]))

  if(symm.fill==T){
    flimits <- c(-maximum, maximum)
    fbreaks <- c(-1,0,1)
    cols=c('red4','red','grey80','blue','blue4')
  } else {
    flimits=c(0, maximum)
    fbreaks <- c(0,maximum)
    cols=inferno(10)
  }

  ##makes a grid of 'xx' x 'yy' size and fills each point with a square with its colour given by 'zz'
  p<- ggplot(data=NULL)+geom_tile(data=DT, aes_string(x=xx, y=yy, fill=zz))+
    theme_bw()+
    ggtitle(title)+
    labs(x=xtitle, y=ytitle, fill=ztitle)+
    scale_x_continuous(expand=c(0,0))+
    # scale_y_continuous(expand=c(0,0), labels=label_scientific10)+
    scale_y_continuous(expand=c(0,0))+
    scale_fill_gradientn(limits=flimits, breaks=fbreaks, colors=cols, ...)+
    theme_bw()+
    theme_bw(base_size = 20)+
    theme(axis.title.x = element_text(size=20, face='bold', vjust=0),
          axis.title.y = element_text(size=20, face='bold', vjust=1.8),
          legend.direction = 'horizontal',
          legend.position = 'bottom',
          legend.key.width = unit(20, "mm"),
          legend.text = element_text(size=10),
          legend.key = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())

  if(xyfix==T){
    p <- p+coord_fixed()
  }
  return(p)
}
