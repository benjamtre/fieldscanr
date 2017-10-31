## Reads all xyz data files with given prefix into a data frame.
## requires 6 columns: frequency, x, y, z, mag, phase
## Adds real and imaginary columns, returns 8 column data table

read.xyz <- function(pathname='Raw Data/', prefix='Data '){

  if(substr(pathname, nchar(pathname),nchar(pathname))!='/'){
    pathname<- paste(pathname, '/', sep='')
  }
  if(substr(pathname, 1,1)=='/'){
    pathname<- substr(pathname, 2, nchar(pathname))
  }

  data.files <- list.files(path = pathname, recursive=T, pattern=paste(prefix, '+[0-9]', sep=''))
  l <- lapply(paste(pathname,data.files,sep=''), data.table::fread)
  DT <- rbindlist(l, use.names = F)

  setnames(DT, c('freq','x', 'y', 'z', 'Mag', 'Phase') )
  DT[,Phase := Phase*pi/180]
  DT[,Real := Re(complex(modulus=Mag, argument=Phase))]
  DT[,Imag := Im(complex(modulus=Mag, argument=Phase))]

  print(paste(length(DT), 'values read in, with dimensions: ',
              length(unique(DT$x)), ',',
              length(unique(DT$y)), ',',
              length(unique(DT$z)), ','))
  return(DT)
}
