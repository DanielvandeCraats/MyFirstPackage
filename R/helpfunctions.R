#'
#'@title Convert KNMI input
#'@description This function converts standard KNMI input datafiles to readable input files for the main SUTRA script.
#'    Function not to be called outside of main program.
#'@param Datafile Name of KNMI data file to read
#'@export

convertKNMI.fun <- function(Datafile){
  setwd(sourceDir)
  data.all <- read.csv(Datafile,skip='31')
  start <- head(which(data.all$YYYYMMDD == Simul.startdate),1)
  end <- start + 24 * Simul.totaltime -1
  if((length(start) == 0L) | (length(end) == 0L))
    {stop('Start or end times do not match weather data input file.')}
  data.all <- data.all[start:end,]
  data.all$HH[nchar(data.all$HH) == 1] <- paste('0',data.all$HH[nchar(data.all$HH) == 1],sep='')
  precip <- cbind(paste(data.all$YYYYMMDD,data.all$HH,sep=''),data.all$RH/10/1000)
  precip[precip[,2] == -1e-4,2] <- 0
  precip <- apply(precip,c(1,2),as.numeric)
  return(precip)
}

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

boundsize.fun <- function(bound){
  bound.sorted <- cbind(bound[order(bound[,2]),],NA)
  bound.diff <- diff(bound.sorted[,2])
  for(i in 2:(nrow(bound.sorted)-1))
  {
    bound.sorted[i,5] <- (bound.diff[i-1]+bound.diff[i])/2
  }
  bound.sorted[1,5] <- 0.5 * bound.diff[1]
  bound.sorted[nrow(bound),5] <- 0.5 * bound.diff[length(bound.diff)]
  boundsize <- bound.sorted[order(bound.sorted[,1]),]
  return(boundsize)
}

createimage.fun <- function(Distr){
  setwd(file.path(sourceDir,'results',Simul.name))

  precip_12 <- c(0,0,unname(tapply(P[,2], (seq_along(P[,2])-1) %/% 12, sum)))
  zlim <- c(min(unlist(Distr)),max(unlist(Distr)))
  #zlim <- c(-5000,25000)
  cols <- colorRampPalette(c('blue','red'))
  ani.options(interval = 1)
  saveHTML({
    for (i in 1:(length(Distr)/3)) {
      #image(Distr[,i],asp=1,main=precip_12[i],zlim=zlim)
      image(Distr[,i],asp=1,main=i,zlim=zlim,col=cols(20))
      tryCatch({
        points(watertablepoints,watertable[i,5:ncol(watertable)],type='b',col='white')}, error=function(e){})
    }
  })
  #file.remove(list.files(file.path(sourceDir,'results',Simul.name,'images'),full.names=T))
  #  dev.off()
  setwd(sourceDir)
}
