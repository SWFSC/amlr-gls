bootstrap_errors<-function(dat=errors, n.reps=200, keep.prop=0.75){
  #library(SDMTools)
  #dat.all<-rbind(dat[[1]], dat[[2]], dat[[3]], dat[[4]])
  dat.all<-rbind(dat[[1]], dat[[2]], dat[[3]])
  dat<-dat.all
  rm(dat.all)
  # this function takes the daily error data and subsamples to come up with new weekly means for bootstrapping
  # set some global parameters for the bootstrap
  n.rows<-dim(dat)[1]
  index<-1:n.rows
  samp.size<-round(keep.prop*n.rows,0)
  # 
  # setup empty matrixes to house the bootstrap data
  tt.lat<-matrix(0, nrow=52, ncol=n.reps)
  tt.long<-matrix(0, nrow=52, ncol=n.reps)
  tt.total<-matrix(0, nrow=52, ncol=n.reps)
  #
  # run the boot strap n.reps times
  for(i in 1:n.reps){
      #
      # set up some indices for the bootstrap
      new.index<-sample(index, samp.size, replace=FALSE, prob=NULL)
      new.dat<-dat[new.index,]
      #
      # now compute the smooted estimates on the delete_n sampled sized
      # compute weekly mean bias in latitude and longitude
      latbias<-tapply(new.dat$LatError, list(new.dat$Week, new.dat$Tag), mean)
      latbias<-latbias[order(dimnames(latbias)[[1]]),]
      longbias<-tapply(new.dat$LongError, list(new.dat$Week, new.dat$Tag), mean)
      longbias<-longbias[order(dimnames(longbias)[[1]]),]
      totalbias<-tapply(new.dat$TotalError, list(new.dat$Week, new.dat$Tag), mean)
      totalbias<-totalbias[order(dimnames(totalbias)[[1]]),]
      # compute weekly mean error in latitude and longitude and total distance. Reorder so week 1 is first, week 52 is last.
      laterr<-tapply(new.dat$LatError, list(new.dat$Week, new.dat$Tag), var)
      #laterr<-laterr[order(dimnames(laterr)[[1]]),]
      #longerr<-tapply(new.dat$LongError, list(new.dat$Week, new.dat$Tag), var)
      #longerr<-longerr[order(dimnames(longerr)[[1]]),]
      #totalerr<-tapply(new.dat$TotalError, list(new.dat$Week, new.dat$Tag), var)
      #totalerr<-totalerr[order(dimnames(totalerr)[[1]]),]
      # compute weekly count of points for each weekly error
      error.n<-tapply(new.dat$LatError, list(new.dat$Week, new.dat$Tag), length)
      error.n<-error.n[order(dimnames(error.n)[[1]]),]
      # for weighted means, turn any NA into 0
      latbias<-ifelse(is.na(latbias), 0, latbias)
      #laterr<-ifelse(is.na(laterr), 0, laterr)
      longbias<-ifelse(is.na(longbias), 0, longbias)
      #longerr<-ifelse(is.na(longerr), 0, longerr)
      totalbias<-ifelse(is.na(totalbias), 0, totalbias)
      #totalerr<-ifelse(is.na(totalerr), 0, totalerr)
      error.n<-ifelse(is.na(error.n), 0, error.n)
      # now weighted means
      # compute weigthed weekly mean in kilometers
      Nweeks<-length(latbias[,1])
      lat.fix<-numeric(Nweeks)
      long.fix<-numeric(Nweeks)
      total.fix<-numeric(Nweeks)
      for(j in 1:Nweeks){
        #i(is.na(summary(latbias[as.numeric(as.character(dimnames(latbias)[[1]]))==i,]))){
        # lat.fix[i]<-NA
        # long.fix[i]<-NA
        # total.fix[i]<-NA
        # else {
        lat.fix[j]<-weighted.mean(latbias[j,], error.n[j,])
        long.fix[j]<-weighted.mean(longbias[j,],error.n[j,])
        total.fix[j]<-weighted.mean(totalbias[j,], error.n[j,])
      }    
      
      out<-data.frame(Week=as.numeric(as.character(dimnames(latbias)[[1]])), Lat_biaskm=lat.fix, Long_biaskm=long.fix, Total_biaskm=total.fix)
      out.new<-data.frame(Week=1:52)
      out<-merge(x=out.new, y=out, by="Week", all.x=TRUE)
      #
      # compute a smoothed prediction for each week
      tt<-with(out, loess(Lat_biaskm~Week, span=0.3, degree=2))
      tt.lat[,i]<-predict(tt, newdata=1:52)
      tt<-with(out, loess(Long_biaskm~Week, span=0.3, degree=2))
      tt.long[,i]<-predict(tt, newdata=1:52)
      tt<-with(out, loess(Total_biaskm~Week, span=0.3, degree=2))
      tt.total[,i]<-predict(tt, newdata=1:52)
      #
  } 
  # plot the first 100 hundred iterations or all the reps if less than 100 
  MAX<-ifelse(n.reps>100, 100, n.reps) 
  print(range(tt.lat, na.rm=TRUE))
  YLIM<-range(tt.lat, na.rm=TRUE)
  YLIM2<-range(tt.long, na.rm=TRUE)
  windows()
  par(mfrow=c(2,1))
  plot(x=1:52, y=tt.lat[,1], type="l", xlab="week", ylab="Latitude bias (km)", ylim=YLIM)
  for(k in 2:MAX){
      lines(x=1:52, y=tt.lat[,k])
  }
  plot(x=1:52, y=tt.long[,1], type="l", xlab="Week", ylab="Longitude bias (km)", ylim=YLIM2)
  for(k in 2:MAX){
    lines(x=1:52, y=tt.long[,k])
  }
  #plot(x=1:52, y=tt.total[,1], type="l", xlab="Week", ylab="Total bias (km)")
  #for(k in 2:MAX){
  #  lines(x=1:52, y=tt.total[,k])
  #}
  mean.func<-function(x){mean(x, na.rm=TRUE)}
  # compute the variance, since it's additive. this will be used to estimate total error
  # use the means as the value for bias correction
  # in bias_estimation(), the raw distnace values will be used to calculate the other error component and added to the "var" compoenents estimated here.
  var.func<-function(x){var(x, na.rm=TRUE)}
  out.latmean<-apply(tt.lat, 1, mean.func)
  out.latvar<-apply(tt.lat, 1, var.func)
  out.longmean<-apply(tt.long, 1, mean.func)
  out.longvar<-apply(tt.long, 1, var.func)
  out.totalmean<-apply(tt.total, 1, mean.func)
  out.totalvar<-apply(tt.total, 1, var.func)
  final<-data.frame(Week=1:52, LatMean=out.latmean, LatVar=out.latvar, LongMean=out.longmean, LongVar=out.longvar, TotalMean=out.totalmean, TotalVar=out.totalvar)
  final
  # end of file
}