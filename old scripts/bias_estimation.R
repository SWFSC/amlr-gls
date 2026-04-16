bias_estimation<-function(gls.data=gls.TFdata_v30, USE.OLD.STUFF=FALSE, PLOT.RESULT=FALSE, BOOTSTRAP=TRUE, N.REPS=10000, KeepProp=0.75 ){
  #library(SDMTools)
  # from deprecated SDMTools at https://github.com/cran/SDMTools/blob/master/R/wt.mean.R
  # define function for weighted mean and variance:
  wt.mean <- function(x,wt) {
    s = which(is.finite(x*wt)); wt = wt[s]; x = x[s] #remove NA info
    return( sum(wt * x)/sum(wt) ) #return the mean
  }

  wt.var <- function(x,wt) {
    s = which(is.finite(x + wt)); wt = wt[s]; x = x[s] #remove NA info
    xbar = wt.mean(x,wt) #get the weighted mean
    return( sum(wt *(x-xbar)^2)*(sum(wt)/(sum(wt)^2-sum(wt^2))) ) #return the variance
  } 

  wt.sd <- function(x,wt) { 
    return( sqrt(wt.var(x,wt)) ) #return the standard deviation
  } 
  
  # create list of weekly errors for each control tag
  errors<-list()
  #errors[[1]]<-controltag_error(GLS="0589", GLS.DATA=gls.data, PLOT=TRUE, USE.OLD=USE.OLD.STUFF)
  #errors[[2]]<-controltag_error(GLS="1799", GLS.DATA=gls.data, PLOT=TRUE,USE.OLD=USE.OLD.STUFF)
  #errors[[3]]<-mobilecontrols(GLS="0583", ARGOS=102162, GLS.DATA=gls.data, PLOT=TRUE,USE.OLD=USE.OLD.STUFF)
  #errors[[4]]<-mobilecontrols(GLS="0588", ARGOS=102156, GLS.DATA=gls.data, PLOT=TRUE,USE.OLD=USE.OLD.STUFF)
  # 2014 data
  errors[[1]]<-controltag_error(GLS="3153", GLS.DATA=tt, PLOT=TRUE, USE.OLD=USE.OLD.STUFF)
  errors[[2]]<-controltag_error(GLS="3188", GLS.DATA=tt, PLOT=TRUE,USE.OLD=USE.OLD.STUFF)
  errors[[3]]<-controltag_error(GLS="3190", GLS.DATA=tt, PLOT=TRUE,USE.OLD=USE.OLD.STUFF)
  #
  # assemble each list component into a single data.frame for analysis
  #error.df<-rbind(errors[[1]], errors[[2]], errors[[3]], errors[[4]])
 error.df<-rbind(errors[[1]], errors[[2]], errors[[3]])
  #
  if(BOOTSTRAP){
    #
    # run a bootstrap procedure to estmate variance of the mean bias in latitude and longitude to include for final error structure
    bs.error<-bootstrap_errors(dat=errors, n.reps=N.REPS, keep.prop=KeepProp)

  }
  #
  # compute weekly mean bias in latitude and longitude
  latbias<-tapply(error.df$LatError, list(error.df$Week, error.df$Tag), mean)
  latbias<-latbias[order(dimnames(latbias)[[1]]),]
  longbias<-tapply(error.df$LongError, list(error.df$Week, error.df$Tag), mean)
  longbias<-longbias[order(dimnames(longbias)[[1]]),]
  totalbias<-tapply(error.df$TotalError, list(error.df$Week, error.df$Tag), mean)
  totalbias<-totalbias[order(dimnames(totalbias)[[1]]),]
  #
  # Reorder so week 1 is first, week 52 is last.
  weeks<-as.character(unique(error.df$Week))
  #
  # compute weekly mean error of latitude and longitude distance for all tags. this is used to estimate a weighted variance
  nweeks<-length(weeks)
  error.out<-matrix(0, ncol=5, nrow=nweeks)
  for(i in 1:nweeks){
      dati<-error.df[error.df$Week==weeks[i],]
      error.out[i,1]<-weeks[i]
      error.out[i,2]<-ifelse(sum(dati$N)==1, NA, wt.var(dati$LatError, dati$N))
      error.out[i,3]<-ifelse(sum(dati$N)==1, NA, wt.var(dati$LongError, dati$N))
      error.out[i,4]<-ifelse(sum(dati$N)==1, NA, wt.var(dati$TotalError, dati$N))
      error.out[i,5]<-ifelse(is.na(error.out[i,4]),0,sum(dati$N, na.rm=TRUE))
  }
  error.out<-data.frame(error.out)
  names(error.out)<-c("Week", "LatErrorWt", "LongErrorWt", "TotalErrorWt", "NpointsWt")
  #
  # ensure things are not factors....
  error.out$Week<-as.numeric(as.character(error.out$Week))
  error.out$LatErrorWt<-as.numeric(as.character(error.out$LatErrorWt))
  error.out$LongErrorWt<-as.numeric(as.character(error.out$LongErrorWt))
  error.out$TotalErrorWt<-as.numeric(as.character(error.out$TotalErrorWt))
  error.out$NpointsWt<-as.numeric(as.character(error.out$NpointsWt))
  #
  # reorder for sequential weeks
  error.out<-error.out[order(error.out$Week),]
  #
  # compute weekly variance from each tag --- I do not think this will be used
  #laterr<-tapply(error.df$LatError, list(error.df$Week, error.df$Tag), var)
  #laterr<-laterr[order(dimnames(laterr)[[1]]),]
  #longerr<-tapply(error.df$LongError, list(error.df$Week, error.df$Tag), var)
  #longerr<-longerr[order(dimnames(longerr)[[1]]),]
  #totalerr<-tapply(error.df$TotalError, list(error.df$Week, error.df$Tag), var)
  #totalerr<-totalerr[order(dimnames(totalerr)[[1]]),]
  # compute weekly count of points for each weekly error
  error.n<-tapply(error.df$LatError, list(error.df$Week, error.df$Tag), length)
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
  for(i in 1:Nweeks){
    #i(is.na(summary(latbias[as.numeric(as.character(dimnames(latbias)[[1]]))==i,]))){
    # lat.fix[i]<-NA
    # long.fix[i]<-NA
    # total.fix[i]<-NA
    # else {
    lat.fix[i]<-weighted.mean(latbias[i,], error.n[i,])
    long.fix[i]<-weighted.mean(longbias[i,],error.n[i,])
    total.fix[i]<-weighted.mean(totalbias[i,], error.n[i,])
  }    
   
  out<-data.frame(Week=as.numeric(as.character(dimnames(latbias)[[1]])), Lat_biaskm=lat.fix, Long_biaskm=long.fix, Total_biaskm=total.fix)
  out.new<-data.frame(Week=1:52)
  # for monthly use line below
  #out.new<-data.frame(Week=1:12)
  out<-merge(x=out.new, y=out, by="Week", all.x=TRUE)
  #
  # compute a smoothed prediction for each week
  tt<-with(out, loess(Lat_biaskm~Week, span=0.3, degree=2))
  tt.p<-predict(tt, newdata=1:52)
  # for monthly, use line below
  #tt.p<-predict(tt, newdata=1:12)
  out$latbias_smooth<-tt.p
  tt<-with(out, loess(Long_biaskm~Week, span=0.3, degree=2))
  tt.p<-predict(tt, newdata=1:52)
  # for monthly, use line below
  #tt.p<-predict(tt, newdata=1:12)
  out$longbias_smooth<-tt.p
  tt<-with(out, loess(Total_biaskm~Week, span=0.3, degree=2))
  tt.p<-predict(tt, newdata=1:52)
  # for monthly, use line below
  #tt.p<-predict(tt, newdata=1:12)
  out$totalbias_smooth<-tt.p
  out<-merge(out, error.out, by="Week", all.x=TRUE)
  #
  if(PLOT.RESULT){
    # plot
    windows()
    par(mfrow=c(3,1))
    plot(x=out[,1],y=out[,2], type="p", pch=1, col=1, xlab="Week of year", ylab="Lat bias (km) - weighted mean", axes=FALSE, ylim=c(-1500, 250))
    # add error bars
    tt<-na.omit(out)
    laterr<-sqrt(tt$LatErrorWt)
    yl<-tt$Lat_biaskm-laterr
    yu<-tt$Lat_biaskm+laterr
    arrows(x0=tt$Week, y0=yl, y1=yu, x1=tt$Week, code=3, angle=90, length=0.03)
    lines(x=out[,1], y=out[,5]) # smoothed latitude bias
    axis(1)
    axis(2, las=1)
    box()
    abline(h=0)
    plot(x=out[,1], y=out[,3],type="p", pch=16, col=1, xlab="Week of year", ylab=" Long bias (km) - weighted mean", axes=FALSE, ylim=c(-400,300))
    # add error bars
    longerr<-sqrt(tt$LongErrorWt)
    yl<-tt$Long_biaskm-longerr
    yu<-tt$Long_biaskm+longerr
    arrows(x0=tt$Week, y0=yl, y1=yu, x1=tt$Week, code=3, angle=90, length=0.03)
    lines(x=out[,1], y=out[,6]) # smoothed longitude
    axis(1)
    axis(2, las=1)
    box()
    abline(h=0)
    plot(x=out[,1], y=out[,4],type="p", pch=16, col=1, xlab="Week of year", ylab="Total bias (km) - weighted mean", axes=FALSE, ylim=c(-200, 1400))
    # add error bars
    totalerr<-sqrt(tt$TotalErrorWt)
    yl<-tt$Total_biaskm-totalerr
    yu<-tt$Total_biaskm+totalerr
    arrows(x0=tt$Week, y0=yl, y1=yu, x1=tt$Week, code=3, angle=90, length=0.03)
    lines(x=out[,1], y=out[,7]) # smoothed total error
    axis(1)
    axis(2, las=1)
    box()
    abline(h=0)
    #
    # plot
    windows()
    par(mfrow=c(2,1))
    plot(x=out[,1],y=out[,2], type="p", pch=16, col=1, xlab="Week of year", ylab="Lat bias (km) - weighted mean", axes=FALSE, ylim=c(-1500, 250))
    # add error bars
    tt<-na.omit(out)
    laterr<-sqrt(tt$LatErrorWt)
    yl<-tt$Lat_biaskm-laterr
    yu<-tt$Lat_biaskm+laterr
    arrows(x0=tt$Week, y0=yl, y1=yu, x1=tt$Week, code=3, angle=90, length=0.03)
    lines(x=out[,1], y=out[,5]) # smoothed latitude bias
    axis(1)
    axis(2, las=1)
    box()
    abline(h=0)
    plot(x=out[,1], y=out[,3],type="p", pch=16, col=1, xlab="Week of year", ylab="Long bias (km) - weighted mean", axes=FALSE, ylim=c(-400,300))
    # add error bars
    longerr<-sqrt(tt$LongErrorWt)
    yl<-tt$Long_biaskm-longerr
    yu<-tt$Long_biaskm+longerr
    arrows(x0=tt$Week, y0=yl, y1=yu, x1=tt$Week, code=3, angle=90, length=0.03)
    lines(x=out[,1], y=out[,6]) # smoothed longitude
    axis(1)
    axis(2, las=1)
    box()
    abline(h=0)
  }
  if(BOOTSTRAP){
    out<-merge(out, bs.error, by="Week", all.x=TRUE)
    # total variance is sum of two independent variances
    out$LatErr<-sqrt(out$LatErrorWt+out$LatVar)
    out$LongErr<-sqrt(out$LongErrorWt+out$LongVar)
    out$TotalErr<-sqrt(out$TotalErrorWt+out$TotalVar)
    # 
    # estimate smoothed predictions for total error
    tt<-with(out, loess(LatErr~Week, span=0.3, degree=2))
    tt.p<-predict(tt, newdata=1:52)
    out$Laterror_smooth<-tt.p
    windows()
    par(mfrow=c(2,2))
    plot(out$Week, out$LatErr, pch=16)
    lines(x=1:52, tt.p)
    tt<-with(out, loess(LongErr~Week, span=0.3, degree=2))
    tt.p<-predict(tt, newdata=1:52)
    out$Longerror_smooth<-tt.p
    plot(out$Week, out$LongErr, pch=16)
    lines(x=1:52, tt.p)
    tt<-with(out, loess(TotalErr~Week, span=0.3, degree=2))
    tt.p<-predict(tt, newdata=1:52)
    out$Totalerror_smooth<-tt.p
  }
  errors[[5]]<-out
  errors
}