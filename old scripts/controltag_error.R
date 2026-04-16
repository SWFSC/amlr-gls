controltag_error<-function(GLS="0589", GLS.DATA=gls.TFdata30, PLOT=TRUE, USE.OLD=FALSE){
  #
  #
  dataloss<-numeric(5)
  Control.dat<-GLS.DATA[GLS.DATA$Tag==GLS,]
  dataloss[1]<-dim(Control.dat)[1]
  # 
  # First order of business: exclude clearly erroneous points (i.e., abs(Latitude) > 90 and any latitude in the northern hemisphere, and any abs(Longitude) > 180)
  Control.dat<-Control.dat[abs(Control.dat$TFLatitude)<90,] # exclude any point not within -90S to 90N
  dataloss[2]<-dim(Control.dat)[1]
  Control.dat<-Control.dat[Control.dat$TFLatitude<0,] # exclude any point in northern hemisphere
  dataloss[3]<-dim(Control.dat)[1]
  Control.dat<-Control.dat[abs(Control.dat$TFLongitude)<180,] # exclude any remaining longitude not between -180 and 180
  dataloss[4]<-dim(Control.dat)[1]
  # exclude points unlikely based on speed filter
  Control.dat<-Control.dat[Control.dat$TFKeep,]
  dataloss[5]<-dim(Control.dat)[1]
  #
  datalossdf<-data.frame(OrigN=dataloss[1],Poles=dataloss[2],NHemi=dataloss[3], BadLongs=dataloss[4], Speed=dataloss[5], ArgosLoss=0)
  print(datalossdf)
  # Control tag coordinates - TRUTH
  Cape.Lat<- -62.46248
  Cape.Long<- -60.7919
  #
  # Estimate daily error of TF estimates as function of TRUTH coordinates
  #Lat.err<-Control.dat$TFLatitude-Cape.Lat
  #Long.err<-Control.dat$TFLongitude-Cape.Long
  #
  # Calculate distance of each error in km
  Lat.err.km<-acos((sin(Control.dat$TFLatitude*pi/180)*sin(Cape.Lat*pi/180))+(cos(Control.dat$TFLatitude*pi/180)*cos(Cape.Lat*pi/180)*cos(Cape.Long*pi/180-Cape.Long*pi/180)))*6382.8
  Long.err.km<-acos((sin(Cape.Lat*pi/180)*sin(Cape.Lat*pi/180))+(cos(Cape.Lat*pi/180)*cos(Cape.Lat*pi/180)*cos(Cape.Long*pi/180-Control.dat$TFLongitude*pi/180)))*6382.8
  Total.err.km<-acos((sin(Control.dat$TFLatitude*pi/180)*sin(Cape.Lat*pi/180))+(cos(Control.dat$TFLatitude*pi/180)*cos(Cape.Lat*pi/180)*cos(Cape.Long*pi/180-Control.dat$TFLongitude*pi/180)))*6382.8
  Lat.err.km<-ifelse(Control.dat$TFLatitude>Cape.Lat, -Lat.err.km, Lat.err.km)
  Long.err.km<-ifelse(Control.dat$TFLongitude>Cape.Long, -Long.err.km, Long.err.km)
  #
  # For each week, calculate an average (bias) and standard error (error) of the GLS position estimate forLatitude and Longitude
  weekofyear<-strftime(Control.dat$Date, format="%U")
  # For each week, calculate an average (bias) and standard error (error) of the GLS position estimate forLatitude and Longitude
  #weekofyear<-strftime(Control.dat$Date, format="%m")
  #
  if(!USE.OLD){
    errors<-data.frame(Date=Control.dat$Date, Week=weekofyear, Tag=rep(GLS, length(weekofyear)), LatError=Lat.err.km, LongError=Long.err.km, TotalError=Total.err.km)
    errors<-errors[order(errors$Week),]
    errors.n<-tapply(errors$Tag, errors$Week, length)
    errors$N<-rep(errors.n, errors.n)
    #errorfile<-paste("c:/users/jth/desktop/animal tagging/errors", GLS, sep="")
    #save(errors, file=errorfile)
   } else {  
     print("here")
    # 
    # calculate weekly mean and standard deviation of errors
    # note that tapply will re-order the weeks as numeric, so if data span the newyear, december data will be after january data, for example
    #Lat.Bias<-tapply(Lat.err, weekofyear, FUN=mean)
    #Long.Bias<-tapply(Long.err, weekofyear, FUN=mean)
    #Lat.Error<-tapply(Lat.err,weekofyear, FUN=sd)
    #Long.Error<-tapply(Long.err, weekofyear, FUN=sd)
    Lat.Bias.km<-tapply(Lat.err.km, weekofyear, FUN=mean)
    Lat.Error.km<-tapply(Lat.err.km, weekofyear, FUN=sd)
    Long.Bias.km<-tapply(Long.err.km, weekofyear, FUN=mean)
    Long.Error.km<-tapply(Long.err.km, weekofyear, FUN=sd)
    Total.Bias<-tapply(Total.err.km, weekofyear, FUN=mean)
    Total.Error<-tapply(Total.err.km, weekofyear, FUN=sd)
    Position.Count<-tapply(Lat.err.km, weekofyear, FUN=length)
    #
    # assemble error data
    # if tags spans a year, order on week number because tapply (above) does this for all the data
    WEEK<-(as.numeric(as.character(unique(weekofyear))))
    WEEK<-WEEK[order(WEEK)]
    errors<-data.frame(Week=as.factor(WEEK), N=Position.Count, LatBias_km=Lat.Bias.km, LatError_km=Lat.Error.km, LongBias_km=Long.Bias.km, LongError_km=Long.Error.km,  TotalBias_km=Total.Bias, TotalError_km=Total.Error)
    #
    if(PLOT){
      # plot error as function of week for Latitude
      dayofyear<-strftime(Control.dat$Date, format="%j")
      windows()
      par(mfrow=c(2,1))
      plot(x=dayofyear, y=Lat.err.km, type="p", pch=1, cex=0.5, col=1, axes=FALSE, xlab="Day of Year", ylab="Latitude bias (km)")
      axis(1)
      axis(2, las=1)
      box()
      abline(h=0)
      day2<-(as.numeric(unique(weekofyear))*7)-3.5
      # for montly estimates use line below
      #day2<-(as.numeric(unique(weekofyear))*30)-15
      points(day2, Lat.Bias.km, pch=16, col="red")
      arrows(x0=day2, y0=Lat.Bias.km-Lat.Error.km, x1=day2, y1=Lat.Bias.km+Lat.Error.km, length=0.05, angle=90, code=3, col="red")
      # plot error as function of week for Longitude
      dayofyear<-strftime(Control.dat$Date, format="%j")
      plot(x=dayofyear, y=Long.err.km, type="p", pch=1, cex=0.5, col=1, axes=FALSE, xlab="Day of Year", ylab="Longitude bias (km)")
      axis(1)
      axis(2, las=1)
      box()
      abline(h=0)
      day2<-(as.numeric(unique(weekofyear))*7)-3.5
      # for montly estimates use line below
      #day2<-(as.numeric(unique(weekofyear))*30)-15
      points(day2, Long.Bias.km, pch=16, col="red")
      arrows(x0=day2, y0=Long.Bias.km-Long.Error.km, x1=day2, y1=Long.Bias.km+Long.Error.km, length=0.05, angle=90, code=3, col="red")
    }
  }  
  # return result
  errors
  #print(str(errors))
  # end of file
}