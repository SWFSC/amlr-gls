process_TFgls<-function(bias.correction=NULL, VMAX=3){
  # file to aggregate GLS data and amend info to each record for quick plotting
  library(argosfilter)
  library(sp)
  library(mapproj)
  #
  # read in TF gls data for a quick speed filter and plot to assess movement
  datapath<-"c:/users/jth/desktop/animal tagging/csvs/2014/"
  #
  # TF data housed in folder above with file name "LAT290_XXXX"., where, XXXX is the four-digit tag identifier.
  #
  #
  # if processing all tags, use the next 4 lines of code and commment out the lines  below for only 1 or a few tags
  tt.files<-list.files(path=datapath)
  #tt.files1<-unlist(strsplit(tt.files, split=".csv"))
  tt<-substr(tt.files,start=8,stop=11)
  n.files<-length(tt.files)
  # 
  # if processo only 1 or a few tags, specify whchh tags here:
  #tt<-c("1883")
  #n.files<-length(tt)
  #
  # for each of thet tags, pull out the tag number, amend some basic info, run the speed filter, and plot data
  #
  imports<-list()                                  
  for(i in 1:n.files){
    print(c(i, tt[i]))                                                                                     
    tagnum<-tt[i]
    # if processing many tags:
    #filepath<-paste(datapath, tt.files[i], sep="")
    #if processing only a few tags, use the line below:
    filepath<-paste("c:/users/jth/desktop/animal tagging/csvs/2014/LAT290_", tagnum,".csv", sep="")
    #
    dat<-read.table(filepath, skip=3, sep=",", as.is=2, colClasses="character", stringsAsFactors=FALSE)
    dat<-data.frame(Date=as.character(dat[,2]), TFLatitude=as.numeric(dat[,7]), TFLongitude=as.numeric(dat[,23]), RawLatitude=as.numeric(dat[,20]), RawLongitude=as.numeric(dat[,21]), Temp=as.numeric(dat[,10]), TempZ=as.numeric(dat[,11]))
    # add for records at beginning of record to be known-location start points for all tags
    SITE<-gls_log[gls_log$Tag==as.numeric(tagnum),]$Site
    SPP<-gls_log[gls_log$Tag==as.numeric(tagnum),]$Spp
    dat$Date<-strptime(dat$Date, format='%m/%d/%Y')
    dat$Date<-as.POSIXct(dat$Date)
    #print(min(dat$Date, na.rm=TRUE))
    date.new<-min(dat$Date, na.rm=TRUE)-c(7,6,5,4,3,2,1)*60*60*24
    CS.POS<-c(-62.4624, -60.7916)
    COPA.POS<-c(-62.2137, -58.4206)
    #print(SITE)
    #print(SPP)
    if(SITE=="CS" & SPP!="CONTROL"){
      dat.new<-data.frame(Date=date.new, TFLatitude=rep(CS.POS[1],7), TFLongitude=rep(CS.POS[2],7), RawLatitude=rep(CS.POS[1],7), RawLongitude=rep(CS.POS[2], 7), Temp=rep(0,7), TempZ=rep(0,7))
      dat<-rbind(dat.new, dat)
    }
    if(SITE=="COPA" & SPP!="CONTROL"){
      dat.new<-data.frame(Date=date.new, TFLatitude=rep(COPA.POS[1],7), TFLongitude=rep(COPA.POS[2],7), RawLatitude=rep(COPA.POS[1],7), RawLongitude=rep(COPA.POS[2], 7), Temp=rep(0,7), TempZ=rep(0,7))
      dat<-rbind(dat.new, dat)
    }
    #print(dat[1:10,1:5])
    dat<-na.omit(dat)
    datdim<-dim(dat)
    LatMed<-median(dat$TFLatitude)
    #if(LatMed>0){
    #  dat$TFLatitude<-dat$TFLatitude/-10
    #  print(paste("Adjusting TFLatS data by factor of -1/10 for Tag ", tt[i], "to correct error in data"))
    #}
    #
    # assign a week of year number to each data point for use with bias correction
    dat$Week<-as.numeric(as.character(strftime(dat$Date, format="%U")))
    #
    #
    #
    # APPEND TAG INFO
    #
    info<-gls_log[gls_log$Tag==as.numeric(tagnum),]
    dat$Tag<-rep(tagnum, datdim[1])
    dat$Spp<-rep(info$Spp, datdim[1])
    dat$Site<-rep(info$Site, datdim[1])
    dat$Study<-rep(info$Study, datdim[1])
    dat$Taxa<-rep(info$Taxa, datdim[1])
    dat$Stage<-rep(info$Stage, datdim[1])
    dat$FieldYearEnd<-rep(info$FieldYear, datdim[1])
    dat$Vmax<-rep(info$Vmax, datdim[1])
    dat$Loc.Qual<-rep("B", datdim[1]) # assign low quality for plotting code (legacy, not necessary at all)
    dat$Deployment<-rep(info$Deployment, datdim[1])
    #
    #
    if(dat$FieldYear[1]==2011){
      #
      # adjust Longitude for 3 hour correction because of error in tag setup
      dat$TFLongitude<-dat$TFLongitude+45
    }
    # uses bias correction, if specified, to adjust TFlatitude and TFlongitude data
    if(!is.null(bias.correction)){
      #
      #bias.correction<-bias_estimation(gls.data=tt, PLOT.RESULT=TRUE, N.REPS=1000, USE.OLD.STUFF=TRUE)
      # this uses the bias correction based on bias in degrees
      #bc<-bias.correction[[5]][,-c(2,3)] # remove two columns to have only week and the smoothed estimates of bias for latitude and longitude. 
      #dat<-merge(dat, bc, by="Week", all.x=TRUE)
      #
      # use bias correction based on degrees
      #dat$LatitudeDeg<-dat$TFLatitude-dat$smoothlat
      #dat$LongitudeDeg<-dat$TFLongitude-dat$smoothlong
      #
      # this uses a bias correction in km that is used to estimate new lat/long coordinates
      bc<-bias.correction[[5]][,c(1,5,6,21,22)] # retain only columns with bias correction and total error estimates from bootstrapping and weighted averages. 
      dat<-merge(dat, bc, by="Week", all.x=TRUE)
      #Rad is radius of earth, estimated in km
      Rad<-6382.8
      #estimate new latitude assuming constant longitude rhumb line
      bearing<-0
      latitude<-dat$TFLatitude*pi/180+dat$latbias_smooth/Rad*cos(bearing*pi/180)
      #latitude<-asin(sin(dat$TFLatitude*pi/180)*cos(dat$latbias_smooth/Rad)+cos(dat$TFLatitude*pi/180)*sin(dat$latbias_smooth/Rad)*cos(bearing*pi/180))
      # estimate new longitude assuming constant latitude rhumb line
      bearing<-90
      q<-cos(latitude)
      dlambda<-dat$longbias_smooth/Rad*sin(bearing*pi/180)/q
      longitude<-((dat$TFLongitude*pi/180+dlambda+pi) %% (2*pi))-pi
      #longitude<-dat$TFLongitude*pi/180+atan2(y=sin(bearing*pi/180)*sin(dat$longbias_smooth/Rad)*cos(dat$TFLatitude*pi/180), x=cos(dat$longbias_smooth/Rad)-sin(dat$TFLatitude*pi/180)*sin(latitude))  
      # end of file
      dat$Latitude<-latitude*180/pi
      dat$Longitude<-longitude*180/pi
    }
    # prior to running a speed filter, remove clearly erroneous points (north of equator, south of pole, beyond 180W or 0E)
    # for TF data
    if(is.null(bias.correction)){
      dat$TFLatitude<-ifelse(dat$TFLatitude > -40, NA, dat$TFLatitude)
      dat$TFLatitude<-ifelse(dat$TFLatitude < -90, NA, dat$TFLatitude)
      dat$TFLongitude<-ifelse(dat$TFLongitude > 0, NA, dat$TFLongitude)
      dat$TFLongitude<-ifelse(dat$TFLongitude < -180, NA, dat$TFLongitude)
    } else {
      dat$Latitude<-ifelse(dat$Latitude > -40, NA, dat$Latitude)
      dat$Latitude<-ifelse(dat$Latitude < -90, NA, dat$Latitude)
      dat$Longitude<-ifelse(dat$Longitude > 0, NA, dat$Longitude)
      dat$Longitude<-ifelse(dat$Longitude < -180, NA, dat$Longitude) 
    }
    #
    # run the speed filter
    # removal of the clearly bad points helps retain some mid-winter data that would otherwise  (wrongly) be excluded by speed filter
    # remove NA from dataset (no known position to estimate)
    dat<-na.omit(dat)
    # on TF positions
    #TFKeep<-vmask(lat=dat$TFLatitude, lon=dat$TFLongitude, dtime=dat$Date, vmax=VMAX)
    LC<-rep("B", length=length(dat$TFLatitude))
    #TFKeep<-sdafilter(lat=dat$TFLatitude, lon=dat$TFLongitude, dtime=dat$Date, vmax=VMAX,lc=LC, ang=c(15, 30, 60,90), distlim=c(1000,5000,10000,15000)*10)
    TFKeep<-sdafilter(lat=dat$TFLatitude, lon=dat$TFLongitude, dtime=dat$Date, vmax=VMAX,lc=LC)
    
    dat$TFKeep<-TFKeep=="not"
    if(!is.null(bias.correction)){
      # on bias-corrected positions
      #Keep<-vmask(lat=dat$Latitude, lon=dat$Longitude, dtime=dat$Date, vmax=VMAX)
      #dat$Keep<-Keep=="not"
      # on km based corrections
      #Keep<-vmask(lat=dat$Latitude, lon=dat$Longitude, dtime=dat$Date, vmax=VMAX)
      Keep<-sdafilter(lat=dat$Latitude, lon=dat$Longitude, dtime=dat$Date, vmax=VMAX,lc=LC)
      dat$Keep <- Keep=="not"
    }
    #print(dat[1:10,1:5])
    #
    # write out the modified data set
    #
    imports[[i]]<-dat
  }
  out<-do.call("rbind", imports)
  out$Month<-as.numeric(strftime(out$Date, format="%m"))
  out$Spp<-as.character(out$Spp)
  out$Site<-as.character(out$Site)
  out$Study<-as.character(out$Study)
  out$Taxa<-as.character(out$Taxa)
  out$Stage<-as.character(out$Stage)
  out$Deployment<-as.character(out$Deployment)
  if(is.null(bias.correction)){
    tTFile<-paste("c:/users/jth/desktop/animal tagging/gls_2014dataBeta_uncorrected_VMAX_", VMAX, ".csv", sep="")
    write.csv(out, file=tTFile)
  } else {
    #tTFile<-paste("c:/users/jth/desktop/animal tagging/gls_data_corrected_VMAX_", VMAX, ".csv", sep="")
    tTFile<-"c:/users/jth/desktop/animal tagging/gls_2014data_temp.csv"
    write.csv(out, file=tTFile)    
  }
  out
}