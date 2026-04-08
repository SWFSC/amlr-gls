compile_raw_data<-function(){
  
  # this function reads the individual .csv files from each tag and combines them
  # these raw data files are housed in a shared NMFS Google drive folders:
  # NMFS SWFSC ESD AMLR Science/Seabirds/GLS
  # The code expects these files to be in "data" folder in the working directory
  # The code will write a large .csv containing data from all tags to your local working directory
  # This raw archive will require error and bias checking to achieve estimated position locations
  # that are equivalent to version archived at NCEI.
  
  # A curated version of this large data set is archived at NCEI with a DOI TBD.
  
  
  library(here)
  
  wd<-here()
  path<-paste(wd, "/raw_data/", sep="")
  files<-list.files(path)
  n.files<-length(files)
  dat<-list()
  for(i in 1:n.files){
    tagid<-substr(files[i], 8, 11)
    pathi<-paste(path, files[i], sep="")
    tt<-read.csv(pathi, header=TRUE, skip=2, 
                 colClasses=c("integer",
                              "character","character",
                              rep("numeric", 20)))
    names(tt)[]<-c("Tag","Date","Time","Sunrise","Sunset",
                   "TFLatN", "TFLatS", "TFNoonN", "TFNoonS",
                   "TFLatErrN", "TFLatErrS", "TFLonErrN", "TFLonErrS",
                   "SST", "SSTDepth", "SSTTime", "MinIntTemp", "MaxPress",
                   "MinPress","Latitude", "Longitude", "TFLonN","TFLonS")
    tt$Tag<-tagid
    dat[[i]]<-tt
  }
  dat<-do.call("rbind",dat)
  write.csv(dat, "gls_raw.csv")
}