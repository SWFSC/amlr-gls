plot_kde_fig<-function(){
  library(sf)
  library(dplyr)
  library(orsifronts)
  library(ggplot2)
  library(eks)
  library(ggOceanMaps)
  library(here)

  
  # 
  #read log file for appending tag info
  
  path<-here()
  path1<-paste(path,"/data/gls_deployment_log.csv", sep="")
  log<-read.csv(path1, header=TRUE)
  fy<-data.frame(Tag=log$Tag, Year=log$FieldYear)
  
  # read in the full GLS data set and filter for penguins from 2012, keeping only records that meet prior speed limits
  path2<-paste(path,"/data/gls_data_ncei.csv", sep="")
  gls<-read.csv(path2, stringsAsFactors = FALSE)%>%
    merge(., fy, by="Tag") %>%
    mutate(Month=month(as.Date(.$Date, format="%m/%d/%Y")))%>%
    filter(Month%in%c(4,5))%>%
    filter(Year==2012)%>%
    filter(Species_Code=="ADPE" | Species_Code=="CHPE")%>%
    filter(Keep==TRUE)
  
  
  # add directions to the remaining tags
  # this assignment is based on the mean bearing of tags during the feather growth period as determined for Hinke et al. 2015. Original code unknown.
  path3<-paste(path, "/data/peng_directions.csv", sep="")
  gls_directions<-read.csv(path3, header=TRUE)
  dirs<-data.frame(Tag=gls_directions$Tag, Direction=gls_directions$Direction)
  tt<-merge(gls, dirs, by="Tag")
  tt$Spp_Dir<-paste(tt$Species_Code,tt$Direction, sep="-")
  SPS<-unique(tt$Spp_Dir)
  
  # pull out the species by direction data, make sf and transform for kernal density analysis
  out<-list()
  for(i in 1:3){
    kdat<-tt[tt$Spp_Dir==SPS[i],]
    kdat<-st_as_sf(kdat, coords=c("Longitude", "Latitude"))%>%
      st_set_crs(4236)%>%
      st_transform(3031)
    out[[i]]<-st_kde(kdat)
  }
  
  # prepare orsifronts
  orsi<-st_as_sf(orsifronts)%>%
    st_set_crs(4326)%>%
    st_transform(3031)
  pf<-orsi[orsi$front=="pf",]
  saccf<-orsi[orsi$front=="saccf",]
  
  
  #plot the map
  p1<-basemap(-45, bathymetry=TRUE)+
    geom_sf(data=st_get_contour(out[[1]],cont=c(75)), fill="orange", alpha=0.5)+ 
    geom_sf(data=st_get_contour(out[[2]],cont=c(75)), fill="magenta", alpha=0.5)+ 
    geom_sf(data=st_get_contour(out[[3]],cont=c(75)),fill="green", alpha=0.5)+
    geom_sf(data=pf)+
    geom_sf(data=saccf)
  
  p1
}