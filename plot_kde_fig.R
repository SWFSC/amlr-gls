plot_kde_fig<-function(){
  library(sf)
  library(dplyr)
  library(orsifronts)
  library(ggplot2)
  library(eks)
  library(ggOceanMaps)
  # 
  
  # read in the full GLS data set and filter for penguins from 2012, keeping only records that meet prior speed limits
  path<-paste(here(),"/data/gls_data.csv", sep="")
  gls<-read.csv(path, stringsAsFactors = FALSE)%>%
    filter(FieldYearEnd==2012)%>%
    filter(Spp=="ADPE" | Spp=="CHPE")%>%
    filter(Month>3 & Month <6)%>%
    filter(Keep==TRUE)
  
  # add directions to the remaining tags
  # this assignment is based on the mean bearing of tags during the feather growth period as determeind for Hinke et al. 2015. Original code unknown.
  path<-paste(here(), "/data/peng_directions.csv", sep="")
  gls_directions<-read.csv(path, header=TRUE)
  dirs<-data.frame(Tag=gls_directions$Tag, Direction=gls_directions$Direction)
  tt<-merge(gls, dirs, by="Tag")
  tt$Spp_Dir<-paste(tt$Spp,tt$Direction, sep="-")
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