plot_raw_data<-function(){
library(sf)
library(here)
library(dplyr)
library(ggplot2)
library(ggOceanMaps)

path<-here()
path1<-paste(path,"/data/gls_deployment_log.csv", sep="")
deploylog<-read.csv(path1, header=TRUE)
fy<-data.frame(Tag=deploylog$Tag, Year=deploylog$Field_Year, Spp=deploylog$Species_Code)


path2<-paste(path,"/data/gls_data_ncei.csv", sep="")
gls<-read.csv(path2, header=TRUE)%>%
  filter(Latitude< -30)%>%
  filter(Longitude <0)

gls<-merge(gls, fy, by="Tag")  


gls<-st_as_sf(gls, coords=c("Longitude","Latitude"))%>%
  st_set_crs(4326)%>%
  filter(Keep==TRUE)%>%
  st_transform(3031)

gls$Year<-as.factor(gls$Year) 

p2<-basemap(-30, bathymetry=TRUE)+
  geom_sf(data=gls, aes(color=Year), alpha=0.3)+
  scale_color_discrete(palette=c("magenta","orange","red"))+
  facet_wrap(~Spp)+
  #theme(legend.background = element_blank())+
  guides(color = guide_legend(override.aes = list(fill = NA)))
p2
}