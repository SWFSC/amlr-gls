library(sf)
library(here)
library(dplyr)
library(ggplot2)
library(ggOceanMaps)

path<-here()
path<-paste(path,"/data/gls_data.csv", sep="")
gls<-read.csv(path, header=TRUE)%>%
  filter(Latitude< -30)%>%
  filter(Longitude <0)%>%
  mutate(Year=as.factor(FieldYearEnd))

gls<-st_as_sf(gls, coords=c("Longitude","Latitude"))%>%
  st_set_crs(4326)%>%
  filter(Keep==TRUE)%>%
  st_transform(3031)
 

p2<-basemap(-30, bathymetry=TRUE)+
  geom_sf(data=gls, aes(color=Year), alpha=0.3)+
  scale_color_discrete(palette=c("magenta","orange","red"))+
  facet_wrap(~Spp)+
  #theme(legend.background = element_blank())+
  guides(color = guide_legend(override.aes = list(fill = NA)))
p2