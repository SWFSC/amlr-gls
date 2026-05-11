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

# read and clean the raw data for obvious errors and nominal speed filter 
path2<-paste(path,"/data/gls_data_ncei.csv", sep="")
gls<-read.csv(path2, header=TRUE)%>%
  filter(Latitude< -30)%>%
  filter(Latitude>-90)%>%
  filter(Longitude <0)%>%
  filter(Keep==TRUE)

# append identifying info from the deployment logs
gls<-merge(gls, fy, by="Tag")  

# create sf object
gls<-st_as_sf(gls, coords=c("Longitude","Latitude"))%>%
  st_set_crs(4326)%>%
  st_transform(3031)

#prep for plotting
gls$Year<-as.factor(gls$Year) 

# create friendly plot labels
new_labels<-c("ADPE"="Adelie", "GEPE"="Gentoo", "CHPE"="Chinstrap", "AFS"="Fur seal", "BRSK"="Brown skua", "LS"="Leopard seal", "CONTROL"="Control")
#plot
p2<-basemap(-30, bathymetry=TRUE)+
  geom_sf(data=gls, aes(color=Year), alpha=0.3)+
  scale_color_discrete(palette=c("magenta","orange","red"))+
  facet_wrap(~Spp, labeller=as_labeller(new_labels))+
  #theme(legend.background = element_blank())+
  guides(color = guide_legend(override.aes = list(fill = NA)))
p2
}