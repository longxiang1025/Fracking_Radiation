setwd("C:/Users/lol087/Downloads/right one/")
files<-list.files(full.names = T)
header_files<-files[grep("Header",files)]
prod_files<-files[grep("Entity",files)]

library(readr)
library(dplyr)
library(lubridate)
wells_list<-list()
for(f in 1:length(header_files)){
  r_w<-read_csv(header_files[f])%>%dplyr::select(`API/UWI`,`Drill Type`,`Surface Latitude (WGS84)`,`Surface Longitude (WGS84)`,`DI Basin`,`Production Type`,`First Prod Date`,`Last Prod Date`,`Measured Depth (TD)`,`Well Count`,`First 6 Oil`,`First 24 Oil`,`First 6 Gas`,`First 24 Gas`)%>%
    filter(`API/UWI`!=0,!is.na(`Surface Latitude (WGS84)`),`Production Type`%in%c("GAS","OIL"," OIL (CYCLIC STEAM)","O&G"))
  print(r_w%>%group_by(`Drill Type`)%>%summarise(m_d=mean(`Measured Depth (TD)`,na.rm=T),oil_prop=mean(`First 6 Oil`/`First 24 Oil`,na.rm=T),gas_prop=mean(`First 6 Gas`/`First 24 Gas`,na.rm=T)))
  r_w$`API/UWI`<-as.character(r_w$`API/UWI`)
  wells_list[[f]]=r_w
}
wells<-bind_rows(wells_list)
names(wells)<-c("API/UWI","Drill_Type","Latitude","Longitude","Basin","Prod_Type","First_Prod_Date","Last_Prod_Date","Depth","Well_Count","F6_Oil","F24_Oil","F6_Gas","F24_Gas")
save(file="Wells_2nd.RData",wells)

prod_list<-list()
for(f in 1:length(prod_files)){
  r_w<-read_csv(prod_files[f])%>%dplyr::select(`API/UWI`,`Monthly Production Date`,`Monthly Oil`,`Monthly Gas`)%>%
    filter(`API/UWI`!=0,!(is.na(`Monthly Oil`)&is.na(`Monthly Gas`)),`Monthly Production Date`>as.Date("1999-12-31"),`Monthly Production Date`<as.Date("2018-01-01"))
  r_w$`API/UWI`<-as.character(r_w$`API/UWI`)
  prod_list[[f]]=r_w
}
prod<-bind_rows(prod_list)
names(prod)<-c("API/UWI","Date","Prod_Oil","Prod_Gas")
for(year in 2001:2017){
  annual_prod<-prod%>%filter(year(Date)==year)
  save(file=paste0("annual_prod/production_",year,".RData"),annual_prod)
}
