#' ---
#' title: "Drilling info of lower 48 states"
#' author: "Longxiang Li"
#' date: "`r format(Sys.Date())`"
#' output: 
#'      html_document:
#'       toc: true
#'       fig_caption: yes
#' ---
#' 
#+ load-data, message = F, echo = F
library(tidyverse)
library(here)
library(knitr)
library(kableExtra)
library(here)
library(dplyr)
library(lubridate)
library(reshape2)
library(raster)
library(gridExtra)
library(gamm4)
library(ggmap)
#+ load all header csv file and clean the data based on production (Some states sperated), message=FALSE,warning=F,echo=F, cache = T
study_period<-interval(ymd("2002-01-01"),ymd("2017-12-31"))
prod_period<-interval(ymd("2002-01-01"),ymd("2018-09-10"))
prjstring<-"+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs "
geoprjstring<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
header_oil_files<-list.files(here::here("data","oil_production_headers"),full.names = T)
header_gas_files<-list.files(here::here("data","gas_production_headers"),full.names = T)
header_files<-c(header_gas_files,header_oil_files)
header_list<-list()
header_names<-c( "API/UWI","Reservoir","Production Type","Drill Type","First Prod Date","Last Prod Date","Completion Date","Spud Date","Surface Latitude (WGS84)","Surface Longitude (WGS84)","Measured Depth (TD)","True Vertical Depth")
for(i in 1:length(header_files)){
  header<-read_csv((header_files[i]))
  header<-dplyr::filter(header,header$'Drill Type'=="V"|header$`Drill Type`=="H")
  header<-dplyr::filter(header,header$`Production Type`=="CBM" ||header$`Production Type`=="OIL" ||header$`Production Type`=="GAS" )
  header<-dplyr::filter(header,header$`Months Produced`>0)
  header<-dplyr::filter(header,header$`Last Prod Date`%within%prod_period)
  header<-dplyr::filter(header,(!is.na(header$`Surface Latitude (WGS84)`))&(!is.na(header$`Surface Longitude (WGS84)`)))
  header<-header[,header_names]
  header$Reservoir<-as.character(header$Reservoir)
  header$`Completion Date`<-as.Date(header$`Completion Date`)
  header_list[[i]]<-header
  #header$`Completion Date`
  #print(paste(i,nrow(header)))
}
header_list<-do.call(rbind,header_list)
names(header_list)[9:10]<-c("Latitude","Longitude")
header_list<-header_list[!is.na(header_list$`API/UWI`),]
header_list<-header_list[!header_list$`API/UWI`=="0",]
header_list<-distinct(header_list,header_list$`API/UWI`,.keep_all=T)
header_list<-filter(header_list,header_list$`Production Type`%in%c("CBM","GAS","OIL","OIL (CYCLIC STEAM)","O&G"))
header_list[header_list$`Production Type`=="OIL (CYCLIC STEAM)",]$`Production Type`<-"OIL"
header_list[header_list$`Production Type`=="O&G",]$`Production Type`<-"CBM"
header_list$`Production Type`=as.factor(header_list$`Production Type`)
coordinates(header_list)<-~Longitude+Latitude
proj4string(header_list)<-geoprjstring
save(header_list,file=here::here("data","All_Wells_Active_After_2001.RData"))
#+Plot the active production activity in the lower 48 states (Data from drillinginfo.com) ,echo=F,message=F,cache=T,fig.width=15,fig.height=9
load(here::here("data","Basic_Geodata/Boundaries.RData"))
plot(bound)
plot(header_list,add=T)
#+ load all production time series csv file and clean the data based on header list, message=FALSE,warning=F,echo=F, cache = T
production_gas_files<-list.files(here::here("data","gas_production_time_series"),full.names = T)
production_oil_files<-list.files(here::here("data","oil_production_time_series"),full.names = T)
production_list<-list()
production_names<-c("API/UWI","Monthly Production Date", "Monthly Gas","Monthly Oil","Monthly Water")
for(i in 1:length(production_files)){
  production<-read_csv(production_files[i])
  production<-production[,production_names]
  production<-filter(production,production$`Monthly Production Date`%within%study_period)
  production<-filter(production,production$`API/UWI`%in%header_list$`API/UWI`)
  production$`API/UWI`<-as.character(production$`API/UWI`)
  production$`Monthly Production Date`<-as.Date(production$`Monthly Production Date`)
  production$`Monthly Gas`<-as.numeric(production$`Monthly Gas`)
  production$`Monthly Water`<-as.numeric(production$`Monthly Water`)
  production$`Monthly Oil`<-as.numeric(production$`Monthly Oil`)
  production_list[[i]]<-production
  print(i)
}
production_data<-bind_rows(production_list)
#production_data<-do.call(rbind,production_list)
save(production_data,file=here::here("data","All_Wells_Active_After_2001_Production_Series.RData"))
#+ load all RadNet Data, message=FALSE,warning=F,echo=F, cache = T
rad<-read_csv(("C:/Users/lol087/Dropbox (Personal)/BetaExplore/data/Processed-RadNet-Beta-byMeas.csv")) %>% 
  mutate(month = lubridate::month(collect_start),
         season = ifelse(month %in% c(12, 1, 2), "Winter", 
                         ifelse(month %in% c(3, 4, 5), "Spring", 
                                ifelse(month %in% c(6, 7, 8), "Summer", "Fall")))) %>% 
  mutate(season = factor(season, levels = c("Winter", "Spring", "Summer", "Fall"))) %>% 
  rename(days_b4_measurement = days_b4_meas, collection_length = meas_length) %>% 
  dplyr::select(-drop_days_b4_meas) %>% 
  dplyr::filter(!Rosner.out & !manual.out)%>%
  dplyr::filter((collect_start%within%period)&(collect_end%within%period))
city_list<-read_csv("C:/Users/lol087/Dropbox (Personal)/Natural-Gas-Radiation/data/raw/Clean-RadNet-Beta-citylist.csv")
coordinates(city_list)<-~lon+lat
proj4string(city_list)<-geoprjstring
rad$Date<-floor_date(rad$collect_start, "month")

if(!exists("US_map")){US_map<-get_googlemap("United States",zoom = 4)}
if(!exists("US_map")){US_map<-get_googlemap("United States",zoom = 4)}
if(!exists("US_map")){US_map<-get_googlemap("United States",zoom = 4)}
if(!exists("US_map")){US_map<-get_googlemap("United States",zoom = 4)}

US_map<-ggmap(US_map)
buffer_list<-list()
rad_frac_data<-list()
f<-0
for(i in 1:length(city_list)){
  buffer_list[[3*(i-1)+1]]<-buffer(city_list[i,],width=15000)
  buffer_list[[3*(i-1)+2]]<-buffer(city_list[i,],width=30000)
  buffer_list[[3*(i-1)+3]]<-buffer(city_list[i,],width=50000)
  #US_map<-US_map+geom_polygon(data = fortify(buffer_list[[3*(i-1)+3]]),
  #                              aes(long, lat, group = group),
  #                              fill = "red", colour = "red", alpha = 0.2)
  city_header<-crop(header_list, buffer_list[[3*(i-1)+3]])
  if(!is.null(city_header)){
    if(nrow(city_header)>2){
      city_prod<-filter(production_data,production_data$'API/UWI'%in%city_header$'API/UWI')
      city_prod<-filter(city_prod,city_prod$`Monthly Production Date`%within%study_period)
      ncol<-(12)*as.period(period)%>%year()+as.period(period)%>%month()+1
      city_calendar<-matrix(nrow = nrow(city_header),ncol = ncol)
      for(j in 1:length(city_header)){
        id<-city_header[j,]$'API/UWI'
        time_series<-filter(city_prod,(city_prod$'API/UWI'==id)&(city_prod$`Monthly Production Date`%within%period))
        if(nrow(time_series)>0){
          ins<-interval(ymd("2007-01-01"),ymd(time_series$`Monthly Production Date`))
          months<-(12)*as.period(ins)%>%year()+as.period(ins)%>%month()+1
          city_calendar[j,months]<-time_series$`Monthly Gas` 
        }
      }
      city.month.prod<-colSums(city_calendar,na.rm=T)
      city.month.activewells<-colSums(!is.na(city_calendar))
      
      h_wells<-which(city_header$`Drill Type`=="H")
      if(length(h_wells)>1){
        h.month.prod<-colSums(city_calendar[h_wells,],na.rm = T)
        h.month.activewells<-colSums(!is.na(city_calendar[h_wells,])) 
      }else if(length(h_wells)==1){
        h.month.prod<-city_calendar[h_wells,]
        h.month.activewells<-(!is.na(city_calendar[h_wells,])) 
      }else{
        h.month.prod<-matrix(0,nrow = ncol,ncol = 1)
        h.month.activewells<-matrix(0,nrow = ncol,ncol = 1)
      }
      
      v_wells<-which(city_header$`Drill Type`=="V")
      if(length(v_wells)>1){
        v.month.prod<-colSums(city_calendar[v_wells,],na.rm = T)
        v.month.activewells<-colSums(!is.na(city_calendar[v_wells,])) 
      }else if(length(v_wells)==1){
        v.month.prod<-city_calendar[v_wells,]
        v.month.activewells<-(!is.na(city_calendar[v_wells,])) 
      }else{
        v.month.prod<-matrix(0,nrow = ncol,ncol = 1)
        v.month.activewells<-matrix(0,nrow = ncol,ncol = 1)
      }
      
      city.month.data<-cbind.data.frame(city.month.prod,city.month.activewells,h.month.prod,h.month.activewells,v.month.prod,v.month.activewells)
      city.month.data$Date<-seq(1,ncol)
      city.month.data$Date<-as.Date("2007-01-01") %m+% months(city.month.data$Date-1)
      names(city.month.data)<-c("Prod","Wells","H_Prod","H_Wells","V_Prod","V_Wells","Date")
      city_rad<-rad[rad$city_state==city_list[i,]$city_state,]
      if(nrow(city_rad)>10){
        f<-f+1
        city_rad<-city_rad[,c("result_amount","collect_start","collect_end")]
        city_rad$Date<-floor_date(city_rad$collect_start, "month")
        city_rad<-merge(city_rad,city.month.data,by="Date")
        city_rad$nmonth<-abs((12)*as.period(interval(city_rad$Date,ymd("2007-01-01")))%>%year()+as.period(interval(city_rad$Date,ymd("2007-01-01")))%>%month())
        city_rad$V_Prod<-city_rad$V_Prod/1000000
        city_rad$H_Prod<-city_rad$H_Prod/1000000
        city_rad$Prod<-city_rad$Prod/1000000
        city_rad$City<-city_list[i,]$city_state
        rad_frac_data[[f]]<-city_rad
        print(paste(length(city_header),"Frac sites around",city_list[i,]$city_state))
      }else{
        print(paste("No enough RadNet measurement",city_list[i,]$city_state))
      }
    }else{
      print(paste("No Frac within 50km",city_list[i,]$city_state))
    } 
  }else{
    print(paste("No Frac within 50km",city_list[i,]$city_state))
  }
}
rad_frac_data<-do.call(rbind,rad_frac_data)
save(rad_frac_data,file="C:/Users/lol087/Dropbox (Personal)/Fracking_RadNet/data/RadNet_And_Gas_50B.RData")
rad_frac_data$City<-as.factor(rad_frac_data$City)
rad_frac_data$log_beta<-log(rad_frac_data$result_amount)
g<-gam(log_beta~Prod+Wells+s(City,bs="re")+s(nmonth,k=40),data=rad_frac_data)
g_h<-gam(log_beta~H_Prod+H_Wells+s(City,bs="re")+s(nmonth,k=40),data=rad_frac_data)
g_v<-gam(log_beta~V_Prod+V_Wells+s(City,bs="re")+s(nmonth,k=40),data=rad_frac_data)

