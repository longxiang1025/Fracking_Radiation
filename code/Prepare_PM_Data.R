#' ---
#' title: "Fracking Production and Radiation"
#' author: "Longxiang Li"
#' date: "`r format(Sys.Date())`"
#' output: 
#'      html_document:
#'       toc: true
#'       fig_caption: yes
#' ---
#' 
#+ load-library, message = F, echo = F
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

study_period<-interval(ymd("2007-01-01"),ymd("2016-12-31"))
prod_period<-interval(ymd("2007-01-01"),ymd("2018-09-10"))
prjstring<-"+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs "
geoprjstring<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#+ load-data and merge all PM mass data, message=F,echo=F
pm_mass_monitor_list<-load("data/PM_Mass/PM_MONITOR_ID_CONVERSION_TABLE.RData")
pm_mass_files<-list.files("data/PM_Mass/",full.names = T)
pm_mass_container<-list()
for(year in 2007:2016){
  annual_pm<-read_csv(pm_mass_files[grepl(as.character(year),pm_mass_files)])%>%
    dplyr::select(c("State Code","County Code","Site Num","Arithmetic Mean","Date Local"))
  annual_pm$ID<-paste0(annual_pm$`State Code`,annual_pm$`County Code`,annual_pm$`Site Num`,year)
  annual_pm<-left_join(annual_pm,PM_MONITOR_ID_CONVERSION_TABLE,by="ID")
  pm_mass_container[[year-2006]]<-annual_pm
}
pm_mass_data<-do.call(rbind,pm_mass_container)
save(file="data/PM_Mass/Daily_PM_Data.RData",pm_mass_data)
#+ load-data and merge all PM speciation data, message=F,echo=F
load("data/PM_Spec/Para_List.RData")
pm_spec_files<-list.files("data/PM_Spec/",full.names = T)
pm_spec_container<-list()
for(year in 2007:2016){
  annual_spec<-read_csv(pm_spec_files[grepl(as.character(year),pm_spec_files)])
  annual_spec<-dplyr::filter(annual_spec,annual_spec$`Parameter Name`%in%para.list$Para)
  annual_spec<-dplyr::select(annual_spec,c("State Code","County Code","Site Num","Parameter Name","Arithmetic Mean","Date Local"))
  annual_spec$ID<-paste0(annual_spec$`State Code`,annual_spec$`County Code`,annual_spec$`Site Num`,year)
  annual_spec<-left_join(annual_spec,PM_MONITOR_ID_CONVERSION_TABLE,by="ID")
  annual_spec<-dplyr::select(annual_spec,c("Parameter Name","Arithmetic Mean","Date Local","Uni_ID","x","y"))
  pm_spec_container[[year-2006]]<-annual_spec
}
pm_spec_data<-do.call(rbind,pm_spec_container)
save(file="data/PM_Mass/Daily_PM_Spec_Data.RData",pm_spec_data)
#+ link PM Mass&Speciation site with RadNet city_list
PM_site_list<-PM_MONITOR_ID_CONVERSION_TABLE[,c("Uni_ID","x","y")]
coordinates(PM_site_list)<-~x+y
proj4string(PM_site_list)<-prjstring
PM_site_list<-spTransform(PM_site_list,CRS(geoprjstring))
PM_site_list$Spec_site<-0
PM_site_list@data[PM_site_list$Uni_ID%in%unique(pm_spec_data$Uni_ID),]$Spec_site<-1

rad<-read_csv(("data/Processed-RadNet-Beta-byMeas.csv")) %>% 
  mutate(month = lubridate::month(collect_start),
         season = ifelse(month %in% c(12, 1, 2), "Winter", 
                         ifelse(month %in% c(3, 4, 5), "Spring", 
                                ifelse(month %in% c(6, 7, 8), "Summer", "Fall")))) %>% 
  mutate(season = factor(season, levels = c("Winter", "Spring", "Summer", "Fall"))) %>% 
  rename(days_b4_measurement = days_b4_meas, collection_length = meas_length) %>% 
  dplyr::select(-drop_days_b4_meas) %>% 
  dplyr::filter(!Rosner.out & !manual.out)%>%
  dplyr::filter((collect_start%within%study_period)&(collect_end%within%study_period))
city_list<-read_csv("data/Processed-RadNet-Beta-citylist.csv")
coordinates(city_list)<-~lon+lat
proj4string(city_list)<-geoprjstring
rad$Date<-floor_date(rad$collect_start, "month")

RadNet_Mass_Table<-list()
RadNet_Spec_Table<-list()
f1<-0
f2<-0
for(i in 1:length(city_list)){
  city<-city_list[i,]
  buffer<-buffer(city,width=50000)
  pm_sites<-crop(PM_site_list,buffer)
  if(!is.null(pm_sites)){
    pm_sites$City_state<-city$city_state
    if(length(pm_sites)>0){
      f1<-f1+1
      RadNet_Mass_Table[[f1]]<-pm_sites@data
      pm_spec_sites<-pm_sites[pm_sites$Spec_site==1,]
      if(length(pm_spec_sites)>0){
        f2<-f2+1
        RadNet_Spec_Table[[f2]]<-pm_spec_sites@data
      }
    } 
  }
}
RadNet_Mass_Table<-do.call(rbind,RadNet_Mass_Table)
RadNet_Spec_Table<-do.call(rbind,RadNet_Spec_Table)
save(RadNet_Mass_Table,RadNet_Spec_Table,file="data/PM_City_PairTable_50B.RData")
#+ Vefity the correlation between Radiation and PM2.5 Mass. The temporal unit here is month.
rad_pm_container<-list()
f<-0
for(i in 1:length(city_list)){
  city<-city_list[i,]
  city_rad<-filter(rad,rad$city_state==city$city_state)
  city_pm_mass_list<-filter(RadNet_Mass_Table,RadNet_Mass_Table$City_state==city$city_state)
  city_pm_mass_data<-filter(pm_mass_data,pm_mass_data$Uni_ID%in%city_pm_mass_list$Uni_ID)
  city_pm_mass_data<-city_pm_mass_data[city_pm_mass_data$`Arithmetic Mean`>1,]
  ins<-interval(ymd("2007-01-01"),ymd(city_pm_mass_data$`Date Local`))
  if(nrow(city_pm_mass_data)>0){
    city_pm_mass_data$month<-(12)*as.period(ins)%>%year()+as.period(ins)%>%month()+1
    city_pm_mass_data_monthly<-city_pm_mass_data%>%group_by(month)%>%summarise(mean_PM=mean(`Arithmetic Mean`,na.rm=T))
    ins<-interval(ymd("2007-01-01"),ymd(city_rad$collect_end))
    if(nrow(city_rad)>0){
      f<-f+1
      city_rad$month<-(12)*as.period(ins)%>%year()+as.period(ins)%>%month()+1
      city_rad_data_monthly<-city_rad%>%group_by(month)%>%summarise(mean_rad=mean(result_amount,na.rm=T))
      city_rad_pm<-left_join(city_rad_data_monthly,city_pm_mass_data_monthly,by="month")
      city_rad_pm$city<-city$city_state
      rad_pm_container[[f]]<-city_rad_pm
    }
  }
}
rad_pm_data<-do.call(rbind,rad_pm_container)
rad_pm_data<-rad_pm_data[(!is.na(rad_pm_data$mean_PM)),]


