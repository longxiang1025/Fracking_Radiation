num<-as.numeric(Sys.getenv("Sim"))
i=num
library(here)
prjstring<-"+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs "
geoprjstring<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

#Load function files-----------------------------------------------------
source(here::here("code","Data_Downloading_Functions.R"))
load(here::here("data","Beta_Measurements_2001.RData"))
load(here::here("data","RadNet.RData"))
radnet_sp<-spTransform(radnet,prjstring)

load(here::here("data","NARR_2001.RData"))
load(here::here("data","HYSPLIT_2001.RData"))
load(here::here("data","Daily_PM_Data.RData"))
pm_mass_data<-pm_mass_data%>%dplyr::select("Uni_ID","Longitude","Latitude","Arithmetic Mean","Date Local")
names(pm_mass_data)<-c("Uni_ID","long","lat","mass","Date")
load(here::here("data","PM_Monitors.RData"))
pm_sp<-spTransform(pm_monitors,prjstring)

rad_pm_link<-create_link_buffer(point = radnet_sp[i,],point_ID = "city_state",
                                  points = pm_sp,points_ID = "Uni_ID",width=100000)


city<-radnet@data[i,"city_state"]
city_rad=rad%>%filter(city_state==city)
city_rad<-city_rad%>%group_by(city_state,collect_start,collect_end)%>%
  summarise(gross_beta=mean(gross_beta))
city_rad$duration=interval(city_rad$collect_start,city_rad$collect_end-1)
city_narr=narr_data%>%filter(city_state==city)
city_hysplit=hys_info%>%filter(city_state==city)
city_pm=pm_mass_data%>%filter(Uni_ID%in%rad_pm_link$Uni_ID)
city_pm=city_pm%>%group_by(Date)%>%summarise(pm=mean(mass,na.rm=T))
city_pm=city_pm%>%filter(pm>0)

if(file.exists(here::here("data","Resub_city_daily_prod",paste0(city,"_Daily_Prod_E2001.RData")))){
  load(here::here("data","Resub_city_daily_prod",paste0(city,"_Daily_Prod_E2001.RData")))
}else{# IF there's no daily production data, we assume there's no production activity nearby and create an empty table.
  table=expand.grid(city=city,date=city_narr$Date,radius=0:6,angle=c(30,45,60))
  table$d_h=0
  table$u_h=0
  table$d_hd=NA
  table$u_hd=NA
  table$d_v=0
  table$u_v=0
  table$d_vd=NA
  table$d_vh=NA
  table$radius=20+table$radius*5
}
for(r in 0:6){
  for( a in c(30,45,60)){
    city_prod=table%>%filter(radius==(20+r*5),angle==a)
    city_data=city_rad%>%left_join(city_narr)%>%
      filter(Date%within%duration)%>%
      left_join(city_prod,by=c("Date"="date"))%>%
      left_join(city_hysplit)%>%
      left_join(city_pm)
    save(file=here::here("data","Resub_city_daily_all",paste0("c_",i,"_",r,"_",a,"_All_Data_Ext.RData")),city_data)
  }
}

