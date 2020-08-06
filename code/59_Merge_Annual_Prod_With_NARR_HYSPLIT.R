num<-as.numeric(Sys.getenv("Sim"))
i=num
for(i in 1:156){
  library(here)
  library(stringr)
  prjstring<-"+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs "
  geoprjstring<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  
  #Load function files-----------------------------------------------------
  load(here::here("data","nuclide_data.RData"))
  load(here::here("data","RadNet.RData"))
  radnet_sp<-spTransform(radnet,prjstring)
  
  load(here::here("data","NARR_2001.RData"))
  load(here::here("data","HYSPLIT_2001.RData"))
  load(here::here("data","Daily_PM_Data.RData"))
  #Extract Lead-210 Data
  pb_measurements<-nuc_data%>%filter(`Nuclides/Radiation`=="Lead-210")
  pb_measurements$Result=as.numeric(pb_measurements$Result)
  pb_measurements$`Combined Standard Uncertainty`=as.numeric(pb_measurements$`Combined Standard Uncertainty`)
  pb_measurements<-pb_measurements[!is.na(pb_measurements$Result),]
  pb_measurements[pb_measurements$Unit=="aCi/m3","Result"]=pb_measurements[pb_measurements$Unit=="aCi/m3","Result"]/1e6
  pb_measurements[pb_measurements$Unit=="aCi/m3","Combined Standard Uncertainty"]=as.numeric(pb_measurements[pb_measurements$Unit=="aCi/m3","Combined Standard Uncertainty"])/1e6
  pb_measurements$Unit="pCi/m3"
  pb_measurements$Year<-as.numeric(stringr::str_split(pb_measurements$`Sample Date`,"-",simplify = T)[,3])
  pb_measurements<-pb_measurements[pb_measurements$Year>10,]
  pb_measurements$city<-str_split(pb_measurements$Location,",",simplify = T)[,1]
  pb_measurements$state<-str_split(pb_measurements$Location,",",simplify = T)[,2]
  pb_measurements$state<-str_trim(pb_measurements$state,side="both")
  pb_measurements$city_state<-paste0(pb_measurements$city,",",pb_measurements$state)
  pb_measurements$year=as.numeric(paste0("20",pb_measurements$Year))
  pb_measurements=pb_measurements[!pb_measurements$state%in%c("HI","AK","PR"),]
  pb_measurements=pb_measurements[pb_measurements$city_state%in%radnet$city_state,]
  names(pb_measurements)[6]="Pb_210"
  #Extract Be-7 Data
  be_measurements<-nuc_data%>%filter(`Nuclides/Radiation`=="Beryllium-7")
  be_measurements$Result=as.numeric(be_measurements$Result)
  be_measurements$`Combined Standard Uncertainty`=as.numeric(be_measurements$`Combined Standard Uncertainty`)
  be_measurements<-be_measurements[!is.na(be_measurements$Result),]
  be_measurements[be_measurements$Unit=="aCi/m3","Result"]=be_measurements[be_measurements$Unit=="aCi/m3","Result"]/1e6
  be_measurements[be_measurements$Unit=="aCi/m3","Combined Standard Uncertainty"]=as.numeric(be_measurements[be_measurements$Unit=="aCi/m3","Combined Standard Uncertainty"])/1e6
  be_measurements$Unit="pCi/m3"
  be_measurements$Year<-as.numeric(stringr::str_split(be_measurements$`Sample Date`,"-",simplify = T)[,3])
  be_measurements<-be_measurements[be_measurements$Year>10,]
  be_measurements$city<-str_split(be_measurements$Location,",",simplify = T)[,1]
  be_measurements$state<-str_split(be_measurements$Location,",",simplify = T)[,2]
  be_measurements$state<-str_trim(be_measurements$state,side="both")
  be_measurements$city_state<-paste0(be_measurements$city,",",be_measurements$state)
  be_measurements$year=as.numeric(paste0("20",be_measurements$Year))
  be_measurements=be_measurements[!be_measurements$state%in%c("HI","AK","PR"),]
  be_measurements=be_measurements[be_measurements$city_state%in%radnet$city_state,]
  names(be_measurements)[6]="Be_7"
  
  pm_mass_data<-pm_mass_data%>%dplyr::select("Uni_ID","Longitude","Latitude","Arithmetic Mean","Date Local")
  names(pm_mass_data)<-c("Uni_ID","long","lat","mass","Date")
  load(here::here("data","PM_Monitors.RData"))
  pm_sp<-spTransform(pm_monitors,prjstring)
  rad_pm_link<-create_link_buffer(point = radnet_sp[i,],point_ID = "city_state",
                                  points = pm_sp,points_ID = "Uni_ID",width=50000)
  
  
  city_pb<-radnet@data[i,"city_state"]
  city_rad=pb_measurements%>%filter(city_state==city_pb)%>%left_join(be_measurements,by=c("city_state"="city_state","year"="year"))%>%dplyr::select("city_state",year,Pb_210,Be_7)
  
  city_narr=narr_data%>%filter(city_state==city_pb)
  city_hysplit=hys_info%>%filter(city_state==city_pb)
  city_pm=pm_mass_data%>%filter(Uni_ID%in%rad_pm_link$Uni_ID)
  city_pm=city_pm%>%group_by(Date)%>%summarise(pm=mean(mass,na.rm=T))
  city_pm=city_pm%>%filter(pm>0)
  
  if(file.exists(here::here("data","city_daily_prod_ext",paste0(city_pb,"_Daily_Prod_E2001.RData")))){
    load(here::here("data","city_daily_prod_ext",paste0(city_pb,"_Daily_Prod_E2001.RData")))
  }else{# IF there's no daily production data, we assume there's no production activity nearby and create an empty table.
    table=expand.grid(city=city_pb,date=city_narr$Date,radius=0:20)
    table$u_h_oil=0
    table$a_h_oil=0
    table$u_h_gas=0
    table$a_h_gas=0
    table$u_v_oil=0
    table$a_v_oil=0
    table$u_v_gas=0
    table$a_v_gas=0
    table$radius=20+table$radius*5
  }
  
  for(r in 0:20){
    city_prod=table%>%filter(radius==(20+r*5))
    city_prod$year=year(city_prod$date)
    city_data=city_rad%>%left_join(city_prod,by=c("year"="year"))%>%
      left_join(city_narr,by=c("date"="Date","city_state"="city_state","year"="year"))%>%
      left_join(city_hysplit,by=c("date"="Date","city_state"="city_state"))%>%
      left_join(city_pm,by=c("date"="Date"))
    city_annual_data=city_data%>%group_by(city_state,year)%>%summarise(Pb_210=mean(Pb_210,na.rm=T),
                                                                       Be_7=mean(Be_7,na.rm=T),
                                                                       radius=mean(radius,na.rm=T),
                                                                       u_h_oil=mean(u_h_oil,na.rm=T),
                                                                       a_h_oil=mean(a_h_oil,na.rm=T),
                                                                       u_h_gas=mean(u_h_gas,na.rm=T),
                                                                       a_h_gas=mean(a_h_gas,na.rm=T),
                                                                       u_v_oil=mean(u_v_oil,na.rm=T),
                                                                       a_v_oil=mean(a_v_oil,na.rm=T),
                                                                       u_v_gas=mean(u_v_gas,na.rm=T),
                                                                       a_v_gas=mean(a_v_gas,na.rm=T),
                                                                       temp=mean(air.2m,na.rm=T),
                                                                       rhum=mean(rhum.2m,na.rm=T),
                                                                       hpbl=mean(hpbl,na.rm=T),
                                                                       soilm=mean(soilm,na.rm=T),
                                                                       wvel=mean(vel,na.rm=T),
                                                                       c_prop=mean(c_prop,na.rm=T),
                                                                       pm=mean(pm,na.rm=T)
                                                                       )
    save(file=here::here("data","annual_city_rad_all_data",paste0(i,"_",r,"_All_Data_Ext.RData")),city_annual_data)
  }
}
