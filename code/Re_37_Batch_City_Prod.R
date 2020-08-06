num<-as.numeric(Sys.getenv("Sim"))
#This is a modification version for re-submission
#The major features include:
#1. Add average distance within the buffer.
#2. Calculate two other angle range: 30 and 60.
#3. Calculat the downwind number for negative control.

library(raster)
library(rgeos)
library(dplyr)
library(splines)
library(rgeos)
library(lubridate)
library(here)

prjstring<-"+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs "
geoprjstring<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

#Load function files-----------------------------------------------------
source(here::here("code","Data_Downloading_Functions.R"))
load(here::here("data","Beta_Measurements_2001.RData"))
load(here::here("data","RadNet.RData"))
load(here::here("data","Wells_3rd.RData"))

coordinates(wells)<-~lon+lat
proj4string(wells)<-geoprjstring
wells<-spTransform(wells,prjstring)
radnet_sp<-spTransform(radnet,prjstring)


#load(here::here("data","Rad_NARR_2001.RData"))
load(here::here("data","NARR_2001.RData"))

i=num
city<-radnet@data[i,"city_state"]
overwrite=file.exists(here::here("data","Resub_city_daily_prod",
                       paste0(city,"_Daily_Prod_E2001.RData"))) 

if(!overwrite){
  rad_well_link<-create_link_buffer(point = radnet_sp[i,],point_ID = "city_state",points = wells,
                                    points_ID = "ApiNo",si_col =c("Pred_DrillType","DrillType","ProdType","SpudDate","CompDate","FirstProdDate","LastProdDate","GasCum","LiqCum","Status"),
                                    width=50000)
  if(!is.null(rad_well_link)){
    rad_well_link$ApiNo<-as.character(rad_well_link$ApiNo)
    city_narr=narr_data%>%filter(city_state==city)
    table=expand.grid(city=city,date=city_narr$Date,radius=0:6,angle=c(30,45,60))
    table$u_h=0
    table$u_v=0
    #Formalize the production type, converting the diverse oil prodcution activity to oil
    rad_well_link<-rad_well_link%>%mutate(ProdType=case_when(
      ProdType=="OIL" ~ "OIL",
      ProdType=="Gas" ~ "Gas",
      ProdType=="O&G" ~ "O&G",
      ProdType=="OIL (CYCLIC STEAM)" ~ "OIL"
    ))
    #According to EIA, most wells produce both gas and liquid some time, so I add another two 
    #columns indicating whether liq/gas was produced
    rad_well_link<-rad_well_link%>%mutate(Oil=case_when(
      is.na(LiqCum)~ FALSE,
      LiqCum==0 ~ FALSE,
      ProdType=="OIL" ~ TRUE,
      ProdType=="O&G" ~ TRUE,
      LiqCum>0 ~ TRUE))
    rad_well_link<-rad_well_link%>%mutate(Gas=case_when(
      is.na(GasCum)~ FALSE,
      GasCum==0 ~ FALSE,
      ProdType=="Gas" ~ TRUE,
      ProdType=="O&G" ~ TRUE,
      GasCum>0 ~ TRUE))
    rad_well_link<-rad_well_link%>%filter(Status!="PERMITTED")
    rad_well_link<-rad_well_link%>%filter(Status!="CANCELLED")
    rad_well_link<-rad_well_link%>%filter(Oil|Gas)
    
    rad_well_link<-rad_well_link%>%mutate(Active_Peroid=case_when(
      !is.na(SpudDate) & !is.na(CompDate) & !is.na(LastProdDate) ~ interval(start=SpudDate,end=LastProdDate),
      !is.na(SpudDate) & is.na(CompDate) & !is.na(LastProdDate) ~ interval(start= SpudDate, end=LastProdDate),
      is.na(SpudDate) & !is.na(CompDate) & !is.na(LastProdDate) ~  interval(start=CompDate,end=LastProdDate),
      is.na(SpudDate) & is.na(CompDate) & !is.na(LastProdDate) ~ interval(start=FirstProdDate,end=LastProdDate),
      !is.na(SpudDate) & !is.na(CompDate) ~ interval( start =  SpudDate, end=CompDate)
    ))
    rad_well_link<-rad_well_link%>%mutate(LastProdDate=case_when(
      is.na(Active_Peroid) ~ as.Date("1990-01-01"),
      !is.na(Active_Peroid) ~ LastProdDate
    ))
    for(row in 1:nrow(table)){
      paras=table[row,]
      #bottom=0+paras$radius*5
      up=20+paras$radius*5
      metes=city_narr%>%filter(Date==paras$date)
      well_ext=rad_well_link%>%
        filter(ymd(paras$date)>int_start(Active_Peroid),dist<up)%>%
        mutate(dir=pi*ifelse(dir>0,dir,360+dir)/180)
      wind_dir=pi*metes$dir/180
      angle=paras$angle
      well_ext=well_ext%>%
        mutate(angle_dif=abs(180*atan2(sin(dir-wind_dir),
                                       cos(dir-wind_dir))/pi))
      wells_upwind=well_ext%>%filter(
        angle_dif<angle
      )
      wells_downwind=well_ext%>%filter(
        angle_dif>(180-angle)
      )
      table[row,c("d_h")]=wells_downwind%>%filter(Pred_DrillType=="H")%>%count()
      table[row,c("d_v")]=wells_downwind%>%filter(Pred_DrillType=="V")%>%count()
      table[row,c("d_hd")]=wells_downwind%>%filter(Pred_DrillType=="H")%>%summarise(hd=mean(dist))
      table[row,c("d_vd")]=wells_downwind%>%filter(Pred_DrillType=="V")%>%summarise(hd=mean(dist))
      
      
      
      table[row,c("u_h")]=wells_upwind%>%filter(Pred_DrillType=="H")%>%count()
      table[row,c("u_v")]=wells_upwind%>%filter(Pred_DrillType=="V")%>%count()
      table[row,c("u_hd")]=wells_upwind%>%filter(Pred_DrillType=="H")%>%summarise(hd=mean(dist))
      table[row,c("u_vd")]=wells_upwind%>%filter(Pred_DrillType=="V")%>%summarise(hd=mean(dist))
      
      
      table[row,"radius"]=20+5*table[row,"radius"]
      if(row%%1000==0){
        print(paste0(Sys.time(),"_",row," % ",nrow(table)))
      }
    }
    save(file=here::here("data","Resub_city_daily_prod",paste0(city,"_Daily_Prod_E2001.RData")),table) 
  } 
}else{
  print(paste0(num," Alreadt Exist!"))
}
