library(SplitR)
library(here)
library(raster)
library(rgeos)
library(lubridate)
#get_met_narr(years = year,path_met_files = "/n/scratchlfs/koutrakis_lab/HYSPLIT/")
nday<-as.numeric(Sys.getenv("Sim"))
setwd("/n/holyscratch01/koutrakis_lab/longxiang/HYSPLIT/")

p_date<-nday+as.Date("2000-01-01")

if(!file.exists(paste0("/n/koutrakis_lab/lab/Fracking_Radiation/data/daily_HYSPLIT_H/",p_date,".RData"))){
  load("/n/koutrakis_lab/lab/Fracking_Radiation/data/Basic_Geodata/continent.RData")
  load("/n/koutrakis_lab/lab/Fracking_Radiation/data/RadNet.RData")
  daily_HYSPLIT_data<-matrix(nrow=length(radnet),ncol=3)
  daily_HYSPLIT_data<-as.data.frame(daily_HYSPLIT_data)
  names(daily_HYSPLIT_data)<-c("city_state","Date","c_prop")
  daily_HYSPLIT_data$city_state<-radnet$city_state
  daily_HYSPLIT_data$Date<-p_date
  for(r in 1:length(radnet)){
    dir.create(paste0("/n/holyscratch01/koutrakis_lab/longxiang/HYSPLIT/",r,"_",nday,"/"),recursive = T)
    trajectory<-hysplit_trajectory(lat= signif(coordinates(radnet[r,])[2], digits = 4),lon= signif(coordinates(radnet[r,])[1], digits = 4),
                                   height = 1000,
                                   model_height = 20000,
                                   duration = 72,
                                   run_period = as.character(p_date),
                                   daily_hours = c(0,6,12,18),direction = "backward",met_type = "narr",
                                   extended_met = FALSE,
                                   return_traj_df = T,
                                   exec_dir = paste0("/n/holyscratch01/koutrakis_lab/longxiang/HYSPLIT/Traj/",r,"_",nday,"/"),
                                   met_dir = "/n/holyscratch01/koutrakis_lab/longxiang/HYSPLIT/NARR",
                                   traj_name=paste0(r,"-",nday))
    coordinates(trajectory)<-~lon+lat
    proj4string(trajectory)<-proj4string(continent)
    inland<-trajectory[continent,]
    daily_HYSPLIT_data[r,"c_prop"]=length(inland)/length(trajectory)
    print(paste(Sys.time(),radnet[r,]$city_state,daily_HYSPLIT_data[r,"c_prop"]))
    unlink(paste0("/n/scratchlfs/koutrakis_lab/HYSPLIT/",r,"_",nday,"/"), recursive = TRUE)
  }
  save(file=paste0("/n/koutrakis_lab/lab/Fracking_Radiation/data/daily_HYSPLIT_H/",p_date,".RData"),daily_HYSPLIT_data)
}else{
  print(paste(Sys.time(),p_date,"Has been created."))
}
