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
library(lmerTest)
library(mgcv)
require("RPostgreSQL")
library(rpostgis)
library(wkb)
pw <- {
"koutrakis"
}
# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "Fracking_Data",
                 host = "localhost", port = 5432,
                 user = "postgres", password = pw)
rm(pw) # removes the password
source(here::here("code","00_Template_SQL_Command.R"))
source(here::here("code","00_Functions.R"))
RadNet_City_List<-read_csv(here::here("data","Processed-RadNet-Beta-citylist.csv"))
radnet_usgs_radio<-read_csv(here::here("data","radnet_radioraster_summary.csv"))
radnet_usgs_radio$city_state<-paste0(radnet_usgs_radio$city,",",radnet_usgs_radio$state)
radnet<-pgGetGeom(con,"RadNet_Sp","radnet_geom")
uwind<-stack(here::here("data","NARR","uwnd.nc"))
uwind<-uwind[[338:469]]
vwind<-stack(here::here("data","NARR","vwnd.nc"))
vwind<-vwind[[338:469]]
radnet_wind<-prepare_wind_field(radnet,uwind,vwind,key="city_state")

hpbl<-stack(here::here("data","NARR","hpbl.nc"))
hpbl<-hpbl[[338:469]]
radnet_hpbl<-as.data.frame(raster::extract(hpbl,radnet))

radnet_hpbl[,"city_state"]=as.factor(radnet@data[,"city_state"])
radnet_hpbl<-gather(radnet_hpbl,time,hpbl,-"city_state")
radnet_hpbl[,"Date"]<-as.Date(substr(radnet_hpbl[,"time"],2,11),format("%Y.%m.%d"))
radnet_hpbl[,"Year"]<-year(radnet_hpbl[,"Date"])
radnet_hpbl[,"Month"]<-month(radnet_hpbl[,"Date"])
radnet_hpbl[,"Day"]<-as.numeric(mday(radnet_hpbl[,"Date"]))
radnet_hpbl[radnet_hpbl$Day<15,"Month"]<- radnet_hpbl[radnet_hpbl$Day<15,"Month"]-1
radnet_hpbl<-radnet_hpbl[!duplicated(radnet_hpbl[,c("city_state","Year","Month")]),]
radnet_hpbl<-radnet_hpbl[,c("city_state","hpbl","Year","Month")]

rhum<-stack(here::here("data","NARR","rhum.nc"))
rhum<-rhum[[338:469]]
radnet_rhum<-as.data.frame(raster::extract(rhum,radnet))

radnet_rhum[,"city_state"]=as.factor(radnet@data[,"city_state"])
radnet_rhum<-gather(radnet_rhum,time,rhum,-"city_state")
radnet_rhum[,"Date"]<-as.Date(substr(radnet_rhum[,"time"],2,11),format("%Y.%m.%d"))
radnet_rhum[,"Year"]<-year(radnet_rhum[,"Date"])
radnet_rhum[,"Month"]<-month(radnet_rhum[,"Date"])
radnet_rhum[,"Day"]<-as.numeric(mday(radnet_rhum[,"Date"]))
radnet_rhum[radnet_rhum$Day<15,"Month"]<- radnet_rhum[radnet_rhum$Day<15,"Month"]-1
radnet_rhum<-radnet_rhum[!duplicated(radnet_rhum[,c("city_state","Year","Month")]),]
radnet_rhum<-radnet_rhum[,c("city_state","rhum","Year","Month")]

for(r in c(25000,50000,75000,100000)){
  nuclide_cmd<-"
  SELECT avg(\"Result\") AS \"pb210\",EXTRACT(YEAR FROM to_date(\"Date\",'DD-MON-YY')) AS \"YEAR\",\"Location\" AS \"city_state\"
  FROM \"Nuclide_Measurement\"
  WHERE \"Nuclides\"='Lead-210'
  GROUP BY \"Location\",\"Nuclides\",\"YEAR\"
  ORDER BY \"Location\",\"Nuclides\",\"YEAR\"
  "
  nuc<-dbGetQuery(con,nuclide_cmd)
  nuc<-nuc[nuc$YEAR>2006,]
  
  city_well_cmd<-"
  SELECT \"API/UWI\",\"Spud Date\",\"Completion Date\",\"city_state\",\"Last Prod Date\",\"Drill Type\",\"DI Basin\",ST_DistanceSphere(\"well_geom\",\"radnet_geom\") AS \"Dist\",180*ST_Azimuth(\"radnet_geom\",\"well_geom\")/pi() AS \"Dir\"
  FROM \"Well_Headers\",\"RadNet_Sp\" 
  WHERE ST_DistanceSphere(\"well_geom\",\"radnet_geom\")<RADIUS
  "
  cmd<-gsub(pattern="RADIUS",replacement=as.character(r),city_well_cmd)
  city_well_relation<-dbGetQuery(con,cmd)
  city_well_relation<-city_well_relation%>%filter(`Last Prod Date`>2006)
  prod_db <- tbl(con, "Well_Production_Table")
  prod_db<-prod_db%>%filter(API%in%city_well_relation$`API/UWI`&Prod_Year>2006&Prod_Year<2018)
  print(Sys.time())
  city_prod<-list()
  for(year in 2007:2017){
    prod_year<-prod_db%>%filter(Prod_Year==year)
    city_prod[[year-2006]]<-city_well_relation%>%
      inner_join(prod_year,by=c("API/UWI"="API"),copy=T)
    print(year)
  }
  city_prod<-do.call(rbind,city_prod)
  print(Sys.time())
  city_prod<-city_prod%>%filter(!is.na(Prod_Year))
  city_prod[city_prod$`Drill Type`=="D",]$`Drill Type`="H"
  city_prod<-city_prod%>%filter(!is.na(`DI Basin`))
  city_prod$Dist<-city_prod$Dist/1000
  city_prod_wind<-left_join(city_prod,radnet_wind,by=c("Prod_Year"="Year","Prod_Month"="Month","city_state"="city_state"))
  city_prod_wind<-city_prod_wind%>%filter(!is.na(uwind))
  #Selet wells with wind direction
  range=45
  low=city_prod_wind$Dir-range
  up=city_prod_wind$Dir+range
  
  r1l=ifelse(low<0,low+360,low)
  r1u=ifelse(low<0,360,up)
  r2l=ifelse(low<0,0,low)
  r2u=up
  
  r3u=ifelse(up>360,r2u-360,up)
  r3l=ifelse(up>360,0,low)
  r4u=ifelse(up>360,360,up)
  r4l=low
  
  w1<- city_prod_wind$dir<r1u&city_prod_wind$dir>r1l
  w2<- city_prod_wind$dir<r2u&city_prod_wind$dir>r2l
  w3<- city_prod_wind$dir<r3u&city_prod_wind$dir>r3l
  w4<- city_prod_wind$dir<r4u&city_prod_wind$dir>r4l
  w<-w1|w2|w3|w4
  city_prod_wind$Within<-w
  
  rad_prod<-city_prod_wind%>%
    group_by(city_state,Prod_Year,Prod_Month,`Drill Type`)%>%
    summarise(sum_oil=sum(Monthly_Oil),
              sum_gas=sum(Monthly_Gas),
              sum_oil_wind=sum(Monthly_Oil[Within]),
              sum_gas_wind=sum(Monthly_Gas[Within]),
              num_oil=sum(Monthly_Oil>0),
              num_gas=sum(Monthly_Gas>0),
              num_oil_wind=sum(Monthly_Oil[Within]>0),
              num_gas_wind=sum(Monthly_Gas[Within]>0),
              basin=names(which.max(table(`DI Basin`))),
              dist=mean(Dist,na.rm=T),
              dist_wind=mean(Dist[Within],na.rm=T),
              first_order_oil_prod=sum(Monthly_Oil/Dist),
              first_order_oil_prod_wind=sum(Monthly_Oil[Within]/Dist[Within]),
              first_order_gas_prod=sum(Monthly_Gas/Dist),
              first_order_gas_prod_wind=sum(Monthly_Gas[Within]/Dist[Within]),
              second_order_oil_prod=sum(Monthly_Oil/Dist^2),
              second_order_oil_prod_wind=sum(Monthly_Oil[Within]/Dist[Within]^2),
              second_order_gas_prod=sum(Monthly_Gas/Dist^2),
              second_order_gas_prod_wind=sum(Monthly_Gas[Within]/Dist[Within]^2),
              first_density=sum(1/Dist),
              first_density_wind=sum(1/Dist[Within]),
              second_density=sum(1/Dist^2),
              second_density_wind=sum(1/Dist[Within]^2)
    )
  
  oil_prod<-production_reshape(rad_prod,"sum_oil","Drill Type",c("city_state","Prod_Year","Prod_Month","basin"),"Oil_Prod")
  oil_fst_prod<-production_reshape(rad_prod,"first_order_oil_prod","Drill Type",c("city_state","Prod_Year","Prod_Month","basin"),"Fst_Oil_Prod")
  oil_snd_prod<-production_reshape(rad_prod,"second_order_oil_prod","Drill Type",c("city_state","Prod_Year","Prod_Month","basin"),"Snd_Oil_Prod")
  gas_prod<-production_reshape(rad_prod,"sum_gas","Drill Type",c("city_state","Prod_Year","Prod_Month","basin"),"Gas_Prod")
  gas_fst_prod<-production_reshape(rad_prod,"first_order_gas_prod","Drill Type",c("city_state","Prod_Year","Prod_Month","basin"),"Fst_Gas_Prod")
  gas_snd_prod<-production_reshape(rad_prod,"second_order_gas_prod","Drill Type",c("city_state","Prod_Year","Prod_Month","basin"),"Snd_Gas_Prod")
  
  oil_prod_wind<-production_reshape(rad_prod,"sum_oil_wind","Drill Type",c("city_state","Prod_Year","Prod_Month","basin"),"Oil_Prod_wind")
  oil_fst_prod_wind<-production_reshape(rad_prod,"first_order_oil_prod_wind","Drill Type",c("city_state","Prod_Year","Prod_Month","basin"),"Fst_Oil_Prod_Wind")
  oil_snd_prod_wind<-production_reshape(rad_prod,"second_order_oil_prod_wind","Drill Type",c("city_state","Prod_Year","Prod_Month","basin"),"Snd_Oil_Prod_Wind")
  gas_prod_wind<-production_reshape(rad_prod,"sum_gas_wind","Drill Type",c("city_state","Prod_Year","Prod_Month","basin"),"Gas_Prod_Wind")
  gas_fst_prod_wind<-production_reshape(rad_prod,"first_order_gas_prod_wind","Drill Type",c("city_state","Prod_Year","Prod_Month","basin"),"Fst_Gas_Prod_Wind")
  gas_snd_prod_wind<-production_reshape(rad_prod,"second_order_gas_prod_wind","Drill Type",c("city_state","Prod_Year","Prod_Month","basin"),"Snd_Gas_Prod_Wind")
  
  num_oil<-production_reshape(rad_prod,"num_oil","Drill Type",c("city_state","Prod_Year","Prod_Month","basin"),"Oil_Num")
  num_oil_wind<-production_reshape(rad_prod,"num_oil_wind","Drill Type",c("city_state","Prod_Year","Prod_Month","basin"),"Oil_Num_Wind")
  num_gas<-production_reshape(rad_prod,"num_gas","Drill Type",c("city_state","Prod_Year","Prod_Month","basin"),"Gas_Num")
  num_gas_wind<-production_reshape(rad_prod,"num_gas_wind","Drill Type",c("city_state","Prod_Year","Prod_Month","basin"),"Gas_Num_Wind")
  density_fst<-production_reshape(rad_prod,"first_density","Drill Type",c("city_state","Prod_Year","Prod_Month","basin"),"Density_Fst")
  density_fst_wind<-production_reshape(rad_prod,"first_density_wind","Drill Type",c("city_state","Prod_Year","Prod_Month","basin"),"Density_Fst_Wind")
  density_snd<-production_reshape(rad_prod,"second_density","Drill Type",c("city_state","Prod_Year","Prod_Month","basin"),"Density_Snd")
  density_snd_wind<-production_reshape(rad_prod,"second_density_wind","Drill Type",c("city_state","Prod_Year","Prod_Month","basin"),"Density_Snd_Wind")
  
  dist<-spread(rad_prod[,c("city_state","Prod_Year","Prod_Month","Drill Type","dist","basin")],key=`Drill Type`,value = "dist")
  names(dist)[5:6]<-paste0(names(dist)[5:6],"_Dist")
  names(dist)[2]<-"YEAR"
  names(dist)[3]<-"MONTH"
  
  
  radon_cmd<-"
    select \"city_state\",\"RI\"
  from \"RadNet_Sp\",\"US_Radon_Potential\"
  where ST_Contains(\"radon_potential_geom\",\"radnet_geom\")
  "
  rad_radon<-dbGetQuery(con,radon_cmd)
  names(rad_radon)[1]<-"city_state"
  names(rad_radon)[2]<-"Radon"
  
  total_beta_cmd<-"
  SELECT avg(result_amount) AS beta,12*date_part('year',age(date_trunc('month',\"collect_end\"),'2001-01-01'))+date_part('month',age(date_trunc('month',\"collect_end\"),'2001-01-01'))AS \"m_month\",EXTRACT(YEAR FROM \"collect_end\" ) AS \"YEAR\",EXTRACT(MONTH FROM \"collect_end\" ) AS \"MONTH\",\"city_state\"
  FROM \"Radnet_Measurement_Table\"
  GROUP BY \"m_month\",\"city_state\",\"YEAR\",\"MONTH\"
  "
  rad_beta<-dbGetQuery(con,total_beta_cmd)
  rad_beta<-rad_beta[rad_beta$m_month>0,]
  
  pm_mass_cmd<-"
  SELECT avg(\"pm_mass\") AS \"mass\",12*date_part('year',age(date_trunc('month',\"Date Local\"),'2001-01-01'))+date_part('month',age(date_trunc('month',\"Date Local\"),'2001-01-01'))AS \"m_month\",\"city\"
  FROM \"PM_MASS_Measurement\",(SELECT \"Uni_ID\" AS \"ID\",\"city_state\" AS \"city\" FROM  \"PM_Monitors\",\"RadNet_Sp\" WHERE ST_DistanceSphere(\"pm_monitor_geom\",\"radnet_geom\")<RADIUS)AS tbl
  WHERE \"Monitor_ID\" = \"ID\" AND (\"Date Local\" BETWEEN '01/01/2001' AND '12/31/2017')
  GROUP BY \"m_month\",\"city\"
  "
  cmd<-gsub(pattern="RADIUS",replacement=as.character(r),pm_mass_cmd)
  rad_pm_mass<-dbGetQuery(con,cmd)
  names(rad_pm_mass)[3]<-"city_state"
  
  pm_spec_cmd<-"
  SELECT avg(\"pm_spec\") AS \"spec\",\"Parameter Name\",12*date_part('year',age(date_trunc('month',\"Date Local\"),'2001-01-01'))+date_part('month',age(date_trunc('month',\"Date Local\"),'2001-01-01'))AS \"m_month\",\"city\"
  FROM \"PM_Spec_Measurement\",(SELECT \"Uni_ID\" AS \"ID\",\"city_state\" AS \"city\" FROM  \"PM_Monitors\",\"RadNet_Sp\" WHERE ST_DistanceSphere(\"pm_monitor_geom\",\"radnet_geom\")<RADIUS)AS tbl
  WHERE \"Monitor_ID\" = \"ID\" AND (\"Date Local\" BETWEEN '01/01/2001' AND '12/31/2018')
  GROUP BY \"m_month\",\"Parameter Name\",\"city\"
  "
  cmd<-gsub(pattern="RADIUS",replacement=as.character(r),pm_spec_cmd)
  rad_pm_spec<-dbGetQuery(con,cmd)
  names(rad_pm_spec)[2]<-"Parameter"
  rad_pm_spec<-(spread(data=rad_pm_spec,key=Parameter,value=spec))
  names(rad_pm_spec)[2]<-"city_state"
  names(rad_pm_spec)<-gsub(" ","_",names(rad_pm_spec))
  
  radnet_basin_cmd<-"
  SELECT \"city_state\",\"name\" AS \"basin_region\"
  FROM \"RadNet_Sp\", \"us_basin\"
  WHERE ST_Intersects(\"radnet_geom\",ST_Buffer(\"basin_geom\",0.2))
  ORDER BY \"city_state\"
  "
  radnet_basin_table<-dbGetQuery(con,radnet_basin_cmd)
  
  beta_wind<-left_join(rad_beta,radnet_wind,by=c("YEAR"="Year","MONTH"="Month","city_state"="city_state"))
  beta_wind<-beta_wind%>%filter(!is.na(uwind))
  beta_wind_hpbl<-left_join(beta_wind,radnet_hpbl,by=c("YEAR"="Year","MONTH"="Month","city_state"="city_state"))
  
  coast_dist_cmd<-"
  SELECT \"city_state\",min(ST_Distance(\"radnet_geom\",ST_Transform(\"coast_geom\",4326))) AS \"Coast_Dist\"
  FROM \"RadNet_Sp\",\"us_medium_shoreline\"
  GROUP BY \"city_state\"
  "
  radnet_coast_dist<-dbGetQuery(con,coast_dist_cmd)
  
  radnet_coords<-cbind.data.frame(radnet$city_state,coordinates(radnet))
  names(radnet_coords)<-c("city_state","Lon","Lat")
  
  
  rad_beta<-rad_beta%>%filter(YEAR>2006&YEAR<2018)
  rad_all<-left_join(rad_beta,gas_prod)
  rad_all<-left_join(rad_all,gas_prod_wind)
  rad_all<-left_join(rad_all,gas_fst_prod)
  rad_all<-left_join(rad_all,oil_prod)
  rad_all<-left_join(rad_all,oil_prod_wind)
  rad_all<-left_join(rad_all,oil_fst_prod)
  rad_all<-left_join(rad_all,num_gas)
  rad_all<-left_join(rad_all,num_gas_wind)
  rad_all<-left_join(rad_all,num_oil)
  rad_all<-left_join(rad_all,num_oil_wind)
  rad_all[is.na(rad_all)]<-0
  rad_all<-left_join(rad_all,nuc)
  rad_all<-left_join(rad_all,density_fst)
  rad_all<-left_join(rad_all,density_fst_wind)
  rad_all<-left_join(rad_all,dist)
  rad_all<-left_join(rad_all,rad_pm_mass)
  rad_all<-left_join(rad_all,rad_pm_spec)
  rad_all<-left_join(rad_all,rad_radon)
  rad_all<-left_join(rad_all,radnet_basin_table)
  rad_all<-left_join(rad_all,radnet_usgs_radio)
  rad_all<-left_join(rad_all,beta_wind_hpbl)
  rad_all<-left_join(rad_all,radnet_coast_dist)
  rad_all<-left_join(rad_all,radnet_coords)
  
  save(rad_all,file = here::here("data",paste0("beta_gas_oil_avg_wind_",r/1000,".RData")))
}


