#The usage of this code is to extract drilling info based on the Lead-210 measurement
#The diff between dataset organized by lead-210 measurement and beta measurement is that the frequency of
#lead-210 is annual while the frequency of beta is 3 days. So, in this dataset, we need to aggregate the drill 
#information based on year.
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
RadNet_City_List<-read_csv(here::here("data","Processed-RadNet-Beta-citylist.csv"))
radnet_usgs_radio<-read_csv(here::here("data","radnet_radioraster_summary.csv"))
radnet_usgs_radio$city_state<-paste0(radnet_usgs_radio$city,",",radnet_usgs_radio$state)
for(r in c(25000,50000,75000,100000)){
  nuclide_cmd<-"
  SELECT avg(\"Result\") AS \"pb210\",EXTRACT(YEAR FROM to_date(\"Date\",'DD-MON-YY')) AS \"YEAR\",\"Location\" AS \"city_state\"
  FROM \"Nuclide_Measurement\"
  WHERE \"Nuclides\"='Lead-210'
  GROUP BY \"Location\",\"Nuclides\",\"YEAR\"
  ORDER BY \"Location\",\"Nuclides\",\"YEAR\"
  "
  nuc<-dbGetQuery(con,nuclide_cmd)
  nuc<-nuc[nuc$YEAR>2013,]
  
  total_gas_cmd<-"
  SELECT sum(\"Monthly_Gas\") AS \"Prod\",COUNT(DISTINCT \"API/UWI\")AS \"Acitive_Num\",COUNT(\"API/UWI\")AS \"Acc_Num\",\"city_state\",EXTRACT(YEAR FROM \"Monthly_Production_Date\") AS \"YEAR\",\"Type\"
  FROM \"Gas_Production_Table\", (SELECT \"API/UWI\",\"city_state\",\"Drill Type\" AS \"Type\" FROM \"Gas_Well_Headers\",\"RadNet_Sp\" WHERE ST_DistanceSphere(\"gas_well_geom\",\"radnet_geom\")<RADIUS)AS tbl
  WHERE \"API/UWI\"=\"API\" AND (\"Monthly_Production_Date\" BETWEEN '01/01/2014' AND '12/31/2016')
  GROUP BY \"YEAR\",\"city_state\",\"Type\"
  "
  cmd<-gsub(pattern="RADIUS",replacement=as.character(r),total_gas_cmd)
  rad_gas<-dbGetQuery(con,cmd)
  rad_gas_prod<-spread(data=rad_gas[,c("city_state","YEAR","Type","Prod")],key=Type,value=Prod)
  names(rad_gas_prod)[3:4]<-c("H_Gas_Prod","V_Gas_Prod")
  rad_gas_num<-spread(data=rad_gas[,c("city_state","YEAR","Type","Acitive_Num")],key=Type,value=Acitive_Num)
  names(rad_gas_num)[3:4]<-c("H_Gas_Num","V_Gas_Num")
  
  total_oil_cmd<-"
  SELECT sum(\"Monthly_Oil\") AS \"Prod\",COUNT(DISTINCT \"API/UWI\")AS \"Acitive_Num\",COUNT(\"API/UWI\")AS \"Acc_Num\",\"city_state\",EXTRACT(YEAR FROM \"Monthly_Production_Date\") AS \"YEAR\",\"Type\"
  FROM \"Oil_Production_Table\", (SELECT \"API/UWI\",\"city_state\",\"Drill Type\" AS \"Type\" FROM \"Oil_Well_Headers\",\"RadNet_Sp\" WHERE ST_DistanceSphere(\"oil_well_geom\",\"radnet_geom\")<RADIUS)AS tbl
  WHERE \"API/UWI\"=\"API\" AND (\"Monthly_Production_Date\" BETWEEN '01/01/2014' AND '12/31/2016')
  GROUP BY \"YEAR\",\"city_state\",\"Type\"
  "
  cmd<-gsub(pattern="RADIUS",replacement=as.character(r),total_oil_cmd)
  rad_oil<-dbGetQuery(con,cmd)
  rad_oil_prod<-spread(data=rad_oil[,c("city_state","YEAR","Type","Prod")],key=Type,value=Prod)
  names(rad_oil_prod)[3:4]<-c("H_Oil_Prod","V_Oil_Prod")
  rad_oil_num<-spread(data=rad_oil[,c("city_state","YEAR","Type","Acitive_Num")],key=Type,value=Acitive_Num)
  names(rad_oil_num)[3:4]<-c("H_Oil_Num","V_Oil_Num")
  
  pm_mass_cmd<-"
  SELECT avg(\"pm_mass\") AS \"mass\",EXTRACT(YEAR FROM \"Date Local\") AS \"YEAR\",\"city_state\"
  FROM \"PM_MASS_Measurement\",(SELECT \"Uni_ID\" AS \"ID\",\"city_state\" FROM  \"PM_Monitors\",\"RadNet_Sp\" WHERE ST_DistanceSphere(\"pm_monitor_geom\",\"radnet_geom\")<RADIUS)AS tbl
  WHERE \"Monitor_ID\" = \"ID\" AND (\"Date Local\" BETWEEN '01/01/2014' AND '12/31/2017')
  GROUP BY \"YEAR\",\"city_state\"
  "
  cmd<-gsub(pattern="RADIUS",replacement=as.character(r),pm_mass_cmd)
  rad_pm_mass<-dbGetQuery(con,cmd)
  
  pm_spec_cmd<-"
  SELECT avg(\"pm_spec\") AS \"spec\",\"Parameter Name\",EXTRACT(YEAR FROM \"Date Local\") AS \"YEAR\",\"city_state\"
  FROM \"PM_Spec_Measurement\",(SELECT \"Uni_ID\" AS \"ID\",\"city_state\" FROM  \"PM_Monitors\",\"RadNet_Sp\" WHERE ST_DistanceSphere(\"pm_monitor_geom\",\"radnet_geom\")<RADIUS)AS tbl
  WHERE \"Monitor_ID\" = \"ID\" AND (\"Date Local\" BETWEEN '01/01/2014' AND '12/31/2017')
  GROUP BY \"YEAR\",\"Parameter Name\",\"city_state\"
  "
  cmd<-gsub(pattern="RADIUS",replacement=as.character(r),pm_spec_cmd)
  rad_pm_spec<-dbGetQuery(con,cmd)
  names(rad_pm_spec)[2]<-"Parameter"
  rad_pm_spec<-(spread(data=rad_pm_spec,key=Parameter,value=spec))
  names(rad_pm_spec)<-gsub(" ","_",names(rad_pm_spec))
  
  radnet_basin_cmd<-"
  SELECT \"city_state\",\"name\" AS \"basin_name\"
  FROM \"RadNet_Sp\", \"us_basin\"
  WHERE ST_Intersects(\"radnet_geom\",\"basin_geom\")
  ORDER BY \"city_state\"
  "
  radnet_basin_table<-dbGetQuery(con,radnet_basin_cmd)
  
  radon_cmd<-"
  SELECT \"citystate\",\"radon\"
  FROM \"Radon_Zones\",(SELECT \"city_state\" AS \"citystate\",\"STUSPS\" AS \"state_sn\",\"COUNTY_NAME\" AS \"county_name\" FROM \"RadNet_Sp\",\"us_state\",\"us_county\" WHERE ST_INTERSECTS(\"radnet_geom\",\"state_geom\") AND ST_INTERSECTS(\"radnet_geom\",\"county_geom\"))AS tb1
  WHERE \"state\"=\"state_sn\" AND \"county\"=\"county_name\"
  "
  rad_radon<-dbGetQuery(con,radon_cmd)
  names(rad_radon)[1]<-"city_state"
  
  
  rad_all<-left_join(nuc,rad_gas_prod)
  rad_all<-left_join(rad_all,rad_gas_num)
  rad_all<-left_join(rad_all,rad_oil_prod)
  rad_all<-left_join(rad_all,rad_oil_num)
  rad_all[is.na(rad_all)] <- 0
  rad_all<-left_join(rad_all,rad_pm_mass)
  rad_all<-left_join(rad_all,rad_pm_spec)
  rad_all<-left_join(rad_all,radnet_basin_table)
  rad_all<-left_join(rad_all,rad_radon)
  
  save(rad_all,file=here::here("data",paste0("pb_gas_oil_",r/1000,".RData")))
}

