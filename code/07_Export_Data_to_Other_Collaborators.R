#' ---
#' title: "Quick check of the difference between "
#' author: "Longxiang Li"
#' date: "`r format(Sys.Date())`"
#' output: 
#'      html_document:
#'       toc: true
#'       fig_caption: yes
#' ---
#' 
#' #Background
#' 
#' require("RPostgreSQL")
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
<<<<<<< HEAD
radnet_usgs_radio$city_state<-paste0(radnet_usgs_radio$city,",",radnet_usgs_radio$state)
=======
radnet_usgs_radio$city_state<-RadNet_City_List$city_state
>>>>>>> 00ce6ee7a313327320ef30d61b334ac1c55e58c7
radnet_basin_table<-dbGetQuery(con,radnet_basin_cmd)
for(r in c(25000,50000,75000,100000)){
  total_gas_cmd<-"
  SELECT sum(\"Monthly_Gas\") AS \"Prod\",COUNT(\"Monthly_Gas\") As \"Num\",\"city\",12*date_part('year',age(date_trunc('month',\"Monthly_Production_Date\"),'2001-01-01'))+date_part('month',age(date_trunc('month',\"Monthly_Production_Date\"),'2001-01-01'))AS \"m_month\",\"Type\"
  FROM \"Gas_Production_Table\",(SELECT \"API/UWI\" AS \"ID\",\"city_state\" AS \"city\" ,\"Drill Type\" AS \"Type\" FROM  \"Gas_Well_Headers\",\"RadNet_Sp\" WHERE ST_DistanceSphere(\"gas_well_geom\",\"radnet_geom\")<RADIUS)AS tbl
  WHERE \"API\" = \"ID\" AND (\"Monthly_Production_Date\" BETWEEN '01/01/2001' AND '12/31/2018')
  GROUP BY \"m_month\",\"city\",\"Type\"
  ORDER BY \"city\""
  cmd<-gsub(pattern="RADIUS",replacement=as.character(r),total_gas_cmd)
  rad_gas<-dbGetQuery(con,cmd)
  names(rad_gas)[3]<-"city_state"
  rad_gas_prod<-spread(data=rad_gas[,c("city_state","m_month","Type","Prod")],key=Type,value=Prod)
  names(rad_gas_prod)[3:4]<-c("H_Gas_Prod","V_Gas_Prod")
  
  rad_gas_num<-spread(data=rad_gas[,c("city_state","m_month","Type","Num")],key=Type,value=Num)
  names(rad_gas_num)[3:4]<-c("H_Gas_Num","V_Gas_Num")
  
  rad_gas<-left_join(rad_gas_num,rad_gas_prod)
  rad_gas[is.na(rad_gas)] <- 0
  rad_gas$Gas_Prod<-rad_gas$H_Gas_Prod+rad_gas$V_Gas_Prod
  rad_gas$Gas_Num<-rad_gas$H_Gas_Num+rad_gas$V_Gas_Num
  
  total_oil_cmd<-"
  SELECT sum(\"Monthly_Oil\") AS \"Prod\",COUNT(\"Monthly_Oil\") As \"Num\",\"city\",12*date_part('year',age(date_trunc('month',\"Monthly_Production_Date\"),'2001-01-01'))+date_part('month',age(date_trunc('month',\"Monthly_Production_Date\"),'2001-01-01'))AS \"m_month\",\"Type\"
  FROM \"Oil_Production_Table\",(SELECT \"API/UWI\" AS \"ID\",\"city_state\" AS \"city\" ,\"Drill Type\" AS \"Type\" FROM  \"Gas_Well_Headers\",\"RadNet_Sp\" WHERE ST_DistanceSphere(\"gas_well_geom\",\"radnet_geom\")<RADIUS)AS tbl
  WHERE \"API\" = \"ID\" AND (\"Monthly_Production_Date\" BETWEEN '01/01/2001' AND '12/31/2018')
  GROUP BY \"m_month\",\"city\",\"Type\"
  ORDER BY \"city\""
  cmd<-gsub(pattern="RADIUS",replacement=as.character(r),total_oil_cmd)
  rad_oil<-dbGetQuery(con,cmd)
  names(rad_oil)[3]<-"city_state"
  rad_oil_prod<-spread(data=rad_oil[,c("city_state","m_month","Type","Prod")],key=Type,value=Prod)
  names(rad_oil_prod)[3:4]<-c("H_Oil_Prod","V_Oil_Prod")
  
  rad_oil_num<-spread(data=rad_oil[,c("city_state","m_month","Type","Num")],key=Type,value=Num)
  names(rad_oil_num)[3:4]<-c("H_Oil_Num","V_Oil_Num")
  
  rad_oil<-left_join(rad_oil_num,rad_oil_prod)
  rad_oil[is.na(rad_oil)] <- 0
  rad_oil$Oil_Prod<-rad_oil$H_Oil_Prod+rad_oil$V_Oil_Prod
  rad_oil$Oil_Num<-rad_oil$H_Oil_Num+rad_oil$V_Oil_Num
  
  radon_cmd<-"
  SELECT \"citystate\",\"radon\"
  FROM \"Radon_Zones\",(SELECT \"city_state\" AS \"citystate\",\"STUSPS\" AS \"state_sn\",\"COUNTY_NAME\" AS \"county_name\" FROM \"RadNet_Sp\",\"us_state\",\"us_county\" WHERE ST_INTERSECTS(\"radnet_geom\",\"state_geom\") AND ST_INTERSECTS(\"radnet_geom\",\"county_geom\"))AS tb1
  WHERE \"state\"=\"state_sn\" AND \"county\"=\"county_name\"
  "
  rad_radon<-dbGetQuery(con,radon_cmd)
  names(rad_radon)[1]<-"city_state"
  
  total_beta_cmd<-"
  SELECT avg(result_amount) AS beta,12*date_part('year',age(date_trunc('month',\"collect_end\"),'2001-01-01'))+date_part('month',age(date_trunc('month',\"collect_end\"),'2001-01-01'))AS \"m_month\",EXTRACT(YEAR FROM \"collect_end\" ) AS \"YEAR\",EXTRACT(MONTH FROM \"collect_end\" ) AS \"MONTH\",\"city_state\"
  FROM \"Radnet_Measurement_Table\"
  GROUP BY \"m_month\",\"city_state\",\"YEAR\",\"MONTH\"
  "
  nuclide_cmd<-"
  SELECT avg(\"Result\"),EXTRACT(YEAR FROM to_date(\"Date\",'DD-MON-YY')) AS \"YEAR\",\"Location\",\"Nuclides\"
  FROM \"Nuclide_Measurement\"
  GROUP BY \"YEAR\",\"Location\",\"Nuclides\"
  ORDER BY \"Location\",\"Nuclides\",\"YEAR\"
  "
  rad_beta<-dbGetQuery(con,total_beta_cmd)
  rad_beta<-rad_beta[rad_beta$m_month>0,]
  rad_qs<-left_join(rad_beta,rad_gas,by=c("m_month","city_state"))
  rad_qs<-left_join(rad_qs,rad_oil,by=c("m_month","city_state"))
  rad_qs<-left_join(rad_qs,rad_radon)
  rad_qs$radon<-as.factor(rad_qs$radon)
  rad_qs[is.na(rad_qs$Gas_Prod),]$Gas_Prod<-0
  rad_qs[is.na(rad_qs$Gas_Num),]$Gas_Num<-0
  rad_qs[is.na(rad_qs$Oil_Prod),]$Oil_Prod<-0
  rad_qs[is.na(rad_qs$Oil_Num),]$Oil_Num<-0
  rad_qs[is.na(rad_qs$H_Gas_Prod),]$H_Gas_Prod<-0
  rad_qs[is.na(rad_qs$H_Gas_Num),]$H_Gas_Num<-0
  rad_qs[is.na(rad_qs$H_Oil_Prod),]$H_Oil_Prod<-0
  rad_qs[is.na(rad_qs$H_Oil_Num),]$H_Oil_Num<-0
  rad_qs[is.na(rad_qs$V_Gas_Prod),]$V_Gas_Prod<-0
  rad_qs[is.na(rad_qs$V_Gas_Num),]$V_Gas_Num<-0
  rad_qs[is.na(rad_qs$V_Oil_Prod),]$V_Oil_Prod<-0
  rad_qs[is.na(rad_qs$V_Oil_Num),]$V_Oil_Num<-0
  address<-strsplit(rad_qs$city_state,",")
  address<-do.call(rbind,address)
  state<-address[,2]
  rad_qs$state<-state
  rad_qs$state<-as.factor(rad_qs$state)
  rad_qs$city_state<-as.factor(rad_qs$city_state)
  
  nuc<-dbGetQuery(con,nuclide_cmd)
  names(nuc)[2]<-"year"
  nuc<-spread(data=nuc,key=Nuclides,value = avg)
  rad_qs<-left_join(rad_qs,nuc,by=c("YEAR"="year","city_state"="Location"))
  rad_qs<-left_join(rad_qs,radnet_usgs_radio)
  rad_qs[rad_qs$city_state=="MIAMI,FL",]$radon=3
  rad_qs<-left_join(rad_qs,radnet_basin_table)
  rad_qs[rad_qs$city_state=="DALLAS,TX",]$basin_name="FORT WORTH"
  names(rad_qs)<-gsub("-","_",names(rad_qs))
  rad_qs$city_state<-as.factor(rad_qs$city_state)
  rad_qs$Date<-as.Date(paste0(rad_qs$YEAR,"-",rad_qs$MONTH,"-01"))
  rad_qs$basin_name<-as.factor(rad_qs$basin_name)
  rad_qs<-rad_qs[rad_qs$YEAR<2018,]
  write.csv(rad_qs,file=here::here("data",paste0("beta_gas_oil_",r/1000,".csv")),na="")
  save(rad_qs,file = here::here("data",paste0("beta_gas_oil_",r/1000,".RData")))
  summary_result<-rad_qs%>%
    group_by(city_state)%>%
    summarise(min_gas_num=min(Gas_Num,na.rm = T),
              max_gas_num=max(Gas_Num,na.rm = T),
              min_gas_prod=min(Gas_Prod,na.rm=T),
              max_gas_prod=max(Gas_Prod,na.rm=T),
              min_gas_h_prod=min(H_Gas_Prod,na.rm=T),
              max_gas_h_prod=max(H_Gas_Prod,na.rm=T),
              min_gas_v_prod=min(V_Gas_Prod,na.rm=T),
              max_gas_v_prod=max(V_Gas_Prod,na.rm=T),
              min_gas_h_num=min(H_Gas_Num,na.rm = T),
              max_gas_h_num=max(H_Gas_Num,na.rm = T),
              min_gas_v_num=min(V_Gas_Num,na.rm = T),
              max_gas_v_num=max(V_Gas_Num,na.rm = T),
              min_oil_num=min(Oil_Num,na.rm = T),
              max_oil_num=max(Oil_Num,na.rm = T),
              min_oil_prod=min(Oil_Prod,na.rm=T),
              max_oil_prod=max(Oil_Prod,na.rm=T),
              min_oil_h_prod=min(H_Oil_Prod,na.rm=T),
              max_oil_h_prod=max(H_Oil_Prod,na.rm=T),
              min_oil_v_prod=min(V_Oil_Prod,na.rm=T),
              max_oil_v_prod=max(V_Oil_Prod,na.rm=T),
              min_oil_h_num=min(H_Oil_Num,na.rm = T),
              max_oil_h_num=max(H_Oil_Num,na.rm = T),
              min_oil_v_num=min(V_Oil_Num,na.rm = T),
              max_oil_v_num=max(V_Oil_Num,na.rm = T),
              min_num=min(Oil_Num,na.rm = T),
              max_num=max(Oil_Num,na.rm = T),
              mean_beta=mean(beta,na.rm=T),
              n_lead_210=length(Lead_210[!is.na(Lead_210)]),
              lead_range=paste(range(Date[!is.na(Lead_210)]),collapse = "~"),
              n_beta=length(beta),
              beta_range=paste(range(Date[!is.na(beta)]),collapse = "~"),
              gas_range=paste(range(Date[Gas_Num>0]),collapse = "~"),
              oil_range=paste(range(Date[Gas_Num>0]),collapse = "~")
    )
  is.na(summary_result)<-sapply(summary_result, is.infinite)
  write.csv(summary_result,file=here::here("data",paste0("summary",r/1000,".csv")))
}


