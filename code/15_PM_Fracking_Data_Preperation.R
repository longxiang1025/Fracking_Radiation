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
pm_monitors<-pgGetGeom(con,"PM_Monitors","pm_monitor_geom")
uwind<-stack(here::here("data","NARR","uwnd.nc"))
uwind<-uwind[[338:469]]
vwind<-stack(here::here("data","NARR","vwnd.nc"))
vwind<-vwind[[338:469]]
pm_wind<-prepare_wind_field(pm_monitors,uwind,vwind,key="Uni_ID")
pm_wind$Uni_ID<-as.numeric(as.character(pm_wind$Uni_ID))

hpbl<-stack(here::here("data","NARR","hpbl.nc"))
hpbl<-hpbl[[338:469]]
p_hpbl<-as.data.frame(raster::extract(hpbl,pm_monitors))

p_hpbl[,"Uni_ID"]=as.factor(pm_monitors@data[,"Uni_ID"])
p_hpbl<-gather(p_hpbl,time,hpbl,-"Uni_ID")
p_hpbl[,"Date"]<-as.Date(substr(p_hpbl[,"time"],2,11),format("%Y.%m.%d"))
p_hpbl[,"Year"]<-year(p_hpbl[,"Date"])
p_hpbl[,"Month"]<-month(p_hpbl[,"Date"])
p_hpbl[,"Day"]<-as.numeric(mday(p_hpbl[,"Date"]))
p_hpbl[p_hpbl$Day<15,"Month"]<- p_hpbl[p_hpbl$Day<15,"Month"]-1
p_hpbl<-p_hpbl[!duplicated(p_hpbl[,c("Uni_ID","Year","Month")]),]
p_hpbl<-p_hpbl[,c("Uni_ID","hpbl","Year","Month")]


r=20000

pm_mass_cmd<-"
SELECT avg(\"pm_mass\") AS \"mass\",12*date_part('year',age(date_trunc('month',\"Date Local\"),'2001-01-01'))+date_part('month',age(date_trunc('month',\"Date Local\"),'2001-01-01'))AS \"m_month\",\"Monitor_ID\",date_part('year',\"Date Local\") AS \"YEAR\",date_part('month',\"Date Local\") AS \"MONTH\" 
FROM \"PM_MASS_Measurement\"
WHERE (\"Date Local\" BETWEEN '01/01/2001' AND '12/31/2017')
GROUP BY \"m_month\",\"Monitor_ID\",\"YEAR\",\"MONTH\"
"
pm_mass<-dbGetQuery(con,pm_mass_cmd)

pm_spec_cmd<-"
 SELECT avg(\"pm_spec\") AS \"spec\",\"Parameter Name\",EXTRACT(YEAR FROM \"Date Local\") AS \"YEAR\",EXTRACT(MONTH FROM \"Date Local\") AS \"MONTH\",\"Monitor_ID\" AS \"Uni_ID\"
  FROM \"PM_Spec_Measurement\"
  WHERE \"Date Local\" BETWEEN '01/01/2001' AND '12/31/2017'
  GROUP BY \"YEAR\",\"MONTH\",\"Parameter Name\",\"Uni_ID\"
"
pm_spec<-dbGetQuery(con,pm_spec_cmd)
names(pm_spec)[2]<-"Parameter"
pm_spec<-(spread(data=pm_spec,key=Parameter,value=spec))
names(pm_spec)<-gsub(" ","_",names(pm_spec))
pm_mass_spec<-left_join(pm_mass,pm_spec,by=c("YEAR"="YEAR","MONTH"="MONTH","Monitor_ID"="Uni_ID"))

monitor_well_cmd<-"
SELECT \"API/UWI\",\"Spud Date\",\"Completion Date\",\"Uni_ID\",\"First Prod Date\",\"Last Prod Date\",\"Drill Type\",\"DI Basin\",ST_DistanceSphere(\"well_geom\",\"pm_monitor_geom\") AS \"Dist\",180*ST_Azimuth(\"pm_monitor_geom\",\"well_geom\")/pi() AS \"Dir\"
FROM \"Well_Headers\",\"PM_Monitors\" 
WHERE ST_DistanceSphere(\"well_geom\",\"pm_monitor_geom\")<RADIUS
"
cmd<-gsub(pattern="RADIUS",replacement=as.character(r),monitor_well_cmd)
print(Sys.time())
monitor_well_relation<-dbGetQuery(con,cmd)
prod_db <- tbl(con, "Well_Production_Table")
prod_db<-prod_db%>%filter(API%in%monitor_well_relation$`API/UWI`&Prod_Year>2001)
print(Sys.time())
monitor_prod<-monitor_well_relation%>%
  left_join(prod_db,by=c("API/UWI"="API"),copy=T)
print(Sys.time())
monitor_prod<-monitor_prod%>%filter(!is.na(Prod_Year))
monitor_prod<-monitor_prod%>%filter(Prod_Year<2018)
monitor_prod[monitor_prod$`Drill Type`=="D",]$`Drill Type`="H"
monitor_prod<-monitor_prod%>%filter(`Production Status`=="ACTIVE")
monitor_prod<-monitor_prod%>%filter(!is.na(`DI Basin`))
monitor_prod$Dist<-monitor_prod$Dist/1000
monitor_prod_wind<-left_join(monitor_prod,pm_wind,by=c("Prod_Year"="Year","Prod_Month"="Month","Uni_ID"="Uni_ID"))
monitor_prod_wind<-monitor_prod_wind%>%filter(!is.na(uwind))

range=60
low=monitor_prod_wind$Dir-range
up=monitor_prod_wind$Dir+range

r1l=ifelse(low<0,low+360,low)
r1u=ifelse(low<0,360,up)
r2l=ifelse(low<0,0,low)
r2u=up

r3u=ifelse(up>360,r2u-360,up)
r3l=ifelse(up>360,0,low)
r4u=ifelse(up>360,360,up)
r4l=low

w1<- monitor_prod_wind$dir<r1u&monitor_prod_wind$dir>r1l
w2<- monitor_prod_wind$dir<r2u&monitor_prod_wind$dir>r2l
w3<- monitor_prod_wind$dir<r3u&monitor_prod_wind$dir>r3l
w4<- monitor_prod_wind$dir<r4u&monitor_prod_wind$dir>r4l
w<-w1|w2|w3|w4
monitor_prod_wind$Within<-w
rm(up,low,r1u,r1l,r2u,r2l,r3l,r3u,r4l,r4u,w1,w2,w3,w4,w,uwind,vwind,monitor_prod)

pm_prod<-monitor_prod_wind%>%
  group_by(Uni_ID,Prod_Year,Prod_Month,`Drill Type`)%>%
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

oil_prod<-production_reshape(pm_prod,"sum_oil","Drill Type",c("Uni_ID","Prod_Year","Prod_Month","basin"),"Oil_Prod")
oil_fst_prod<-production_reshape(pm_prod,"first_order_oil_prod","Drill Type",c("Uni_ID","Prod_Year","Prod_Month","basin"),"Fst_Oil_Prod")
oil_snd_prod<-production_reshape(pm_prod,"second_order_oil_prod","Drill Type",c("Uni_ID","Prod_Year","Prod_Month","basin"),"Snd_Oil_Prod")
gas_prod<-production_reshape(pm_prod,"sum_gas","Drill Type",c("Uni_ID","Prod_Year","Prod_Month","basin"),"Gas_Prod")
gas_fst_prod<-production_reshape(pm_prod,"first_order_gas_prod","Drill Type",c("Uni_ID","Prod_Year","Prod_Month","basin"),"Fst_Gas_Prod")
gas_snd_prod<-production_reshape(pm_prod,"second_order_gas_prod","Drill Type",c("Uni_ID","Prod_Year","Prod_Month","basin"),"Snd_Gas_Prod")

oil_prod_wind<-production_reshape(pm_prod,"sum_oil_wind","Drill Type",c("Uni_ID","Prod_Year","Prod_Month","basin"),"Oil_Prod_wind")
oil_fst_prod_wind<-production_reshape(pm_prod,"first_order_oil_prod_wind","Drill Type",c("Uni_ID","Prod_Year","Prod_Month","basin"),"Fst_Oil_Prod_Wind")
oil_snd_prod_wind<-production_reshape(pm_prod,"second_order_oil_prod_wind","Drill Type",c("Uni_ID","Prod_Year","Prod_Month","basin"),"Snd_Oil_Prod_Wind")
gas_prod_wind<-production_reshape(pm_prod,"sum_gas_wind","Drill Type",c("Uni_ID","Prod_Year","Prod_Month","basin"),"Gas_Prod_Wind")
gas_fst_prod_wind<-production_reshape(pm_prod,"first_order_gas_prod_wind","Drill Type",c("Uni_ID","Prod_Year","Prod_Month","basin"),"Fst_Gas_Prod_Wind")
gas_snd_prod_wind<-production_reshape(pm_prod,"second_order_gas_prod_wind","Drill Type",c("Uni_ID","Prod_Year","Prod_Month","basin"),"Snd_Gas_Prod_Wind")

num_oil<-production_reshape(pm_prod,"num_oil","Drill Type",c("Uni_ID","Prod_Year","Prod_Month","basin"),"Oil_Num")
num_oil_wind<-production_reshape(pm_prod,"num_oil_wind","Drill Type",c("Uni_ID","Prod_Year","Prod_Month","basin"),"Oil_Num_Wind")
num_gas<-production_reshape(pm_prod,"num_gas","Drill Type",c("Uni_ID","Prod_Year","Prod_Month","basin"),"Gas_Num")
num_gas_wind<-production_reshape(pm_prod,"num_gas_wind","Drill Type",c("Uni_ID","Prod_Year","Prod_Month","basin"),"Gas_Num_Wind")
density_fst<-production_reshape(pm_prod,"first_density","Drill Type",c("Uni_ID","Prod_Year","Prod_Month","basin"),"Density_Fst")
density_fst_wind<-production_reshape(pm_prod,"first_density_wind","Drill Type",c("Uni_ID","Prod_Year","Prod_Month","basin"),"Density_Fst_Wind")
density_snd<-production_reshape(pm_prod,"second_density","Drill Type",c("Uni_ID","Prod_Year","Prod_Month","basin"),"Density_Snd")
density_snd_wind<-production_reshape(pm_prod,"second_density_wind","Drill Type",c("Uni_ID","Prod_Year","Prod_Month","basin"),"Density_Snd_Wind")

dist<-spread(pm_prod[,c("Uni_ID","Prod_Year","Prod_Month","Drill Type","dist","basin")],key=`Drill Type`,value = "dist")
dist<-dist%>%group_by(Uni_ID,Prod_Year,Prod_Month)%>%summarise(basin=names(table(basin))[1],H=sum(H,na.rm=T),V=sum(V,na.rm=T))
names(dist)[5:6]<-paste0(names(dist)[5:6],"_Dist")
names(dist)[2]<-"YEAR"
names(dist)[3]<-"MONTH"

monitor_well_relation[is.na(monitor_well_relation$`Completion Date`),]$`Completion Date`=monitor_well_relation[is.na(monitor_well_relation$`Completion Date`),]$'First Prod Date'
monitor_well_relation[is.na(monitor_well_relation$`Spud Date`),]$'Spud Date'=monitor_well_relation[is.na(monitor_well_relation$`Spud Date`),]$'Completion Date'%m+% months(-1)

monitor_well_relation$spud_year<-year(monitor_well_relation$`Spud Date`)
monitor_well_relation$spud_month<-month(monitor_well_relation$`Spud Date`)

monitor_well_relation$comp_year<-year(monitor_well_relation$`Completion Date`)
monitor_well_relation$comp_month<-month(monitor_well_relation$`Completion Date`)


well_construction_summary<-expand.grid(unique(pm_monitors$Uni_ID),seq(from=2007,to=2016),seq(from=1,to=12))
names(well_construction_summary)<-c("Uni_ID","YEAR","MONTH")
well_construction_summary<-oil_prod%>%left_join(well_construction_summary)
well_construction_summary$Date<-ymd(paste0(well_construction_summary$YEAR,formatC(well_construction_summary$MONTH,2,2,flag=0),"01",by="-"))
temp<-left_join(well_construction_summary,monitor_well_relation,by=c("Uni_ID"="Uni_ID"))
temp$process_period<-interval(temp$`Spud Date`,temp$`Completion Date`)
temp<-temp[temp$Date%within%temp$process_period,c("Uni_ID","API/UWI","YEAR","MONTH","Spud Date","Completion Date","Drill Type","Dist","Dir")]
temp<-temp[!is.na(temp$Uni_ID),]
temp$process_interval<-interval(temp$`Spud Date`,temp$`Completion Date`)
temp[(temp$process_interval%%months(1))>10,]$`Spud Date`=temp[(temp$process_interval%%months(1))>10,]$`Completion Date`%m+%months(-3)
temp<-temp[ymd(paste0(temp$YEAR,formatC(temp$MONTH,2,2,flag=0),"01",by="-"))%within%temp$process_interval,c("Uni_ID","API/UWI","YEAR","MONTH","Spud Date","Completion Date","Drill Type","Dist","Dir")]
names(temp)[7]<-"Drill_Type"
well_construction_summary<-temp%>%
  group_by(Uni_ID,YEAR,MONTH,Drill_Type)%>%
  dplyr::summarise(
    n_cons=length(Dist),
    cons_dist=mean(Dist)
    )
well_construction_summary[is.na(well_construction_summary)] <- 0
exp_data<- spread(well_construction_summary[1:100,c("Uni_ID","YEAR","MONTH","Drill_Type","n_cons")],key="Drill_Type",value="n_cons")
exp_data[is.na(exp_data)]<-0
exp_data$H=exp_data$D+exp_data$H
exp_data$G<-exp_data$H+exp_data$V
names(exp_data)[5:7]<-c("H_Cons","V_Cons","G_Cons")

pm_all<-left_join(pm_wind,oil_prod,by=c("Year"="YEAR","Month"="MONTH","Uni_ID"="Uni_ID"))
pm_all<-left_join(pm_all,oil_fst_prod,by=c("Year"="YEAR","Month"="MONTH","Uni_ID"="Uni_ID","basin"="basin"))
pm_all<-left_join(pm_all,oil_fst_prod_wind,by=c("Year"="YEAR","Month"="MONTH","Uni_ID"="Uni_ID","basin"="basin"))
pm_all<-left_join(pm_all,oil_snd_prod,by=c("Year"="YEAR","Month"="MONTH","Uni_ID"="Uni_ID","basin"="basin"))
pm_all<-left_join(pm_all,oil_snd_prod_wind,by=c("Year"="YEAR","Month"="MONTH","Uni_ID"="Uni_ID","basin"="basin"))

pm_all<-left_join(pm_all,gas_prod,by=c("Year"="YEAR","Month"="MONTH","Uni_ID"="Uni_ID","basin"="basin"))
pm_all<-left_join(pm_all,gas_fst_prod,by=c("Year"="YEAR","Month"="MONTH","Uni_ID"="Uni_ID","basin"="basin"))
pm_all<-left_join(pm_all,gas_fst_prod_wind,by=c("Year"="YEAR","Month"="MONTH","Uni_ID"="Uni_ID","basin"="basin"))
pm_all<-left_join(pm_all,gas_snd_prod,by=c("Year"="YEAR","Month"="MONTH","Uni_ID"="Uni_ID","basin"="basin"))
pm_all<-left_join(pm_all,gas_snd_prod_wind,by=c("Year"="YEAR","Month"="MONTH","Uni_ID"="Uni_ID","basin"="basin"))

pm_all<-left_join(pm_all,num_oil,by=c("Year"="YEAR","Month"="MONTH","Uni_ID"="Uni_ID","basin"="basin"))
pm_all<-left_join(pm_all,num_oil_wind,by=c("Year"="YEAR","Month"="MONTH","Uni_ID"="Uni_ID","basin"="basin"))
pm_all<-left_join(pm_all,num_gas,by=c("Year"="YEAR","Month"="MONTH","Uni_ID"="Uni_ID","basin"="basin"))
pm_all<-left_join(pm_all,num_gas_wind,by=c("Year"="YEAR","Month"="MONTH","Uni_ID"="Uni_ID","basin"="basin"))
pm_all<-left_join(pm_all,density_fst,by=c("Year"="YEAR","Month"="MONTH","Uni_ID"="Uni_ID","basin"="basin"))
pm_all<-left_join(pm_all,density_fst_wind,by=c("Year"="YEAR","Month"="MONTH","Uni_ID"="Uni_ID","basin"="basin"))
pm_all<-left_join(pm_all,density_snd,by=c("Year"="YEAR","Month"="MONTH","Uni_ID"="Uni_ID","basin"="basin"))
pm_all<-left_join(pm_all,density_snd_wind,by=c("Year"="YEAR","Month"="MONTH","Uni_ID"="Uni_ID","basin"="basin"))
pm_all[is.na(pm_all)]<-0
pm_all<-left_join(pm_all,dist,by=c("Year"="YEAR","Month"="MONTH","Uni_ID"="Uni_ID","basin"="basin"))
pm_all<-left_join(pm_all,pm_mass,by=c("Year"="YEAR","Month"="MONTH","Uni_ID"="Monitor_ID"))
pm_all<-left_join(pm_all,pm_spec,by=c("Year"="YEAR","Month"="MONTH","Uni_ID"="Uni_ID"))
pm_all<-left_join(pm_all,exp_data,by=c("Year"="YEAR","Month"="MONTH","Uni_ID"="Uni_ID"))
pm_coords<-cbind.data.frame(pm_monitors@data$Uni_ID,coordinates(pm_monitors))
names(pm_coords)=c("Uni_ID","Lon","Lat")
pm_all<-left_join(pm_all,pm_coords)

save(pm_all,file=here::here("data",paste0("pm_oil_gas_prod_",r/1000,".RData")))

pm_all<-pm_all[!is.na(pm_all$mass),]

pm_all$Uni_ID<-as.factor(pm_all$Uni_ID)

o_model=bam(mass~vel+V_Oil_Prod+H_Oil_Prod+s(Year)+s(Month)+s(Lon,Lat)+s(Uni_ID,bs="re"),data=pm_all)

g_model=bam(mass~vel+s(V_Gas_Prod)+s(Year)+s(Month)+s(Lon,Lat)+s(Uni_ID,bs="re"),data=pm_all)

plot(g_model,page=1)

model=bam(mass~vel+s(Year,k=10)+s(Month,k=12)+s(Lon,Lat,k=100,fx=T)+s(Uni_ID,bs="re"),data=pm_all)
summary(model)
model_1=bam(mass~vel+s(Year,k=10)+V_Oil_Num+s(Month,k=12)+s(Lon,Lat,k=100,fx=T)+s(Uni_ID,bs="re"),data=pm_all)
summary(mode_1)
anova(model_1,model)
plot(model,page=1)
