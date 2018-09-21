require("RPostgreSQL")
library(rpostgis)
library(lubridate)
library(dplyr)
pw <- {
  "koutrakis"
}

prjstring<-"+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs "
geoprjstring<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "Fracking_Data",
                 host = "localhost", port = 5432,
                 user = "postgres", password = pw)
rm(pw) # removes the password

#create the table of radnet monitor from the city_list data
RadNet_City_List<-read_csv(here::here("data","Processed-RadNet-Beta-citylist.csv"))
coordinates(RadNet_City_List)<-~lon+lat
proj4string(RadNet_City_List)<-geoprjstring
pgInsert(con,name="RadNet_Sp",data.obj=RadNet_City_List)

#create the table of radnet measurement 
rad<-read_csv(here::here("data","Processed-RadNet-Beta-byMeas.csv")) %>% 
  mutate(month = lubridate::month(collect_start),
         season = ifelse(month %in% c(12, 1, 2), "Winter", 
                         ifelse(month %in% c(3, 4, 5), "Spring", 
                                ifelse(month %in% c(6, 7, 8), "Summer", "Fall")))) %>% 
  mutate(season = factor(season, levels = c("Winter", "Spring", "Summer", "Fall"))) %>% 
  rename(days_b4_measurement = days_b4_meas, collection_length = meas_length) %>% 
  dplyr::filter(!Rosner.out & !manual.out)
rad$Date<-floor_date(rad$collect_start, "month")
dbWriteTable(con, "radnet_measurement_table", 
             value = rad, append = TRUE, row.names = FALSE)

#create the table of gas_well_header from the active_wells data
load(here::here("data","All_Gas_Wells_Active_After_2001.RData"))
pgInsert(con,name="Gas_Well_Headers",data.obj=header_list)
#create the table of  production to store all monthlty production data
load(here::here("data","All_Gas_Wells_Active_After_2001_Production_Series.RData"))
dbWriteTable(con, "production_table", 
             value = production_data, append = TRUE, row.names = FALSE)
#create the table of pm monitors
load(here::here("data","PM_MONITOR_ID_CONVERSION_TABLE.RData"))
monitor_list<-distinct(monitor_list[,c("Uni_ID","Longitude","Latitude")])
coordinates(monitor_list)<-~Longitude+Latitude
proj4string(monitor_list)<-geoprjstring
pgInsert(con,name="PM_Monitors",data.obj = monitor_list)
#create the table of pm mass 
load(here::here("data","Daily_PM_Data.RData"))
pm_mass_data<-pm_mass_data[,c("Date Local","Arithmetic Mean","Uni_ID")]
names(pm_mass_data)[2]<-"pm_mass"
dbWriteTable(con,"PM_MASS_Measurement",value=pm_mass_data,append=F,rownames=F)

#create the table of pm spec
load(here::here("data","Daily_PM_Spec_Data.RData"))
pm_spec_data<-pm_spec_data[,c("Date Local","Arithmetic Mean","Uni_ID","Parameter Name")]
names(pm_spec_data)[2]<-"pm_spec"
dbWriteTable(con,"PM_Spec_Measurement",value=pm_spec_data,append=T,rownames=F)

#create the table of gamma measurement time series
load(here::here("data","gamma_data.RData"))
state_city<-do.call(rbind,strsplit(gamma_data$LOCATION_NAME,split = ":"))
city_state<-paste0(state_city[,2],",",state_city[,1])
city_state<-gsub("[[:blank:]]", "",city_state)
gamma_data$city_state<-city_state
gamma_data<-gamma_data[city_state%in%unique(RadNet_City_List$city_state),]
dbWriteTable(con,"Gamma_Measurement",value =gamma_data,append=T,rownames=F)

#create the table of nuclide measurement
load(here::here("data","nuclide_data.RData"))
nuc_data$Location<-gsub("[[:blank:]]", "",nuc_data$Location)
nuc_data<-nuc_data[nuc_data$Location%in%unique(RadNet_City_List$city_state),]
names(nuc_data)<-c("Location","Medium","Date","Procedure_Name","Nuclides","Result","Uncertainty","MDC","Unit")
nuc_data<-distinct(.data=nuc_data,Location,Nuclides,Date,.keep_all=T)
nuc_data$Result<-as.numeric(nuc_data$Result)
nuc_data[nuc_data$Unit=="aCi/m3",]$Result<-nuc_data[nuc_data$Unit=="aCi/m3",]$Result/1000000
nuc_data$Unit<-"pCi/m3"
dbWriteTable(con,"Nuclide_Measurement",value = nuc_data,append=T,rownames=F)
#sample selection
#case 1: select all meaurements of pm mass around dallas.tx in 2007
cmd<-"SELECT \"Monitor_ID\",\"Date Local\" AS \"Date\",\"pm_mass\",\"city\"
FROM \"PM_MASS_Measurement\",(SELECT \"Uni_ID\" AS \"ID\",\"city_state\" AS \"city\" FROM  \"PM_Monitors\",\"RadNet_Sp\" WHERE ST_DistanceSphere(\"pm_monitor_geom\",\"radnet_geom\")<50000)AS tbl
WHERE \"Monitor_ID\" = \"ID\" AND (\"Date Local\" BETWEEN '01/01/2007' AND '12/31/2007') "
city_pm_pair<- dbGetQuery(con, cmd)
dallas_pair<-filter(city_pm_pair,city=="DALLAS,TX")
dallas_pair$Monitor_ID<-as.factor(dallas_pair$Monitor_ID)
p<-ggplot(data=dallas_pair,aes(x=Date,y=pm_mass,color=Monitor_ID))+geom_line(size=2)
#case 2:calculate the monthly mean of sulfate around fort worth, tx in 2008
cmd<-"SELECT \"Monitor_ID\",\"Date Local\" AS \"Date\",\"pm_spec\",\"Parameter Name\",\"city\"
FROM \"PM_Spec_Measurement\",(SELECT \"Uni_ID\" AS \"ID\",\"city_state\" AS \"city\" FROM  \"PM_Monitors\",\"RadNet_Sp\" WHERE ST_DistanceSphere(\"pm_monitor_geom\",\"radnet_geom\")<50000)AS tbl
WHERE \"Monitor_ID\" = \"ID\" AND (\"Date Local\" BETWEEN '01/01/2008' AND '12/31/2008') AND \"Parameter Name\"= 'Sulfate PM2.5 LC'"
city_sulfate_pair<- dbGetQuery(con, cmd)
dallas_sulfate_pair<-filter(city_sulfate_pair,city=="DALLAS,TX")
dallas_sulfate_pair$Monitor_ID<-as.factor(dallas_sulfate_pair$Monitor_ID)
p<-ggplot(data=dallas_sulfate_pair,aes(x=Date,y=pm_spec,color=Monitor_ID))+geom_line(size=2)
#case 3: calculate the monthly average of sulfate around Dallas Tx from 2001 to 2017
cmd<-"SELECT \"Monitor_ID\",avg(\"pm_spec\") AS \"spec\",\"Parameter Name\",\"city\",12*date_part('year',age(date_trunc('month',\"Date Local\"),'2001-01-01'))+date_part('month',age(date_trunc('month',\"Date Local\"),'2001-01-01'))AS \"m_month\"
FROM \"PM_Spec_Measurement\",(SELECT \"Uni_ID\" AS \"ID\",\"city_state\" AS \"city\" FROM  \"PM_Monitors\",\"RadNet_Sp\" WHERE ST_DistanceSphere(\"pm_monitor_geom\",\"radnet_geom\")<50000)AS tbl
WHERE \"Monitor_ID\" = \"ID\" AND (\"Date Local\" BETWEEN '01/01/2001' AND '12/31/2018') AND \"Parameter Name\"= 'Sulfate PM2.5 LC'
GROUP BY \"Monitor_ID\",\"m_month\",\"Parameter Name\",\"city\""
dallas_month_sulfate<- dbGetQuery(con, cmd)
dallas_month_sulfate<-filter(dallas_month_sulfate,city=="DALLAS,TX")
dallas_month_sulfate$Monitor_ID<-as.factor(dallas_month_sulfate$Monitor_ID)
p<-ggplot(dallas_month_sulfate,aes(x=m_month,y=spec,color=Monitor_ID))+geom_line(size=2)
