require("RPostgreSQL")
library(rpostgis)
library(lubridate)
library(dplyr)
library(raster)
#Load raster data into the database
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

#create the table of oil_well_header from the active_wells data
load(here::here("data","All_Gas_Wells_Active_After_2001.RData"))
#pgInsert(con,name="Gas_Well_Headers",data.obj=header_list,geom = "gas_well_geom")
#create the table of  production to store all monthlty oil production data
load(here::here("data","All_Gas_Wells_Active_After_2001_Production_Series.RData"))
production_data<-filter(production_data,production_data$`API/UWI`%in%header_list$`API/UWI`)
names(production_data)<-c("API","Monthly_Production_Date", "Monthly_Gas", "Monthly_Oil","Monthly_Water"  )
production_data<-filter(production_data,Monthly_Gas>(-1)&Monthly_Oil>(-1)&Monthly_Water>(-1))
dupled_rows_top<-which(duplicated(production_data[,c("API","Monthly_Production_Date")],fromLast=F))
dupled_rows_bottom<-which(duplicated(production_data[,c("API","Monthly_Production_Date")],fromLast=T))
dupled_rows<-c(dupled_rows_top,dupled_rows_bottom)
dupled_rows<-unique(dupled_rows)
uniqu_records<-production_data[-dupled_rows,]
dupled_records<-production_data[dupled_rows,]
cleaned_records<-dupled_records%>%
  group_by(API,Monthly_Production_Date)%>%
  summarise(sum_Monthly_Gas=sum(Monthly_Gas,na.rm=T),
            sum_Monthly_Oil=sum(Monthly_Oil,na.rm=T),
            sum_Monthly_Water=sum(Monthly_Water,na.rm=T)
  )
names(cleaned_records)<-c("API","Monthly_Production_Date", "Monthly_Gas", "Monthly_Oil","Monthly_Water"  )
production_data<-rbind.data.frame(uniqu_records,cleaned_records)
dbWriteTable(con, "Gas_Production_Table", 
             value = production_data,row.names = FALSE,append=T)

load(here::here("data","All_Oil_Wells_Active_After_2001.RData"))
#pgInsert(con,name="Gas_Well_Headers",data.obj=header_list,geom = "gas_well_geom")
#create the table of  production to store all monthlty oil production data
load(here::here("data","All_Oil_Wells_Active_After_2001_Production_Series.RData"))
production_data<-filter(production_data,production_data$`API/UWI`%in%header_list$`API/UWI`)
names(production_data)<-c("API","Monthly_Production_Date", "Monthly_Gas", "Monthly_Oil","Monthly_Water"  )
production_data<-filter(production_data,Monthly_Gas>(-1)&Monthly_Oil>(-1)&Monthly_Water>(-1))
dupled_rows_top<-which(duplicated(production_data[,c("API","Monthly_Production_Date")],fromLast=F))
dupled_rows_bottom<-which(duplicated(production_data[,c("API","Monthly_Production_Date")],fromLast=T))
dupled_rows<-c(dupled_rows_top,dupled_rows_bottom)
dupled_rows<-unique(dupled_rows)
uniqu_records<-production_data[-dupled_rows,]
dupled_records<-production_data[dupled_rows,]
cleaned_records<-dupled_records%>%
  group_by(API,Monthly_Production_Date)%>%
  summarise(sum_Monthly_Gas=sum(Monthly_Gas,na.rm=T),
            sum_Monthly_Oil=sum(Monthly_Oil,na.rm=T),
            sum_Monthly_Water=sum(Monthly_Water,na.rm=T)
  )
names(cleaned_records)<-c("API","Monthly_Production_Date", "Monthly_Gas", "Monthly_Oil","Monthly_Water"  )
production_data<-rbind.data.frame(uniqu_records,cleaned_records)
dbWriteTable(con, "Oil_Production_Table", 
             value = production_data,row.names = FALSE,append=T)

