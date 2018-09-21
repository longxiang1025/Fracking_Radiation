#' ---
#' title: "Use the new database to create a case study of Dallas/Fort Worth TX Area"
#' author: "Longxiang Li"
#' date: "`r format(Sys.Date())`"
#' output: 
#'      html_document:
#'       toc: true
#'       fig_caption: yes
#' ---
#' 
#' #Background
#' I moved all the current research data into a local PostgreSQL database with a spatial extention. A huge advantage of using database instead
#' millions of data files is to simplify the future data loading/flter time consumption. In addition, after setting the rule of storing data
#' all the following research data to be stored in this database (even by some other collaborators) can be used seamlessly with current dataset.
#' In other words, we don't need to read through the docs/code, if they exists, to understand the data manipulating process. The data extraction job process will be self-explanatory.
#' 
#' # Past Work
#' Last week, after spending days to extract driling info arund all radiation monitors with buffers ranging from 30km to 125km (Due to the 
#' increaing number of wells along with biger buffer). We figure out that the story of the relationship between beta raidation and local 
#' horizontal drilling may differ between cities. We went through several sample cities and come to a conclusion that we need to have a closer look
#' at each city, even though later we think we should attach more weight to Dallas/Fort Worth Area where historical vertical drilling is not 
#' prevailing. In the continuous discussion during weekend, we talked about adding gamma raidation data and other radionuclide into the study.
#' These additional highlight the importance of data management through database.
#' 
#' #Summary 
#' In this report, we use the new database to do the Dallas/Fort Worth case study. At the end of this report, I'll also plot all the production and number
#' of wells information for all cities with RadNet monitors nearby. So in fact, this's also an explanatory study instead of some serious one. The 
#' goal of this study is also helping us get farmiliar with the data.
#+ load-library, message = F, echo = F
library(mgcv)
library(ggmap)
library(raster)
library(lubridate)
library(knitr)
library(dplyr)
library(grid)
library(gridExtra)
library(tidyverse)
library(reshape2)
library(MASS)
library(RPostgreSQL)
library(rpostgis)
#' # Steps to use the new database.
#' 
#' Now, We can use the new database to finish the data extraction in <5s. Before this, it took hours to do the same job.
#' 
#' First we need to connect to the local database
#+ create connection
#save the password seperately
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
#' After gettting connected, we can list all the dataset within it. It worth noting that, new dataset such as gamma data will be added later. To 
#' collect all the exposure data in the same database will simplify the extraction process for other collaborators. We need to scan the inventory
#' first. In the database, we now have three geometric dataset (PM monitors, RadNet monitors and Gas_Wells). Attahced with them, we have four
#'  measurement time series Kdataset (PM Mass, PM speciation, beta radiation and production)
#list all tables
dbListTables(con)
#List all geometric data
dbListTables(con)
#'
#'Before focusing on Fort Worth/Dallas area, all cities with a RadNet monitor are plotted.
#+ Extract monthly mean pm2.5 mass measurement around radiation monitors,echo=F,message=FALSE,warning=F,cache = T,fig.width=15,fig.height=9
source(here::here("code","00_Template_SQL_Command.R"))
RadNet_City_List<-read_csv(here::here("data","Processed-RadNet-Beta-citylist.csv"))
r=50000
spec="Sulfate PM2.5 LC"
for(i in 1:nrow(RadNet_City_List)){
  city_name<-RadNet_City_List$city_state[i]
  cmd<-gsub(pattern="CITYNAME",replacement=city_name,pm_mass_cmd)
  cmd<-gsub(pattern="RADIUS",replacement=as.character(r),cmd)
  city_pm_pair<- dbGetQuery(con, cmd)
  city_pm_pair$Monitor_ID<-as.factor(city_pm_pair$Monitor_ID)
  
  cmd<-gsub(pattern="CITYNAME",replacement=city_name,pm_spec_cmd)
  cmd<-gsub(pattern="RADIUS",replacement=as.character(r),cmd)
  city_pm_spec<- dbGetQuery(con, cmd)
  
  cmd<-gsub(pattern="CITYNAME",replacement=city_name,total_gas_cmd)
  cmd<-gsub(pattern="RADIUS",replacement=as.character(r),cmd)
  city_gas_prod<- dbGetQuery(con, cmd)
  
  if(nrow(city_gas_prod)>10&nrow(city_pm_pair)>100&nrow(city_pm_spec)>100){
    city_gas_prod$Type<-as.factor(city_gas_prod$Type)
    
    plot.data<-melt(data=city_gas_prod[,c("m_month","Type","Prod")],id=c("m_month","Type"))
    plot.data<-distinct(plot.data)
    prod.plot<-ggplot(data=plot.data,aes(x=m_month,y=value,fill=Type))+
      geom_area(colour="black", size=.2, alpha=.9)+
      scale_fill_brewer("Blues")+
      ggtitle(paste0("Monthly gas production of ",city_name," Within ",r/1000," km"))
    
    plot.data<-melt(data=city_gas_prod[,c("m_month","Type","Num")],id=c("m_month","Type"))
    plot.data<-distinct(plot.data)
    num.plot<-ggplot(data=plot.data,aes(x=m_month,y=value,fill=Type))+
      geom_area(colour="black", size=.2, alpha=.9)+
      scale_fill_brewer("Blues")+
      ggtitle(paste0("Number of Acitve Wells of ", city_name," Within ",r/1000," km"))
    grid.arrange(prod.plot,num.plot,ncol=2)
    
    monthly_pm_mass_mean<-aggregate.data.frame(city_pm_pair[,c("mass")],by=list(city_pm_pair$m_month),FUN=mean)
    names(monthly_pm_mass_mean)<-c("m_month","pm_mass")
    pm_plot<-ggplot(data=monthly_pm_mass_mean,aes(y=pm_mass,x=m_month,))+geom_point()+geom_smooth()
    pm_plot<- pm_plot+ labs(y = "PM2.5 mass",
                          x = "# of months from 01/01/2001",
                          colour = "Type")
    pm_plot<- pm_plot + theme(legend.position="bottom",
                           panel.background = element_rect(fill = NA,colour = "grey50"),
                           panel.grid.major = element_line(colour = "grey50"))
    
    names(city_pm_spec)[3]<-"parameter"
    city_pm_spec$parameter<-as.factor(city_pm_spec$parameter)
    monthly_pm_spec_mean<-aggregate(spec~m_month+parameter,data=city_pm_spec,mean)
    monthly_pm_spec_mean<-merge(monthly_pm_mass_mean,monthly_pm_spec_mean)
    monthly_pm_spec_mean$spec_percentage<-monthly_pm_spec_mean$spec/monthly_pm_spec_mean$pm_mass
    spec_plot<-ggplot(data=monthly_pm_spec_mean,aes(x=m_month,y=spec_percentage,color=parameter))+geom_line()
    spec_plot<-spec_plot+theme(legend.position="bottom",
                               panel.background = element_rect(fill = NA,colour = "grey50"),
                               panel.grid.major = element_line(colour = "grey50"))
    grid.arrange(pm_plot,spec_plot,ncol=1)
    
  }else{
    print(paste0(city_name," Doesn't has natual gas production within a radius of ",r))
  }
}


