#' ---
#' title: "Map Drillinginfo Data and create a list"
#' author: "Longxiang Li"
#' date: "`r format(Sys.Date())`"
#' output: 
#'      html_document:
#'       toc: true
#'       fig_caption: yes
#' ---
#' 
#+ load-data, message = F, echo = F
library(tidyverse)
library(here)
library(knitr)
library(kableExtra)
library(here)
library(dplyr)
library(lubridate)
library(reshape2)
library(gridExtra)
library(gamm4)
#+ load csv file and clean the data based on production, message=FALSE, echo=F, cache = T
setwd("C:/Users/lol087/Dropbox (Personal)/2018-08-09-DrillingInfo/data/")
prod.header <- read.csv("raw/production-headers/PA Production Headers.csv")
prod.time<-read.csv("raw/production-time-series/PA Active Wells Production Time Series.csv")
prod.time2<-read.csv("raw/production-time-series/PA Inactive Wells Production Time Series.csv")
prod.time<-rbind.data.frame(prod.time,prod.time2)
rm(prod.time2)
prod.header$First.Prod.Date<-(as.character(prod.header$First.Prod.Date))
prod.header$Last.Prod.Date<-(as.character(prod.header$Last.Prod.Date))
prod.header<-prod.header[nchar(prod.header$First.Prod.Date)>0&nchar(prod.header$First.Prod.Date)>0,]
prod.header$First.Prod.Date<-as.Date(prod.header$First.Prod.Date)
prod.header$Last.Prod.Date<-as.Date(prod.header$Last.Prod.Date)
prod.time<-prod.time[prod.time$API.UWI%in%prod.header$API.UWI,]
prod.time$Monthly.Production.Date<-as.Date(prod.time$Monthly.Production.Date)
sum.gas<-prod.time %>% group_by(API.UWI) %>%
  summarise(First=min(Monthly.Production.Date),
            Last=max(Monthly.Production.Date),
            All_Prod=sum(Monthly.Gas))
sum.gas<-sum.gas[sum.gas$All_Prod>0,]
sum.oil<-prod.time %>% group_by(API.UWI) %>%
  summarise(First=min(Monthly.Production.Date),
            Last=max(Monthly.Production.Date),
            All_Prod=sum(Monthly.Oil))
sum.gas<-sum.gas[sum.gas$All_Prod>0,]
sum.oil<-sum.oil[sum.oil$All_Prod>0,]
prod.header.gas<-filter(prod.header,(API.UWI%in%sum.gas$API.UWI)&(!is.na(Surface.Latitude..WGS84.)))
prod.header.oil<-filter(prod.header,(API.UWI%in%sum.oil$API.UWI)&(!is.na(Surface.Latitude..WGS84.)))
#' #Plot the oil well and gas well in Pennsylvania
#' Based on the filtered dataset, we plot locations of gas wells and oil wells in PA. There're two RadNet monitors located in Pittsburgh
#' and Bloomburg. So I created two groups of buffers sourrounding these two cities at radius if 15km, 30km and 50km. (Only the buffers of 50km are ploted with red circle).
#' The gas&oil wells are ploted with semi-transparent points. So we can have a big picture of the geographical pattern. The solid points within the red circle 
#' represent the location of gas&oil wells within the buffer.
#+ prepare geographic background data, message=FALSE, echo=F, cache = T
suppressMessages(library(sp))
suppressMessages(library(ggmap))
suppressMessages(library(raster))
setwd("C:/Users/lol087/Dropbox (Personal)/2018-08-09-DrillingInfo/data/")
load("Basic_Geodata/Boundaries.RData")
prjstring<-"+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs "
geoprjstring<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
if(!exists("Pitt")){Pitt<-geocode("Pittsburgh,PA")}
if(!exists("Bloom")){Bloom<-geocode("Bloomsburg,PA")}
centers<-rbind.data.frame(Pitt,Bloom)
centers$city<-c("Pittsburgh","Bloomsburg")
coordinates(centers)<-~lon+lat
proj4string(centers)<-geoprjstring
if(class(prod.header.oil)=="data.frame"){
  coordinates(prod.header.oil)<-~Surface.Longitude..WGS84.+Surface.Latitude..WGS84.
  proj4string(prod.header.oil)<-geoprjstring 
}
if(class(prod.header.gas)=="data.frame"){
  coordinates(prod.header.gas)<-~Surface.Longitude..WGS84.+Surface.Latitude..WGS84.
  proj4string(prod.header.gas)<-geoprjstring 
}
#+ create three buffer circle with radius 15km,30km,50km., message=FALSE, echo=F, cache = T
buffer_list<-list()
for(i in 1:length(centers)){
  buffer_list[[3*(i-1)+1]]<-buffer(centers[i,],width=15000)
  buffer_list[[3*(i-1)+2]]<-buffer(centers[i,],width=30000)
  buffer_list[[3*(i-1)+3]]<-buffer(centers[i,],width=50000)
}
if(!exists("PA_map")){PA_map<-get_googlemap("PA",zoom = 7)}
#+ PA Gas well locations,echo=F,message=F,cache=T
map<-ggmap(PA_map)
map<-map+geom_polygon(data = fortify(bound[24,]),
                      aes(long, lat, group = group),
                     colour = "black", alpha = 0)
gas_map<-map+geom_point(data=as.data.frame(coordinates(prod.header.gas)),aes(x=Surface.Longitude..WGS84.,y=Surface.Latitude..WGS84.),size=0.25,alpha=0.05)
prod.header.gas.std<-crop(prod.header.gas,buffer_list[[3]])
if(!is.null(prod.header.gas.std)){
gas_map<-gas_map+geom_point(data=as.data.frame(coordinates(prod.header.gas.std)),aes(x=Surface.Longitude..WGS84.,y=Surface.Latitude..WGS84.),size=0.5)
gas_map<-gas_map+geom_polygon(data = fortify(buffer_list[[3]]),
                              aes(long, lat, group = group),
                              fill = "red", colour = "red", alpha = 0.2)
}
prod.header.gas.std<-crop(prod.header.gas,buffer_list[[6]])
if(!is.null(prod.header.gas.std)){
gas_map<-gas_map+geom_point(data=as.data.frame(coordinates(prod.header.gas.std)),aes(x=Surface.Longitude..WGS84.,y=Surface.Latitude..WGS84.),size=0.5)
gas_map<-gas_map+geom_polygon(data = fortify(buffer_list[[6]]),
                              aes(long, lat, group = group),
                              fill = "red", colour = "red", alpha = 0.2)
}
gas_map<-gas_map+ggtitle("Gas Wells in Pennsylvania")
#' We can see there're more unconventional energy production around Pittsburgh than Bloomburg. In addition, the gas production (No. of wells) is more active 
#' than the oil production. It's a little funky to find some many production in the national forest on the border.
#+ PA Oil well locations,echo=F,message=F,cache=T,fig.width=15,fig.height=9
oil_map<-map+geom_point(data=as.data.frame(coordinates(prod.header.oil)),aes(x=Surface.Longitude..WGS84.,y=Surface.Latitude..WGS84.),size=0.25,alpha=0.05)
prod.header.oil.std<-crop(prod.header.oil,buffer_list[[3]])
if(!is.null(prod.header.oil.std)){
  oil_map<-oil_map+geom_point(data=as.data.frame(coordinates(prod.header.oil.std)),aes(x=Surface.Longitude..WGS84.,y=Surface.Latitude..WGS84.),size=0.5)
  oil_map<-oil_map+geom_polygon(data = fortify(buffer_list[[3]]),
                                aes(long, lat, group = group),
                                fill = "red", colour = "red", alpha = 0.2) 
}
prod.header.oil.std<-crop(prod.header.oil,buffer_list[[6]])
if(!is.null(prod.header.oil.std)){
oil_map<-oil_map+geom_point(data=as.data.frame(coordinates(prod.header.oil.std)),aes(x=Surface.Longitude..WGS84.,y=Surface.Latitude..WGS84.),size=0.5)
oil_map<-oil_map+geom_polygon(data = fortify(buffer_list[[6]]),
                              aes(long, lat, group = group),
                              fill = "red", colour = "red", alpha = 0.2)
}
oil_map<-oil_map+ggtitle("Oil Wells in Pennsylvania")
grid.arrange(gas_map,oil_map,ncol=2)
#' #Summarise the monthly oil/gas activities around two RadNet monitors
#' In this section, gas/oil wells within the buffer will be aggregated along multiple dimensions:
#' *Monthly number of active gas wells   
#' 
#' *Monthly number of gas wells under construction  
#' 
#' *Monthly gas production  
#' 
#' First, we start from gas well around Pittsburg within a buffer of 50km. Currently, we set the study period as 01/01/2007-12/31/2016. The
#' operation time of RadNet in Pittsburgh is from 1987 to 2018. So we can match them. There're more than 1000 gas wells within this buffer.
#' First, we plot the monthly production of every gas well within the buffer.No value (gaps) means that no gas production during that period. 
#+Pitts 15km case study part I.,message=T,echo=F,cache=T,fig.width=15,fig.height=9
pitt_gas_header<-crop(prod.header.gas,buffer_list[[3]])
pitt_gas_prod_time<-filter(prod.time,API.UWI%in%pitt_gas_header$API.UWI)
period<-interval(ymd("2007-01-01"),ymd("2016-12-31"))
pitt_gas_prod_time<-filter(pitt_gas_prod_time,Monthly.Production.Date%within%period)
ncol<-(12)*as.period(period)%>%year()+as.period(period)%>%month()+1
pitt_calendar<-matrix(nrow = nrow(pitt_gas_header),ncol = ncol)
for(i in 1:length(pitt_gas_header)){
  id<-pitt_gas_header[i,]$API.UWI
  time_series<-filter(pitt_gas_prod_time,(API.UWI==id)&(Monthly.Production.Date%within%period))
  if(nrow(time_series)>0){
    ins<-interval(ymd("2007-01-01"),ymd(time_series$Monthly.Production.Date))
    months<-(12)*as.period(ins)%>%year()+as.period(ins)%>%month()+1
    pitt_calendar[i,months]<-time_series$Monthly.Gas 
  }
}
pitt_gas_prod_time$API.UWI<-as.factor(pitt_gas_prod_time$API.UWI)
if(nrow(pitt_gas_header)>100){
  ids<-sample_n(pitt_gas_header@data,100)
  pitt_gas_prod_time_plot<-filter(pitt_gas_prod_time,API.UWI%in%ids$API.UWI)
}else{
  pitt_gas_prod_time_plot<-pitt_gas_prod_time
}
ggplot(pitt_gas_prod_time_plot,aes(x=Monthly.Production.Date,y=API.UWI))+
  geom_tile(aes(fill=Monthly.Gas),size=0.25)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("Monthly gas production per well (Randomly sampled 100 wells)")
#' Second, we try to plot the production and wells in two drilling types (horizontal and vertical). Light blue area in the left panel reflects the unconventional
#' gas production and in the right panel indicates the number of wells. Dark blue means the production and number of vertical drilling. The productivity of horizontal 
#' drill is very impressive. We got over 90% gas from a relatively small fraction of well fleet.
#+Pitts 15km case study Part II plot by drilling type,message=F,echo=F,cache=F,fig.width=15,fig.height=9
h_wells<-which(pitt_gas_header$Drill.Type=="H")
v_wells<-which(pitt_gas_header$Drill.Type=="V")
pitt.month.prod<-colSums(pitt_calendar,na.rm=T)
pitt.month.activewells<-colSums(!is.na(pitt_calendar))
h.month.prod<-colSums(pitt_calendar[h_wells,],na.rm = T)
v.month.prod<-colSums(pitt_calendar[v_wells,],na.rm = T)
h.month.activewells<-colSums(!is.na(pitt_calendar[h_wells,]))
v.month.activewells<-colSums(!is.na(pitt_calendar[v_wells,]))

pitt.month.data<-cbind.data.frame(pitt.month.prod,pitt.month.activewells,h.month.prod,h.month.activewells,v.month.prod,v.month.activewells)
pitt.month.data$Date<-seq(1,120)
pitt.month.data$Date<-as.Date("2007-01-01") %m+% months(pitt.month.data$Date-1)
names(pitt.month.data)<-c("Prod","Wells","H_Prod","H_Wells","V_Prod","V_Wells","Date")

prod.plot.data<-melt(data=pitt.month.data[,c("H_Prod","V_Prod","Date")],id=c("Date"))
prod.plot<-ggplot(data=prod.plot.data,aes(x=Date,y=value,fill=variable))+
  geom_area(colour="black", size=.2, alpha=.9)+
  scale_fill_brewer("Blues")+
  ggtitle("Monthly gas production of two types of drill")
well.plot.data<-melt(data=pitt.month.data[,c("H_Wells","V_Wells","Date")],id=c("Date"))
well.plot<-ggplot(data=well.plot.data,aes(x=Date,y=value,fill=variable))+
  geom_area(colour="black", size=.2, alpha=.6)+
  scale_fill_brewer("Blues")+
  ggtitle("Monthly number of active gas wells of two types of drill")
grid.arrange(prod.plot,well.plot, ncol=2)
#' #Explore the relationship between radiation and gas production activity.
#' In this part two GAM models are used to model the correlation between gas production and radiation. This's only an EXPLORATORY result.
#' In both models, 12 knots are assigned to variable "year" and 12 knots are assigned to variable "month" in order to control for temperal confounding.
#' The only other variable is the gas production in vertial drills (Model 1)and gas production in horizontal drills (Model 2).  
#+Prepare Radnet data cleaned by Annelise,message=F,echo=F,cache=F
library(mgcv)
rad<-read_csv(("C:/Users/lol087/Dropbox (Personal)/BetaExplore/data/Processed-RadNet-Beta-byMeas.csv")) %>% 
  mutate(month = lubridate::month(collect_start),
         season = ifelse(month %in% c(12, 1, 2), "Winter", 
                         ifelse(month %in% c(3, 4, 5), "Spring", 
                                ifelse(month %in% c(6, 7, 8), "Summer", "Fall")))) %>% 
  mutate(season = factor(season, levels = c("Winter", "Spring", "Summer", "Fall"))) %>% 
  rename(days_b4_measurement = days_b4_meas, collection_length = meas_length) %>% 
  dplyr::select(-drop_days_b4_meas) %>% 
  dplyr::filter(!Rosner.out & !manual.out)%>%
  dplyr::filter(city_name=="PITTSBURGH")%>%
  dplyr::filter((collect_start%within%period)&(collect_end%within%period))
rad$Date<-floor_date(rad$collect_start, "month")
rad<-merge(rad,pitt.month.data,by="Date")
rad$nmonth<-abs((12)*as.period(interval(rad$Date,ymd("2007-01-01")))%>%year()+as.period(interval(rad$Date,ymd("2007-01-01")))%>%month())
rad$V_Prod<-rad$V_Prod/1000000
rad$H_Prod<-rad$H_Prod/1000000
rad$Prod<-rad$Prod/1000000
#' In Model 1, We can see after controlling for temperal trend, vertical drill production is correlated with radiation.But this relationship is not linear.
#+Run a GAM model to detect vertical correlation,message=T,echo=F,cache=F
g_v<-gam(result_amount~s(V_Prod)+s(month,k=12)+s(year,k=10),data = rad)
summary(g_v)
#' In Model 2, We can see after controlling for temperal trend, horitonal drill production is lightly correlated with radiation. This correlation is too weak for me.
#+Run a GAM model to detect horizontal correlation,message=T,echo=F,cache=F
g_h<-gam(result_amount~s(H_Prod)+s(month,k=12)+s(year,k=10),data = rad)
summary(g_h)
#' # Summary 
#' Through this study, we streamline the workflow for a single RadNet monitor and have some exploratary result.
#' 
#' To-do-list next week:
#' 
#' * Add PM2.5 mass/speciation data into the model.  
#' * Use the same method to organize data for other RadNet monitors.   
#' * Aggregate data from multiple monitors and model it with a mixed-effect model.  
#' * Talk with other faculties about the right model formula.
