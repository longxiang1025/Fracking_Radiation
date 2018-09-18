#' ---
#' title: "Model the relation between natural gas production and beta radiation"
#' author: "Longxiang Li"
#' date: "`r format(Sys.Date())`"
#' output: 
#'      html_document:
#'       toc: true
#'       fig_caption: yes
#' ---
#' 
#' # Introduction
#' In this report, we'll go through the workflow of data collection and compare the models.All the following models will start from 
#' here. 
#' 
#' # Data 
#' Study period: from 01-01-2007 to 12-31-2016
#' 
#' Study region: Lower 48 states of the United States
#' 
#' Data sources: 
#' 
#' Drilling data:
#' 
#' We colleced drilling information from drillinginfo.com. From the database, we extracted the natural gas wells active during the study 
#' period, both horizontal and vertical drillings are included. In the figure, the blue points indicate the location of these wells.
#' 
#' 
#' Radiation data:
#' 
#' We collected the beta radiation measured across the U.S through 139 radiation monitors mostly located within metropolitan areas. 
#' Due to the purpose of this monitor network, the exact locations are kept confidential. So we use the geocoded center of the city as
#' an atternative location. But due to the small spatial variation of beta radiation, it's safe to use this approximation. In the figure,
#' this network is represented by red points.
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
#+ load-data,message=F,echo=F
study_period<-interval(ymd("2001-01-01"),ymd("2017-12-31"))
prod_period<-interval(ymd("2001-01-01"),ymd("2018-09-10"))
prjstring<-"+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs "
geoprjstring<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
load( here::here("data","RadNet_And_Gas_50B.Rdata"))
names(rad_frac_data)[2]<-"beta"
RadNet_City_List<-read_csv(here::here("data","Processed-RadNet-Beta-citylist.csv"))
load(here::here("data","Basic_Geodata","Boundaries.RData"))
rad_frac_data$City<-as.factor(rad_frac_data$City)
load(here::here("data","All_Gas_Wells_Active_After_2007.RData"))
#+ general visualization,echo=F,message=F,cache=T,warning=F,fig.width=15,fig.height=9
while(!exists("US_map")){try(US_map<-get_googlemap("United States",zoom = 4))}

data_1<-header_list[,c("Longitude","Latitude")]
names(data_1)<-c("lon","lat")
data_1$Type="Gas_Well"
data_2<-RadNet_City_List[,c("lon","lat")]
data_2$Type="RadNet_Monitor"
vis_data<-rbind.data.frame(data_1,data_2)

map<-ggmap(US_map)
map<-map+geom_polygon(data = fortify(bound),
                      aes(long, lat, group = group),
                      colour = "black", alpha = 0)
map<-map+geom_point(data=vis_data,aes(x=lon,y=lat,color=Type,alpha=Type,size=Type))+
  scale_color_manual(values = c("Gas_Well" = "Blue",'RadNet_Monitor' = "Red"))+
  scale_alpha_manual(values= c("Gas_Well" = 0.05,'RadNet_Monitor' = 0.8))+
  scale_size_manual(values=c("Gas_Well" = 0.25,'RadNet_Monitor' = 1.5))
print(map)
#' # Pair Radiation Data and Drilling Data
#' In this study, buffers of serveral radius ranging from 15km to 100 km are created for each RadNet monitor. Then we select the drilling
#' wells within these buffers. In the following figure, two cities, Denver and Pittsburgh, are selected as example to show the steps. 
#' In the following figure, a translucent circle with a radius of 50 kilometers is created as the selection standard. within the buffer,
#' wells represented by solid black points are drills selected in the following study. In this figure, we can find that Pittsburgh has denser
#' natural gas production in study period.
#+ Detail visualization for two example areas,echo=F,message=F,warning=F,cache=T,fig.width=15,fig.height=9
coordinates(RadNet_City_List)<-~lon+lat
proj4string(RadNet_City_List)<-geoprjstring
coordinates(header_list)<-~Longitude+Latitude
proj4string(header_list)<-geoprjstring

while(!exists("Pitt_map")){try(Pitt_map<-get_googlemap("PITTSBURGH,PA",zoom = 9))}

while(!exists("Den_map")){try(Den_map<-get_googlemap("DENVER,CO",zoom = 9))}

p_map<-ggmap(Pitt_map)
p_map<-p_map+geom_point(data=as.data.frame(coordinates(header_list)),aes(x=Longitude,y=Latitude),color="Blue",alpha=0.1,size=0.5)
pitt_buffer<-buffer(RadNet_City_List[RadNet_City_List$city_state=="PITTSBURGH,PA",],50000)
p_map<-p_map+geom_polygon(data = fortify(pitt_buffer),
                      aes(long, lat, group = group),
                      colour = "red",fill="red", alpha = 0.1)
pitt_header<-crop(header_list,pitt_buffer)
p_map<-p_map+geom_point(data=as.data.frame(coordinates(pitt_header)),aes(x=Longitude,y=Latitude),size=1)

d_map<-ggmap(Den_map)
d_map<-d_map+geom_point(data=as.data.frame(coordinates(header_list)),aes(x=Longitude,y=Latitude),color="Blue",alpha=0.1,size=0.5)
den_buffer<-buffer(RadNet_City_List[RadNet_City_List$city_state=="DENVER,CO",],50000)
d_map<-d_map+geom_polygon(data = fortify(den_buffer),
                          aes(long, lat, group = group),
                          colour = "red",fill="red", alpha = 0.1)
den_header<-crop(header_list,den_buffer)
d_map<-d_map+geom_point(data=as.data.frame(coordinates(den_header)),aes(x=Longitude,y=Latitude),size=1)

detail_map<-grid.arrange(p_map,d_map,ncol=2)
#' At the end of this section, we select the RadNet monitors with active natural gas production in study period.
#' After this selection, out of the 139 RadNet monitors, only 37 are left (in the 50km buffer case). In the following figure, the selected monitors
#' are labeled by a circle witha a radius of 100km. the majority of RadNet monitors are excluded at this stage due to lack of active natural
#' gas production around.
#+ The last status of data before analysis,echo=F,message=F,warning=T,cache=T,fig.width=15,fig.height=9
t<-RadNet_City_List[RadNet_City_List$city_state%in%(unique(rad_frac_data$City)),]
for(i in 1:length(t)){
  center<-t[i,]
  map<-map+geom_polygon(data = fortify(buffer(center,50000)),
                            aes(long, lat, group = group),
                            colour = "red",fill="red", alpha = 0.1)
}
print(map)
#' # A Closer Look At Gas Prodcution Around Cities
#' 
#'Based on this pair result, we can calculate the monthly production and monthly number of active natural gas for each city/RadNet monitors.
#' In the study period, the monthly natural gas production skyrocketed after the introduction of horizontal drill. In Pittsburg, horizontal 
#' drills produced most of the gas with a small fraction of wells. In Denver, a similar story happened after 2012. But it's noticable that,
#' the number of wells around Denver is much smaller than that of Pittsburg.The number of active horizontal wells around Denver ranges from 0 to 200. The
#' number of active horizontal wells around Pittsburgh ranges from 1000 to 8000.
#+ Summary of drill production,echo=F,message=F,warning=F,cache=T,fig.keep='none'
pitt.plot.data<-melt(data=rad_frac_data[rad_frac_data$City=="PITTSBURGH,PA",c("H_Prod","V_Prod","Date")],id=c("Date"))
pitt.plot.data<-distinct(pitt.plot.data)
prod.plot<-ggplot(data=pitt.plot.data,aes(x=Date,y=value,fill=variable))+
  geom_area(colour="black", size=.2, alpha=.9)+
  scale_fill_brewer("Blues")+
  ggtitle("Monthly gas production of Pittsburgh")
pitt.plot.data<-melt(data=rad_frac_data[rad_frac_data$City=="PITTSBURGH,PA",c("H_Wells","V_Wells","Date")],id=c("Date"))
pitt.plot.data<-distinct(pitt.plot.data)
well.plot<-ggplot(data=pitt.plot.data,aes(x=Date,y=value,fill=variable))+
  geom_area(colour="black", size=.2, alpha=.6)+
  scale_fill_brewer("Blues")+
  ggtitle("Monthly number of active gas wells of Pittsburgh")
pitt.plot<-grid.arrange(prod.plot,well.plot, ncol=2)

den.plot.data<-melt(data=rad_frac_data[rad_frac_data$City=="DENVER,CO",c("H_Prod","V_Prod","Date")],id=c("Date"))
den.plot.data<-distinct(den.plot.data)
prod.plot<-ggplot(data=den.plot.data,aes(x=Date,y=value,fill=variable))+
  geom_area(colour="black", size=.2, alpha=.9)+
  scale_fill_brewer("Blues")+
  ggtitle("Monthly gas production of Denver")
den.plot.data<-melt(data=rad_frac_data[rad_frac_data$City=="DENVER,CO",c("H_Wells","V_Wells","Date")],id=c("Date"))
den.plot.data<-distinct(den.plot.data)
well.plot<-ggplot(data=den.plot.data,aes(x=Date,y=value,fill=variable))+
  geom_area(colour="black", size=.2, alpha=.6)+
  scale_fill_brewer("Blues")+
  ggtitle("Monthly number of active gas wells of Denver")
den.plot<-grid.arrange(prod.plot,well.plot, ncol=2)
#+ Plot of drill production side by side,echo=F,message=F,warning=F,cache=F,fig.width=15,fig.height=9
grid.arrange(pitt.plot,den.plot,nrow=2)
#' # Explore the Relationship between Horizontal Drill and Radiation
#' 
#' At the beginning of this section, we have 6-day mean beta radiation from 39 RadNet monitors and monthly production data from wells 
#' around RadNet monitors. It's reasonable to assume the production rate is stable within a single month for a drill, so we assign the 
#' production and number of active wells to every 6-day radiation measurement. In other words, for a month and monitor, we have five records
#' with different radiation level but the same natural gas production data.The primary goal of the following models is to explore the effects 
#' of unconventional natural gas production on the local radiation level. 
#' 
#' Before modeling them, we can summarize and visualize the monitor-specific data. The range of number of active horizontal wells differs
#' wildly between cities. The mimimum range belongs to MOBILE,AL, GRAND RAPIDS,MI and other cities. But for some other cities with a longer
#' history horizontal drilling, the number of monthly active wells could range from 1905 to 8952 in FORT WORTH,TX.
#+ Monitor-Drill Summary,echo=F,message=T,fig.keep='none'
names(rad_frac_data)[2]<-"beta"
rad_frac_data$log_beta<-log(rad_frac_data$beta)
rad_wells_summary<-rad_frac_data%>%
  group_by(City)%>%
  summarise(min_h_wells=min(H_Wells),
            max_h_wells=max(H_Wells),
            min_v_wells=min(V_Wells),
            max_v_wells=max(V_Wells),
            min_h_prod=min(H_Prod),
            max_h_prod=max(H_Prod),
            min_v_prod=min(V_Prod),
            max_v_prod=max(V_Prod))
kable(rad_wells_summary,caption = "City Specific Natural Gas Production/Wells")
#' ## Cities where number of wells has bigger range
#' For cities with big range of production or number of montly active wells, we can create scatter plot to have a general idea about the 
#' correlation between horizontal natural gas production activity with the beta radiation.
#' 
#' In Dallas, TX. There's a positive correlation
#' between the radiation and production&number of wells, as shown in the upper row of the following figure. The lower row shows the time
#' series of beta radiation (blue stacks), horizontal natural gas production (red line) and number of active wells (green line). There seems
#' an increasing trend of beta radiation. But this trend is not obvious.
#+ Dallas Plot,echo=F,message=T,out.width="95%",cache=T
rad_frac_data<-rad_frac_data%>%
  group_by(City,Date)%>%
  summarise(beta=mean(beta),
            H_Prod=mean(H_Prod),
            H_Wells=mean(H_Wells))
Dallas_Data<-rad_frac_data[rad_frac_data$City=="DALLAS,TX",]
sct_1<-ggplot(data=Dallas_Data,aes(x=H_Prod,y=beta))+
  geom_point(size=0.5)+
  ggtitle("Rad Vs Production")+theme_bw()

sct_2<-ggplot(data=Dallas_Data,aes(x=H_Wells,y=beta))+
  geom_point(size=0.5)+
  ggtitle("Rad Vs Wells")+theme_bw()

cvt_1 <- ggplot(Dallas_Data, aes(x = Date))
cvt_1 <- cvt_1 + geom_line(aes(y = H_Prod, colour = "H_Prod"),size=0.5)
cvt_1 <- cvt_1 + geom_col(aes(y = beta*1000, colour = "beta"),alpha=0.05,size=0.5,position="identity")
cvt_1 <- cvt_1 + scale_y_continuous(sec.axis = sec_axis(~./1000, name = "Radiation"))
cvt_1 <- cvt_1 + scale_colour_manual(values = c("blue", "red"))
cvt_1 <- cvt_1 + labs(y = "Horizontal Production",
              x = "Date and time",
              colour = "Parameter")
cvt_1 <- cvt_1 + theme(legend.position="bottom",
                       panel.background = element_rect(fill = NA,colour = "grey50"),
                       panel.grid.major = element_line(colour = "grey50"))

cvt_2 <- ggplot(Dallas_Data, aes(x = Date))
cvt_2 <- cvt_2 + geom_line(aes(y = H_Wells, colour = "H_Wells"),size=0.5)
cvt_2 <- cvt_2 + geom_col(aes(y = beta*50000, colour = "beta"),size=0.5,alpha=0.05,position="identity")
cvt_2 <- cvt_2 + scale_y_continuous(sec.axis = sec_axis(~./50000, name = "Radiation"))
cvt_2 <- cvt_2 + scale_colour_manual(values = c("blue", "Green"))
cvt_2 <- cvt_2 + labs(y = "Horizontal Wells",
                      x = "Date and time",
                      colour = "Parameter")
cvt_2 <- cvt_2 + theme(legend.position="bottom",
                       panel.background = element_rect(fill = NA,colour = "grey50"),
                       panel.grid.major = element_line(colour = "grey50"))

Dallas_plot<-grid.arrange(sct_1,sct_2,cvt_1,cvt_2,nrow=2, ncol=2,
                          top = textGrob("Dallas,TX",gp=gpar(fontsize=15,font=2)))
#' In Denver, CO, the correlation between beta and fracking production is positive when the production/number of wells are low. It seems 
#' when the production pass a point, the correlation is not remarkable.
#+ Denver Plot,echo=F,message=T,out.width="95%",cache=T
DENVER_Data<-rad_frac_data[rad_frac_data$City=="DENVER,CO",]
sct_1<-ggplot(data=DENVER_Data,aes(x=H_Prod,y=beta))+
  geom_point(size=0.5)+
  ggtitle("Rad Vs Production")+theme_bw()

sct_2<-ggplot(data=DENVER_Data,aes(x=H_Wells,y=beta))+
  geom_point(size=0.5)+
  ggtitle("Rad Vs Wells")+theme_bw()

cvt_1 <- ggplot(DENVER_Data, aes(x = Date))
cvt_1 <- cvt_1 + geom_line(aes(y = H_Prod, colour = "H_Prod"),size=0.5)
cvt_1 <- cvt_1 + geom_col(aes(y = beta*100, colour = "beta"),alpha=0.05,size=0.5,position="identity")
cvt_1 <- cvt_1 + scale_y_continuous(sec.axis = sec_axis(~./100, name = "Radiation"))
cvt_1 <- cvt_1 + scale_colour_manual(values = c("blue", "red"))
cvt_1 <- cvt_1 + labs(y = "Horizontal Production",
                      x = "Date and time",
                      colour = "Parameter")
cvt_1 <- cvt_1 + theme(legend.position="bottom",
                       panel.background = element_rect(fill = NA,colour = "grey50"),
                       panel.grid.major = element_line(colour = "grey50"))

cvt_2 <- ggplot(DENVER_Data, aes(x = Date))
cvt_2 <- cvt_2 + geom_line(aes(y = H_Wells, colour = "H_Wells"),size=0.5)
cvt_2 <- cvt_2 + geom_col(aes(y = beta*2000, colour = "beta"),alpha=0.05,size=0.5,position="identity")
cvt_2 <- cvt_2 + scale_y_continuous(sec.axis = sec_axis(~./2000, name = "Radiation"))
cvt_2 <- cvt_2 + scale_colour_manual(values = c("blue", "Green"))
cvt_2 <- cvt_2 + labs(y = "Horizontal Wells",
                      x = "Date and time",
                      colour = "Parameter")
cvt_2 <- cvt_2 + theme(legend.position="bottom",
                       panel.background = element_rect(fill = NA,colour = "grey50"),
                       panel.grid.major = element_line(colour = "grey50"))

DENVER_plot<-grid.arrange(sct_1,sct_2,cvt_1,cvt_2,nrow=2, ncol=2,
                        top = textGrob("DENVER,CO",gp=gpar(fontsize=15,font=2)))
#' In Pittsburgh, PA, the general correlation between beta radiation and fracking production/wells are both not significant.
#+ Pittsburgh Plot,echo=F,message=T,out.width="95%",cache=T
PITT_Data<-rad_frac_data[rad_frac_data$City=="PITTSBURGH,PA",]
sct_1<-ggplot(data=PITT_Data,aes(x=H_Prod,y=beta))+
  geom_point(size=0.5)+
  ggtitle("Rad Vs Production")+theme_bw()

sct_2<-ggplot(data=PITT_Data,aes(x=H_Wells,y=beta))+
  geom_point(size=0.5)+
  ggtitle("Rad Vs Wells")+theme_bw()

cvt_1 <- ggplot(PITT_Data, aes(x = Date))
cvt_1 <- cvt_1 + geom_line(aes(y = H_Prod, colour = "H_Prod"),size=0.5)
cvt_1 <- cvt_1 + geom_col(aes(y = beta*2000, colour = "beta"),alpha=0.05,size=0.5,position="identity")
cvt_1 <- cvt_1 + scale_y_continuous(sec.axis = sec_axis(~./2000, name = "Radiation"))
cvt_1 <- cvt_1 + scale_colour_manual(values = c("blue", "red"))
cvt_1 <- cvt_1 + labs(y = "Horizontal Production",
                      x = "Date and time",
                      colour = "Parameter")
cvt_1 <- cvt_1 + theme(legend.position="bottom",
                       panel.background = element_rect(fill = NA,colour = "grey50"),
                       panel.grid.major = element_line(colour = "grey50"))

cvt_2 <- ggplot(PITT_Data, aes(x = Date))
cvt_2 <- cvt_2 + geom_line(aes(y = H_Wells, colour = "H_Wells"),size=0.5)
cvt_2 <- cvt_2 + geom_col(aes(y = beta*50000, colour = "beta"),alpha=0.05,size=0.5,position="identity")
cvt_2 <- cvt_2 + scale_y_continuous(sec.axis = sec_axis(~./50000, name = "Radiation"))
cvt_2 <- cvt_2 + scale_colour_manual(values = c("blue", "Green"))
cvt_2 <- cvt_2 + labs(y = "Horizontal Wells",
                      x = "Date and time",
                      colour = "Parameter")
cvt_2 <- cvt_2 + theme(legend.position="bottom",
                       panel.background = element_rect(fill = NA,colour = "grey50"),
                       panel.grid.major = element_line(colour = "grey50"))

PITT_plot<-grid.arrange(sct_1,sct_2,cvt_1,cvt_2,nrow=2, ncol=2,
                        top = textGrob("Pittsburg,PA",gp=gpar(fontsize=15,font=2)))
#' ## Cities where number of wells has narrow range
#' Not every city has hundreds of horizontal wells around. We can binarize all measurements based on the existense of horizontal drill.
#' Then compare the radiation between these two categorize.In the following figure, we can see the difference between these two status:
#' with fracking well and without fracking well.
#+ Small fracking cities plot, echo=F, message=F, fig.width=16,fig.height=9,warning=F
Grand_Data<-rad_frac_data[rad_frac_data$City=="GRAND RAPIDS,MI",]
Grand_Data$H_Wells<-as.factor(Grand_Data$H_Wells>0)
g1<-ggplot(data=Grand_Data,aes(x=beta,fill=H_Wells))+geom_histogram(stat = "density",position = "identity",alpha=0.35)+ggtitle("GRAND RAPIDS,MI")

MOBILE_Data<-rad_frac_data[rad_frac_data$City=="MOBILE,AL",]
MOBILE_Data$H_Wells<-as.factor(MOBILE_Data$H_Wells>0)
g2<-ggplot(data=MOBILE_Data,aes(x=beta,fill=H_Wells))+geom_histogram(stat = "density",position = "identity",alpha=0.35)+ggtitle("MOBILE,AL")

grid.arrange(g1,g2,ncol=2)
#' #Model the correlation between horizontal drill and beta radiation.
#' Mixed effect model implemented in mgcv package is used in this study. We set our primary interest variable as fixed effect including:
#' number of active monthly wells, monthly production, but vertical and horizontal wells are modeled. For random effects, each city is 
#' assigned a random intercept to control for spatial confounding. In addition to control for temporal confounding, a penalized spline was
#' used to approximate temporal trend.
#+ Mixed Effect Model to all data. 50Km case study.,cache=T
for(r in c("30","50","75","100","125")){
  load( here::here("data",paste0("RadNet_And_Gas_",r,"B.Rdata")))
  names(rad_frac_data)[2]<-"beta"
  rad_wells_summary<-rad_frac_data%>%
    group_by(City)%>%
    summarise(min_h_wells=min(H_Wells),
              max_h_wells=max(H_Wells),
              min_v_wells=min(V_Wells),
              max_v_wells=max(V_Wells),
              min_h_prod=min(H_Prod),
              max_h_prod=max(H_Prod),
              min_v_prod=min(V_Prod),
              max_v_prod=max(V_Prod))
  rad_frac_data$log_beta<-log(rad_frac_data$beta)
  rad_frac_data$City<-as.factor(rad_frac_data$City)
  print(paste0(r,"km buffer case study"))
  rad_h_frac_data<-filter(rad_frac_data,rad_frac_data$City%in%unique(rad_wells_summary[rad_wells_summary$max_h_wells>0,]$City))
  #Our first model regards number of montthly active horizontal drills. The number of horizontal drills is positively significantly correlated beta radiation in this study. While the monthly production is negetive correlated with the beta radiation.
  g_h<-bam(beta~H_Wells+H_Prod+s(nmonth,k=120)+s(City,bs="re"),data=rad_h_frac_data,method="REML")
  print(paste0(r,"km buffer case study: Model 1"))
  print((summary(g_h))$p.table)
  #Based on this model, the maximum number of wells times slope is approximately over 50% of the mean radiation.
  print(range(rad_h_frac_data$H_Wells)[2]*coef(g_h)[2]/mean(rad_frac_data$beta))
  #Our second model only regards the number of wells. Based on the summary, after removing the production, the number of wells still remarkably relates with radiation. But the slope is only half of the coefficient of MODEl 1.
  g_h_wells<-bam(beta~H_Wells+s(nmonth,k=120)+s(City,bs="re"),data=rad_h_frac_data,method="REML")
  print(paste0(r,"km buffer case study: Model 2"))
  print((summary(g_h_wells))$p.table)
  #Our third model only regards production of the well. Based on the summary, after removing the number, the slope of production gets larger (close to 0).
  g_h_prod<-bam(beta~H_Prod+s(nmonth,k=120)+s(City,bs="re"),data=rad_h_frac_data,method="REML")
  print(paste0(r,"km buffer case study: Model 3"))
  print((summary(g_h_prod))$p.table)
}




