#' ---
#' title: "EDA of Drill Data"
#' author: "Longxiang Li"
#' date: "`r format(Sys.Date())`"
#' output: 
#'      pdf_document:
#'       toc: true
#'      fig_caption: yes
#' ---
#' 
#' # Introduction
#' 
#'The primary goal of this study is to explore the relationship between background $\beta$ radiation level and the drilling activity.
#'Much of the petroleum and natural gas developed in the U.S. was created in the earth's crust at the site of ancient seas by the decay of sea life.
#'As a result, these shale, petroleum and gas deposits often occur in aquifers containing brine (salt water).
#' Radionuclides, along with other minerals that are dissolved in the brine, separate and settle out, forming various wastes at the surface:
#'
#' * Mineral scales inside pipes.
#' 
#' * Sludges/sediments.
#' 
#' * Contaminated equipment or components.
#' 
#' * Produced waters.
#' 
#' Most of the produced waster is re-injected to the formation. But the disposal of the potential harzardous solid waste is almost unmonitored. It's 
#' reported in the news that the these wastes are dumped in open-air pit and residents nearby are complaining. Even though the citizens were most concerned
#' about the chemicals left in the waste such as benzne and the unknown risk in the fracking liquid, the radiation may also be emitted. So, the on-site
#' There's no statistic about the production/disposal of these solid waste. But it seems the solid waster from drilling goes to local waste management facility.
#' Besides the waste, the drilling wells themselves can accelerate the leaking of underground radon.
#' 
#' If this's the case, we may detect the variation of proxy of radon and relates it with the drilling activities.
#' 
#' # Data Collection and Management
#' In the past weeks, We have loaded all the data within our reach to the database and extract the data we need in our desired format.
#' Currently, we organize our information based on the RadNet monitors per month. The original measurement frequency of $\beta$ radiation is 3 days. But to
#' fit the data reporting frequency of drilling, we aggregated the $\beta$ radiation measurement (monthly average). Then we got the monthly $\beta$ data merged 
#' with gas/oil production data. This binding was based on the buffer around tha RadNet monitor. All gas/oil drilling information within this buffer was
#' aggregated (number of wells, sum production). Besides the general aggregation, we also did categorical aggregation based on drilling type (horizontal and vertical drilling). To 
#' find the optimal buffer radius, we created four cases: 25 km, 50 km, 75 km and 100 km.
#' 
#' These organized data files are stored in the shared dropbox folder now. In addition to the monthly $\beta$, categorized/overall production data, we
#' also add $PM_{2.5}$ mass data and $PM_{2.5}$ speciation data, field basin polygon data, USGS aeroradiometric data (including Potassium, Thorium and Uranium).These files
#' need strict check before being shared with other collaborators. Thee are some other datasets not included in the final .csv files such as the $\gamma$
#' measurement. 
#' 
#+ load-data, message = F, echo = F
library(tidyverse)
library(mgcv)
library(lme4)
library(ggplot2)
library(ggmap)
library(rpostgis)
library(RPostgreSQL)
library(raster)
library(gridExtra)
library(kableExtra)
library(formatR)
library(grid)
library(cowplot)
library(here)
#' # Exploratory Analysis
#' We can check the data quality by visualizing it. We can have a general idea of the data collection process.
#+ connected to Fracking databse and extract location of wells,echo=F,message=F,cache=T
pw <- {
  "koutrakis"
}
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "Fracking_Data",
                 host = "localhost", port = 5432,
                 user = "postgres", password = pw)
rm(pw)
us_basin<-pgGetGeom(con,"us_basin","basin_geom")
us_gas<-pgGetGeom(con,"Gas_Well_Headers","gas_well_geom")
us_oil<-pgGetGeom(con,"Oil_Well_Headers","oil_well_geom")
radnet<-pgGetGeom(con,"RadNet_Sp","radnet_geom")
#'First, we extract the location of wells and map it over basin boundaries. In the following plot, all active wells during the study period 01/01/2002-
#'12/21/2017 are ploted. 
#'We can see that the majority of gas drilling (blue points) and oil 
#'drilling (green points) are located within the basin (grey polygon). It's believed that the concentration of radionuclides varies remarkably
#'between geological formations. Adding basin as a categorical factor may help in the following study. 
#+Visualize the location of all wells,echo=F,warning=F,message=F,cache=T,out.width='95%'
rm(center)
center<-ggmap::geocode("United States",source = "dsk")
while(is.na(center[1])){try(center<-geocode("United States"))}
while(!exists("US_map")){try(US_map<-get_googlemap(as.numeric(center),zoom = 4,key="AIzaSyBKUgVXmsfj_HQ77IxRjSSNs7eQxYtyNis"))}
map<-ggmap(US_map)
map<-map+geom_polygon(data = fortify(us_basin),
                      aes(long, lat, group = group),
                      colour = "black", fill="grey",alpha = 0.5,size=1)
part_gas<-cbind.data.frame("Gas_Well",coordinates(us_gas))
names(part_gas)<-c("Type","lon","lat")
part_oil<-cbind.data.frame("Oil_Well",coordinates(us_oil))
names(part_oil)<-c("Type","lon","lat")
part_rad<-cbind.data.frame("RadNet_Monitor",coordinates(radnet))
names(part_rad)<-c("Type","lon","lat")
all_vis_data<-rbind.data.frame(part_gas,part_oil,part_rad)
map<-map+geom_point(data=all_vis_data,aes(x=lon,y=lat,color=Type,alpha=Type,size=Type))+
  scale_color_manual(values = c("Gas_Well" = "navyblue","Oil_Well" = "darkgreen",'RadNet_Monitor' = "Red"))+
  scale_alpha_manual(values= c("Gas_Well" = 0.05,"Oil_Well" = 0.05,'RadNet_Monitor' = 0.8))+
  scale_size_manual(values=c("Gas_Well" = 0.25,"Oil_Well" = 0.25,'RadNet_Monitor' = 1.5))
map<-map+ggtitle("Location of Gas/Oil Drillings in the Lower 48 States")
print(map)
#'Then we plot the locations of vertical drillings and horizontal drillings. Horizontal drillings are prevailing now due to its higher productivity.
#'Currently, the majority of new constructed drills are horizontal.But this trend doesn't occurs in every field. We can see that there're only few 
#'hrizontal drillingin Lousaina where the dominant drilling type is still vertical.
#+Visualize the location of horizontal and vertical drills,echo=F,warning=F,message=F,cache=T,out.width='95%'
map<-ggmap(US_map)
map<-map+geom_polygon(data = fortify(us_basin),
                      aes(long, lat, group = group),
                      colour = "black", fill="grey",alpha = 0.5,size=0.5)
part_gas<-cbind.data.frame("H_Gas_Well",coordinates(us_gas[us_gas$`Drill Type`=="H",]))
names(part_gas)<-c("Type","lon","lat")
part_oil<-cbind.data.frame("H_Oil_Well",coordinates(us_oil[us_oil$`Drill Type`=="H",]))
names(part_oil)<-c("Type","lon","lat")
part_rad<-cbind.data.frame("RadNet_Monitor",coordinates(radnet))
names(part_rad)<-c("Type","lon","lat")
vis_data<-rbind.data.frame(part_gas,part_oil,part_rad)
h_map<-map+geom_point(data=vis_data,aes(x=lon,y=lat,color=Type,alpha=Type,size=Type))+
  scale_color_manual(values = c("H_Gas_Well" = "navyblue","H_Oil_Well" = "darkgreen",'RadNet_Monitor' = "Red"))+
  scale_alpha_manual(values= c("H_Gas_Well" = 0.05,"H_Oil_Well" = 0.05,'RadNet_Monitor' = 0.8))+
  scale_size_manual(values=c("H_Gas_Well" = 0.25,"H_Oil_Well" = 0.25,'RadNet_Monitor' = 1.5))+
  theme(legend.position="bottom")
h_map<-h_map+ggtitle("Location of Fracking Drillings in the Lower 48 States")
h_map<-h_map+theme(plot.title = element_text(size = 6, face = "bold"))

map<-ggmap(US_map)
map<-map+geom_polygon(data = fortify(us_basin),
                      aes(long, lat, group = group),
                      colour = "black", fill="grey",alpha = 0.5,size=0.5)
part_gas<-cbind.data.frame("V_Gas_Well",coordinates(us_gas[us_gas$`Drill Type`=="V",]))
names(part_gas)<-c("Type","lon","lat")
part_oil<-cbind.data.frame("V_Oil_Well",coordinates(us_oil[us_oil$`Drill Type`=="V",]))
names(part_oil)<-c("Type","lon","lat")
part_rad<-cbind.data.frame("RadNet_Monitor",coordinates(radnet))
names(part_rad)<-c("Type","lon","lat")
vis_data<-rbind.data.frame(part_gas,part_oil,part_rad)
v_map<-map+geom_point(data=vis_data,aes(x=lon,y=lat,color=Type,alpha=Type,size=Type))+
  scale_color_manual(values = c("V_Gas_Well" = "navyblue","V_Oil_Well" = "darkgreen",'RadNet_Monitor' = "Red"))+
  scale_alpha_manual(values= c("V_Gas_Well" = 0.05,"V_Oil_Well" = 0.05,'RadNet_Monitor' = 0.8))+
  scale_size_manual(values=c("V_Gas_Well" = 0.25,"V_Oil_Well" = 0.25,'RadNet_Monitor' = 1.5))+
  theme(legend.position="bottom")
v_map<-v_map+ggtitle("Location of Conventional Drillings in the Lower 48 States")
v_map<-v_map+theme(plot.title = element_text(size = 6, face = "bold"))
grid.arrange(h_map,v_map,nrow=1)
#'# Dataset of all measurement within the basin area
#' We select the data within the basin area after 2012.In the following figure, we plot the trend of $\beta$ radiation in each city.
#' We can see it seems that, the trend of $\beta$ for each city is different. Since this's not our primary research interest, we 
#' can regard this as a cross random effect because the time doesn't work equally for every city.
#' 
#' It's also worth noting that, the duration of measruement for each RadNet monitor varies.So we need to filter the data based on the
#' duration. In the figure we can see that: KALISPELL,MT, MOBILE,AL, PAINESVILLE,OH and TOPEKA,KS can be excluded. 
#' 
#+ summarize the data,echo=F, message=F,warning=F,cache=F
load(here::here("data","beta_gas_oil_50.RData"))
rad_qs_zones<-rad_qs[!is.na(rad_qs$basin_name),]
rad_qs_zones<-rad_qs_zones[rad_qs_zones$YEAR>2006,]
rad_qs_zones<-rad_qs_zones[!is.na(rad_qs_zones$mass),]
rad_qs_zones<-rad_qs_zones[!rad_qs_zones$city_state%in%c("KALISPELL,MT", "CORPUS CHRISTI,TX","MOBILE,AL", "PAINESVILLE,OH","TOPEKA,KS","LUBBOCK,TX","LANSING,MI","HARLINGEN,TX"),]

summary_result<-rad_qs_zones%>%
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
rad_qs_zones<-filter(rad_qs_zones,city_state%in%unique(summary_result[summary_result$max_gas_num>0|summary_result$max_oil_prod>0,]$city_state))

#+ rescale the production and number of wells data,echo=F,message=F
rad_qs_zones$H_Gas_Prod<-rad_qs_zones$H_Gas_Prod/1000000
rad_qs_zones$V_Gas_Prod<-rad_qs_zones$V_Gas_Prod/1000000
rad_qs_zones$Gas_Prod<-rad_qs_zones$Gas_Prod/1000000

rad_qs_zones$H_Gas_Num<-rad_qs_zones$H_Gas_Num/100
rad_qs_zones$V_Gas_Num<-rad_qs_zones$V_Gas_Num/100
rad_qs_zones$Gas_Num<-rad_qs_zones$Gas_Num/100

rad_qs_zones$H_Oil_Prod<-rad_qs_zones$H_Oil_Prod/1000000
rad_qs_zones$V_Oil_Prod<-rad_qs_zones$V_Oil_Prod/1000000
rad_qs_zones$Oil_Prod<-rad_qs_zones$Oil_Prod/1000000

rad_qs_zones$H_Oil_Num<-rad_qs_zones$H_Oil_Num/100
rad_qs_zones$V_Oil_Num<-rad_qs_zones$V_Oil_Num/100
rad_qs_zones$Oil_Num<-rad_qs_zones$Oil_Num/100
rad_qs_zones$log_beta<-log(rad_qs_zones$beta)
rad_qs_zones$btpm<-rad_qs_zones$beta/rad_qs_zones$mass
rad_qs_zones$logbtpm<-rad_qs_zones$log_beta/rad_qs_zones$mass
rad_qs_zones$radon<-as.numeric(as.character(rad_qs_zones$radon))
rad_gas_zones<-filter(rad_qs_zones,city_state%in%unique(summary_result[summary_result$max_gas_num>20,]$city_state))

#+ Clean the data based on the duration of beta measurement, echo=F, message=F,warning=F,out.width='95%',cache=T
ggplot(data=rad_qs_zones,aes(x=Date,y=beta))+geom_point(size=0.5)+geom_smooth(size=0.5)+facet_wrap(~city_state)+
  theme( axis.text = element_text( size = 5 ),
         axis.text.x = element_text( size = 5 ),
         axis.title = element_text( size = 5, face = "bold" ),
         legend.position="none",
         strip.text = element_text(size = 5))
ggplot(data=rad_qs_zones,aes(x=Date,y=Gas_Num))+geom_point(size=0.5)+geom_smooth(size=0.5)+facet_wrap(~city_state)+
  theme( axis.text = element_text( size = 5 ),
         axis.text.x = element_text( size = 5 ),
         axis.title = element_text( size = 5, face = "bold" ),
         legend.position="none",
         strip.text = element_text(size = 5))
ggplot(data=rad_qs_zones,aes(x=Date,y=Gas_Prod))+geom_point(size=0.5)+geom_smooth(size=0.5)+facet_wrap(~city_state)+
  theme( axis.text = element_text( size = 5 ),
         axis.text.x = element_text( size = 5 ),
         axis.title = element_text( size = 5, face = "bold" ),
         legend.position="none",
         strip.text = element_text(size = 5))
ggplot(data=rad_qs_zones,aes(x=Date,y=Oil_Num))+geom_point(size=0.5)+geom_smooth(size=0.5)+facet_wrap(~city_state)+
  theme( axis.text = element_text( size = 5 ),
         axis.text.x = element_text( size = 5 ),
         axis.title = element_text( size = 5, face = "bold" ),
         legend.position="none",
         strip.text = element_text(size = 5))
ggplot(data=rad_qs_zones,aes(x=Date,y=Oil_Prod))+geom_point(size=0.5)+geom_smooth(size=0.5)+facet_wrap(~city_state)+
  theme( axis.text = element_text( size = 5 ),
         axis.text.x = element_text( size = 5 ),
         axis.title = element_text( size = 5, face = "bold" ),
         legend.position="none",
         strip.text = element_text(size = 5))
#' If we have a closer look at the city-specific data, we can see the trend of gas/oil production and construction within a radius of the city. For 
#' Fort Worth, TX, the gas production of horizontal drilling was actually declining. In the lower panel, dash thick line indicates the construction while
#' the thin solid line indicates the spuding number After the spuding peak and construction around 2012, the new
#' construction was almost pended due to the low oil price, the production decreased gradually. There's a single oil well spudding/construction in 2015.
#' After that, the oil production declined gradually.
#'

#+ visualize a demo city Fort Worth, TX,  echo=F, message=F,warning=F,cache=T,fig.width=16,fig.height=16
for(i in 1:length(unique(rad_qs_zones$city_state))){
  rm(center)
  rm(city_map)
  city<-unique(rad_qs_zones$city_state)[i]
  city_name<-city
  center<-coordinates(radnet[radnet$city_state==city_name,])
  while(!exists("city_map")){try(city_map<-get_googlemap(as.numeric(center),zoom = 9,key="AIzaSyBKUgVXmsfj_HQ77IxRjSSNs7eQxYtyNis"))}
  bb<-as.numeric(attr(city_map,"bb"))
  city_map<-ggmap::ggmap(city_map)
  city_data<-all_vis_data[all_vis_data$lon>bb[2]&all_vis_data$lon<bb[4]&all_vis_data$lat>bb[1]&all_vis_data$lon<bb[3],]
  print(nrow(city_data[city_data$Type=="Gas_Well",]))
  print(nrow(city_data[city_data$Type=="Oil_Well",]))
  print(nrow(city_data))
  
  city_buffer<-buffer(radnet[radnet$city_state==city_name,],50000)
  city_map<-city_map+geom_polygon(data = fortify(city_buffer),
                                  aes(long, lat, group = group),
                                  colour = "red",fill="red", alpha = 0.1)
  city_map<-city_map+
    geom_point(data=city_data,aes(x=lon,y=lat,color=Type,alpha=Type))+
    scale_color_manual(values = c('Gas_Well' = "Blue",'Oil_Well' = "Green",'RadNet_Monitor' = "Red"))+
    scale_alpha_manual(values= c('Gas_Well' = 0.3,'Oil_Well' = 0.3,'RadNet_Monitor' = 1))+
    scale_size_manual(values = c('Gas_Well' = 8,'Oil_Well' = 8,'RadNet_Monitor' = 12))+
    theme(legend.position = "bottom")
  print(city_map)
  beta_plot<-ggplot(data=rad_qs_zones[rad_qs_zones$city_state==city,],aes(x=Date,y=beta,group=YEAR))+geom_boxplot()
  gas_plot<-ggplot(data=rad_qs_zones[rad_qs_zones$city_state==city,],aes(x=Date,y=Gas_Prod))+geom_point()
  oil_plot<-ggplot(data=rad_qs_zones[rad_qs_zones$city_state==city,],aes(x=Date,y=Oil_Prod))+geom_point()
  oil_n_plot<-ggplot(data=rad_qs_zones[rad_qs_zones$city_state==city,],aes(x=Date,y=n_oil_cons))+geom_point()
  gas_n_plot<-ggplot(data=rad_qs_zones[rad_qs_zones$city_state==city,],aes(x=Date,y=n_gas_cons))+geom_point()
  pm_plot<-ggplot(data=rad_qs_zones[rad_qs_zones$city_state==city,],aes(x=Date,y=mass,group=YEAR))+geom_boxplot()
  
  print(cowplot::plot_grid(beta_plot,gas_plot,oil_plot,oil_n_plot,gas_n_plot,pm_plot))
}

fortworth_data<-rad_qs_zones[rad_qs_zones$city_state=="FORT WORTH,TX",]
h_gas_prod<-fortworth_data[,c("Date","H_Gas_Prod")]
names(h_gas_prod)<-c("Date","Gas_Prod")
h_gas_prod$Type="H"
v_gas_prod<-fortworth_data[,c("Date","V_Gas_Prod")]
names(v_gas_prod)<-c("Date","Gas_Prod")
v_gas_prod$Type="V"
gas_prod_data<-rbind.data.frame(h_gas_prod,v_gas_prod)
gas_prod_plot<-ggplot(data=gas_prod_data,aes(x=Date,y=Gas_Prod,fill=Type))+geom_bar(stat='identity')+ggtitle("Gas Production Plot")

h_oil_prod<-fortworth_data[,c("Date","H_Oil_Prod")]
names(h_oil_prod)<-c("Date","Oil_Prod")
h_oil_prod$Type="H"
v_oil_prod<-fortworth_data[,c("Date","V_Oil_Prod")]
names(v_oil_prod)<-c("Date","Oil_Prod")
v_oil_prod$Type="V"
oil_prod_data<-rbind.data.frame(h_oil_prod,v_oil_prod)
oil_prod_plot<-ggplot(data=oil_prod_data,aes(x=Date,y=Oil_Prod,fill=Type))+geom_bar(stat='identity')+ggtitle("Oil Production Plot")

h_gas_cons<-fortworth_data[,c("Date", "n_h_gas_spud","n_h_gas_cons")]
names(h_gas_cons)<-c("Date","Gas_Spud","Gas_Cons")
h_gas_cons$Type="H"
v_gas_cons<-fortworth_data[,c("Date", "n_v_gas_spud","n_v_gas_cons")]
names(v_gas_cons)<-c("Date","Gas_Spud","Gas_Cons")
v_gas_cons$Type="V"
gas_cons_data<-rbind.data.frame(h_gas_cons,v_gas_cons)
gas_cons_plot<-ggplot(data=gas_cons_data,aes(x=Date))+geom_line(aes(y=Gas_Spud,color=Type))
gas_cons_plot<-gas_cons_plot+geom_line(aes(y=Gas_Cons,color=Type),linetype=2,size=2)+ggtitle("Gas construction plot")

h_oil_cons<-fortworth_data[,c("Date", "n_h_oil_spud","n_h_oil_cons")]
names(h_oil_cons)<-c("Date","Oil_Spud","Oil_Cons")
h_oil_cons$Type="H"
v_oil_cons<-fortworth_data[,c("Date", "n_v_oil_spud","n_v_oil_cons")]
names(v_oil_cons)<-c("Date","Oil_Spud","Oil_Cons")
v_oil_cons$Type="V"
oil_cons_data<-rbind.data.frame(h_oil_cons,v_oil_cons)
oil_cons_plot<-ggplot(data=oil_cons_data,aes(x=Date))+geom_line(aes(y=Oil_Spud,color=Type))
oil_cons_plot<-oil_cons_plot+geom_line(aes(y=Oil_Cons,color=Type),linetype=2,size=2)+ggtitle("Oil construction plot")

grid.arrange(gas_prod_plot,oil_prod_plot,gas_cons_plot,oil_cons_plot,
             top = textGrob("Gas/Oil Plots of Fort Worth, TX",gp=gpar(fontsize=10,font=3)))
#' We can also have a closer look at the production around Pittsburgh, PA.
#+ visualize a demo city Pittsburgh, PA,  echo=F, message=F,warning=F,cache=F,out.width='95%'

pittsburgh_data<-rad_qs_zones[rad_qs_zones$city_state=="PITTSBURGH,PA",]
h_gas_prod<-pittsburgh_data[,c("Date","H_Gas_Prod")]
names(h_gas_prod)<-c("Date","Gas_Prod")
h_gas_prod$Type="H"
v_gas_prod<-pittsburgh_data[,c("Date","V_Gas_Prod")]
names(v_gas_prod)<-c("Date","Gas_Prod")
v_gas_prod$Type="V"
gas_prod_data<-rbind.data.frame(h_gas_prod,v_gas_prod)
gas_prod_plot<-ggplot(data=gas_prod_data,aes(x=Date,y=Gas_Prod,fill=Type))+geom_bar(stat='identity')+ggtitle("Gas Production Plot")

h_oil_prod<-pittsburgh_data[,c("Date","H_Oil_Prod")]
names(h_oil_prod)<-c("Date","Oil_Prod")
h_oil_prod$Type="H"
v_oil_prod<-pittsburgh_data[,c("Date","V_Oil_Prod")]
names(v_oil_prod)<-c("Date","Oil_Prod")
v_oil_prod$Type="V"
oil_prod_data<-rbind.data.frame(h_oil_prod,v_oil_prod)
oil_prod_plot<-ggplot(data=oil_prod_data,aes(x=Date,y=Oil_Prod,fill=Type))+geom_bar(stat='identity')+ggtitle("Oil Production Plot")

h_gas_cons<-pittsburgh_data[,c("Date", "n_h_gas_spud","n_h_gas_cons")]
names(h_gas_cons)<-c("Date","Gas_Spud","Gas_Cons")
h_gas_cons$Type="H"
v_gas_cons<-pittsburgh_data[,c("Date", "n_v_gas_spud","n_v_gas_cons")]
names(v_gas_cons)<-c("Date","Gas_Spud","Gas_Cons")
v_gas_cons$Type="V"
gas_cons_data<-rbind.data.frame(h_gas_cons,v_gas_cons)
gas_cons_plot<-ggplot(data=gas_cons_data,aes(x=Date))+geom_line(aes(y=Gas_Spud,color=Type))
gas_cons_plot<-gas_cons_plot+geom_line(aes(y=Gas_Cons,color=Type),linetype=2,size=2)+ggtitle("Gas construction plot")

h_oil_cons<-pittsburgh_data[,c("Date", "n_h_oil_spud","n_h_oil_cons")]
names(h_oil_cons)<-c("Date","Oil_Spud","Oil_Cons")
h_oil_cons$Type="H"
v_oil_cons<-pittsburgh_data[,c("Date", "n_v_oil_spud","n_v_oil_cons")]
names(v_oil_cons)<-c("Date","Oil_Spud","Oil_Cons")
v_oil_cons$Type="V"
oil_cons_data<-rbind.data.frame(h_oil_cons,v_oil_cons)
oil_cons_plot<-ggplot(data=oil_cons_data,aes(x=Date))+geom_line(aes(y=Oil_Spud,color=Type))
oil_cons_plot<-oil_cons_plot+geom_line(aes(y=Oil_Cons,color=Type),linetype=2,size=2)+ggtitle("Oil construction plot")

grid.arrange(gas_prod_plot,oil_prod_plot,gas_cons_plot,oil_cons_plot,
             top = textGrob("Gas/Oil Plots of Pittsburgh, PA",gp=gpar(fontsize=10,font=3)))

