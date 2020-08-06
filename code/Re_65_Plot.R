library(here)
library(readr)
library(raster)
library(tidyr)
library(dplyr)
library(reshape2)
library(rgeos)
library(ggplot2)
library(ggforce)
library(cowplot)

prjstring<-"+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs "
geoprjstring<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

RadNet_City_List<-read_csv(here::here("data","Processed-RadNet-Beta-citylist.csv"))
radnet_usgs_radio<-read_csv(here::here("data","radnet_radioraster_summary.csv"))
source(here::here("code","00_Functions.R"))
radnet_usgs_radio$city_state<-paste0(radnet_usgs_radio$city,",",radnet_usgs_radio$state)
load(here::here("data","RadNet.RData"))

states<-shapefile(here::here("data","Basic_Geodata","cb_2017_us_state_500k.shp"))
plays<-shapefile(here::here("data","Basic_Geodata","ShalePlays_US_EIA_Apr2019.shp"))
#radon_zone<-shapefile(here::here("data","USGS_Rn","usagrp_polygon.shp"))
load(here::here("data","Wells_3rd.RData"))

load(here::here("data","PM_MONITOR_ID_CONVERSION_TABLE.RData"))
monitor_list<-distinct(monitor_list[,c("Uni_ID","Longitude","Latitude")])
coordinates(monitor_list)<-~Longitude+Latitude
proj4string(monitor_list)<-geoprjstring
pm_monitors<-monitor_list

load(here::here("data","Daily_PM_Data.RData"))
pm_mass_data<-pm_mass_data[,c("Date Local","Arithmetic Mean","Uni_ID")]
names(pm_mass_data)[2]<-"pm_mass"


county<-shapefile(here::here("data","Basic_Geodata","cb_2017_us_county_500k.shp"))
usgs_u238<-raster(here::here("data","USGS_Radiometric","NAMrad_U1.tif"))
states<-states[as.numeric(states$GEOID)<60,]
states<-states[states$STUSPS!="AK"&states$STUSPS!="HI",]
states<-spTransform(states,CRS(proj4string(usgs_u238)))
county<-county[as.numeric(county$STATEFP)<60,]
county<-county[county$STATEFP!="02",]
county<-county[county$STATEFP!="15",]
county<-spTransform(county,CRS(proj4string(usgs_u238)))


wells<-wells%>%mutate(Oil=case_when(
  is.na(LiqCum)~ FALSE,
  LiqCum==0 ~ FALSE,
  ProdType=="OIL" ~ TRUE,
  ProdType=="O&G" ~ TRUE,
  LiqCum>0 ~ TRUE))
wells<-wells%>%mutate(Gas=case_when(
  is.na(GasCum)~ FALSE,
  GasCum==0 ~ FALSE,
  ProdType=="Gas" ~ TRUE,
  ProdType=="O&G" ~ TRUE,
  GasCum>0 ~ TRUE))

wells$x<-wells$lon
wells$y<-wells$lat
wells<-wells%>%filter(Status!="PERMITTED")
wells<-wells%>%filter(Status!="CANCELLED")
wells<-wells%>%filter(Oil|Gas)
coordinates(wells)<-~x+y
proj4string(wells)<-geoprjstring
wells<-spTransform(wells,CRS(proj4string(usgs_u238)))
pm_monitors<-spTransform(pm_monitors,CRS(proj4string(usgs_u238)))
radnet<-spTransform(radnet,CRS(proj4string(usgs_u238)))
plays<-spTransform(plays,CRS(proj4string(usgs_u238)))
pm_monitors<-pm_monitors[states,]

uwells<-wells[wells$DrillType=="H",]
uwells[is.na(uwells$CompDate),"CompDate"]=uwells[is.na(uwells$CompDate),]$FirstProdDate

radnet$Within=F
within<-radnet[buffer(plays,width=50000),]
outside<-radnet[!radnet$city_state%in%within$city_state,]
within$Within=T
radnet=bind(within,outside)

radnet_vis<-data.frame(coordinates(radnet))
radnet_vis$type=radnet$Within
radnet_vis[radnet_vis$type,"type"]="O&G RadNet Monitors"
radnet_vis[radnet_vis$type=="FALSE","type"]="Other RadNet Monitors"
names(radnet_vis)[1:2]<-c("x","y")
pm_monitors_vis<-data.frame(coordinates(pm_monitors[gBuffer(radnet,byid=T,width=100000),]))
pm_monitors_vis$type="PM Monitors"
names(pm_monitors_vis)[1:2]<-c("x","y")
#point_vis<-rbind.data.frame(pm_monitors_vis,radnet_vis)
point_vis=radnet_vis
well_vis=data.frame(coordinates(uwells),"UO&G Well")
names(well_vis)=c("x","y","type")
well_vis$type="UOGD Well"
g1<-ggplot()+
  geom_polygon(data=plays,aes(x=long,y=lat,group=group),color="gray",linetype="solid",fill="gray",size=0.25,show.legend = F)+
  geom_point(data=well_vis,aes(x=x,y=y,color=type,shape=type,size=type),stroke=0.03,fill="#5e3c99")+
  geom_polygon(data=states,aes(x=long,y=lat,group=group),color="black",linetype="solid",fill=NA,size=0.55,show.legend = F)+
  geom_point(data=point_vis,aes(x=x,y=y,col=type,shape=type,size=type),fill="#e66101",stroke = 2.0)+
  scale_color_manual(breaks=c("O&G RadNet Monitors","Other RadNet Monitors","UOGD Well"),values=c("Black","#e66101","#5e3c99"))+
  scale_shape_manual(breaks=c("O&G RadNet Monitors","Other RadNet Monitors","UOGD Well"),values=c(24,17,21))+
  scale_size_manual(breaks=c("O&G RadNet Monitors","Other RadNet Monitors","UOGD Well"),values=c(3,3,0.75))+
  coord_equal()+
  guides(fill = guide_legend(nrow=1,override.aes = list(alpha = 1),
                             title.position = "top",
                             label.position="bottom",
                             label.hjust = -0.05,
                             keyheight = 0.75))+
  theme(panel.border = element_rect(colour = "black", size=1,fill=NA),
        panel.background = element_rect(colour = "black", size=1,fill=NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.key.height = unit(1.25,"lines"),
        legend.key.width=unit(2.25,"lines"),
        legend.text = element_text(size=10,face='bold.italic'),
        legend.position = c(0.02, 0.13),
        legend.direction="vertical",
        legend.background = element_blank(),
        legend.spacing.x  =  unit(1,"lines"))

g1

######Figr 2
uwind<-stack(here::here("data","NARR","uwnd.10m.2014.nc"))
vwind<-stack(here::here("data","NARR","vwnd.10m.2014.nc"))
radnet<-radnet[radnet$city_state=="DALLAS,TX",]
uwind<-uwind[[330]]
vwind<-vwind[[330]]
radnet_wind<-prepare_wind_field(radnet,uwind,vwind,key="city_state")
data_a=radnet_wind[radnet_wind$city_state=="DALLAS,TX",]
radnet<-spTransform(radnet,CRS(proj4string(usgs_u238)))
buffer<-gBuffer(radnet,width = 50000,quadsegs=200)
rad_vis<-cbind.data.frame(coordinates(radnet),radnet$city_state)
names(rad_vis)<-c("x","y","city")
wells<-crop(wells,buffer)
wells<-wells[!is.na(wells$CompDate),]
temp_wind<-cbind.data.frame(coordinates(uwind),values(uwind),values(vwind))
names(temp_wind)<-c("x","y","u","v")
coordinates(temp_wind)<-~x+y
proj4string(temp_wind)<-proj4string(uwind)
wind_vis<-cbind.data.frame(temp_wind$x,temp_wind$y,temp_wind$u,temp_wind$v)
names(wind_vis)<-c("x","y","u","v")

dallas_extent<-spTransform(gBuffer(radnet,width = 50000,quadsegs=200),proj4string(uwind))
temp_wind<-crop(temp_wind,dallas_extent)
temp_wind<-spTransform(temp_wind,proj4string(usgs_u238))
wind_vis<-cbind.data.frame(temp_wind$x,temp_wind$y,temp_wind$u,temp_wind$v)
names(wind_vis)<-c("x","y","u","v")
wind_vis$vel=sqrt(wind_vis$u^2+wind_vis$v^2)
wind_vis$size=ifelse(wind_vis$vel>2,"Wind_Big","Wind_Medium")
wind_vis[wind_vis$vel<1.75,]$size<-"Wind_Small"

extent=c(coordinates(radnet)[1]-30000,coordinates(radnet)[1]+30000,
         coordinates(radnet)[2]-30000,coordinates(radnet)[2]+30000)
extent<-extent(extent)

u_vis<-cbind.data.frame(coordinates(wells[wells$Pred_DrillType=="H"&wells$CompDate<as.Date("2015-01-01"),]))

v_vis<-cbind.data.frame(coordinates(wells[wells$Pred_DrillType=="V"&wells$CompDate<as.Date("2015-01-01"),]))

pie <- data.frame(
  state = c('Out','In','In','Out'),
  start = c(0, pi*mean(data_a$dir-45)/180,pi*mean(data_a$dir)/180,pi*mean(data_a$dir+45)/180),
  end = c(pi*mean(data_a$dir-45)/180,pi*mean(data_a$dir)/180,pi*mean(data_a$dir+45)/180,2*pi),
  color=c("Out","In","In","Out"),
  alpha=c('Out','In','In','Out'),
  fill=c("Out","In","In","Out"),
  stringsAsFactors = FALSE
)
dallas_city_extent<-shapefile(here::here("data","Basic_Geodata","CityLimit.shp"))
dallas_city_extent<-spTransform(dallas_city_extent,proj4string(usgs_u238))

u_g_2<-ggplot()+
  coord_fixed()+
  xlim(c(coordinates(radnet)[1]-53000,coordinates(radnet)[1]+53000))+
  ylim(c(coordinates(radnet)[2]-80000,coordinates(radnet)[2]+53000))+
  geom_point(data=u_vis,aes(x=x,y=y),color="#006CD1",size=1)+
  geom_polygon(data=gBuffer(radnet,width=20000,quadsegs = 200),aes(x=long,y=lat,group=group),fill=NA,col="#994F00",show.legend = F)+
  geom_polygon(data=gBuffer(radnet,width=30000,quadsegs = 200),aes(x=long,y=lat,group=group),fill=NA,col="#994F00",show.legend = F)+
  geom_polygon(data=gBuffer(radnet,width=40000,quadsegs = 200),aes(x=long,y=lat,group=group),fill=NA,col="#994F00",show.legend = F)+
  geom_polygon(data=gBuffer(radnet,width=50000,quadsegs = 200),aes(x=long,y=lat,group=group),fill=NA,col="#994F00",show.legend = F)+
  geom_arc_bar(aes(x0 = rad_vis$x, y0 = rad_vis$y, r0 = 0, r = 50050, start = start, end = end, 
                   fill = fill,alpha=alpha,color=color),size=1.5,
               data = pie)+
  scale_color_manual(values = c("Out"=NA,"In"="#994F00"))+
  scale_fill_manual(values = c("Out"="white","In"="#994F00"))+
  scale_alpha_manual(values=c("Out"=0.5,"In"=0.15))+
  geom_polygon(data=dallas_city_extent,aes(x=long,y=lat,group=group),fill="Grey",col="Grey",alpha=0.5,show.legend = F)+
  geom_segment(data=wind_vis,aes(x=x,y=y,xend=x+5000*u,yend=y+5000*v),size = 1,arrow = arrow(length = unit(0.2,"cm")))+
  geom_text(data=rad_vis,aes(x=x,y=y,label="Dallas"),size=5)+
  geom_text(data=rad_vis,aes(x=x+20000*sin((data_a$dir[1]-45)*pi/180),y=y+20000*cos((data_a$dir[1]-45)*pi/180)),label="20 km",angle=225-data_a$dir[1],color="Black",size=4,fontface="bold")+
  geom_text(data=rad_vis,aes(x=x+30000*sin((data_a$dir[1]-45)*pi/180),y=y+30000*cos((data_a$dir[1]-45)*pi/180)),label="30 km",angle=225-data_a$dir[1],color="Black",size=4,fontface="bold")+
  geom_text(data=rad_vis,aes(x=x+40000*sin((data_a$dir[1]-45)*pi/180),y=y+40000*cos((data_a$dir[1]-45)*pi/180)),label="40 km",angle=225-data_a$dir[1],color="Black",size=4,fontface="bold")+
  geom_text(data=rad_vis,aes(x=x+50000*sin((data_a$dir[1]-45)*pi/180),y=y+50000*cos((data_a$dir[1]-45)*pi/180)),label="50 km",angle=225-data_a$dir[1],color="Black",size=4,fontface="bold")+
  ggtitle("Date: 2014-11-26")+
  theme(plot.title = element_text(hjust = 0.5,vjust = 0.9,face="bold",margin = ggplot2::margin(t = 10, b = -20)),
        legend.position="none",
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        panel.border = element_rect(colour = "black", size=1,fill=NA))+
  annotate("text",x=coordinates(radnet)[1]-52000,
           y=coordinates(radnet)[2]+50000,
           label="B",size=5)+
  annotate("text",x=coordinates(radnet)[1]-30000,
           y=coordinates(radnet)[2]-65000,
           label="Wind Velocity: 2.25 m/s \n Wind Direction: SW       ",size=3.5)+
  annotate("text",x=coordinates(radnet)[1]+29000,
           y=coordinates(radnet)[2]-68000,
           size=3.5,
           label = "N = 2      (R = 20 km)\nN = 192   (R = 30 km)\nN = 870  (R = 40 km)\nN = 2086 (R = 50 km)")
############# 
#the second half of Figure-2
uwind<-stack(here::here("data","NARR","uwnd.10m.2007.nc"))
vwind<-stack(here::here("data","NARR","vwnd.10m.2007.nc"))
radnet<-radnet[radnet$city_state=="DALLAS,TX",]
uwind<-uwind[[330]]
vwind<-vwind[[330]]
radnet_wind<-prepare_wind_field(radnet,uwind,vwind,key="city_state")
data_b=radnet_wind[radnet_wind$city_state=="DALLAS,TX",]
radnet<-spTransform(radnet,CRS(proj4string(usgs_u238)))
buffer<-gBuffer(radnet,width = 50000,quadsegs=200)
rad_vis<-cbind.data.frame(coordinates(radnet),radnet$city_state)
names(rad_vis)<-c("x","y","city")
wells<-crop(wells,buffer)
wells<-wells[!is.na(wells$CompDate),]
temp_wind<-cbind.data.frame(coordinates(uwind),values(uwind),values(vwind))
names(temp_wind)<-c("x","y","u","v")
coordinates(temp_wind)<-~x+y
proj4string(temp_wind)<-proj4string(uwind)
wind_vis<-cbind.data.frame(temp_wind$x,temp_wind$y,temp_wind$u,temp_wind$v)
names(wind_vis)<-c("x","y","u","v")

dallas_extent<-spTransform(gBuffer(radnet,width = 40000,quadsegs=200),proj4string(uwind))
temp_wind<-crop(temp_wind,dallas_extent)
temp_wind<-spTransform(temp_wind,proj4string(usgs_u238))
wind_vis<-cbind.data.frame(temp_wind$x,temp_wind$y,temp_wind$u,temp_wind$v)
names(wind_vis)<-c("x","y","u","v")
wind_vis$vel=sqrt(wind_vis$u^2+wind_vis$v^2)
wind_vis$size=ifelse(wind_vis$vel>2,"Wind_Big","Wind_Medium")

extent=c(coordinates(radnet)[1]-30000,coordinates(radnet)[1]+30000,
         coordinates(radnet)[2]-30000,coordinates(radnet)[2]+30000)
extent<-extent(extent)

u_vis<-cbind.data.frame(coordinates(wells[wells$Pred_DrillType=="H"&wells$CompDate<as.Date("2008-01-01"),]))

v_vis<-cbind.data.frame(coordinates(wells[wells$Pred_DrillType=="V"&wells$CompDate<as.Date("2015-01-01"),]))

pie <- data.frame(
  state = c('Out','In','In','Out'),
  start = c(0, pi*mean(data_b$dir-45)/180,pi*mean(data_b$dir)/180,pi*mean(data_b$dir+45)/180),
  end = c(pi*mean(data_b$dir-45)/180,pi*mean(data_b$dir)/180,pi*mean(data_b$dir+45)/180,2*pi),
  color=c("Out","In","In","Out"),
  alpha=c('Out','In','In','Out'),
  fill=c("Out","In","In","Out"),
  stringsAsFactors = FALSE
)
dallas_city_extent<-shapefile(here::here("data","Basic_Geodata","CityLimit.shp"))
dallas_city_extent<-spTransform(dallas_city_extent,proj4string(usgs_u238))

u_g_1<-ggplot()+
  coord_fixed()+
  xlim(c(coordinates(radnet)[1]-53000,coordinates(radnet)[1]+53000))+
  ylim(c(coordinates(radnet)[2]-80000,coordinates(radnet)[2]+53000))+
  geom_point(data=u_vis,aes(x=x,y=y),color="#006CD1",size=1)+
  geom_polygon(data=gBuffer(radnet,width=20000,quadsegs = 200),aes(x=long,y=lat,group=group),fill=NA,col="#994F00",show.legend = F)+
  geom_polygon(data=gBuffer(radnet,width=30000,quadsegs = 200),aes(x=long,y=lat,group=group),fill=NA,col="#994F00",show.legend = F)+
  geom_polygon(data=gBuffer(radnet,width=40000,quadsegs = 200),aes(x=long,y=lat,group=group),fill=NA,col="#994F00",show.legend = F)+
  geom_polygon(data=gBuffer(radnet,width=50000,quadsegs = 200),aes(x=long,y=lat,group=group),fill=NA,col="#994F00",show.legend = F)+
  geom_arc_bar(aes(x0 = rad_vis$x, y0 = rad_vis$y, r0 = 0, r = 50050, start = start, end = end, 
                   fill = fill,alpha=alpha,color=color),size=1.5,
               data = pie)+
  scale_color_manual(values = c("Out"=NA,"In"="#994F00"))+
  scale_fill_manual(values = c("Out"="white","In"="#994F00"))+
  scale_alpha_manual(values=c("Out"=0.5,"In"=0.15))+
  geom_polygon(data=dallas_city_extent,aes(x=long,y=lat,group=group),fill="Grey",col="Grey",alpha=0.5,show.legend = F)+
  geom_segment(data=wind_vis,aes(x=x,y=y,xend=x+5000*u,yend=y+5000*v),size = 1,arrow = arrow(length = unit(0.2,"cm")))+
  geom_text(data=rad_vis,aes(x=x,y=y,label="Dallas"),size=5)+
  geom_text(data=rad_vis,aes(x=x+20000*sin((data_b$dir[1]-45)*pi/180),y=y+20000*cos((data_b$dir[1]-45)*pi/180)),label="20 km",angle=225-data_b$dir[1],color="Black",size=4,fontface="bold")+
  geom_text(data=rad_vis,aes(x=x+30000*sin((data_b$dir[1]-45)*pi/180),y=y+30000*cos((data_b$dir[1]-45)*pi/180)),label="30 km",angle=225-data_b$dir[1],color="Black",size=4,fontface="bold")+
  geom_text(data=rad_vis,aes(x=x+40000*sin((data_b$dir[1]-45)*pi/180),y=y+40000*cos((data_b$dir[1]-45)*pi/180)),label="40 km",angle=225-data_b$dir[1],color="Black",size=4,fontface="bold")+
  geom_text(data=rad_vis,aes(x=x+50000*sin((data_b$dir[1]-45)*pi/180),y=y+50000*cos((data_b$dir[1]-45)*pi/180)),label="50 km",angle=225-data_b$dir[1],color="Black",size=4,fontface="bold")+
  ggtitle("Date: 2007-11-26")+
  theme(plot.title = element_text(hjust = 0.5,vjust = 0.9,face="bold",margin = ggplot2::margin(t = 10, b = -20)),
        legend.position="none",
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        panel.border = element_rect(colour = "black", size=1,fill=NA))+
  annotate("text",x=coordinates(radnet)[1]-52000,
           y=coordinates(radnet)[2]+50000,
           label="A",size=5)+
  annotate("text",x=coordinates(radnet)[1]-30000,
           y=coordinates(radnet)[2]-65000,
           label="Wind Velocity: 3.33 m/s \n Wind Direction: NW       ",size=3.5)+
  annotate("text",x=coordinates(radnet)[1]+29000,
           y=coordinates(radnet)[2]-68000,
           size=3.5,
           label = "N = 0      (R = 20 km)\nN = 41    (R = 30 km)\nN = 93    (R = 40 km)\nN = 249  (R = 50 km)")

#Figure 3
model_results=read_csv(here::here("Re_Results.csv"))
names(model_results)[1:10]=c("Index","Coef","Sd","t","L_CI","U_CI","Scale","var","radius","angle")
xlab="Buffer Radius (km)"
ylab=bquote(Increase~In~Gross~beta~radiation~of~particulate~(~mBq/m^3))
y_scale=3700
#y_scale=100000

u_g<-model_results%>%filter(var%in%c("u_h","u_v"),radius<7,angle==45)%>%ggplot()+
  geom_line(aes(x=20+(radius)*5,y=y_scale*Coef,color=var),size=0.75,linetype="dashed")+
  geom_point(aes(x=20+(radius)*5,y=y_scale*Coef,color=var),size=3)+
  geom_errorbar(aes(x=20+(radius)*5,ymax=y_scale*U_CI,ymin=y_scale*L_CI,color=var,width=1),linetype="solid",size=1)+
  scale_color_manual(values=c("Red","Blue"),labels=c("UOGD","COGD"))+
  xlab(xlab)+
  ylab(ylab)+
  geom_abline(intercept = 0,slope = 0)+
  scale_x_continuous(breaks = seq(20,50,5),labels =seq(20,50,5),expand=c(0,0),limits = c(15,55))+
  scale_y_continuous(breaks=seq(-0.005,0.03,0.005),limits=c(-0.002,0.03))+
  theme_classic()+
  theme(plot.title = element_text(size=14,hjust = 0.5,vjust = 0.9,face="bold",margin = ggplot2::margin(t = 10, b = -20)),
        legend.position = c(0.7,0.8),
        legend.title = element_blank(),
        legend.text = element_text(size=12,face = "bold"),
        legend.key.width = unit(5,"lines"),
        legend.key.height = unit(3,"lines"),
        axis.text = element_text(size=14,face="bold"),
        axis.title.x = element_text(size = 14,face="bold"),
        axis.title.y = element_text(size = 14,face="bold"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", size=1,fill=NA))
u_g

#Subregional analysis

sub_extent=states[states$STUSPS%in%c("PA","NY","MD","OH","WV","NJ"),]
well_vis=data.frame(coordinates(uwells[sub_extent,]),"UO&G Well")
names(well_vis)=c("x","y","type")
well_vis$type="UOGD Well"

radnet_vis<-data.frame(coordinates(radnet[sub_extent,]))
names(radnet_vis)[1:2]<-c("x","y")
point_vis=radnet_vis

buffer20<-buffer(radnet[sub_extent,],width = 20000,dissolve=F)
buffer50<-buffer(radnet[sub_extent,],width = 50000,dissolve=F)

ggplot()+
  geom_point(data=well_vis,aes(x=x,y=y),color="#5e3c99",size=0.1)+
  geom_polygon(data=sub_extent,aes(x=long,y=lat,group=group),color="black",linetype="dotted",fill=NA,size=0.5,show.legend = F)+
  geom_point(data=point_vis,aes(x=x,y=y),color="#e66101",shape=17,size=3)+
  geom_polygon(data=buffer20,aes(x=long,y=lat,group=group),color="red",fill=NA,size=0.5)+
  geom_polygon(data=buffer50,aes(x=long,y=lat,group=group),color="red",fill=NA,size=0.25)+
  coord_equal(xlim =extent(sub_extent)[1:2],
                  ylim=extent(sub_extent)[3:4],clip = "on" )+
  guides(fill = guide_legend(nrow=1,override.aes = list(alpha = 1),
                             title.position = "top",
                             label.position="bottom",
                             label.hjust = -0.05,
                             keyheight = 0.75))+
  theme(panel.border = element_rect(colour = "black", size=1,fill=NA),
        panel.background = element_rect(colour = "black", size=1,fill=NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.key.height = unit(1.25,"lines"),
        legend.key.width=unit(2.25,"lines"),
        legend.text = element_text(size=10,face='bold.italic'),
        legend.position = c(0.02, 0.13),
        legend.direction="vertical",
        legend.background = element_blank(),
        legend.spacing.x  =  unit(1,"lines"))
#Subregion 2
sub_extent=states[states$STUSPS%in%c("WY","ND","SD","MT","CO","UT","NM"),]
well_vis=data.frame(coordinates(uwells[sub_extent,]),"UO&G Well")
names(well_vis)=c("x","y","type")
well_vis$type="UOGD Well"

radnet_vis<-data.frame(coordinates(radnet[sub_extent,]))
names(radnet_vis)[1:2]<-c("x","y")
point_vis=radnet_vis

buffer20<-buffer(radnet[sub_extent,],width = 20000,dissolve=F)
buffer50<-buffer(radnet[sub_extent,],width = 50000,dissolve=F)

ggplot()+
  geom_point(data=well_vis,aes(x=x,y=y),color="#5e3c99",size=0.1)+
  geom_polygon(data=sub_extent,aes(x=long,y=lat,group=group),color="black",linetype="dotted",fill=NA,size=0.5,show.legend = F)+
  geom_point(data=point_vis,aes(x=x,y=y),color="#e66101",shape=17,size=3)+
  geom_polygon(data=buffer20,aes(x=long,y=lat,group=group),color="red",fill=NA,size=0.5)+
  geom_polygon(data=buffer50,aes(x=long,y=lat,group=group),color="red",fill=NA,size=0.25)+
  coord_equal(xlim =extent(sub_extent)[1:2],
              ylim=extent(sub_extent)[3:4],clip = "on" )+
  guides(fill = guide_legend(nrow=1,override.aes = list(alpha = 1),
                             title.position = "top",
                             label.position="bottom",
                             label.hjust = -0.05,
                             keyheight = 0.75))+
  theme(panel.border = element_rect(colour = "black", size=1,fill=NA),
        panel.background = element_rect(colour = "black", size=1,fill=NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.key.height = unit(1.25,"lines"),
        legend.key.width=unit(2.25,"lines"),
        legend.text = element_text(size=10,face='bold.italic'),
        legend.position = c(0.02, 0.13),
        legend.direction="vertical",
        legend.background = element_blank(),
        legend.spacing.x  =  unit(1,"lines"))

#sensitivity analysis to central angle
model_results=read_csv(here::here("Re_Results.csv"))
names(model_results)[1:10]=c("Index","Coef","Sd","t","L_CI","U_CI","Scale","var","radius","angle")
xlab="Buffer Radius (km)"
ylab=bquote(Increase~In~Gross~beta~radiation~of~particulate~(~mBq/m^3))
y_scale=3700
#y_scale=100000
model_results$angle=as.factor(model_results$angle)
model_results$h_j=as.numeric(model_results$angle)

model_results%>%filter(var%in%c("u_h"),radius<7)%>%ggplot()+
  geom_line(aes(x=20+(radius)*5+0.5*h_j,y=y_scale*Coef,color=angle),size=0.75,linetype="dashed")+
  geom_point(aes(x=20+(radius)*5+0.5*h_j,y=y_scale*Coef,color=angle),size=3)+
  geom_errorbar(aes(x=20+(radius)*5+0.5*h_j,ymax=y_scale*U_CI,ymin=y_scale*L_CI,color=angle,width=1),
                linetype="solid",size=1)+
  scale_color_manual(breaks=c("30","45","60"),
                     values=c("#cc79a7","#0072b2","#f0e442"),
                     labels=c("30 Degree","45 Degree","60 Degree"))+
  xlab(xlab)+
  ylab(ylab)+
  geom_abline(intercept = 0,slope = 0)+
  scale_x_continuous(breaks = seq(20,50,5),labels =seq(20,50,5),expand=c(0,0),limits = c(15,55))+
  scale_y_continuous(breaks=seq(-0.005,0.05,0.005),limits=c(-0.002,0.045))+
  theme_classic()+
  theme(plot.title = element_text(size=14,hjust = 0.5,vjust = 0.9,face="bold",margin = margin(t = 10, b = -20)),
        legend.position = c(0.7,0.8),
        legend.title = element_blank(),
        legend.text = element_text(size=12,face = "bold"),
        legend.key.width = unit(5,"lines"),
        legend.key.height = unit(3,"lines"),
        axis.text = element_text(size=14,face="bold"),
        axis.title.x = element_text(size = 14,face="bold"),
        axis.title.y = element_text(size = 14,face="bold"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", size=1,fill=NA))
u_g
