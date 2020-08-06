library(here)
library(dplyr)
library(stringr)
library(ggplot2)
library(raster)
library(rgeos)
prjstring<-"+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs "
geoprjstring<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

files<-list.files("/n/scratchlfs/koutrakis_lab/longxiang/beta_rgn_model/model_final_Gross")
rgn=str_split(files,pattern = "=",simplify = T)[,1]
radius=str_split(files,pattern = "=",simplify = T)[,2]
metrics=str_sub(str_split(files,pattern = "=",simplify = T)[,3],start = 1,end=7)
models<-cbind.data.frame(files,radius,metrics)
names(models)<-c("file_name","radius","metric")
models$metric=as.character(models$metric)
models$radius=as.numeric(as.character(models$radius))
models$rgn=as.character(rgn)
models$file_name=as.character(models$file_name)
models<-models%>%arrange(radius)
models$Direction=str_sub(models$metric,1,1)
models$Type=str_sub(models$metric,3,3)
#models$Prod=str_sub(models$metric,5,7)
models$Coef=0
models$Coef_md=0
models$CI_Low=0
models$CI_Up=0
models$CI_Up_md=0
models$CI_Low_md=0
models$range=0

for(i in 1:nrow(models)){
  load(paste0("/n/scratchlfs/koutrakis_lab/longxiang/beta_rgn_model/model_final_Gross/",models[i,"file_name"]))
  #row.names(basic_coef)[15]
  #hist(inf$alt.fixed[,15])
  #which.min(inf$alt.fixed[,15])
  models[i,"Coef"]<-basic_coef[15,1]
  models[i,c("CI_Low","CI_Up")]<-bCI[17,]
  models[i,"Coef_md"]<-md_coef[15,1]
  models[i,c("CI_Up_md","CI_Low_md")]<-bCI_md[17,]
}

xlab="Radius of the buffer (km)"
oil_ylab=bquote(Increase~of~gross~beta~radiation~per~100~upwind~oil~wells~~~~(pCi/m^3))
gas_ylab=bquote(Increase~of~gross~beta~radiation~per~100~upwind~gas~wells~~~~(pCi/m^3))
y_scale=100

gas_g_1<-models%>%filter(rgn=="1",Prod=="gas",Direction=="u",radius<9,radius>0)%>%ggplot()+
  geom_line(aes(x=20+(radius)*5,y=y_scale*Coef,color=Type),size=0.75)+
  geom_point(aes(x=20+(radius)*5,y=y_scale*Coef,color=Type),size=2)+  geom_errorbar(aes(x=20+(radius)*5,ymax=y_scale*CI_Up,ymin=y_scale*CI_Low,color=Type,width=1))+
  scale_color_manual(values=c("Red","Blue"),labels=c("Unconventional Gas","Conventional Gas"))+
  xlab(xlab)+
  ylab(gas_ylab)+
  ggtitle("Region 1") +
  coord_cartesian()+
  geom_abline(intercept = 0,slope = 0)+
  scale_x_continuous(breaks = seq(25,60,5),labels =seq(25,60,5),expand=c(0,0),limits = c(20,65))+
  scale_y_continuous(breaks=seq(-0.0005,0.001,0.0005),limits=c(-0.0005,0.001))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5,vjust = 0.9,face="bold",margin = margin(t = 10, b = -20)),
        legend.position = c(0.7,0.8),
        legend.title = element_blank(),
        legend.text = element_text(size=10,face = "bold"),
        legend.key.width = unit(1,"cm"),
        axis.text = element_text(size=8,face="bold"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", size=1,fill=NA))

gas_g_2<-models%>%filter(rgn=="2",Prod=="gas",Direction=="u",radius<9,radius>0)%>%ggplot()+
  geom_line(aes(x=20+(radius)*5,y=y_scale*Coef,color=Type),size=0.75)+
  geom_point(aes(x=20+(radius)*5,y=y_scale*Coef,color=Type),size=2)+  geom_errorbar(aes(x=20+(radius)*5,ymax=y_scale*CI_Up,ymin=y_scale*CI_Low,color=Type,width=1))+
  scale_color_manual(values=c("Red","Blue"),labels=c("Unconventional Gas","Conventional Gas"))+
  xlab(xlab)+
  ylab(gas_ylab)+
  ggtitle("Region 2") +
  geom_abline(intercept = 0,slope = 0)+
  scale_x_continuous(breaks = seq(25,65,5),labels =seq(25,65,5),expand=c(0,0),limits = c(25,65))+
  scale_y_continuous(breaks=seq(-0.0005,0.001,0.0005),limits=c(-0.0005,0.001))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5,vjust = 0.9,face="bold",margin = margin(t = 10, b = -20)),
        legend.position = c(0.7,0.8),
        legend.title = element_blank(),
        legend.text = element_text(size=10,face = "bold"),
        legend.key.width = unit(1,"cm"),
        axis.text = element_text(size=8,face="bold"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", size=1,fill=NA))
gas_g_3<-models%>%filter(rgn=="3",Prod=="gas",Direction=="u",radius<9,radius>0)%>%ggplot()+
  geom_line(aes(x=20+(radius)*5,y=y_scale*Coef,color=Type),size=0.75)+
  geom_point(aes(x=20+(radius)*5,y=y_scale*Coef,color=Type),size=2)+  geom_errorbar(aes(x=20+(radius)*5,ymax=y_scale*CI_Up,ymin=y_scale*CI_Low,color=Type,width=1))+
  scale_color_manual(values=c("Red","Blue"),labels=c("Unconventional Gas","Conventional Gas"))+
  xlab(xlab)+
  ylab(gas_ylab)+
  ggtitle("Region 3") +
  geom_abline(intercept = 0,slope = 0)+
  scale_x_continuous(breaks = seq(25,65,5),labels =seq(25,65,5),expand=c(0,0),limits = c(25,65))+
  scale_y_continuous(breaks=seq(-0.0005,0.002,0.0005),limits=c(-0.0005,0.002))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5,vjust = 0.9,face="bold",margin = margin(t = 10, b = -20)),
        legend.position = c(0.7,0.8),
        legend.title = element_blank(),
        legend.text = element_text(size=10,face = "bold"),
        legend.key.width = unit(1,"cm"),
        axis.text = element_text(size=8,face="bold"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", size=1,fill=NA))
oil_g_1<-models%>%filter(rgn=="1",Prod=="oil",Direction=="u",radius<9,radius>0)%>%ggplot()+
  geom_line(aes(x=20+(radius)*5,y=y_scale*Coef,color=Type),size=0.75)+
  geom_point(aes(x=20+(radius)*5,y=y_scale*Coef,color=Type),size=2)+
  geom_errorbar(aes(x=20+(radius)*5,ymax=y_scale*CI_Up,ymin=y_scale*CI_Low,color=Type,width=1))+
  scale_color_manual(values=c("Red","Blue"),labels=c("Unconventional Oil","Conventional Oil"))+
  xlab(xlab)+
  ylab(oil_ylab)+
  coord_cartesian()+
  ggtitle("Region 1") +
  geom_abline(intercept = 0,slope = 0)+
  scale_x_continuous(breaks = seq(25,65,5),labels =seq(25,65,5),expand=c(0,0),limits = c(25,65))+
  scale_y_continuous(breaks=seq(-0.0005,0.001,0.0005),limits=c(-0.0005,0.001))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5,vjust = 0.9,face="bold",margin = margin(t = 10, b = -20)),
        legend.position = c(0.7,0.8),
        legend.title = element_blank(),
        legend.text = element_text(size=10,face = "bold"),
        legend.key.width = unit(1,"cm"),
        axis.text = element_text(size=8,face="bold"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", size=1,fill=NA))
oil_g_2<-models%>%filter(rgn=="2",Prod=="oil",Direction=="u",radius<9,radius>0)%>%ggplot()+
  geom_line(aes(x=20+(radius)*5,y=y_scale*Coef,color=Type),size=0.75)+
  geom_point(aes(x=20+(radius)*5,y=y_scale*Coef,color=Type),size=2)+
  geom_errorbar(aes(x=20+(radius)*5,ymax=y_scale*CI_Up,ymin=y_scale*CI_Low,color=Type,width=1))+
  scale_color_manual(values=c("Red","Blue"),labels=c("Unconventional Oil","Conventional Oil"))+
  xlab(xlab)+
  ylab(oil_ylab)+
  ggtitle("Region 2") +
  geom_abline(intercept = 0,slope = 0)+
  scale_x_continuous(breaks = seq(25,65,5),labels =seq(25,65,5),expand=c(0,0),limits = c(25,65))+
  scale_y_continuous(breaks=seq(-0.0005,0.001,0.0005),limits=c(-0.0005,0.001))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5,vjust = 0.9,face="bold",margin = margin(t = 10, b = -20)),
        legend.position = c(0.7,0.8),
        legend.title = element_blank(),
        legend.text = element_text(size=10,face = "bold"),
        legend.key.width = unit(1,"cm"),
        axis.text = element_text(size=8,face="bold"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", size=1,fill=NA))
oil_g_3<-models%>%filter(rgn=="3",Prod=="oil",Direction=="u",radius<9,radius>0)%>%ggplot()+
  geom_line(aes(x=20+(radius)*5,y=y_scale*Coef,color=Type),size=0.75)+
  geom_point(aes(x=20+(radius)*5,y=y_scale*Coef,color=Type),size=2)+  geom_errorbar(aes(x=20+(radius)*5,ymax=y_scale*CI_Up,ymin=y_scale*CI_Low,color=Type,width=1))+
  scale_color_manual(values=c("Red","Blue"),labels=c("Unconventional Oil","Conventional Oil"))+
  xlab(xlab)+
  ylab(gas_ylab)+
  ggtitle("Region 3") +
  geom_abline(intercept = 0,slope = 0)+
  scale_x_continuous(breaks = seq(25,65,5),labels =seq(25,65,5),expand=c(0,0),limits = c(25,65))+
  scale_y_continuous(breaks=seq(-0.0005,0.002,0.0005),limits=c(-0.0005,0.002))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5,vjust = 0.9,face="bold",margin = margin(t = 10, b = -20)),
        legend.position = c(0.7,0.8),
        legend.title = element_blank(),
        legend.text = element_text(size=10,face = "bold"),
        legend.key.width = unit(1,"cm"),
        axis.text = element_text(size=8,face="bold"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", size=1,fill=NA))

load(here::here("data","RadNet.RData"))
usgs_u238<-raster(here::here("data","USGS_Radiometric","NAMrad_U1.tif"))
radnet<-spTransform(radnet,CRS(proj4string(usgs_u238)))
states<-shapefile(here::here("data","Basic_Geodata","cb_2017_us_state_500k.shp"))
county<-shapefile(here::here("data","Basic_Geodata","cb_2017_us_county_500k.shp"))
states<-states[as.numeric(states$GEOID)<60,]
states<-states[states$STUSPS!="AK"&states$STUSPS!="HI",]
states<-spTransform(states,CRS(proj4string(usgs_u238)))
county<-county[as.numeric(county$STATEFP)<60,]
county<-county[county$STATEFP!="02",]
county<-county[county$STATEFP!="15",]
county<-spTransform(county,CRS(proj4string(usgs_u238)))
load(here::here("data","PM_MONITOR_ID_CONVERSION_TABLE.RData"))
monitor_list<-distinct(monitor_list[,c("Uni_ID","Longitude","Latitude")])
coordinates(monitor_list)<-~Longitude+Latitude
proj4string(monitor_list)<-geoprjstring
pm_monitors<-monitor_list
pm_monitors<-spTransform(pm_monitors,CRS(proj4string(usgs_u238)))

load(here::here("data","Wells_3rd.RData"))
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
ras_data<-cbind.data.frame(coordinates(usgs_u238),values(usgs_u238))
names(ras_data)<-c("x","y","u")

moving_window=matrix(0,nrow=41,41)
for(x in 1:41){
  for(y in 1:41){
    if(((x-21)^2+(y-21)^2)<401){
      moving_window[x,y]=1
    }
  }
}

#wells<-wells[!is.na(wells$CompDate),]
uwells<-wells[wells$DrillType=="H",]
u_oil_raster<-rasterize(coordinates(uwells[uwells$Oil==T,]),y=usgs_u238, fun='count') 
u_oil_raster<-focal(u_oil_raster,moving_window,fun='sum',na.rm=T)
u_gas_raster<-rasterize(coordinates(uwells[uwells$Gas==T,]),y=usgs_u238, fun='count')
u_gas_raster<-focal(u_gas_raster,moving_window,fun='sum',na.rm=T)

load(here("data","OG_RadNet.RData"))

radnet_vis<-data.frame(coordinates(radnet))
radnet_vis$type=og_Region$within
radnet_vis[radnet_vis$type,"type"]="O&G RadNet Monitors"
radnet_vis[radnet_vis$type=="FALSE","type"]="Outside RadNet Monitors"
names(radnet_vis)[1:2]<-c("x","y")


rgn_parcel=list(c("PA","WV","OH","NY"),
                c("TX","LA","AR","MS","AL","OK"),
                c("UT","CO","WY","MT","ND","SD","NM","AZ","ID","NE")
)

rgn_1<-states[states$NAME%in%c("West Virginia","Pennsylvania","Ohio","New York","Maryland","Delaware","New Jersey"),]
rgn_2<-states[states$NAME%in%c("Texas","Louisiana","Oklahoma","Arkansas","Mississippi","Alabama"),]
rgn_3<-states[states$NAME%in%c("Utah","Colorado","Wyoming","Montana","North Dakota","South Dakota","Idaho","Nebraska","New Mexico","Arizona"),]
radnet$type=og_Region$within

radnet_buf<-gBuffer(radnet[rgn_1,],byid=T,width=50000)
radnet_vis<-data.frame(coordinates(radnet[rgn_1,]),radnet[rgn_1,"type"]@data)
radnet_vis[radnet_vis$type,"type"]="O&G RadNet Monitors"
radnet_vis[radnet_vis$type=="FALSE","type"]="Outside RadNet Monitors"
names(radnet_vis)[1:2]<-c("x","y")
point_vis=radnet_vis

well_vis=data.frame(coordinates(uwells[rgn_1,]),"UO&G Well")
names(well_vis)=c("x","y","type")
#radnet_vis<-data.frame(coordinates(radnet[rgn_1,]))
#radnet_vis$type="RadNet"
#pm_monitors_vis<-data.frame(coordinates(pm_monitors[radnet_buf,]))
#pm_monitors_vis$type="PM_Monitors"
#names(pm_monitors_vis)[1:2]<-c("x","y")
#point_vis<-rbind.data.frame(pm_monitors_vis,radnet_vis)
rgn_1_full<-ggplot()+
  geom_point(data=well_vis,aes(x=x,y=y,color=type,shape=type,size=type),stroke=0.05,fill="darkgreen")+
  geom_polygon(data=rgn_1,aes(x=long,y=lat,group=group),color="black",linetype="solid",fill=NA,show.legend = F)+
  geom_polygon(data=radnet_buf,aes(x=long,y=lat,group=group),color="red",fill=NA,size=0.5)+
  geom_point(data=radnet_vis,aes(x=x,y=y,col=type,shape=type,size=type),fill="Red",stroke = 1.5)+
  scale_color_manual("RadNet",breaks=c("O&G RadNet Monitors","Outside RadNet Monitors","UO&G Well"),values=c("Black","Red","Black"))+
  scale_shape_manual("RadNet",breaks=c("O&G RadNet Monitors","Outside RadNet Monitors","UO&G Well"),values=c(24,17,21))+
  scale_size_manual("RadNet",breaks=c("O&G RadNet Monitors","Outside RadNet Monitors","UO&G Well"),values=c(3,3,0.75))+
  scale_fill_manual("Number of UO wells within 20 km",
                    breaks=c(1,2,3,4,5),
                    values=c("#edf8e9","#bae4b3","#74c476","#238b45","dark green"),
                    labels=c(">0",">50",">100",">1000",">6000"),guide = "colorbar")+
  guides(fill = guide_legend(nrow=1,override.aes = list(alpha = 1), 
                             title.position = "top",
                             keywidth = unit(2,"lines"),
                             keyheight = unit(1,"lines"),
                             label.position="bottom",
                             label.hjust = -0.1))+
  ggtitle("Marcellus-Utica Region") +
  coord_fixed()+
  theme(plot.title = element_text(hjust = 0.5,vjust = 0.9,face="bold",margin = margin(t = 10, b = -20)),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.key.height = unit(1,"lines"),
        legend.key.width=unit(1,"lines"),
        legend.text = element_text(size=12,face='italic'),
        legend.position = c(0.8, 0.1),
        legend.direction="vertical",
        panel.border = element_rect(colour = "black", size=1,fill=NA))
rgn=extent(rgn_1)
rgn_1_zoom=ggplot()+
  geom_polygon(data=states,aes(x=long,y=lat,group=group),color="black",linetype="solid",size=0.25,fill=NA,show.legend = F)+
  geom_rect(aes(xmin=rgn@xmin,xmax=rgn@xmax,ymin=rgn@ymin,ymax=rgn@ymax),fill=NA,color="red",size=1)+
  coord_fixed()+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.background = element_blank(),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.title = element_blank(),
    legend.key.height = unit(1,"lines"),
    legend.key.width=unit(1.5,"lines"),
    legend.text = element_text(size=6,face='italic'),
    legend.position = c(0.125, 0.1),
    legend.direction="horizontal",
    panel.border = element_rect(colour = "black", size=0.5,fill=NA))
extent_1<-rgn_1_full+annotation_custom(ggplotGrob(rgn_1_zoom),xmin=rgn@xmin,xmax=rgn@xmin+(rgn@xmax-rgn@xmin)/4,
                                       ymin=rgn@ymax-(rgn@ymax-rgn@ymin)/4,ymax=rgn@ymax)


radnet_buf<-gBuffer(radnet[rgn_2,],byid=T,width=50000)
radnet_vis<-data.frame(coordinates(radnet[rgn_2,]),radnet[rgn_2,"type"]@data)
radnet_vis[radnet_vis$type,"type"]="O&G RadNet Monitors"
radnet_vis[radnet_vis$type=="FALSE","type"]="Outside RadNet Monitors"
names(radnet_vis)[1:2]<-c("x","y")
point_vis=radnet_vis
well_vis=data.frame(coordinates(uwells[rgn_2,]),"UO&G Well")
names(well_vis)=c("x","y","type")

#radnet_vis<-data.frame(coordinates(radnet[rgn_1,]))
#radnet_vis$type="RadNet"
#pm_monitors_vis<-data.frame(coordinates(pm_monitors[radnet_buf,]))
#pm_monitors_vis$type="PM_Monitors"
#names(pm_monitors_vis)[1:2]<-c("x","y")
#point_vis<-rbind.data.frame(pm_monitors_vis,radnet_vis)
rgn_2_full<-ggplot()+
  geom_point(data=well_vis,aes(x=x,y=y,color=type,shape=type,size=type),stroke=0.05,fill="darkgreen")+
  geom_polygon(data=rgn_2,aes(x=long,y=lat,group=group),color="black",linetype="solid",fill=NA,show.legend = F)+
  geom_polygon(data=radnet_buf,aes(x=long,y=lat,group=group),color="red",fill=NA,size=0.5)+
  geom_point(data=radnet_vis,aes(x=x,y=y,col=type,shape=type,size=type),fill="Red",stroke = 1.5)+
  scale_color_manual("RadNet",breaks=c("O&G RadNet Monitors","Outside RadNet Monitors","UO&G Well"),values=c("Black","Red","Black"))+
  scale_shape_manual("RadNet",breaks=c("O&G RadNet Monitors","Outside RadNet Monitors","UO&G Well"),values=c(24,17,21))+
  scale_size_manual("RadNet",breaks=c("O&G RadNet Monitors","Outside RadNet Monitors","UO&G Well"),values=c(3,3,0.75))+
  scale_fill_manual("Number of UO wells within 20 km",
                    breaks=c(1,2,3,4,5),
                    values=c("#edf8e9","#bae4b3","#74c476","#238b45","dark green"),
                    labels=c(">0",">50",">100",">1000",">4000"),guide = "colorbar")+
  guides(fill = guide_legend(nrow=1,override.aes = list(alpha = 1), 
                             title.position = "top",
                             keywidth = unit(2,"lines"),
                             keyheight = unit(1,"lines"),
                             label.position="bottom",
                             label.hjust = -0.1))+
  ggtitle("Permian-Haynesville Region") +
  coord_fixed()+
  theme(plot.title = element_text(hjust = 0.5,vjust = 0.9,face="bold",margin = margin(t = 10, b = -20)),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.key.height = unit(1,"lines"),
        legend.key.width=unit(1,"lines"),
        legend.text = element_text(size=12,face='italic'),
        legend.position = c(0.1, 0.1),
        legend.direction="vertical",
        panel.border = element_rect(colour = "black", size=1,fill=NA))
rgn=extent(rgn_2)
rgn_2_zoom=ggplot()+
  geom_polygon(data=states,aes(x=long,y=lat,group=group),color="black",linetype="solid",size=0.25,fill=NA,show.legend = F)+
  geom_rect(aes(xmin=rgn@xmin,xmax=rgn@xmax,ymin=rgn@ymin,ymax=rgn@ymax),fill=NA,color="red",size=1)+
  coord_fixed()+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.background = element_blank(),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.title = element_text(size =8, face = 'bold'),
    legend.key.height = unit(1,"lines"),
    legend.key.width=unit(2,"lines"),
    legend.text = element_text(size=8,face='italic'),
    legend.position = c(0.12, 0.1),
    legend.direction="horizontal",
    panel.border = element_rect(colour = "black", size=0.5,fill=NA))
extent_2<-rgn_2_full+annotation_custom(ggplotGrob(rgn_2_zoom),xmin=rgn@xmin+3*(rgn@xmax-rgn@xmin)/4,xmax=rgn@xmax,
                                       ymin=rgn@ymin,ymax=rgn@ymin+(rgn@ymax-rgn@ymin)/4)


radnet_buf<-gBuffer(radnet[rgn_3,],byid=T,width=50000)
radnet_vis<-data.frame(coordinates(radnet[rgn_3,]),radnet[rgn_3,"type"]@data)
radnet_vis[radnet_vis$type,"type"]="O&G RadNet Monitors"
radnet_vis[radnet_vis$type=="FALSE","type"]="Outside RadNet Monitors"
names(radnet_vis)[1:2]<-c("x","y")
point_vis=radnet_vis
well_vis=data.frame(coordinates(uwells[rgn_3,]),"UO&G Well")
names(well_vis)=c("x","y","type")


#radnet_vis<-data.frame(coordinates(radnet[rgn_1,]))
#radnet_vis$type="RadNet"
#pm_monitors_vis<-data.frame(coordinates(pm_monitors[radnet_buf,]))
#pm_monitors_vis$type="PM_Monitors"
#names(pm_monitors_vis)[1:2]<-c("x","y")
#point_vis<-rbind.data.frame(pm_monitors_vis,radnet_vis)
rgn_3_full<-ggplot()+
  geom_point(data=well_vis,aes(x=x,y=y,color=type,shape=type,size=type),stroke=0.05,fill="darkgreen")+
  geom_polygon(data=rgn_3,aes(x=long,y=lat,group=group),color="black",linetype="solid",fill=NA,show.legend = F)+
  geom_polygon(data=radnet_buf,aes(x=long,y=lat,group=group),color="red",fill=NA,size=0.5)+
  geom_point(data=radnet_vis,aes(x=x,y=y,col=type,shape=type,size=type),fill="Red",stroke = 1.5)+
  scale_color_manual("RadNet",breaks=c("O&G RadNet Monitors","Outside RadNet Monitors","UO&G Well"),values=c("Black","Red","Black"))+
  scale_shape_manual("RadNet",breaks=c("O&G RadNet Monitors","Outside RadNet Monitors","UO&G Well"),values=c(24,17,21))+
  scale_size_manual("RadNet",breaks=c("O&G RadNet Monitors","Outside RadNet Monitors","UO&G Well"),values=c(3,3,0.75))+
  scale_fill_manual("Number of UO wells within 20 km",
                    breaks=c(1,2,3,4,5),
                    values=c("#edf8e9","#bae4b3","#74c476","#238b45","dark green"),
                    labels=c(">0",">50",">100",">1000",">4000"),guide = "colorbar")+
  guides(fill = guide_legend(nrow=1,override.aes = list(alpha = 1), 
                             title.position = "top",
                             keywidth = unit(2,"lines"),
                             keyheight = unit(1,"lines"),
                             label.position="bottom",
                             label.hjust = -0.1))+
  ggtitle("Bakken-Niobrara Region") +
  coord_fixed()+
  theme(plot.title = element_text(hjust = 0.5,vjust = 0.9,face="bold",margin = margin(t = 10, b = -20)),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.key.height = unit(1,"lines"),
        legend.key.width=unit(1,"lines"),
        legend.text = element_text(size=12,face='italic'),
        legend.position = c(0.8, 0.1),
        legend.direction="vertical",
        panel.border = element_rect(colour = "black", size=1,fill=NA))
rgn=extent(rgn_3)
rgn_3_zoom=ggplot()+
  geom_polygon(data=states,aes(x=long,y=lat,group=group),color="black",linetype="solid",size=0.25,fill=NA,show.legend = F)+
  geom_rect(aes(xmin=rgn@xmin,xmax=rgn@xmax,ymin=rgn@ymin,ymax=rgn@ymax),fill=NA,color="red",size=1)+
  coord_fixed()+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.background = element_blank(),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.title = element_text(size =8, face = 'bold'),
    legend.key.height = unit(1,"lines"),
    legend.key.width=unit(2,"lines"),
    legend.text = element_text(size=8,face='italic'),
    legend.position = c(0.8, 0.25),
    legend.direction="horizontal",
    panel.border = element_rect(colour = "black", size=0.5,fill=NA))
extent_3<-rgn_3_full+annotation_custom(ggplotGrob(rgn_3_zoom),xmin=rgn@xmin+3*(rgn@xmax-rgn@xmin)/4,xmax=rgn@xmax,
                                       ymin=rgn@ymin+(rgn@ymax-rgn@ymin)/4,ymax=rgn@ymin+(rgn@ymax-rgn@ymin)/2)

up_row_1=cowplot::plot_grid(NULL,extent_1,NULL,rel_widths = c(1,4,1),nrow=1,labels = c(NA,"A",NA),label_x = 0.04,label_y = 0.95)
bottom_row=cowplot::plot_grid(oil_g_1,gas_g_1,labels = c("B","C"),label_x = 0.12,label_y = 0.95)

g1<-cowplot::plot_grid(up_row,bottom_row,nrow=2,rel_heights = c(1,1))

up_row_2=cowplot::plot_grid(NULL,extent_2,NULL,rel_widths = c(1,5,1),nrow=1,labels = c(NA,"B",NA),label_x = 0.04,label_y = 0.95)
bottom_row=cowplot::plot_grid(oil_g_2,gas_g_2,labels = c("B","C"),label_x = 0.12,label_y = 0.95)

g2<-cowplot::plot_grid(up_row,bottom_row,nrow=2,rel_heights = c(1,1))

up_row_3=cowplot::plot_grid(NULL,extent_3,NULL,rel_widths = c(1,6,1),nrow=1,labels = c(NA,"C",NA),label_x = 0.04,label_y = 0.95)
bottom_row=cowplot::plot_grid(oil_g_3,gas_g_3,labels = c("B","C"),label_x = 0.12,label_y = 0.95)


g3<-cowplot::plot_grid(up_row,bottom_row,nrow=2,rel_heights = c(1,1))
cowplot::plot_grid(extent_1,extent_2,extent_3,nrow=3)

up<-cowplot::plot_grid(extent_3,extent_1,labels=c("C","A"),label_x = c(0.04),label_y = c(0.88,0.83))
down=cowplot::plot_grid(extent_2,labels = c("B"),label_x = 0.05,label_y = c(0.9))
cowplot::plot_grid(up,down,nrow = 2,rel_heights = c(1,1))
cowplot::plot_grid(oil_g_1,gas_g_1,labels = c("A","B"),label_x = 0.12,label_y = 0.95)
cowplot::plot_grid(oil_g_2,gas_g_2,labels = c("A","B"),label_x = 0.12,label_y = 0.95)
cowplot::plot_grid(oil_g_3,gas_g_3,labels = c("A","B"),label_x = 0.12,label_y = 0.95)

