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
library(scales)
library(ggsn)
library(maps)
library(rgeos)
library(sp)
library(cowplot)
library(ggforce)
options(dplyr.print_max = 1e9)
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
RadNet_City_List<-read_csv(here::here("data","Processed-RadNet-Beta-citylist.csv"))
radnet_usgs_radio<-read_csv(here::here("data","radnet_radioraster_summary.csv"))
radnet_usgs_radio$city_state<-paste0(radnet_usgs_radio$city,",",radnet_usgs_radio$state)
radnet<-pgGetGeom(con,"RadNet_Sp","radnet_geom")
uwind<-stack(here::here("data","NARR","uwnd.nc"))
uwind<-uwind[[338:469]]
vwind<-stack(here::here("data","NARR","vwnd.nc"))
vwind<-vwind[[338:469]]
radnet_wind<-prepare_wind_field(radnet,uwind,vwind,key="city_state")
#Fig-1-Baisc Data
basins<-pgGetGeom(con,"us_basin",geom="basin_geom")
states<-pgGetGeom(con,"us_state",geom="state_geom")
wells<-pgGetGeom(con,"Well_Headers",geom="well_geom")
coastline<-pgGetGeom(con,"us_medium_shoreline",geom = "coast_geom")
pm_monitors<-pgGetGeom(con,"PM_Monitors",geom="pm_monitor_geom")
radon_zone<-pgGetGeom(con,"US_Radon_Potential",geom="radon_potential_geom")
county<-pgGetGeom(con,"us_county",geom="county_geom")
usgs_u238<-pgGetRast(con,"USGS_RadioRaster",band=3)
basins<-spTransform(basins,CRS(proj4string(usgs_u238)))
states<-states[as.numeric(states$GEOID)<60,]
states<-states[states$STUSPS!="AK"&states$STUSPS!="HI",]
states<-spTransform(states,CRS(proj4string(usgs_u238)))
county<-county[as.numeric(county$STATEFP)<60,]
county<-county[county$STATEFP!="02",]
county<-county[county$STATEFP!="15",]
county<-spTransform(county,CRS(proj4string(usgs_u238)))
wells<-spTransform(wells,CRS(proj4string(usgs_u238)))
coastline<-spTransform(coastline,CRS(proj4string(usgs_u238)))
radon_zone<-spTransform(radon_zone,CRS(proj4string(usgs_u238)))
pm_monitors<-spTransform(pm_monitors,CRS(proj4string(usgs_u238)))
radnet<-spTransform(radnet,CRS(proj4string(usgs_u238)))
pm_monitors<-pm_monitors[states,]
usgs_u238<-crop(usgs_u238,states)
usgs_u238<-mask(usgs_u238,usgs_u238<0,maskvalue=T)
usgs_u238<-mask(usgs_u238,usgs_u238>12,maskvalue=T)
usgs_u238<-mask(usgs_u238,states)
small_extent<-raster::extent(states[states$STUSPS=="WV",])
small_extent<-as(small_extent,"SpatialPolygons")

ras_data<-cbind.data.frame(coordinates(usgs_u238),values(usgs_u238))
names(ras_data)<-c("x","y","u")

basin_center<- cbind.data.frame(coordinates(basins),basins@data)
names(basin_center)<-c("x","y","gid","name","area_sq_km","area_sq_mi")

wells<-wells[wells$`Producing Status`=="ACTIVE",]
uwells_vis<-data.frame(coordinates(wells[wells$`Drill Type`=="H"|wells$`Drill Type`=="D",]))
uwells_vis$type<-"U_Wells"
vwells_vis<-data.frame(coordinates(wells[wells$`Drill Type`=="V",]))
vwells_vis$type<-"V_Wells"
radnet_vis<-data.frame(coordinates(radnet))
radnet_vis$type="RadNet"
pm_monitors_vis<-data.frame(coordinates(pm_monitors))
pm_monitors_vis$type="PM_Monitors"
point_vis<-rbind.data.frame(vwells_vis,uwells_vis,pm_monitors_vis,radnet_vis)

cols <- c("U_Wells"= "darkgreen", "V_Wells" = "darkblue", "PM_Monitors" = "Orange", "RadNet" = "Red","F1_States"="black","Basins"="black")
sizes<-c("U_Wells"= 0.00001, "V_Wells" = 0.0001, "PM_Monitors" = 1.5, "RadNet" = 2,"F1_States"=0.5,"Basins"=0.5)
shapes<-c("U_Wells"= 16, "V_Wells" = 16, "PM_Monitors" = 18, "RadNet" = 17,"F1_States"=NA,"Bains"=NA)
linetypes<-c("U_Wells"= "blank", "V_Wells" = "blank", "PM_Monitors" = "blank", "RadNet" = "blank","F1_States"="solid","Basins"="solid")
g1<-ggplot()+
  geom_raster(data=ras_data,aes(x=x,y=y,fill=u))+
  scale_fill_distiller("Aeroradiometric U",type="div",palette = "Spectral",na.value="white",limits=c(0.5,6),guide = "colorbar")+
  geom_polygon(data=states,aes(x=long,y=lat,group=group),color="black",size=0.5,linetype="solid",fill=NA,show.legend = F)+
  geom_point(data=point_vis,aes(x=x,y=y,size=type,color=type,shape=type),show.legend = F)+
  geom_polygon(data=small_extent,aes(x=long,y=lat,group=group),color="black",size=0.75,fill=NA,linetype="dashed",show.legend = F)+
  #geom_text(data=basin_center,aes(x=x,y=y,label=name,fontface="bold"),size=3,colour="grey20")+
  scale_color_manual(values = cols)+
  scale_size_manual(values=sizes)+
  scale_shape_manual(values=shapes)+
  scale_linetype_manual(values=linetypes)+
  theme(panel.border = element_rect(colour = "black", size=2),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.title = element_text(size =3, face = 'italic'),
        legend.key.size = unit(0.25, "cm"),
        legend.text = element_text(size=3),
        legend.position = "none")+
  blank() +
  north(x.min = min(coordinates(states)[,1]),x.max = max(coordinates(states)[,1]),
        y.min = min(coordinates(states)[,2]),y.max = max(coordinates(states)[,2]),location="bottomright")+
  ggsn::scalebar(dist = 500,st.size=1.5,st.dist = 0.025,height=0.01,location = "bottomleft",
           x.min = min(coordinates(states)[,1]),x.max = max(coordinates(states)[,1]),
           y.min = min(coordinates(states)[,2]),y.max = max(coordinates(states)[,2]))

#############################################################
proj4string(small_extent)<-CRS(proj4string(usgs_u238))
radon_zone_small<-crop(radon_zone,small_extent)
radon_zone_small_fort<-fortify(radon_zone_small,region="RI")
radon_zone_small_fort$id<-as.numeric(as.character(radon_zone_small_fort$id))
county_small<-crop(county,small_extent)
states_small<-crop(states,small_extent)
wells<-wells[wells$`Producing Status`=="ACTIVE",]
uwells_vis<-data.frame(coordinates(crop(wells[wells$`Drill Type`=="H"|wells$`Drill Type`=="D",],small_extent)))
uwells_vis$type<-"U_Wells"
uwells_vis$order=3
vwells_vis<-data.frame(coordinates(crop(wells[wells$`Drill Type`=="V",],small_extent)))
vwells_vis$type<-"V_Wells"
vwells_vis$order=4
radnet_vis<-data.frame(coordinates(crop(radnet,small_extent)))
radnet_vis$type="RadNet"
radnet_vis$order=1
pm_monitors_vis<-data.frame(coordinates(crop(pm_monitors,small_extent)))
pm_monitors_vis$type="PM_Monitors"
pm_monitors_vis$order=2
small_point_vis<-rbind.data.frame(vwells_vis,uwells_vis,pm_monitors_vis,radnet_vis)
radnet_vis_label<-radnet_vis
radnet_vis_label$label=crop(radnet,small_extent)$city_state
cols <- c("U_Wells"= "darkgreen", "V_Wells" = "darkblue", "PM_Monitors" = "Orange", "RadNet" = "Red","Radon_Zones"="grey","County"="black","States"="black")
sizes<-c("U_Wells"= 0.00001, "V_Wells" = 0.0001, "PM_Monitors" = 1.5, "RadNet" = 2,"Radon_Zones"=1,"County"=0.25,"States"=0.75)
shapes<-c("U_Wells"= 16, "V_Wells" = 16, "PM_Monitors" = 18, "RadNet" = 17,"Radon_Zones"=NA,"County"=NA,"States"=NA)
linetypes<-c("U_Wells"= "blank", "V_Wells" = "blank", "PM_Monitors" = "blank", "RadNet" = "blank","Radon_Zones"="solid","County"="dotted","States"="solid")


g2<-ggplot()+
  geom_polygon(data=radon_zone_small_fort,aes(x=long,y=lat,group=group,fill=id),alpha=0.5)+
  scale_fill_continuous(name="Radon Index            ",low=rgb(255,245,240,max = 255),high=rgb(153,0,13,max=255),guide = "colourbar")+
  guides(fill = guide_colourbar(barwidth = 10, barheight = 0.35))+
  geom_path(data=radon_zone_small,aes(x=long,y=lat,group=group,color="Radon_Zones"),linetype="solid",size=2,show.legend = F)+
  geom_path(data=county_small,aes(x=long,y=lat,group=group,color="County"),linetype="dotted",size=0.75,show.legend = F)+
  geom_path(data=states_small,aes(x=long,y=lat,group=group,color="States"),linetype="dashed",size=1,show.legend = F)+
  geom_point(data=small_point_vis,aes(x=x,y=y,col=type,shape=type,size=type),show.legend = F)+
  scale_color_manual("Legend",values=cols)+
  scale_shape_manual("Legend",values=shapes)+
  scale_size_manual("Legend",values = sizes)+
  scale_linetype_manual(name="Legend",values=linetypes)+
  guides(colour = guide_legend(override.aes = list(linetype=c("dotted","blank","blank","solid","solid","blank","blank"),
                                                     shape=c(NA,18,17,NA,NA,16,16),
                                                   color=c("black","orange","gold","grey","black","darkgreen","darkblue"),
                                                   size=c(1.5,3,4,2,1.5,2,2)
                                                   )),
         size="none",shape="none",linetype="none")+
  theme(panel.border = element_rect(colour = "black", size=2),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.title = element_text(size =3, face = 'italic'),
        legend.text = element_text(size=3),
        legend.key = element_rect(colour = 'grey', fill = NA, size = 0.5),
        legend.key.width = unit(1.5, "cm"),
        legend.direction = "horizontal"
        )
radon_legend<-get_legend(g2)
g2<-ggplot()+
  geom_polygon(data=radon_zone_small_fort,aes(x=long,y=lat,group=group,fill=id),alpha=0.5,show.legend = F)+
  scale_fill_continuous(name="Radon Index       ",low=rgb(255,245,240,max = 255),high=rgb(153,0,13,max=255),guide = "colourbar")+
  geom_path(data=radon_zone_small,aes(x=long,y=lat,group=group,color="Radon_Zones"),linetype="solid",size=2)+
  geom_path(data=county_small,aes(x=long,y=lat,group=group,color="County"),linetype="dotted",size=0.75)+
  geom_path(data=states_small,aes(x=long,y=lat,group=group,color="States"),linetype="dashed",size=1)+
  geom_point(data=small_point_vis,aes(x=x,y=y,col=type,shape=type,size=type))+
  scale_color_manual("Legend",values=cols)+
  scale_shape_manual("Legend",values=shapes)+
  scale_size_manual("Legend",values = sizes)+
  scale_linetype_manual(name="Legend",values=linetypes)+
  guides(colour = guide_legend(override.aes = list(linetype=c("dotted","blank","blank","solid","solid","blank","blank"),
                                                   shape=c(NA,18,17,NA,NA,16,16),
                                                   color=c("black","orange","red","grey","black","darkgreen","darkblue"),
                                                   size=c(0.5,1.5,2,1,0.75,1,1)
  )),
  size="none",shape="none",linetype="none")+
  theme(panel.border = element_rect(colour = "black", size=2),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.title = element_text(size =6, face = 'italic'),
        legend.text = element_text(size=4),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.25,"cm"),
        legend.direction = "vertical")
vec_legend=get_legend(g2)
g2<-ggplot()+
  geom_polygon(data=radon_zone_small_fort,aes(x=long,y=lat,group=group,fill=id),alpha=0.5,show.legend = F)+
  scale_fill_continuous(name="Radon Index",low=rgb(255,245,240,max = 255),high=rgb(153,0,13,max=255),guide = "colourbar")+
  geom_path(data=radon_zone_small,aes(x=long,y=lat,group=group,color="Radon_Zones"),linetype="solid",size=1)+
  geom_path(data=county_small,aes(x=long,y=lat,group=group,color="County",linetype="County",size="County"))+
  geom_path(data=states_small,aes(x=long,y=lat,group=group,color="States",size="States",linetype="States"))+
  geom_point(data=small_point_vis,aes(x=x,y=y,col=type,shape=type,size=type))+
  scale_color_manual("Legend",values=cols)+
  scale_shape_manual("Legend",values=shapes)+
  scale_size_manual("Legend",values = sizes)+
  scale_linetype_manual(name="Legend",values=linetypes)+
  guides(colour = guide_legend(override.aes = list(linetype=c("dotted","blank","blank","solid","solid","blank","blank"),
                                                   shape=c(NA,18,17,NA,NA,16,16),
                                                   color=c("black","orange","gold","grey","black","darkgreen","darkblue"),
                                                   size=c(1.5,3,4,2,1.5,2,2)
  )),
  size="none",shape="none",linetype="none")+
  theme(panel.border = element_rect(colour = "black", size=2),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.title = element_text(size =10, face = 'italic'),
        legend.text = element_text(size=10),
        legend.key = element_rect(colour = 'grey', fill = NA, size = 0.5),
        legend.key.width = unit(2, "cm"),
        legend.direction = "horizontal",
        legend.position = "none"
  )
#############################################################################
usgs_small<-crop(usgs_u238,small_extent)
ras_data_small<-cbind.data.frame(coordinates(usgs_small),values(usgs_small))
names(ras_data_small)<-c("x","y","u")
point_vis<-rbind(pm_monitors_vis,radnet_vis)


g3<- ggplot()+
  geom_raster(data=ras_data_small,aes(x=x,y=y,fill=u))+
  scale_fill_distiller("Aeroradiometric U",type="div",palette = "Spectral",na.value="white",limits=c(0.5,6),guide = "colorbar")+
  guides(fill = guide_colourbar(barwidth = 10, barheight = 0.35))+
  geom_path(data=county_small,aes(x=long,y=lat,group=group,color="County",linetype="County",size="County"),show.legend = F)+
  geom_path(data=states_small,aes(x=long,y=lat,group=group,color="States",size="States",linetype="States"),show.legend = F)+
  geom_point(data=point_vis,aes(x=x,y=y,col=type,shape=type,size=type),show.legend = F)+
  geom_text(data=radnet_vis_label,aes(x=x,y=y,label=label,fontface="bold"),size=4,nudge_y = -8000,show.legend = F)+
  scale_color_manual("Legend",values=cols)+
  scale_shape_manual("Legend",values=shapes)+
  scale_size_manual("Legend",values = sizes)+
  scale_linetype_manual(name="Legend",values=linetypes)+
  theme(panel.border = element_rect(colour = "black", size=2),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.title = element_text(size =3, face = 'italic'),
        legend.text = element_text(size=3),
        legend.key = element_rect(colour = 'grey', fill = NA, size = 0.5),
        legend.key.width = unit(2, "cm"),
        legend.direction = "horizontal",
        legend.position = "bottom"
  )
usgs_legend<-get_legend(g3)
g3<- ggplot()+
  geom_raster(data=ras_data_small,aes(x=x,y=y,fill=u))+
  scale_fill_distiller("Aeroradiometric U",type="div",palette = "Spectral",na.value="white",limits=c(0.5,6),guide = "colorbar")+
  geom_path(data=county_small,aes(x=long,y=lat,group=group,color="County",linetype="County",size="County"),show.legend = F)+
  geom_path(data=states_small,aes(x=long,y=lat,group=group,color="States",size="States",linetype="States"),show.legend = F)+
  geom_point(data=point_vis,aes(x=x,y=y,col=type,shape=type,size=type),show.legend = F)+
  geom_text(data=radnet_vis_label,aes(x=x,y=y,label=label,fontface="bold"),size=2,color="white",nudge_y = -11000,show.legend = F)+
  scale_color_manual("Legend",values=cols)+
  scale_shape_manual("Legend",values=shapes)+
  scale_size_manual("Legend",values = sizes)+
  scale_linetype_manual(name="Legend",values=linetypes)+
  theme(panel.border = element_rect(colour = "black", size=2),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.title = element_text(size =10, face = 'italic'),
        legend.text = element_text(size=3),
        legend.key = element_rect(colour = 'grey', fill = NA, size = 0.25),
        legend.key.width = unit(1.5, "cm"),
        legend.direction = "horizontal",
        legend.position = "none"
  )
###########################
g2<-ggdraw(g2)+geom_rect(aes(xmin = 0.125, xmax =0.155, ymin = 0.85, ymax = 0.89),
                 colour = "white", fill = "white")
g3<-ggdraw(g3)+geom_rect(aes(xmin = 0.125, xmax =0.155, ymin = 0.85, ymax = 0.89),
                         colour = "white", fill = "white")
color_legend<-plot_grid(usgs_legend,radon_legend,rel_heights = c(1,1),nrow=2)
legends<-plot_grid(vec_legend,color_legend,rel_widths = c(2,5),nrow=1)
right_col<-plot_grid(g2, g3, labels = c("B", "C"),label_size = 4,label_x = 0.116,label_y = 0.89, rel_heights = c(1, 1), nrow = 2)
left_col<-plot_grid(g1,legends,labels= c("A",NULL),label_size = 4,label_x = 0.05,label_y = 0.9,rel_heights = c(2.5,0.6),ncol=1)
g<-plot_grid(left_col,right_col,nrow=1,rel_widths = c(2.5,1))
ggsave("fig1.pdf",g,width=7,height=4,unit="in")
##################################
r<-100000
total_beta_cmd<-"
SELECT avg(result_amount) AS beta,12*date_part('year',age(date_trunc('month',\"collect_end\"),'2001-01-01'))+date_part('month',age(date_trunc('month',\"collect_end\"),'2001-01-01'))AS \"m_month\",EXTRACT(YEAR FROM \"collect_end\" ) AS \"YEAR\",EXTRACT(MONTH FROM \"collect_end\" ) AS \"MONTH\",\"city_state\"
FROM \"Radnet_Measurement_Table\"
GROUP BY \"m_month\",\"city_state\",\"YEAR\",\"MONTH\"
"
rad_beta<-dbGetQuery(con,total_beta_cmd)
rad_beta<-rad_beta[rad_beta$m_month>0,]
beta_wind<-left_join(rad_beta,radnet_wind,by=c("YEAR"="Year","MONTH"="Month","city_state"="city_state"))
beta_wind<-beta_wind%>%filter(!is.na(uwind))
beta_wind<-beta_wind[beta_wind$city_state=="DALLAS,TX",]

city_well_cmd<-"
SELECT \"API/UWI\",\"Spud Date\",\"Completion Date\",\"city_state\",\"Last Prod Date\",\"Drill Type\",\"DI Basin\",ST_DistanceSphere(\"well_geom\",\"radnet_geom\") AS \"Dist\",180*ST_Azimuth(\"radnet_geom\",\"well_geom\")/pi() AS \"Dir\"
FROM \"Well_Headers\",\"RadNet_Sp\" 
WHERE ST_DistanceSphere(\"well_geom\",\"radnet_geom\")<RADIUS AND \"city_state\"='DALLAS,TX'
"
cmd<-gsub(pattern="RADIUS",replacement=as.character(r),city_well_cmd)
city_well_relation<-dbGetQuery(con,cmd)
prod_db <- tbl(con, "Well_Production_Table")
prod_db<-prod_db%>%filter(API%in%city_well_relation$`API/UWI`&Prod_Year>2001)
print(Sys.time())
city_prod<-city_well_relation%>%
  left_join(prod_db,by=c("API/UWI"="API"),copy=T)
print(Sys.time())
city_prod<-city_prod%>%filter(!is.na(Prod_Year))
city_prod<-city_prod%>%filter(Prod_Year<2018)
city_prod[city_prod$`Drill Type`=="D",]$`Drill Type`="H"
#city_prod<-city_prod%>%filter(`Production Status`=="ACTIVE")
city_prod<-city_prod%>%filter(!is.na(`DI Basin`))
city_prod$Dist<-city_prod$Dist/1000
city_prod_wind<-left_join(city_prod,radnet_wind,by=c("Prod_Year"="Year","Prod_Month"="Month","city_state"="city_state"))
city_prod_wind<-city_prod_wind%>%filter(!is.na(uwind))
save(file = here::here("data","dallas_data.RData"))
data_a<-city_prod_wind[city_prod_wind$Prod_Year==2016&city_prod_wind$Prod_Month==12,]
data_b<-city_prod_wind[city_prod_wind$Prod_Year==2007&city_prod_wind$Prod_Month==8,]


wells<-pgGetGeom(con,"Well_Headers",geom="well_geom")
radnet<-pgGetGeom(con,"RadNet_Sp","radnet_geom")

radnet<-radnet[radnet$city_state=="DALLAS,TX",]

radnet<-spTransform(radnet,CRS(proj4string(usgs_u238)))
wells<-spTransform(wells,CRS(proj4string(usgs_u238)))
buffer<-gBuffer(radnet,width = 70000,quadsegs=200)
rad_vis<-cbind.data.frame(coordinates(radnet),radnet$city_state)
names(rad_vis)<-c("x","y","city")

######################################################################################
#For Data a
well_s<-crop(wells,gBuffer(radnet,width=r))
well_s<-well_s[well_s$`API/UWI`%in%data_a$`API/UWI`,]
well_s@data<-inner_join(wells@data,data_a,by="API/UWI")

#handle the wind data
temp_uwind<-uwind$X2016.12.31.23.56.02
temp_vwind<-vwind$X2016.12.31.23.56.02
temp_wind<-cbind.data.frame(coordinates(temp_uwind),values(temp_uwind),values(temp_vwind))
names(temp_wind)<-c("x","y","u","v")
coordinates(temp_wind)<-~x+y
proj4string(temp_wind)<-proj4string(uwind)
dallas_extent<-spTransform(gBuffer(radnet,width = 75000,quadsegs=200),proj4string(uwind))
temp_wind<-crop(temp_wind,dallas_extent)
temp_wind<-spTransform(temp_wind,proj4string(usgs_u238))
wind_vis<-cbind.data.frame(temp_wind$x,temp_wind$y,temp_wind$u,temp_wind$v)
names(wind_vis)<-c("x","y","u","v")

oil_vis<-cbind.data.frame(coordinates(well_s[well_s$Monthly_Oil>0,]),well_s[well_s$Monthly_Oil>0,]$Monthly_Oil,well_s[well_s$Monthly_Oil>0,]$`Drill Type.x`)
names(oil_vis)<-c("x","y","Prod","Type")
oil_vis$type="Oil_Well"

gas_vis<-cbind.data.frame(coordinates(well_s[well_s$Monthly_Gas>0,]),well_s[well_s$Monthly_Gas>0,]$Monthly_Gas,well_s[well_s$Monthly_Gas>0,]$`Drill Type.x`)
names(gas_vis)<-c("x","y","Prod","Type")
gas_vis$type="Gas_Well"
gas_vis$Prod<-gas_vis$Prod/1000

well_s_vis<-rbind.data.frame(gas_vis,oil_vis)
well_s_vis[well_s_vis$Type=="D",]$Type="H"
dallas <- c(left = -97.5, bottom = 31.8, right = -95.5, top = 33.8)

pie <- data.frame(
  state = c('Out','In','In','Out'),
  start = c(0, pi*mean(data_a$dir-45)/180,pi*mean(data_a$dir)/180,pi*mean(data_a$dir+45)/180),
  end = c(pi*mean(data_a$dir-45)/180,pi*mean(data_a$dir)/180,pi*mean(data_a$dir+45)/180,2*pi),
  color=c("Out","In","In","Out"),
  alpha=c('Out','In','In','Out'),
  fill=c("Out","In","In","Out"),
  stringsAsFactors = FALSE
)
dallas_city_extent<-shapefile(here::here("Data","Basic_Geodata","CityLimit.shp"))
dallas_city_extent<-spTransform(dallas_city_extent,proj4string(usgs_u238))

well_s_vis$size=ifelse(well_s_vis$Prod>50,"Big","Medium")
well_s_vis[well_s_vis$Prod<15,]$size="Small"
well_s_vis$size=paste0(well_s_vis$type,"_",well_s_vis$size)
wind_vis$vel=sqrt(wind_vis$u^2+wind_vis$v^2)
wind_vis$size=ifelse(wind_vis$vel>1.5,"Wind_Big","Wind_Medium")
wind_vis[wind_vis$vel<1.2,]$size<-"Wind_Small"
dallas_fig<-ggplot(dallas.df) + 
  geom_point(data=well_s_vis,aes(x=x,y=y,size=size,color=size))+
  geom_segment(data=wind_vis,aes(x=x,y=y,xend=x+10000*u,yend=y+10000*v),size = 2,arrow = arrow(length = unit(0.1,"cm")))+
  scale_color_manual("O&G Production",labels=c("Gas (>50k Mcf)","Gas (15~50k Mcf)","Gas (<15k Mcf)", "Oil (>50 bbl)", "Oil (15~50 bbl)","Oil (<15 bbl)"),
                     values=c("green","green","green","blue","blue","blue"))+
  scale_size_manual("O&G Production",labels=c("Gas (>50k Mcf)","Gas (15~50k Mcf)","Gas (<15k Mcf)", "Oil (>50 bbl)", "Oil (15~50 bbl)","Oil (<15 bbl)"),
                    values=c(5,2,1,5,2,1))
oglengd<-get_legend(dallas_fig)

dallas_fig<-ggplot()+
  geom_arc_bar(aes(x0 = rad_vis$x, y0 = rad_vis$y, r0 = 0, r = 70000, start = start, end = end, 
                                      fill = fill,alpha=alpha,color=color,size=color),
                                  data = pie)+
  geom_segment(data=wind_vis,aes(x=x,y=y,xend=x+10000*u,yend=y+10000*v),size = 2,arrow = arrow(length = unit(0.1,"cm")),show.legend = F)+
  geom_polygon(data=dallas_city_extent,aes(x=long,y=lat,group=group),fill="Grey",col="Grey",alpha=0.5)+
  scale_color_manual("Upwind zone",labels=c("Buffer Zone","Upwind Zone"),values = c("Out"="red","In"="red"))+
  scale_size_manual("Upwind zone",labels=c("Buffer Zone","Upwind Zone"),values=c("Out"=0.5,"In"=2))+
  scale_fill_manual("Upwind zone",labels=c("Buffer Zone","Upwind Zone"),values = c("Out"=NA,"In"="red"))+
  scale_alpha_manual("Upwind zone",labels=c("Buffer Zone","Upwind Zone"),values=c("Out"=NA,"In"=0.1))+
  theme(legend.title=element_blank())
dallas_fig
zonelegned=get_legend(dallas_fig)

dallas_fig_a<-ggplot(dallas.df) + 
  #geom_point(aes(x=x, y=y, col=rgb(layer.1/256, layer.2/256, layer.3/256))) + 
  #scale_color_identity()+
  geom_point(data=well_s_vis[well_s_vis$Type=="V",],aes(x=x,y=y,size=Prod,color=type),fill="black")+
  scale_size_area(max_size = 5)+
  geom_arc_bar(aes(x0 = rad_vis$x, y0 = rad_vis$y, r0 = 0, r = 90000, start = start, end = end, 
                   fill = fill,alpha=alpha,color=color),size=1.5,
               data = pie,show.legend = F)+
  scale_color_manual(values = c("Out"=NA,"In"="red","Gas_Well"="green","Oil_Well"="blue"))+
  scale_fill_manual(values = c("Out"="white","In"="red"))+
  scale_alpha_manual(values=c("Out"=0.5,"In"=0.1))+
  geom_segment(data=wind_vis,aes(x=x,y=y,xend=x+5000*u,yend=y+5000*v),size = 0.5,arrow = arrow(length = unit(0.1,"cm")),show.legend = F)+
  geom_polygon(data=buffer,aes(x=long,y=lat,group=group),fill=NA,col="Red",show.legend = F)+
  geom_polygon(data=gBuffer(radnet,width=50000,quadsegs = 200),aes(x=long,y=lat,group=group),fill=NA,col="Red",show.legend=F)+
  geom_polygon(data=gBuffer(radnet,width=25000,quadsegs = 200),aes(x=long,y=lat,group=group),fill=NA,col="Red",show.legend = F)+
  geom_polygon(data=dallas_city_extent,aes(x=long,y=lat,group=group),fill="Grey",col="Grey",alpha=0.5,show.legend = F)+
  geom_polygon(data=gBuffer(radnet,width=90000,quadsegs = 200),aes(x=long,y=lat,group=group),fill=NA,col="Red",show.legend = F)+
  coord_fixed()+
  geom_text(data=rad_vis,aes(x=x,y=y,label=city),size=3)+
  geom_text(data=rad_vis,aes(x=x+25000*sin((data_a$dir[1]-45)*pi/180),y=y+25000*cos((data_a$dir[1]-45)*pi/180)),label="25 km",angle=225-data_a$dir[1],color="Black",size=2)+
  geom_text(data=rad_vis,aes(x=x+50000*sin((data_a$dir[1]-45)*pi/180),y=y+50000*cos((data_a$dir[1]-45)*pi/180)),label="50 km",angle=225-data_a$dir[1],color="Black",size=2)+
  geom_text(data=rad_vis,aes(x=x+70000*sin((data_a$dir[1]-45)*pi/180),y=y+70000*cos((data_a$dir[1]-45)*pi/180)),label="75 km",angle=225-data_a$dir[1],color="Black",size=2)+
  geom_text(data=rad_vis,aes(x=x+90000*sin((data_a$dir[1]-45)*pi/180),y=y+90000*cos((data_a$dir[1]-45)*pi/180)),label="100 km",angle=225-data_a$dir[1],color="Black",size=2)+
  theme(legend.position="none",
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())
dallas_fig_a

dallas_fig_b<-ggplot(dallas.df) + 
  #geom_point(aes(x=x, y=y, col=rgb(layer.1/256, layer.2/256, layer.3/256))) + 
  #scale_color_identity()+
  geom_point(data=well_s_vis[well_s_vis$Type=="H",],aes(x=x,y=y,size=Prod,color=type),fill="black")+
  scale_size_area(max_size = 5)+
  geom_arc_bar(aes(x0 = rad_vis$x, y0 = rad_vis$y, r0 = 0, r = 90000, start = start, end = end, 
                   fill = fill,alpha=alpha,color=color),size=1.5,
               data = pie,show.legend = F)+
  scale_color_manual(values = c("Out"=NA,"In"="red","Gas_Well"="green","Oil_Well"="blue"))+
  scale_fill_manual(values = c("Out"="white","In"="red"))+
  scale_alpha_manual(values=c("Out"=0.5,"In"=0.1))+
  geom_segment(data=wind_vis,aes(x=x,y=y,xend=x+5000*u,yend=y+5000*v),size = 0.5,arrow = arrow(length = unit(0.1,"cm")),show.legend = F)+
  geom_polygon(data=buffer,aes(x=long,y=lat,group=group),fill=NA,col="Red",show.legend = F)+
  geom_polygon(data=gBuffer(radnet,width=50000,quadsegs = 200),aes(x=long,y=lat,group=group),fill=NA,col="Red",show.legend=F)+
  geom_polygon(data=gBuffer(radnet,width=25000,quadsegs = 200),aes(x=long,y=lat,group=group),fill=NA,col="Red",show.legend = F)+
  geom_polygon(data=gBuffer(radnet,width=90000,quadsegs = 200),aes(x=long,y=lat,group=group),fill=NA,col="Red",show.legend = F)+
  geom_polygon(data=dallas_city_extent,aes(x=long,y=lat,group=group),fill="Grey",col="Grey",alpha=0.5,show.legend = F)+
  coord_fixed()+
  geom_text(data=rad_vis,aes(x=x,y=y,label=city),size=3)+
  geom_text(data=rad_vis,aes(x=x+25000*sin((data_a$dir[1]-45)*pi/180),y=y+25000*cos((data_a$dir[1]-45)*pi/180)),label="25 km",angle=225-data_a$dir[1],color="Black",size=2)+
  geom_text(data=rad_vis,aes(x=x+50000*sin((data_a$dir[1]-45)*pi/180),y=y+50000*cos((data_a$dir[1]-45)*pi/180)),label="50 km",angle=225-data_a$dir[1],color="Black",size=2)+
  geom_text(data=rad_vis,aes(x=x+70000*sin((data_a$dir[1]-45)*pi/180),y=y+70000*cos((data_a$dir[1]-45)*pi/180)),label="75 km",angle=225-data_a$dir[1],color="Black",size=2)+
  geom_text(data=rad_vis,aes(x=x+90000*sin((data_a$dir[1]-45)*pi/180),y=y+90000*cos((data_a$dir[1]-45)*pi/180)),label="100 km",angle=225-data_a$dir[1],color="Black",size=2)+
  theme(legend.position="none",
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())
dallas_fig_b
#################################################################################################
#For Data b, 2007 8
well_s<-crop(wells,gBuffer(radnet,width=r))
well_s<-well_s[well_s$`API/UWI`%in%data_b$`API/UWI`,]
well_s@data<-inner_join(wells@data,data_b,by="API/UWI")

#handle the wind data
temp_uwind<-uwind$X2007.09.01.00.56.02
temp_vwind<-vwind$X2007.09.01.00.56.02
temp_wind<-cbind.data.frame(coordinates(temp_uwind),values(temp_uwind),values(temp_vwind))
names(temp_wind)<-c("x","y","u","v")
coordinates(temp_wind)<-~x+y
proj4string(temp_wind)<-proj4string(uwind)
dallas_extent<-spTransform(gBuffer(radnet,width = 75000,quadsegs=200),proj4string(uwind))
temp_wind<-crop(temp_wind,dallas_extent)
temp_wind<-spTransform(temp_wind,proj4string(usgs_u238))
wind_vis<-cbind.data.frame(temp_wind$x,temp_wind$y,temp_wind$u,temp_wind$v)
names(wind_vis)<-c("x","y","u","v")

oil_vis<-cbind.data.frame(coordinates(well_s[well_s$Monthly_Oil>0,]),well_s[well_s$Monthly_Oil>0,]$Monthly_Oil,well_s[well_s$Monthly_Oil>0,]$`Drill Type.x`)
names(oil_vis)<-c("x","y","Prod","Type")
oil_vis$type="Oil_Well"

gas_vis<-cbind.data.frame(coordinates(well_s[well_s$Monthly_Gas>0,]),well_s[well_s$Monthly_Gas>0,]$Monthly_Gas,well_s[well_s$Monthly_Gas>0,]$`Drill Type.x`)
names(gas_vis)<-c("x","y","Prod","Type")
gas_vis$type="Gas_Well"
gas_vis$Prod<-gas_vis$Prod/1000

well_s_vis<-rbind.data.frame(gas_vis,oil_vis)
well_s_vis[well_s_vis$Type=="D",]$Type="H"
dallas <- c(left = -97.5, bottom = 31.8, right = -95.5, top = 33.8)

pie <- data.frame(
  state = c('Out','In','In','Out'),
  start = c(0, pi*mean(data_b$dir-45)/180,pi*mean(data_b$dir)/180,pi*mean(data_b$dir+45)/180),
  end = c(pi*mean(data_b$dir-45)/180,pi*mean(data_b$dir)/180,pi*mean(data_b$dir+45)/180,2*pi),
  color=c("Out","In","In","Out"),
  alpha=c('Out','In','In','Out'),
  fill=c("Out","In","In","Out"),
  stringsAsFactors = FALSE
)
dallas_city_extent<-shapefile(here::here("Data","Basic_Geodata","CityLimit.shp"))
dallas_city_extent<-spTransform(dallas_city_extent,proj4string(usgs_u238))

well_s_vis$size=ifelse(well_s_vis$Prod>50,"Big","Medium")
well_s_vis[well_s_vis$Prod<15,]$size="Small"
well_s_vis$size=paste0(well_s_vis$type,"_",well_s_vis$size)

dallas_fig<-ggplot(dallas.df) + 
  geom_point(data=well_s_vis,aes(x=x,y=y,size=size,color=size))+
  #geom_arc_bar(aes(x0 = rad_vis$x, y0 = rad_vis$y, r0 = 0, r = 70000, start = start, end = end, 
  #                 fill = color,alpha=color,color=color,size=color),
  #             data = pie)+
  scale_color_manual(guide="legend",
                     labels=c("Buffer Zone","Gas (>50k Mcf)","Gas (15~50k Mcf)","Gas (<15k Mcf)", "Oil (>50 bbl)", "Oil (15~50 bbl)","Oil (<15 bbl)","Upwind Zone"),
                     breaks=c("Out","Gas_Well_Big","Gas_Well_Medium","Gas_Well_Small","Oil_Well_Big","Oil_Well_Medium","Oil_Well_Small","In"),
                     values=c("Out"="red","Gas_Well_Big"="green","Gas_Well_Medium"="green","Gas_Well_Small"="green",
                              "Oil_Well_Big"="blue","Oil_Well_Medium"="blue","Oil_Well_Small"="blue","In"="red"))+
  scale_size_manual(guide="legend",
                    labels=c("Buffer Zone","Gas (>50k Mcf)","Gas (15~50k Mcf)","Gas (<15k Mcf)", "Oil (>50 bbl)", "Oil (15~50 bbl)","Oil (<15 bbl)","Upwind Zone"),
                    breaks=c("Out","Gas_Well_Big","Gas_Well_Medium","Gas_Well_Small","Oil_Well_Big","Oil_Well_Medium","Oil_Well_Small","In"),
                    values=c("Out"=1,"Gas_Well_Big"=5,"Gas_Well_Medium"=2,"Gas_Well_Small"=1,
                             "Oil_Well_Big"=5,"Oil_Well_Medium"=2,"Oil_Well_Small"=1,"In"=1.5))+
  scale_fill_manual(guide = "legend",
                    labels=c("Buffer Zone","Gas (>50k Mcf)","Gas (15~50k Mcf)","Gas (<15k Mcf)", "Oil (>50 bbl)", "Oil (15~50 bbl)","Oil (<15 bbl)","Upwind Zone"),
                    breaks=c("Out","Gas_Well_Big","Gas_Well_Medium","Gas_Well_Small","Oil_Well_Big","Oil_Well_Medium","Oil_Well_Small","In"),
                    values = c("Out"="white","Gas_Well_Big"=NA,"Gas_Well_Medium"=NA,"Gas_Well_Small"=NA,
                               "Oil_Well_Big"=NA,"Oil_Well_Medium"=NA,"Oil_Well_Small"=NA,"In"="red"))+
  scale_alpha_manual(guide = "legend",
                     labels=c("Buffer Zone","Gas (>50k Mcf)","Gas (15~50k Mcf)","Gas (<15k Mcf)", "Oil (>50 bbl)", "Oil (15~50 bbl)","Oil (<15 bbl)","Upwind Zone"),
                     breaks=c("Out","Gas_Well_Big","Gas_Well_Medium","Gas_Well_Small","Oil_Well_Big","Oil_Well_Medium","Oil_Well_Small","In"),
                     values =c("Out"=0.5,"Gas_Well_Big"=NA,"Gas_Well_Medium"=NA,"Gas_Well_Small"=NA,
                               "Oil_Well_Big"=NA,"Oil_Well_Medium"=NA,"Oil_Well_Small"=NA,"In"=0.1) )+
  guides(size = guide_legend("Legend"),
         colour = guide_legend("Legend"))
dallas_fig
oglegend<-get_legend(dallas_fig)

dallas_fig<-ggplot()+
  geom_arc_bar(aes(x0 = rad_vis$x, y0 = rad_vis$y, r0 = 0, r = 70000, start = start, end = end, 
                   fill = fill,alpha=alpha,color=color,size=color),
               data = pie)+
  geom_segment(data=wind_vis,aes(x=x,y=y,xend=x+5000*u,yend=y+5000*v),size = 0.5,arrow = arrow(length = unit(0.1,"cm")),show.legend = F)+
  #geom_polygon(data=dallas_city_extent,aes(x=long,y=lat,group=group),fill="Grey",col="Grey",alpha=0.5)+
  scale_color_manual(name=NULL,
                     labels=c("Buffer Zone","Gas (>50k Mcf)","Gas (15~50k Mcf)","Gas (<15k Mcf)", "Oil (>50 bbl)", "Oil (15~50 bbl)","Oil (<15 bbl)","Upwind Zone"),
                     breaks=c("Out","Gas_Well_Big","Gas_Well_Medium","Gas_Well_Small","Oil_Well_Big","Oil_Well_Medium","Oil_Well_Small","In"),
                     values=c("Out"="red","Gas_Well_Big"="green","Gas_Well_Medium"="green","Gas_Well_Small"="green",
                              "Oil_Well_Big"="blue","Oil_Well_Medium"="blue","Oil_Well_Small"="blue","In"="red"))+
  scale_size_manual(name=NULL,
                    labels=c("Buffer Zone","Gas (>50k Mcf)","Gas (15~50k Mcf)","Gas (<15k Mcf)", "Oil (>50 bbl)", "Oil (15~50 bbl)","Oil (<15 bbl)","Upwind Zone"),
                    breaks=c("Out","Gas_Well_Big","Gas_Well_Medium","Gas_Well_Small","Oil_Well_Big","Oil_Well_Medium","Oil_Well_Small","In"),
                    values=c("Out"=1,"Gas_Well_Big"=5,"Gas_Well_Medium"=2,"Gas_Well_Small"=1,
                             "Oil_Well_Big"=5,"Oil_Well_Medium"=2,"Oil_Well_Small"=1,"In"=1.5))+
  scale_fill_manual(name = NULL,
                    labels=c("Buffer Zone","Gas (>50k Mcf)","Gas (15~50k Mcf)","Gas (<15k Mcf)", "Oil (>50 bbl)", "Oil (15~50 bbl)","Oil (<15 bbl)","Upwind Zone"),
                    breaks=c("Out","Gas_Well_Big","Gas_Well_Medium","Gas_Well_Small","Oil_Well_Big","Oil_Well_Medium","Oil_Well_Small","In"),
                    values = c("Out"="white","Gas_Well_Big"=NA,"Gas_Well_Medium"=NA,"Gas_Well_Small"=NA,
                               "Oil_Well_Big"=NA,"Oil_Well_Medium"=NA,"Oil_Well_Small"=NA,"In"="red"))+
  scale_alpha_manual(name = NULL,
                     labels=c("Buffer Zone","Gas (>50k Mcf)","Gas (15~50k Mcf)","Gas (<15k Mcf)", "Oil (>50 bbl)", "Oil (15~50 bbl)","Oil (<15 bbl)","Upwind Zone"),
                     breaks=c("Out","Gas_Well_Big","Gas_Well_Medium","Gas_Well_Small","Oil_Well_Big","Oil_Well_Medium","Oil_Well_Small","In"),
                     values =c("Out"=0.5,"Gas_Well_Big"=NA,"Gas_Well_Medium"=NA,"Gas_Well_Small"=NA,
                               "Oil_Well_Big"=NA,"Oil_Well_Medium"=NA,"Oil_Well_Small"=NA,"In"=0.1) )
dallas_fig
zonelegned=get_legend(dallas_fig)

dallas_fig_c<-ggplot(dallas.df) + 
  #geom_point(aes(x=x, y=y, col=rgb(layer.1/256, layer.2/256, layer.3/256))) + 
  #scale_color_identity()+
  geom_point(data=well_s_vis[well_s_vis$Type=="V",],aes(x=x,y=y,size=Prod,color=type),fill="black")+
  scale_size_area(max_size = 5)+
  geom_arc_bar(aes(x0 = rad_vis$x, y0 = rad_vis$y, r0 = 0, r = 90000, start = start, end = end, 
                   fill = fill,alpha=alpha,color=color),size=1.5,
               data = pie,show.legend = F)+
  scale_color_manual(values = c("Out"=NA,"In"="red","Gas_Well"="green","Oil_Well"="blue"))+
  scale_fill_manual(values = c("Out"="white","In"="red"))+
  scale_alpha_manual(values=c("Out"=0.5,"In"=0.1))+
  geom_segment(data=wind_vis,aes(x=x,y=y,xend=x+5000*u,yend=y+5000*v),size = 0.5,arrow = arrow(length = unit(0.1,"cm")),show.legend = F)+
  geom_polygon(data=buffer,aes(x=long,y=lat,group=group),fill=NA,col="Red",show.legend = F)+
  geom_polygon(data=gBuffer(radnet,width=50000,quadsegs = 200),aes(x=long,y=lat,group=group),fill=NA,col="Red",show.legend=F)+
  geom_polygon(data=gBuffer(radnet,width=25000,quadsegs = 200),aes(x=long,y=lat,group=group),fill=NA,col="Red",show.legend = F)+
  geom_polygon(data=gBuffer(radnet,width=90000,quadsegs = 200),aes(x=long,y=lat,group=group),fill=NA,col="Red",show.legend = F)+
  geom_polygon(data=dallas_city_extent,aes(x=long,y=lat,group=group),fill="Grey",col="Grey",alpha=0.5,show.legend = F)+
  coord_fixed()+
  geom_text(data=rad_vis,aes(x=x,y=y,label=city),size=3)+
  geom_text(data=rad_vis,aes(x=x+25000*sin((data_b$dir[1]-45)*pi/180),y=y+25000*cos((data_b$dir[1]-45)*pi/180)),label="25 km",angle=225-data_b$dir[1],color="Black",size=2)+
  geom_text(data=rad_vis,aes(x=x+50000*sin((data_b$dir[1]-45)*pi/180),y=y+50000*cos((data_b$dir[1]-45)*pi/180)),label="50 km",angle=225-data_b$dir[1],color="Black",size=2)+
  geom_text(data=rad_vis,aes(x=x+70000*sin((data_b$dir[1]-45)*pi/180),y=y+70000*cos((data_b$dir[1]-45)*pi/180)),label="75 km",angle=225-data_b$dir[1],color="Black",size=2)+
  geom_text(data=rad_vis,aes(x=x+90000*sin((data_b$dir[1]-45)*pi/180),y=y+90000*cos((data_b$dir[1]-45)*pi/180)),label="100 km",angle=225-data_b$dir[1],color="Black",size=2)+
  theme(legend.position="none",
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())
dallas_fig_c

dallas_fig_d<-ggplot(dallas.df) + 
  #geom_point(aes(x=x, y=y, col=rgb(layer.1/256, layer.2/256, layer.3/256))) + 
  #scale_color_identity()+
  geom_point(data=well_s_vis[well_s_vis$Type=="H",],aes(x=x,y=y,size=Prod,color=type),fill="black")+
  scale_size_area(max_size = 5)+
  geom_arc_bar(aes(x0 = rad_vis$x, y0 = rad_vis$y, r0 = 0, r = 90000, start = start, end = end, 
                   fill = fill,alpha=alpha,color=color),size=1.5,
               data = pie,show.legend = F)+
  scale_color_manual(values = c("Out"=NA,"In"="red","Gas_Well"="green","Oil_Well"="blue"))+
  scale_fill_manual(values = c("Out"="white","In"="red"))+
  scale_alpha_manual(values=c("Out"=0.5,"In"=0.1))+
  geom_segment(data=wind_vis,aes(x=x,y=y,xend=x+5000*u,yend=y+5000*v),size = 0.5,arrow = arrow(length = unit(0.1,"cm")),show.legend = F)+
  geom_polygon(data=buffer,aes(x=long,y=lat,group=group),fill=NA,col="Red",show.legend = F)+
  geom_polygon(data=gBuffer(radnet,width=50000,quadsegs = 200),aes(x=long,y=lat,group=group),fill=NA,col="Red",show.legend=F)+
  geom_polygon(data=gBuffer(radnet,width=25000,quadsegs = 200),aes(x=long,y=lat,group=group),fill=NA,col="Red",show.legend = F)+
  geom_polygon(data=gBuffer(radnet,width=90000,quadsegs = 200),aes(x=long,y=lat,group=group),fill=NA,col="Red",show.legend = F)+
  geom_polygon(data=dallas_city_extent,aes(x=long,y=lat,group=group),fill="Grey",col="Grey",alpha=0.5,show.legend = F)+
  coord_fixed()+
  geom_text(data=rad_vis,aes(x=x,y=y,label=city),size=3)+
  geom_text(data=rad_vis,aes(x=x+25000*sin((data_b$dir[1]-45)*pi/180),y=y+25000*cos((data_b$dir[1]-45)*pi/180)),label="25 km",angle=225-data_b$dir[1],color="Black",size=2)+
  geom_text(data=rad_vis,aes(x=x+50000*sin((data_b$dir[1]-45)*pi/180),y=y+50000*cos((data_b$dir[1]-45)*pi/180)),label="50 km",angle=225-data_b$dir[1],color="Black",size=2)+
  geom_text(data=rad_vis,aes(x=x+70000*sin((data_b$dir[1]-45)*pi/180),y=y+70000*cos((data_b$dir[1]-45)*pi/180)),label="75 km",angle=225-data_b$dir[1],color="Black",size=2)+
  geom_text(data=rad_vis,aes(x=x+90000*sin((data_b$dir[1]-45)*pi/180),y=y+90000*cos((data_b$dir[1]-45)*pi/180)),label="100 km",angle=225-data_b$dir[1],color="Black",size=2)+
  theme(legend.position="none",
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())
dallas_fig_d

up_row<-plot_grid(dallas_fig_a,dallas_fig_b,NULL,nrow=1,labels = c("A","B",NA),label_x = 0.1,label_y = 0.9,rel_widths = c(5,5,3))
legend<-plot_grid(oglegend,zonelegned,nrow=2,rel_heights = c(4,2))
legend_col<-plot_grid(legend,NULL,nrow=1,rel_widths = c(1,1))
legend_col<-plot_grid(NULL,legend_col,rel_widths = c(1,5))
bot_row<-plot_grid(dallas_fig_c,dallas_fig_d,legend_col,nrow=1,labels=c("C","D",NA),label_x=0.1,label_y = 0.9,rel_widths = c(5,5,3))
dallas_fig<-plot_grid(up_row,bot_row,nrow=2,rel_heights = c(1,1))

ggsave("dallas.pdf",dallas_fig,width=7,height=5,unit="in")
#####################################################################
#
load(here::here("data","beta_gas_oil_avg_wind_50.RData"))
rad_all$beta<-1000*rad_all$beta
rad_all$Oil_Field<-(rad_all$G_Oil_Num>0)
rad_all$Gas_Field<-(rad_all$G_Gas_Num>0)
rad_all$Play<-rad_all$Oil_Field|rad_all$Gas_Field
play_beta_data<-rad_all[rad_all$Play,]
out_beta_data<-rad_all[!rad_all$Play,]
play_beta_data$type="Play"
play_beta_data$lb<-log(play_beta_data$beta)
out_beta_data$lb<-log(out_beta_data$beta)
out_beta_data$type="Out"
vis_data<-rbind.data.frame(play_beta_data[,c("city_state","lb","m_month","type","YEAR")],
                           out_beta_data[,c("city_state","lb","m_month","type","YEAR")])
ggplot(data=vis_data,aes(x=m_month,y=lb))+geom_boxplot()

                         