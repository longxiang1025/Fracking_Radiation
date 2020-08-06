#Get the environment variable for system
num=as.integer(as.character(Sys.getenv("Sim")))
print(num)
######Load Necessary packages
library(raster)
library(dplyr)
library(sf)
######set global parameter
p=(num%%23)+1#1:23
d=as.integer(num/23)+1#1:365
######Source Global function
source(here::here("code","00_Functions.R"))
######Define Global constant
prjstring<-"+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs "
geoprjstring<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
######Load spatial boundaris O&G plays
load(here::here("data","Basic_Geodata","EIA_major_plays.RData"))
usgs_u238<-raster(here::here("data","USGS_Radiometric","NAMrad_U1.tif"))
usgs_u238[is.na(usgs_u238[])] <- 0 
######Load the completed wells at the end of 2017
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
uwells<-wells[wells$DrillType=="H",]
uwells[is.na(uwells$CompDate),"CompDate"]=uwells[is.na(uwells$CompDate),]$FirstProdDate
######Borrow 1km grid from usgs_u238 data (no need to return)
major_plays<-spTransform(major_plays,CRS(proj4string(usgs_u238)))
wells<-spTransform(wells,CRS(proj4string(usgs_u238)))
uwells<-wells[wells$DrillType=="H",]
uwells[is.na(uwells$CompDate),"CompDate"]=uwells[is.na(uwells$CompDate),]$FirstProdDate
uwells=uwells[uwells$CompDate<as.Date("2017-12-31"),]
uwells=st_as_sf(uwells)
######Start from a small patch
area=major_plays[p,]
area_uwells=st_crop(uwells,st_buffer(st_as_sf(area),30000))
grids<-crop(usgs_u238,area)
grid_points<-rasterToPoints(grids,spatial = T)
grid_points<-raster::intersect(grid_points,area)
print(paste(Sys.time(),length(grid_points),"Sq km^2"))
#####Create buffers with a radius of 20 km
grid_points<-st_as_sf(grid_points)
grid_points$ID=1:nrow(grid_points)
if(!dir.exists(here::here("data","grid_UOG","grids"))){
  dir.create(here::here("data","grid_UOG","grids"),recursive = T)
}
if(!file.exists(here::here("data","grid_UOG","grids",paste0(p,".RData")))){
  save(file=here::here("data","grid_UOG","grids",paste0(p,".RData")),grid_points)
}
grid_points_bf<-st_buffer(x=grid_points,dist=20000,nQuadSegs=10)
grid_points_bf=grid_points_bf%>%dplyr::select(ID)
#####Load wind direction data
uwind<-stack(here::here("data","NARR","uwnd.10m.2017.nc"))
vwind<-stack(here::here("data","NARR","vwnd.10m.2017.nc"))
#####Start a loop based on Julian day
uwind<-uwind[[d]]
vwind<-vwind[[d]]
#get the wind direction
grid_wind<-prepare_wind_field(as(grid_points, "Spatial"),uwind,vwind,key="ID")
#convert the 2d vector into unit(30km) vecotr
grid_wind$u=grid_wind$uwind*30000/grid_wind$vel
grid_wind$v=grid_wind$vwind*30000/grid_wind$vel
#create upwind triangles with a side of 30km
#1st counerclosewise rotate 45 degree
grid_wind$u1=cos(-pi/4)*grid_wind$u-sin(-pi/4)*grid_wind$v
grid_wind$v1=sin(-pi/4)*grid_wind$u+cos(-pi/4)*grid_wind$v
#then closewise rotate 45 degree
grid_wind$u2=cos(pi/4)*grid_wind$u-sin(pi/4)*grid_wind$v
grid_wind$v2=sin(pi/4)*grid_wind$u+cos(pi/4)*grid_wind$v
#get the nodes of triangle
grid_wind[,c("x","y")]=st_coordinates(grid_points)
grid_wind$x1=grid_wind$x+grid_wind$u1
grid_wind$y1=grid_wind$y+grid_wind$v1
grid_wind$x2=grid_wind$x+grid_wind$u2
grid_wind$y2=grid_wind$y+grid_wind$v2
#create equal-side triangles with the nodes
lst <- lapply(1:nrow(grid_wind), function(i){
  ## create a matrix of coordinates that also 'close' the polygon
  res <- matrix(c(grid_wind[i, 'x'], grid_wind[i, 'y'],
                  grid_wind[i, 'x1'], grid_wind[i, 'y1'],
                  grid_wind[i, 'x2'], grid_wind[i, 'y2'],
                  grid_wind[i, 'x'], grid_wind[i, 'y'])  ## need to close the polygon
                  , ncol =2, byrow = T
    )
    ## create polygon objects
    st_polygon(list(res))
  })
trigs <- st_sf(st_sfc(lst))
st_crs(trigs)<-st_crs(grid_points)
daily_uwells<-area_uwells%>%dplyr::filter(CompDate<(as.Date("2017-01-01")+(d-1)))
lst<-lapply(1:nrow(grid_wind),function(i){
  t<-st_intersection(trigs[i,],grid_points_bf[i,])
  t<-st_intersection(t,daily_uwells)
nrow(t)})
lst=unlist(lst)
grid_points$UOG=lst
result=as.data.frame(grid_points)
result$Date=as.Date("2017-01-01")+(d-1)
print(Sys.time())
print(as.Date("2017-01-01")+(d-1))
result=result%>%dplyr::select(ID,UOG,Date)
save(file=here::here("data","grid_UOG",paste0(p,"_",d,".RData")),result)

