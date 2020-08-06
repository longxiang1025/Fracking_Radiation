######Load Necessary packages#######
library(raster)
library(dplyr)
library(sf)
library(stringr)
library(ggplot2)
p_num=5
load(here::here("data","Basic_Geodata","EIA_major_plays.RData"))
######Set global parameter##########
######Load Daily UOG grid data######
files=list.files(here::here("data","grid_UOG"))
files<-as.data.frame(files)
files$p=stringr::str_split(files$files,"_",simplify = T)[,1]
files$jday=stringr::str_sub(files$files,start=stringr::str_locate(files$files,"_")[,2]+1,end=stringr::str_locate(files$files,".RData")[,1]-1)
files$jday=as.numeric(files$jday)
files=files[!is.na(files$jday),]

load(here::here("data","grid_UOG","grids",paste0(p_num,".RData")))
grid_points=grid_points%>%dplyr::select(ID)
grid_points$U_UOG=0
for(j in 1:365){
  daily_files=files%>%filter(jday==j,p==p_num)
  if(nrow(daily_files)>0){
    load(here::here("data","grid_UOG",daily_files$files))
    grid_points$U_UOG=grid_points$U_UOG+(result$UOG>500)
  }
}
rast_data<-cbind.data.frame(st_coordinates(grid_points),grid_points$U_UOG)
names(rast_data)=c("x","y","UOG")
summary(rast_data$UOG)
print(ggplot(data=rast_data)+geom_raster(aes(x=x,y=y,fill=UOG>0))) 

