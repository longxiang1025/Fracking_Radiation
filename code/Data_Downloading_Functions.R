library(raster)
library(dplyr)
library(here)
library(rgeos)
library(lubridate)
##############Data Download From NARR server#############################################
#@para 1: Name of the meteorological metric                                             #
#@para 2: Year                                                                          #
#@para 3: Location of Points to extrac the stacks                                       #
#########################################################################################
#Example Usage:                                                                         #
# load(here::here("data","Rad_NARR_2001.RData"))
# load(here::here("data","Beta_Measurements_2001.RData"))
# load(here::here("data","RadNet.RData"))
# load(here::here("data","Basic_Geodata","Coast_Lines.RData"))
# 
# narr_list<-list()
# for(year in 2001:2017){
#   annual_data<-download_narr_daily("snowc",year,radnet)
#   for(var in c("uwnd.10m","vwnd.10m","rhum.2m","air.2m","hpbl","soilm")){
#     t<-download_narr_daily(var,year,radnet)
#     annual_data[,get("var")]<-t[,get("var")]
#     print(paste(Sys.time(),var,year))
#   }
#   narr_list[[year-2000]]<-annual_data
# }
# narr_data<-do.call(rbind,narr_list)
# narr_data[,"dir"]<- (270-atan2(narr_data[,"vwnd.10m"],narr_data[,"uwnd.10m"])*180/pi)%%360
# narr_data[,"vel"]<-sqrt(narr_data[,"vwnd.10m"]^2+narr_data[,"uwnd.10m"]^2)
# narr_data$src=NA
# coast_lines_sim<-gSimplify(coast_lines,tol=100000)
# radnet_sp<-spTransform(radnet,proj4string(coast_lines))
# for(r in 1:length(radnet_sp)){
#    if(is.null(crop(coast_lines_sim,gBuffer(radnet_sp[r,],width=300000)))){
#      narr_data[narr_data$RadNet_ID==r,"src"]="Ter"
#      print(radnet_sp[r,"city_state"]$city_state)
#    }
#  }
# source(here::here("code","Buffer_Functions.R"))
# load(here::here("data","NARR_700000.RData"))
# for(i in 700001:nrow(narr_data)){
#   if(is.na(narr_data[i,"src"])){
#      narr_data[i,"src"]<-check_air_src(point=radnet_sp[narr_data[i,"RadNet_ID"],],
#                                        dir=narr_data[i,"dir"],
#                                        width = 300000,
#                                        coast_lines = coast_lines_sim)
#   }
#   if(i%%100000==0){
#     print(paste(i,Sys.time(),radnet_sp@data[narr_data[i,"RadNet_ID"],"city_state"],narr_data[i,"dir"],narr_data[i,"src"]))
#     save(file=here::here("data",paste0("NARR_",i,".RData")),narr_data)
# 
#   }
# }
# radnet$ID<-rownames(radnet@data)
# narr_data$RadNet_ID<-as.character(narr_data$RadNet_ID)
# narr_data<-left_join(narr_data,radnet@data[,c("ID","city_state")],by=c("RadNet_ID"="ID"))
# narr_data$Date<-as.Date(narr_data$doy-1,origin = as.Date(paste0(narr_data$year,"-01-01")))
# save(file=here::here("data","NARR_2001.RData"),narr_data)
#########################################################################################
download_narr_daily<-function(var,year,points){
  link=paste0("ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/monolevel/",var,".",year,".nc")
  curl::curl_download(url=link,destfile = here::here("data","NARR",paste0(var,"_",year,".nc")),mode = "wb")
  variable<-stack(here::here("data","NARR",paste0(var,"_",year,".nc")))
  points<-spTransform(points,proj4string(variable))
  ext_value<-extract(variable,points)
  ext_value<-c(ext_value)
  ext_value<-as.data.frame(ext_value)
  ext_value$doy<-as.integer(0:(nrow(ext_value)-1)/length(points))+1
  ext_value$year<-year
  names(ext_value)[1]=var
  ext_value$RadNet_ID=1:length(points)
  unlink(here::here("data","NARR",paste0(var,"_",year,".nc")))
  return(ext_value)
}

#############Convert a month into a


#########Link Date to Interval###########################################################
#The aim of this function is to link two data frames with their date. The date within th#
#e interval leads to a link. An interval has multiple links to single days.             #
#@para 1: the data.frame with interval                                                  #
#@para 2: the name of the interval column                                               #
#@para 3: the data.frame with date                                                      #
#@para 2: the name of the date column                                                   #
#@output: two data.frame with an extra column called "Link" (can be costumized) are     #
#@ returned to replace the original data.frames                                         #
#########################################################################################
#Example Usage:                                                                         #
#links<-interval_date_link(rad,"duration","ID",narr_data,"Date","RadNet_ID",            #
#  Link = "beta_narr_link")                                                             #
#########################################################################################
interval_date_link<-function(interval_data,interval_col,interval_ID,date_data,date_col,date_ID,Link="Link"){
  result<-list()
  interval_data<-as.data.frame(interval_data)
  interval_data[,Link]<-NA
  date_data<-as.data.frame(date_data)
  date_data[,Link]<-NA
  date_data[,date_ID]=as.numeric(date_data[,date_ID])
  for(i in 1:nrow(interval_data)){
    d=interval_data[i,c(interval_col,interval_ID)]
    interval_data[i,Link]=i
    date_data[(date_data$Date%within%d[,interval_col]&date_data$RadNet_ID==d[,interval_ID]),Link]=i
    if(i%%1000==0){
      print(paste(Sys.time(),i))
    }
  }
  result[[1]]<-interval_data
  result[[2]]<-date_data
  return(result)
}


##################Create links within buffer#############################################
#The aim of this function is to select points within a buffer of point then computer    #
#the relatitive relation between the center point and points                            #
#@para 1: the center point (monitor)                                                    #
#@para 2: the ID column of monitor point                                                #
#@para 3: the points (wells)                                                            #
#@para 4: the ID column of points (API)                                                 #
#@para 5: the width of buffer (default is 250km)                                        #
#@para 6: the additional col of points copied                                           #
#return is a list of points related with point with direction and distance added        #
#########################################################################################
#Examplt Usage:
#create_link_buffer(point = radnet_sp[i,],point_ID = "city_state",
#points = pm_sp,points_ID = "Uni_ID",width=50000)
#########################################################################################
create_link_buffer<-function(point,point_ID,points,points_ID,si_col=NULL,width=250000){
  out<-NULL
  t<-crop(points,gBuffer(point,width=width))
  if(is.null(t)){
    out=NULL
  }else{
    points<-crop(points,gBuffer(point,width=2*width))
    ids<-t@data[,points_ID]
    w_ids<-points@data[,points_ID]
    points[w_ids%in%ids,]
    
    if(is.null(t)){
      print(paste(point@data[,point_ID],"has no neighbour points"))
    }else{
      xy_table<-cbind.data.frame(coordinates(point),coordinates(t))
      names(xy_table)<-c("origin_x","origin_y","x","y")
      xy_table$dist<-sqrt((xy_table$x-xy_table$origin_x)^2+(xy_table$y-xy_table$origin_y)^2)/1000
      xy_table$dir<-180*atan2(xy_table$x-xy_table$origin_x,xy_table$y-xy_table$origin_y)/pi
      if(!is.null(si_col)){
        out<-cbind.data.frame(point@data[,point_ID],t@data[,points_ID],t@data[,si_col],xy_table[,c("dist","dir")])
        
      }else{
        out<-cbind.data.frame(point@data[,point_ID],t@data[,points_ID],xy_table[,c("dist","dir")])
      } 
      names(out)[1]<-point_ID
      names(out)[2]<-points_ID 
    } 
  }
  return(out)
}

################Convert a month numeric into interval####################################
#The aim of this function is to expand a number into an interval                        #
#@para 1: the date of the 1st day of month                                              #                                                                   #
#return is the interval object                                                          #
#########################################################################################
month_interval<-function(date){
  date0=date
  d<-days_in_month(date0)
  return(interval(date0,date0+d-1))
}

##############Reshpae the production data###############################################
#The aim of this function is to convert the long form of production data into a wide   #
#form


