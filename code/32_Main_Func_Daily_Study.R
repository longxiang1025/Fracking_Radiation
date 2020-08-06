library(raster)
library(rgeos)
library(dplyr)
library(splines)
library(rgeos)
library(lubridate)
library(mgcv)
library(optimx)
options(dplyr.show_progress = F)
prjstring<-"+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs "
geoprjstring<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

#Load function files-----------------------------------------------------
source(here::here("code","Buffer_Functions.R"))
source(here::here("code","Plotting_Functions.R"))
source(here::here("code","Data_Downloading_Functions.R"))
source(here::here("code","35_Expand_Prod_Table_Function.R"))
source(here::here("code","36_Reshape_Prod_Data_Function.R"))
#Load the necessary data files
load(here::here("data","RadNet_Mine_Power.RData"))
rad_mine_power<-radnet_mine%>%dplyr::select(city_state,n_coal_plants,n_u_facility)
load(here::here("data","Beta_Measurements_2001.RData"))
load(here::here("data","Daily_PM_Data.RData"))
load(here::here("data","NARR_2001.RData"))
pm_mass_data<-pm_mass_data%>%dplyr::select("Uni_ID","Longitude","Latitude","Arithmetic Mean","Date Local")
names(pm_mass_data)<-c("Uni_ID","long","lat","mass","Date")
load(here::here("data","RadNet.RData"))
load(here::here("data","PM_Monitors.RData"))
load(here::here("data","All_Wells.RData"))
# Process Production Data -------------------------------------------------

# load(here::here("data","All_Production_Series.RData"))
# production_data<-production_data%>%select(`API/UWI`,`Monthly Production Date`,`Monthly Gas`,`Monthly Oil`)
# names(production_data)<-c("API/UWI","Date","Gas_Prod","Oil_Prod")
# production_data<-production_data%>%filter(Date > as.Date("2000-12-31"),Date < as.Date("2018-01-01"),(is.na(Gas_Prod)+is.na(Oil_Prod))<2)
# production_data[is.na(production_data$Gas_Prod),"Gas_Prod"]<-0
# production_data[production_data$Gas_Prod<0,"Gas_Prod"]<-0
# production_data[is.na(production_data$Oil_Prod),"Oil_Prod"]<-0
# production_data[production_data$Oil_Prod<0,"Oil_Prod"]<-0
# production_data$year<-year(production_data$Date)
# save(file=here::here("data","Production_2011.RData"),production_data)
#load(here::here("data","Production_2011.RData"))

# Coordinate Conversion ---------------------------------------------------

wells<-spTransform(header_list,prjstring)
wells@data[wells$`Drill Type`=="D",]$`Drill Type`="H"
radnet_sp<-spTransform(radnet,prjstring)
pm_sp<-spTransform(pm_monitors,prjstring)




# Add basic information to every radnet monitor ---------------------------
# radnet_basic<-as.data.frame(radnet@data[,1])
# names(radnet_basic)[1]<-"city_state"
# 
# dist<-dist2Line(radnet,spTransform(coast_lines,geoprjstring))
# radnet_basic$Dist2Coast<-(as.numeric(dist[,1]))/1000# Add the distance to coast information
# 
# rad_u<-raster(here::here("data","USGS_Radiometric","NAMrad_U1.tif"))
# radnet_basic$U<-raster::extract(rad_u,radnet,buffer=30000,fun=median,na.rm=T)# Add the background Uranium concentration into the basic table
# radnet_basic[is.na(radnet_basic$U),"U"]<-mean(radnet_basic[!is.na(radnet_basic$U),"U"])
# 
# radon_zone<-shapefile(here::here("data","USGS_Rn","usagrp_polygon.shp"))
# radnet_basic$Radon<-raster::extract(radon_zone,spTransform(radnet,proj4string(radon_zone)),buffer=25000,fun=median)[,"RI"]
# radnet_basic[is.na(radnet_basic$Radon),"Radon"]<-mean(radnet_basic[!is.na(radnet_basic$Radon),"Radon"],na.rm=T)#Add the Radon potential info into the basic table
# 
# radnet_basic<-radnet_basic%>%left_join(rad_mine_power)
#radnet_basic$Lat<-coordinates(radnet)[,2]
# save(file=here::here("data","RadNet_Basic.RData"),radnet_basic)
load(here::here("data","RadNet_Basic.RData"))

# Create the links between RadNet and Wells within 250 km-----------------
rad_well_link<-list()
for(i in 1:length(radnet_sp)){
  rad_well_link[[i]]<-create_link_buffer(point = radnet_sp[i,],point_ID = "city_state",points = wells,points_ID = "API/UWI",si_col ="Drill Type")
}
rad_well_link<-bind_rows(rad_well_link)
rm(wells,header_list)
# Create the links between RadNet and pm monitors within 50km--------------
rad_pm_link<-list()
for(i in 1:length(radnet_sp)){
  rad_pm_link[[i]]<-create_link_buffer(point = radnet_sp[i,],point_ID = "city_state",
                                       points = pm_sp,points_ID = "Uni_ID",width=50000)
}
rad_pm_link<-bind_rows(rad_pm_link)

# Link RadNet with PM -----------------------------------------------------
# 
# rad_pm<-list()
# f=0
# for(i in 1:length(radnet)){
#  city_state<-radnet[i,]@data[,"city_state"]
#  city_rad<-rad[rad$city_state==city_state,]
#  for(y in unique(city_rad$year)){
#    t<-city_rad%>%filter(year==y)%>%
#      left_join(rad_pm_link)%>%select("city_state","gross_beta","collect_start","collect_end","duration","Uni_ID")
#    t<-t%>%left_join(pm_mass_data)%>%
#      filter(Date%within%duration)%>%
#      dplyr::select(city_state,gross_beta,Uni_ID,collect_start,collect_end,Date,mass)
#    t<-t%>%group_by(collect_start,collect_end)%>%summarise(beta<-mean(gross_beta),
#                                                           pm<-mean(mass))
#    names(t)<-c("collect_start","collect_end","beta","pm")
#    if(nrow(t)>0){
#      f=f+1
#      t$city_state=city_state
#      rad_pm[[f]]<-t
#      print(paste(city_state,y))
#    }
#    rm(t)
#  }
# }
# rad_pm<-bind_rows(rad_pm)
# rad_pm[rad_pm$pm<0,"pm"]=0
# save(file=here::here("data","Rad_PM_2001.RData"),rad_pm)
load(here::here("data","Rad_PM_2001.RData"))





# Link RadNet with NARR ---------------------------------------------------

# rad_narr<-list()
# f=0
# for(i in 1:length(radnet)){
#   city_state<-radnet[i,]@data[,"city_state"]
#   city_rad<-rad[rad$city_state==city_state,]
#   city_narr<-narr_data[narr_data$city_state==city_state,]
#   for(y in unique(city_rad$year)){
#     t<-left_join(city_rad[city_rad$year==y,],city_narr[city_narr$year==y,],by=c("city_state"="city_state","year"="year"))%>%filter(Date%within%duration)
#     if(nrow(t)>0){
#       f=f+1
#       t<-t%>%group_by(collect_start,collect_end)%>%summarise(beta=mean(gross_beta,na.rm=T),
#                                                           m_snowc=mean(snowc,na.rm=T),
#                                                           m_uwnd=mean(uwnd.10m,na.rm=T),
#                                                           m_vwnd=mean(vwnd.10m,na.rm=T),
#                                                           m_rhum=mean(rhum.2m,na.rm=T),
#                                                           m_temp=mean(air.2m,na.rm=T),
#                                                           m_hpbl=mean(hpbl,na.rm=T),
#                                                           m_soil=mean(soilm,na.rm=T),
#                                                           prop_ter=mean(src=="Ter",na.rm=T))
#       t<-as.data.frame(t)
#       t$city_state=city_state
#       t$year=y
#       rad_narr[[f]]<-t
#       print(paste(city_state,y))
#       }
#   }
# }
# rad_narr<-bind_rows(rad_narr)
# rad_narr$wdir<- (270-atan2(rad_narr[,"m_vwnd"],rad_narr[,"m_uwnd"])*180/pi)%%360
# rad_narr$wvel<-sqrt(rad_narr[,"m_vwnd"]^2+rad_narr[,"m_uwnd"]^2)
# save(file=here::here("data","Rad_NARR_2001.RData"),rad_narr)
load(here::here("data","Rad_NARR_2001.RData"))


# Add HYSPLIT Result to Rad Data ------------------------------------------
 hys_trajs<-list.files(here::here("data","daily_HYSPLIT_H"),full.names = T)
 hys_info<-list()
 for(t in 1:length(hys_trajs)){
   load(hys_trajs[[t]])
   hys_info[[t]]<-daily_HYSPLIT_data
 }
 hys_info<-bind_rows(hys_info)
 save(file=here::here("data","HYPLIT_2001.RData"),hys_info)
 rad_hys<-list()
# f=0
# for(i in 1:length(radnet)){
#   city_state<-radnet[i,]@data[,"city_state"]
#   city_rad<-rad[rad$city_state==city_state,]
#   if(nrow(city_rad)>0){
#     f=f+1
#     city_hys<-hys_info[hys_info$city_state==city_state,]
#     t<-city_rad%>%left_join(city_hys)%>%
#       filter(Date%within%duration)%>%
#       select(city_state,collect_start,collect_end,Date,gross_beta,c_prop)%>%
#       group_by(collect_start)%>%
#       summarise(m_prop=mean(c_prop),
#                 beta=mean(gross_beta))
#     t$city_state=city_state
#     rad_hys[[f]]<-t 
#   }
# }
# rad_hys<-bind_rows(rad_hys)
#save(file=here::here("data","Rad_HYSP_2001.RData"),rad_hys)
load(here::here("data","Rad_HYSP_2001.RData"))
# rad_narr<-bind_rows(rad_narr)
# rad_narr$wdir<- (270-atan2(rad_narr[,"m_vwnd"],rad_narr[,"m_uwnd"])*180/pi)%%360
# rad_narr$wvel<-sqrt(rad_narr[,"m_vwnd"]^2+rad_narr[,"m_uwnd"]^2)
# save(file=here::here("data","Rad_NARR_2001.RData"),rad_narr)

# Link RadNet (with NARR) with Wells --------------------------------------------------

# rad_well<-list()
# f=0
# for(i in 1:length(radnet)){
#   city_state<-radnet[i,]@data[,"city_state"]
#   city_rad<-rad_narr[rad_narr$city_state==city_state,c("city_state","collect_start","collect_end","wdir","year")]
#   city_rad<-city_rad[!is.na(city_rad$collect_start),]
#   for(y in unique(city_rad$year)){
#     t<-city_rad%>%filter(year==y)%>%left_join(rad_well_link,by=c("city_state"="city_state"))
#     if(nrow(t>0)){
#       t$rad_duration=interval(start = t$collect_start,end=t$collect_end)
#       c<-production_data%>%filter(year==y,`API/UWI`%in%t$`API/UWI`)
#       if(nrow(c)>0){
#         f=f+1
#         c$prod_duraton<-month_interval(c$Date)
#         prod<-inner_join(t,c,by=c("year"="year","API/UWI"="API/UWI"))%>%filter(!is.na(Date),int_overlaps(rad_duration,prod_duraton))
#         #####################
#         #multiple buffers   #
#         #####################
#         brackets=c(100,150,200,250)
#         prod$brks<-apply(t(prod$dist),2,FUN = function(x) sum(x>brackets))
#         prod$dir<-ifelse(prod$dir>0,prod$dir,360+prod$dir)#convert the location vector to the same scale
#         prod$upwind<-abs((prod$dir-prod$wdir + 180) %% 360 - 180)<45#calculate the angle diff between wind vector and location vector
#         prod=prod%>%group_by(city_state,collect_start,collect_end,`Drill Type`,brks,upwind)%>%summarise(n_oil=sum(Oil_Prod>0),
#                                                                                                         n_gas=sum(Gas_Prod>0),
#                                                                                                         oil_prod=sum(Oil_Prod),
#                                                                                                         gas_prod=sum(Gas_Prod))
#         prod[(month(prod$collect_start)!=month(prod$collect_end))&(year(prod$collect_start)==year(prod$collect_end)),(ncol(prod)-3):ncol(prod)]=
#           prod[(month(prod$collect_start)!=month(prod$collect_end))&(year(prod$collect_start)==year(prod$collect_end)),(ncol(prod)-3):ncol(prod)]/2
#         prod=as.data.frame(prod)
#         rad_well[[f]]<-as.data.frame(prod)
#         print(paste(city_state,y,nrow(c),"Production Records"))
#       }else{ print(paste(city_state,y,nrow(c),"No Active Production Data"))}
#     }else{ print(paste(city_state,y,"No Radiation measurement")) }
#   }
# }
# rad_prod<-bind_rows(rad_well)
# save(file=here::here("data","Rad_Prod_2001.RData"),rad_prod)
files=list.files(here::here("data","city_daily_prod"))
well_list<-list()
f=0
for(file in files){
   f=f+1
   load(here::here("data","city_daily_prod",file))
   well_list[[f]]<-rad_prod
}
rad_prod<-bind_rows(well_list)
save(file=here::here("data","Rad_Prod_2001_3rd.RData"),rad_prod)
load(here::here("data","Rad_Prod_2001_2nd.RData"))


# Link All ----------------------------------------------------------------


result<-as.data.frame("model")#create a dataframe to store the results
breaks<-1:41
result<-expand(data=result,metric=c("n_oil","oil_prod","n_gas","gas_prod"),Drill=c("H","V"),Direct=c(TRUE,FALSE),brks=breaks)
model_summaries=matrix(0,ncol=16,nrow=nrow(result))
result<-cbind.data.frame(result,model_summaries)
for( m in c("n_oil","oil_prod","n_gas","gas_prod")){
  for (d in c("H","V")){
    for( w in c(FALSE)){
      rad_all_data<-list()
      f=0
      for(city in radnet$city_state){
        city_rad<-rad%>%filter(city_state==city)#radiation data table
        city_rad<-city_rad[city_rad$year<2018,]
        if(nrow(city_rad)>0){
          f=f+1
          city_pm<-rad_pm%>%filter(city_state==city)#pm re-organized by radiatiaon measurement data table
          city_narr<-rad_narr%>%filter(city_state==city)#narr re-organized by radiationmeasurement data table
          city_hys<-rad_hys%>%filter(city_state==city)
          city_prod<-rad_prod%>%filter(city_state==city)# raw production table
          t<-expand_prod(rad_table=city_rad,
                         prod_table=city_prod,metric= m,brks = breaks) # Expand the production table to full length
          city_metric<-prod_reshape(m,t,direct=w,dtype=d,brk=breaks) # Extract the number of all direction fracking oil wells in 4 brackets.
          suppressMessages(city_data<-city_rad%>%left_join(city_pm)%>%left_join(city_narr)%>%left_join(city_metric)%>%left_join(city_hys)%>%
            filter(!is.na(pm)))
          rad_all_data[[f]]<-city_data
          #print(paste(Sys.time(),city))
        }else{
          print(paste(Sys.time(),"No gross beta measurements in",city))
        }
      }
      rad_all_data<-bind_rows(rad_all_data)
      rad_all_data$pm_s<-rad_all_data$pm-10
      suppressMessages( rad_all_data<-rad_all_data%>%left_join(radnet_basic))
      rad_all_data$city_state<-as.factor(rad_all_data$city_state)
      rad_all_data$beta<-rad_all_data$beta*1000
      #basic_formula="beta~sp_trend+(1|city_state)+(0+pm_s|city_state)+pm_s*"
      metric_terms<-paste0("B",breaks,"_",m)
      if(grepl("n_",m)){
        rad_all_data[,metric_terms]=rad_all_data[,metric_terms]/1e3
      }
      if(grepl("_prod",m)){
        rad_all_data[,metric_terms]=rad_all_data[,metric_terms]/1e6
      }
      rad_all_data$ndays<-rad_all_data$ndays/1000
      rad_all_data$doy<-(yday(rad_all_data$collect_start)+yday(rad_all_data$collect_end))/2
      rad_all_data$m_temp<-rad_all_data$m_temp-273
      rad_all_data$Dist2Coast<-rad_all_data$Dist2Coast/1000
      rad_all_data$m_hpbl<-rad_all_data$m_hpbl/1000
      rad_all_data[is.na(rad_all_data$m_soil),]$m_soil=0
      rad_all_data$m_soil<-rad_all_data$m_soil/1000
      rad_all_data$lbeta<-log(rad_all_data$beta)
      rad_all_data$year_s<-rad_all_data$year-2000
      rad_all_data$c_lat<-rad_all_data$Lat-39
      rad_all_data$c_doy<-rad_all_data$doy-180
      rad_all_data$month<-month(rad_all_data$collect_start)
      rad_all_data$month<-as.factor(rad_all_data$month)
      n_city<-as.data.frame(rad_all_data%>%group_by(city_state)%>%summarise(n=length(city_state)))
      rad_all_data<-rad_all_data%>%filter(city_state%in%n_city[n_city$n>300,"city_state"])
      #n_year<-as.data.frame(rad_all_data%>%group_by(city_state)%>%summarise(ny=length(unique(year))))
      #n_month<-as.data.frame(rad_all_data%>%group_by(city_state)%>%summarise(ny=length(unique(month))))
      basic_formula="beta~m_prop+m_temp+m_hpbl+wvel+m_rhum+m_soil*c_lat+Dist2Coast+U+n_u_facility+year+(1|city_state/month)+pm_s*"
      for(k in breaks){
        model<-lmer(paste0(basic_formula,metric_terms[k]),data=rad_all_data,REML=F,
                              control =lmerControl(optimizer = "Nelder_Mead",optCtrl = list(maxfun=2e5)))
        summ<-summary(model)
        summ
        infos<-c(as.vector(t(summ$coefficients[row.names(summ$coefficients)%in%c(metric_terms[k],"pm_s",paste0("pm_s:",metric_terms[k])),c(1,2,5)])),summ$AICtab,range(rad_all_data[,metric_terms[k]]))
        result[result$metric==m&result$Drill==d&result$Direct==w&result$brks==k,5:20]<-infos
      }
      print(result[result$metric==m&result$Drill==d&result$Direct==w,c(1:4,8:10,20)])
    }
  }
}

lmerCtrl.optx <- function(method, ...)
  lmerControl(optimizer="optimx", ..., optCtrl=list(method=method))
