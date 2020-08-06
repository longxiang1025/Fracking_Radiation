library(sp)
library(rgeos)
###########Check Air Source###################################
#Estimate the source of air mass by 1) create a buffer wedge##
#2)crop the coastline with the buffer wedge 3) If there's any#
#Coastline within the wedge, the air mass is maritime, other-#
#-wise, it's terrestrial                                     #
#Para 1: center of the buffer, always monitor                #
#Para 2: Wind direction, traditional direction: toward the in#
#coming direction, north is 0&360, clockwise                 #
#Para 3: Width of the buffer                                 #
#Para 4: Target coastline                                    #
#Para 5:Optional the direction range of wedge                #
##############################################################
# An example to use it
# narr_data$src=NA
# narr_data$city_state<-as.character(narr_data$city_state)
# for(r in 1:length(radnet_sp)){
#   if(is.null(crop(coast_lines_sim,gBuffer(radnet_sp[r,],width=300000)))){
#     narr_data[narr_data$city_state==radnet_sp[r,"city_state"]$city_state,"src"]="Ter"
#     print(radnet_sp[r,"city_state"]$city_state)
#   }
# }
# for(i in 1:nrow(narr_data)){
#   if(is.na(narr_data[i,"src"])){
#     narr_data[i,"src"]<-check_air_src(point=radnet_sp[radnet_sp$city_state==narr_data[i,"city_state"],],
#                                       dir=narr_data[i,"dir"],
#                                       width = 300000,
#                                       coast_lines = coast_lines_sim) 
#   }
#   print(paste(i,Sys.time(),narr_data[i,"city_state"],narr_data[i,"dir"],narr_data[i,"src"]))
# }
#############################################################
check_air_src<-function(point,dir,width,coast_lines,range=45){
  r=width
  buffer<-gBuffer(point,width = r)
  c_b<-crop(coast_lines,buffer)
  src=NA
  if(is.null(c_b)){
    src="Ter"
  }else{
    p1<-coordinates(point)
    p2<-c(p1[1]+2*r*sin(pi*(dir-range)/180),p1[2]+2*r*cos(pi*(dir-range)/180))
    p3<-c(p1[1]+2*r*sin(pi*(dir+range)/180),p1[2]+2*r*cos(pi*(dir+range)/180))
    triangle<-Polygon(rbind(p1,p2,p3))
    triangle = Polygons(list(triangle),1)
    triangle = SpatialPolygons(list(triangle))
    buffer<-crop(buffer,triangle)
    mass_src<-crop(coast_lines,buffer)
    src<-ifelse(is.null(mass_src),"Ter","Mar") 
    rm(p1,p2,p3,triangle,mass_src)
  }
  rm(r,buffer)
  return(src)
}


#########Create Multiple Buffers and Extract Value Gradient###
#
multiple_buffers<-function(brks,metric,years=2014:2016,monitor_basic,group=c("YEAR","MONTH")){
  m_brks<-list()
  m_b=0
  for(b in brks){
    m_b=m_b+1
    all_prod<-list()
    f=0
    for(y in as.numeric(years)){
      f=f+1
      load(here::here(paste0("data/city_prod/R_",b,"_YEAR_",y,".RData")))
      all_prod[[f]]<-city_result[,c("city_state","YEAR","MONTH",metric)]
    }
    all_prod<-bind_rows(all_prod)
    names(all_prod)[4]<-"M"
    if(("YEAR"%in%group)&("MONTH"%in%group)){
      #for gross beta model
      avg_prod<-all_prod%>%group_by(city_state,YEAR,MONTH)%>%summarise(m=sum(M))
      monitor_prod<-monitor_basic%>%left_join(avg_prod,by=c("city_state"="city_state","YEAR"="YEAR","MONTH"="MONTH")) 
    }else if(("YEAR"%in%group)&(!"MONTH"%in%group)){
      #for Lead-210 model
      avg_prod<-all_prod%>%group_by(city_state,YEAR)%>%summarise(m=sum(M))
      monitor_prod<-monitor_basic%>%left_join(avg_prod,by=c("city_state"="city_state","Year"="YEAR")) 
    }
    if(grepl("Prod",metric)){
      monitor_prod$m=monitor_prod$m/1e6
    }
    if(grepl("Num",metric)){
      monitor_prod$m=monitor_prod$m/1e3
    }
    monitor_prod[is.na(monitor_prod$m),"m"]<-0
    monitor_prod$r=b
    m_brks[[m_b]]<-monitor_prod
  }
  m_brks<-do.call(rbind,m_brks)
  monitor_output<-cbind.data.frame(monitor_basic,matrix(nrow=nrow(monitor_basic),ncol=length(brks)))
  names(monitor_output)[ncol(monitor_basic)+seq(1:(length(brks)))]<-paste0("B",seq(1:(length(brks))),"_",metric)
  for(b in 1:(length(brks))){
    if(b==1){
      monitor_output[,paste0("B",b,"_",metric)]<-m_brks[m_brks$r==brks[b],"m"]
    }else{
      monitor_output[,paste0("B",b,"_",metric)]<-m_brks[m_brks$r==brks[b],"m"]-
        m_brks[m_brks$r==brks[b-1],"m"] 
    }
  }
  return(monitor_output)
}
