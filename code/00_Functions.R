#######################################
#Extract the wind field stack raster data by location of points (SpatialPointsDataFrame)
#And re-format the data into a long,instead of wide, format.
#points: points of interest, in this study is the location of RadNet monitors
#u: the u-components of wind field
#v: the v-components of wind field
#key: the primary key of the points data, in this study is the city_state
#######################################
prepare_wind_field<-function(points,u,v,key){
  #check the necessary libraries
  require(lubridate)
  require(dplyr)
  require(tidyr)
  require(raster)
  #set the primary key of the table
  p_u<-as.data.frame(raster::extract(u,points))
  p_u[,key]=as.factor(points@data[,key])
  p_u<-gather(p_u,time,uwind,-key)
  p_u$time=names(u)
  p_v<-as.data.frame(raster::extract(v,points))
  p_v[,key]=as.factor(points@data[,key])
  p_v<-gather(p_v,time,vwind,-key)
  p_v$time=names(v)
  #filter the data and standardize the date
  p<-left_join(p_u,p_v)
  
  p[,"Date"]<-as.Date(substr(p[,"time"],2,11),format("%Y.%m.%d"))
  p[,"Year"]<-year(p[,"Date"])
  p[,"Month"]<-month(p[,"Date"])
  p[,"Day"]<-as.numeric(mday(p[,"Date"]))
  p[p$Day<15,"Month"]<- p[p$Day<15,"Month"]-1
  p<-p[,c(key,"uwind","vwind","Year","Month")]
  #calculate the wind direction and velocity
  p[,"dir"]<- (270-atan2(p[,"vwind"],p[,"uwind"])*180/pi)%%360
  p[,"vel"]<-sqrt(p[,"vwind"]^2+p[,"uwind"]^2)
  #remove the duplicate of wind of Nov
  p<-p[!duplicated(p[,c(key,"Year","Month")]),]
  return(p)
}

####################################
#Production data massage
####################################
production_reshape<-function(data,value,key,infos,extension){
  exp_data<-spread(data[,c(infos,key,value)],key=key,value = value)
  exp_data[is.na(exp_data)]<-0 
  if(!"H"%in%names(exp_data)){
    exp_data$H<-0
  }
  if(!"V"%in%names(exp_data)){
    exp_data$V<-0
  }
  exp_data$G<-exp_data$H+exp_data$V
  if(infos[1]=="Uni_ID"){
    exp_data=exp_data%>%group_by(Uni_ID,Prod_Year,Prod_Month)%>%summarise(basin=names(table(basin))[1],H=sum(H),V=sum(V),G=sum(G))
  }else{
    exp_data=exp_data%>%group_by(city_state,Prod_Year,Prod_Month)%>%summarise(basin=names(table(basin))[1],H=sum(H),V=sum(V),G=sum(G))
  }
  names(exp_data)[(length(infos)+1):(length(infos)+3)]<-paste0(names(exp_data)[(length(infos)+1):(length(infos)+3)],"_",extension)
  names(exp_data)[2]<-"YEAR"
  names(exp_data)[3]<-"MONTH"
  return(exp_data)
}

######################################
#list all objects
######################################
.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.dim)
  names(out) <- c("Type", "Size", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}
lsos <- function(..., n=10) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}
