library(raster)
library(dplyr)
########Classify monitors#####################################
#The objective of this function is to classify the monitors  #
#based on the distance to wells. All monitors are classified #
#into 4 classes:                                             #
#1.Clear OG wells: H|V wells wihtin 100-250 km               #
#2.Unkown OG Wells: percentage of wells without drilling typ #
# over 15%                                                   #
#3.Clear Background wells: monitor without wells within 250km#
#4.Remote OG wells: monitors with wells only over 100km      #
##############################################################
monitor_class<-function(monitor,wells){
  #monitor<-radnet_sp
  #load(here::here("data","Wells_2nd.RData"))
  source(here::here("code","Data_Downloading_Functions.R"))
  well_characters<-matrix(NA,ncol=5,nrow=length(monitor))
  for(i in 1:length(monitor)){
    t<-create_link_buffer(monitor[i,],point_ID = "city_state",points = wells,points_ID = "API/UWI",si_col ="Drill_Type",width=250000) 
    if(!is.null(t)){
        u_ratio=nrow(t%>%filter(Drill_Type=="U"))/nrow(t)
        h_ratio=nrow(t%>%filter(Drill_Type%in%c("D","H")))/nrow(t%>%filter(Drill_Type%in%c("H","V","D")))
        n_close=nrow(t%>%filter(Drill_Type%in%c("H","V","D"),dist<100))
        n_remote=nrow(t%>%filter(Drill_Type%in%c("H","V","D"),dist>100))
        min_dist=min(t$dist)
        well_characters[i,]<-c(u_ratio,h_ratio,n_close,n_remote,min_dist) 
      }
    }
  well_characters<-as.data.frame(well_characters)
  well_characters$city_state=monitor@data[,"city_state"]
  names(well_characters)<-c("Unkown_ratio","H_radio","Num_Near","Num_Remote","closest","city_state")
  return(well_characters)
}
  