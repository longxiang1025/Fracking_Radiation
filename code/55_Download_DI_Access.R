r<-as.numeric(Sys.getenv("Sim"))

library(httr)
library(purrr)
library(jsonlite)
library(here)
library(raster)
library(dplyr)

load(here::here("data","Basic_Geodata","Boundaries.RData"))
load(here::here("data","Basic_Geodata","Counties.RData"))
states<-bound@data%>%dplyr::select(STATEFP,STUSPS,NAME)
counties<-county@data%>%dplyr::select(STATEFP,NAME)
names(states)<-c("STATEFP","SS_NAME","S_NAME")
names(counties)<-c("STATEFP","C_NAME")
s_c_table<-states%>%left_join(counties)
load(here::here("data","token","daily_token.RData"))
#Your own token should be provided here
#for(r in 1:nrow(s_c_table)){
##########
state_query=s_c_table[r,"SS_NAME"]
county_query=toupper(s_c_table[r,"C_NAME"])
if(grepl(".",county_query)){
  county_query=URLencode(county_query)
}
if(!file.exists(paste0("/n/scratchlfs/koutrakis_lab/Well_Production_Data_0829/",state_query,"_",county_query,".RData")))
{
  if(grepl("ST.%20",county_query)){
    county_query=gsub("ST.%20","ST%20",county_query)
  }
  if( startsWith(county_query,"MC")&state_query!="TX"){
    county_query=  gsub('^([A-Z]{2})([A-Z]+)$', '\\1%20\\2', county_query)
  }
  if(county_query=="MC%20KENZIE"){
    county_query="MCKENZIE"
  }
  #####
  #former url to download entity query result
  url=paste0("https://di-api.drillinginfo.com/v2/direct-access/producing-entities?&pagesize=100000&County=",county_query,
             "%20\\(",state_query,"\\)")
  re=GET(url=url, add_headers("x-api-key"= "2d1d300a96f5b2385b1bc085776cfa3a",
                              "authorization"= paste0("Bearer ",token)))
  if(re$status_code==401){
    token<-POST("https://di-api.drillinginfo.com/v2/direct-access/tokens?grant_type=client_credentials",
                add_headers("x-api-key"="2d1d300a96f5b2385b1bc085776cfa3a",
                            "content-type"="application/x-www-form-urlencoded",
                            "authorization"="***")
    )
    
    token=content(token)$access_token
    
    save(file=here("data","token","daily_token.RData"),token)
    re=GET(url=url, add_headers("x-api-key"= "2d1d300a96f5b2385b1bc085776cfa3a",
                                "authorization"= paste0("Bearer ",token)))
  }
  entities<-content(re,as="text",encoding = "UTF-8" )
  entities<-jsonlite::fromJSON(entities,simplifyDataFrame = TRUE)
  #the connection between entitiy and wellbore is not valid unless the first 12 unit of ApiNo and GovermentID are identical
  #####
  if(length(entities)==0){
    print(paste(Sys.time(),state_query,county_query,"No wells"))
  }else{
    entities$ApiNo_12<-substr(entities$ApiNo,1,12)
    #New url to download wellbore directly.
    url=paste0("https://di-api.drillinginfo.com/v2/direct-access/wellbores?&pagesize=100000&County=",county_query,"&StateProvince=",state_query)
    
    re=GET(url=url, add_headers("x-api-key"= "2d1d300a96f5b2385b1bc085776cfa3a",
                                "authorization"= paste0("Bearer ",token)))
    if(re$status_code==401){
      token<-POST("https://di-api.drillinginfo.com/v2/direct-access/tokens?grant_type=client_credentials",
                  add_headers("x-api-key"="2d1d300a96f5b2385b1bc085776cfa3a",
                              "content-type"="application/x-www-form-urlencoded",
                              "authorization"="***")
      )
      
      token=content(token)$access_token
      
      save(file=here("data","token","daily_token.RData"),token)
      re=GET(url=url, add_headers("x-api-key"= "2d1d300a96f5b2385b1bc085776cfa3a",
                                  "authorization"= paste0("Bearer ",token)))
    }
    wells<-content(re,as="text",encoding = "UTF-8" )
    wells<-jsonlite::fromJSON(wells,simplifyDataFrame = TRUE)
    wells<-wells%>%dplyr::filter(GovernmentID%in%entities$ApiNo_12)
    print(paste(r,"  ",Sys.time(),state_query,county_query,nrow(wells),"wells"))
    ###########
    #Code to download production data
    prod_list<-list()
    # record_list<-list()
    for(w in 1:nrow(entities)){
     etid=entities[w,"EntityId"]
     url=paste0("https://di-api.drillinginfo.com/v2/direct-access/producing-entity-stats?entityid=",etid)
     prod=GET(url=url, add_headers("x-api-key"= "2d1d300a96f5b2385b1bc085776cfa3a",
                                   "authorization"= paste0("Bearer ",token)))
     tries=0
     
     if(prod$status_code==401){
       token<-POST("https://di-api.drillinginfo.com/v2/direct-access/tokens?grant_type=client_credentials",
                   add_headers("x-api-key"="2d1d300a96f5b2385b1bc085776cfa3a",
                               "content-type"="application/x-www-form-urlencoded",
                               "authorization"="***")
       )
       
       token=content(token)$access_token
       
       save(file=here("data","token","daily_token.RData"),token)
       prod=GET(url=url, add_headers("x-api-key"= "2d1d300a96f5b2385b1bc085776cfa3a",
                                   "authorization"= paste0("Bearer ",token)))
     }
     
     while(prod$status_code!=200&tries<20){
       prod=GET(url=url, add_headers("x-api-key"= "2d1d300a96f5b2385b1bc085776cfa3a",
                                     "authorization"= paste0("Bearer ",token)))
       tries=tries+1
     }
     if(prod$status_code==200){
       prod<-content(prod,as="text",encoding = "UTF-8")
       prod<-jsonlite::fromJSON(prod,simplifyDataFrame = TRUE)
       prod_list[[w]]=prod 
     }
  ##################
  #   apino=t[w,"ApiNo"]
  #   gas_cum=prod$GasCum
  #   oil_cum=prod$LiqCum
  #   if(is.na(oil_cum)){oil_cum=0}
  #   if(is.na(gas_cum)){gas_cum=0}
  #   if(apino!=0&(gas_cum>0|oil_cum>0)){
  #     url=paste0("https://di-api.drillinginfo.com/v2/direct-access/producing-entity-details?&pagesize=100000&entityid=",etid)
  #     record=GET(url=url, add_headers("x-api-key"= "2d1d300a96f5b2385b1bc085776cfa3a",
  #                                     "authorization"= paste0("Bearer ",token)))
  #     tries=0
  #     while(record$status_code!=200&tries<20){
  #       reocrd=GET(url=url, add_headers("x-api-key"= "2d1d300a96f5b2385b1bc085776cfa3a",
  #                                       "authorization"= paste0("Bearer ",token)))
  #       tries=tries+1
  #     }
  #     if(record$status_code==200){
  #       record<-content(record,as="text",encoding = "UTF-8")
  #       record<-jsonlite::fromJSON(record,simplifyDataFrame = TRUE)
  #       record_list[[w]]=record 
  #     }
  #   }
  #   if(w%%100==0){
  #     print(paste(w,"Out of ",nrow(t),"is finished"))
  #   }
  # }
  # prod_list<-bind_rows(prod_list)
  # record_list<-bind_rows(record_list)
  # production_record=record_list
  # prod_list$FirstProdDate<-as.Date(prod_list$FirstProdDate,"%Y-%m-%dT%H:%M:%SZ")
  # prod_list$LastProdDate<-as.Date(prod_list$LastProdDate,"%Y-%m-%dT%H:%M:%SZ")
  # t<-t%>%left_join(prod_list,by=c("EntityId"="EntityId"))
  ###########
   } 
    prod_list<-bind_rows(prod_list)
    entities=entities%>%left_join(prod_list,by="EntityId")
    wells=wells%>%inner_join(entities%>%select(ApiNo_12,FirstProdDate,LastProdDate,Latitude,Longitude),
                             by=c("GovernmentID"="ApiNo_12"))
    wells[is.na(wells$WGS84Latitude),"WGS84Latitude"]=  wells[is.na(wells$WGS84Latitude),"Latitude"]
    wells[is.na(wells$WGS84Longitude),"WGS84Longitude"]=  wells[is.na(wells$WGS84Longitude),"Longitude"]
    wells$SpudDate<-as.Date(wells$SpudDate,"%Y-%m-%dT%H:%M:%SZ")
    wells$CompletionDate<-as.Date(wells$CompletionDate,"%Y-%m-%dT%H:%M:%SZ")
    wells$OnProductionDate<-as.Date(wells$OnProductionDate,"%Y-%m-%dT%H:%M:%SZ")
    wells$FirstProdDate<-as.Date(wells$FirstProdDate,"%Y-%m-%dT%H:%M:%SZ")
    wells$LastProdDate<-as.Date(wells$LastProdDate,"%Y-%m-%dT%H:%M:%SZ")
    save(file=paste0("/n/scratchlfs/koutrakis_lab/Well_Production_Data_0829/",state_query,"_",county_query,".RData"),wells)
  }
}else{
  print(paste(Sys.time(),state_query,county_query,"File Exists"))
}
#well_files<-list.files("/n/scratchlfs/koutrakis_lab/Well_Production_Data/",full.names = T)
#well_list<-list()
#for(w in 1:length(well_files)){
#  load(well_files[[w]])
#  well_list[[w]]<-t
#}
#wells<-bind_rows(well_list)
#wells<-wells%>%filter(ProdType%in%c("OIL","GAS","O&G","OIL (CYCLIC STEAM)"))
#wells<-wells%>%filter(!is.na(Longitude))
#save(file="/n/koutrakis_lab/lab/Fracking_Radiation/data/Wells.RData",wells)
