library(here)
library(dplyr)
library(lubridate)
library(ggplot2)
well_prod_files<-list.files("/n/scratchlfs/koutrakis_lab/Well_Production_Data_0821/",full.names = T)
wells_list<-list()
for(f in 1:length(well_prod_files)){
  load(well_prod_files[f])
  wells<-wells%>%filter(!(is.na(SpudDate)&is.na(CompletionDate)&is.na(OnProductionDate)))
  wells<-wells%>%filter(!is.na(WGS84Latitude))
  wells_list[[f]]=wells
  if(f%%50==0){
    print(paste(Sys.time(),f,"Out of",length(well_prod_files)," has been loaded"))
  }
}
wells<-bind_rows(wells_list)
#1)Only keep the induplicated record
wells<-wells%>%distinct()
#2654032 wells are left

#At this step, the number of wells is 3417541
#######1) Only keep wells with actual production, exluding disposal, dry well and other types of wells
wells<-wells%>%filter(FluidType%in%c("CYCLIC STEAM",
                                     "GAS",
                                     "GAS OR COALBED",
                                     "OIL",
                                     "OIL & GAS"))
#2010964 wells left. Most of the wells are Oil (1156422)
summary(as.factor(wells$FluidType))
#2)remove any wells ABANDONED, INACTIVE, ORPHAN, P & A, SHUT-IN, TA before 01/01/2000
wells<-wells%>%filter(CurrentStatus!="UNKNOWN")
wells$CurrentStatusDate=as.Date(wells$CurrentStatusDate)
wells=wells%>%mutate(CurrentStatusDate=replace(CurrentStatusDate,
                                               CurrentStatusDate>"2100-01-01",
                                               "2007-08-17"))
#2010956 wells are with unknown status date, out of them 36776 are active. Only them are kept
t<-wells%>%filter(is.na(CurrentStatusDate))
wells<-wells%>%filter((!is.na(CurrentStatusDate))|(CurrentStatus%in%c("ACTIVE","COMPLETED","DRILLED")))
#1799954 wells left. The wells without status update date were set as 2019-09-01
wells<-wells%>%mutate(CurrentStatusDate=replace(CurrentStatusDate,
                                       (CurrentStatus%in%c("ACTIVE","COMPLETED","DRILLED")),
                                       as.Date("2019-09-01")))
#wells with absurd current update date are adjusted
wells[wells$CurrentStatusDate>"2019-09-01","CurrentStatusDate"]=wells[wells$CurrentStatusDate>"2019-09-01","CurrentStatusDate"]-14600
#wells abandoned before 2001-01-01 are excluded from the dataset
wells=wells%>%filter(!(CurrentStatusDate<"2001-01-01"&!(CurrentStatus%in%c("ACTIVE","COMPLETED","DRILLED"))))
#1207940 wells left. Waht's left are currently active wells or wells abandoned after 2001-01-01
t<-wells%>%filter(is.na(SpudDate),is.na(CompletionDate),is.na(OnProductionDate))
#All wells have at least one of these three dates,major plays have fair data quality
t<-wells%>%group_by(StateProvince)%>%summarize(mean(is.na(SpudDate)),mean(is.na(CompletionDate)),mean(is.na(OnProductionDate)))
print(as_tibble(t),n=30)

#The only horizontal well with spuddate after completion date is visually checked and adjusted
wells=wells%>%mutate(SpudDate=replace(SpudDate,
                                      (SpudDate>CompletionDate&WellboreType=="H"),
                                      as.Date("2015-06-02")))
t<-wells%>%group_by(StateProvince)%>%summarise(mean(WellboreType=="H"),mean(WellboreType=="V",mean(WellboreType=="D"),mean(WellboreType=="U")))
print(as_tibble(t),n=30)
#3)Handle the spuddate completiondate and production date
#combine completion date and production date because they should be very similar
wells[is.na(wells$CompletionDate),"CompletionDate"]=wells[is.na(wells$CompletionDate),"OnProductionDate"]
#19377 wells with spud date but no completion date, 4309 is active, 5426 is drilled, 2735 is inactive. These should be kept 
# inactive intentionally
t<-wells%>%filter(is.na(CompletionDate),!is.na(SpudDate))
#199532 wells with completion date but no spudding date
t<-wells%>%filter(!is.na(CompletionDate),is.na(SpudDate))


wells$Dur_Spud_Comp=difftime(wells$CompletionDate,wells$SpudDate,units = "days")
wells$Dur_Spud_Comp=as.numeric(wells$Dur_Spud_Comp)

wells%>%filter(year(SpudDate)==1906,WellboreType=="H")
wells%>%filter(WellboreType=="H")%>%group_by(year(SpudDate))%>%summarise(gap=median(Dur_Spud_Comp,na.rm=T))%>%print(n=135)

wells$Dur_Comp_Prod=difftime(as.Date(wells$FirstProdDate),as.Date(wells$CompDate),units = "days")
wells$Dur_Comp_Prod=as.numeric(wells$Dur_Comp_Prod)
