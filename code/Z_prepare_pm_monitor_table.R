pm_mass_files<-list.files("data/PM_Mass/",full.names = T)
pm_mass_container<-list()

for(year in 2001:2017){
  annual_pm<-read_csv(pm_mass_files[grepl(as.character(year),pm_mass_files)])%>%
    dplyr::select(c("State Code","County Code","Site Num","Arithmetic Mean","Date Local","Longitude","Latitude"))
  annual_pm$ID<-paste0(annual_pm$`State Code`,annual_pm$`County Code`,annual_pm$`Site Num`,year)
  #annual_pm<-left_join(annual_pm,PM_MONITOR_ID_CONVERSION_TABLE,by="ID")
  pm_mass_container[[year-2000]]<-annual_pm
}
pm_mass_data<-do.call(rbind,pm_mass_container)
pm_monitor<-distinct(pm_mass_data[,c("Longitude","Latitude")])
pm_monitor$Uni_ID<-seq(1:nrow(pm_monitor))
annual_table_list<-list()
for(year in 2001:2017){
  annual_pm<-read_csv(pm_mass_files[grepl(as.character(year),pm_mass_files)])%>%
    dplyr::select(c("State Code","County Code","Site Num","Arithmetic Mean","Date Local","Longitude","Latitude"))
  annual_pm$ID<-paste0(annual_pm$`State Code`,annual_pm$`County Code`,annual_pm$`Site Num`,year)
  annual_pm<-left_join(annual_pm,pm_monitor,by=c("Longitude","Latitude"))
  annual_pm<-distinct(annual_pm[,c("ID","Uni_ID")])
  annual_table_list[[year-2000]]=annual_pm
}
monitor_list<-do.call(rbind,annual_table_list)
monitor_list<-left_join(monitor_list,pm_monitor)
save(monitor_list,file=here::here("data","PM_MONITOR_ID_CONVERSION_TABLE.RData"))
