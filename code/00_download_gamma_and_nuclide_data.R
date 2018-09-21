library(tidyverse)
library(here)
library(knitr)
library(kableExtra)
library(here)
library(dplyr)
library(lubridate)
library(reshape2)
library(gridExtra)
library(gamm4)
library(RCurl)
library(httr)
library(rvest)
library(XML)
body<-read_html("https://www.epa.gov/radnet/radnet-csv-file-downloads")
links<-body %>% html_nodes("a") %>% html_attr('href')
t<-links[grep(pattern = "20[0-9][0-9]",links,perl=F)]
gamma_container<-list()
f=0
for(i in 2:(length(t)-1)){
  try({
    txt<-GET(t[i])
    if(txt$status_code==200){
      f<-f+1
      temp<-read_csv(content(txt,"text"))
      gamma_container[[f]]<-temp 
      print(t[i])
    }
  })
}
gamma_data<-do.call(rbind,gamma_container)
save(gamma_data,file=here::here("data","gamma_data.RData"))


nuc_list<-read_csv(here::here("data","nuclides_list.csv"),col_names = F)
names(nuc_list)<-"nuclide"
temp_link="https://ofmpub.epa.gov/enviro/erams_query_v2.simple_output?pStation=0&media=AIR-FILTER&radi=NUC&Fromyear=2000&Toyear=2018&units=Traditional"
nuc_container<-list()
f<-0
for(i in 1:nrow(nuc_list)){
  link<-gsub("NUC",nuc_list[i,],temp_link)
  info<-GET(link)
  body<-content(info,"text",encoding = "UTF-8")
  body<-read_html(body)
  table<-html_nodes(body,xpath = "//*[@id='main-content']/div[2]/div/div/div[2]/div/div/div/table[1]")
  data<-html_table(table)
  if(length(data)>0){
    data<-data[[1]]
    if(nrow(data)>5){
      f<-f+1
      nuc_container[[f]]<-data
      print(nuc_list[i,])
    } 
  }
}
nuc_data<-do.call(rbind,nuc_container)
save(nuc_data,file=here::here("data","nuclide_data.RData"))
               