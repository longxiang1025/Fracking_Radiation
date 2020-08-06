library(here)
library(stringr)
library(dplyr)
all_files=list.files(here::here("data","Resub_city_daily_all"))
for(f in all_files){
  m<-try(
    load(here::here("data","Resub_city_daily_all",f))
    )
  if(class(m)=="try-error"){
    print(f)
  }
}
paras<-as.data.frame(str_split(all_files,pattern = "_",simplify = T))
names(paras)=c("Index","city","radius","angle","Info1","Info2","Info3")
paras<-paras[,c(2,3,4)]
paras$city<-as.numeric(as.character(paras$city))
paras$radius<-as.numeric(as.character(paras$radius))
paras$angle<-as.numeric(as.character(paras$angle))

temp=paras%>%group_by(city)%>%count()
temp%>%filter(n<21)