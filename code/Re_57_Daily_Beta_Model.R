sim<-as.numeric(Sys.getenv("Sim"))
metrics=c("u_h","d_h","u_v","d_v")
#sim from 0-27
m=sim%%4

m=m+1
r=as.integer(sim/4)

library(here)
library(stringr)
library(dplyr)
library(lme4)
library(optimx)
library(sp)
library(lubridate)
library(boot)
library(ggplot2)
library(raster)
library(merTools)
prjstring<-"+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs "
geoprjstring<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
sim_times=500

load(here::here("data","RadNet.RData"))
radnet_sp<-spTransform(radnet,prjstring)
load(here::here("data","RadNet_Basic.RData"))
plays<-shapefile(here::here("data","Basic_Geodata","ShalePlays_US_EIA_Apr2019.shp"))
plays<-spTransform(plays,prjstring)

radnet_sp$Within=F
within<-radnet_sp[buffer(plays,width=50000),]
outside<-radnet_sp[!radnet_sp$city_state%in%within$city_state,]
within$Within=T
radnet_sp=bind(within,outside)

radnet_basic$state=str_split(radnet_basic$city_state,",",simplify = T)[,2]
sun_spots<-read.csv(here::here("data","SN_ms_tot_V2.0.csv"),header = F)
names(sun_spots)<-c("Year","Month","Num","Sd","nobs","flag")
sun_spots$Year<-as.numeric(as.character(sun_spots$Year))
sun_spots$Month<-as.numeric(as.character(sun_spots$Month))
sun_spots$Num=as.numeric(as.character(sun_spots$Num))

lmerCtrl.optx <- function(method, ...)
  lmerControl(optimizer="optimx", ..., optCtrl=list(method=method))

all_files<-list.files(here::here("data","Resub_city_daily_all"))
paras<-as.data.frame(str_split(all_files,pattern = "_",simplify = T))
names(paras)=c("Index","city","radius","angle","Info1","Info2","Info3")
paras<-paras[,c(2,3,4)]
paras$city<-as.numeric(as.character(paras$city))
paras$radius<-as.numeric(as.character(paras$radius))
paras$angle<-as.numeric(as.character(paras$angle))

sel_files<-all_files[paras$radius==0&paras$angle==45]
dataset=list()
for(f in 1:length(sel_files)){
  try({
    load(here::here("data","Resub_city_daily_all",sel_files[f]))
    dataset[[f]]<-city_data
      })
}
dataset<-bind_rows(dataset)
dataset<-dataset%>%filter(!is.na(pm))
dataset<-dataset%>%left_join(radnet_basic)
dataset<-dataset%>%dplyr::select(city_state,gross_beta,year,snowc,doy,rhum.2m,air.2m,hpbl,soilm,vel,Date,
                                 u_h,d_h,u_v,d_v,c_prop,pm,Dist2Coast,U,K,n_u_facility,Lat,state)
dataset$temp=dataset$air.2m
dataset$inv_hpbl=1/dataset$hpbl
dataset$rhum=dataset$rhum.2m
#Per to Petros, we sepearte all radnet monitors into two groups: O&G monitors and outside monitors
#load("/n/scratchlfs/koutrakis_lab/longxiang/Fracking_Radiation/data/OG_RadNet.RData")
dataset_summ<-dataset%>%group_by(city_state)%>%summarise(n=length(city_state))
dataset_list<-dataset_summ%>%filter(n>500)
dataset<-dataset%>%filter(city_state%in%dataset_list$city_state)

dataset$lbeta=log(dataset$gross_beta)
dataset=dataset%>%filter(pm<35)
dataset=dataset%>%filter(vel<6)
dataset=dataset%>%filter(hpbl<3000,hpbl>150)
dataset=dataset%>%filter(gross_beta<0.04)
dataset$l_pm=log(dataset$pm)
dataset$Month=month(dataset$Date)
dataset<-dataset%>%left_join(sun_spots,by=c("year"="Year","Month"="Month"))

dataset$city_state=as.factor(dataset$city_state)
dataset$Lat_diff=abs(dataset$Lat-35)
dataset$year=dataset$year-2000
dataset$vel_c=dataset$vel-3
dataset$pm_c=dataset$pm-10
dataset<-dataset%>%filter(!is.na(c_prop),!is.na(soilm))

mod_data<-dataset%>%dplyr::filter(!(state=="KY"&year==2017))

save(file = here::here("data","Mod_Data.RData"),mod_data)
load(file = here::here("data","Mod_Data.RData"))

#city_state!="LOS ANGELES,CA",city_state!="ANAHEIM,CA"

basic_formula="gross_beta~c_prop+temp+I(temp^2)+year+I(year^2)+inv_hpbl+pm_c+rhum+Dist2Coast+U*soilm+Num*Lat_diff+vel+pm_c*"
model=lme4::lmer(paste0(basic_formula,"u_h+(1|city_state)"),data=mod_data,REML=T)
summary(model)
confint.merMod(model,level=0.95,method = "Wald")

pred_data=dataset%>%group_by(year)%>%summarise(c_prop=mean(c_prop,na.rm=T),
                                               temp=mean(temp,na.rm=T),
                                               inv_hpbl=mean(inv_hpbl,na.rm=T),
                                               pm_c=mean(pm_c,na.rm=T),
                                               rhum=mean(rhum,na.rm=T),
                                               Dist2Coast=mean(Dist2Coast,na.rm=T),
                                               U=mean(U,na.rm=T),
                                               soilm=mean(soilm,na.rm=T),
                                               Lat_diff=mean(Lat_diff,na.rm=T),
                                               vel_c=mean(vel_c),na.rm=T,
                                               Num=mean(Num,na.rm=T))

pred_data=cbind.data.frame(mean(dataset$c_prop,na.rm=T),
                           mean(dataset$temp,na.rm=T),
                           mean(dataset$inv_hpbl,na.rm=T),
                           mean(dataset$pm_c,na.rm=T),
                           mean(dataset$rhum,na.rm=T),
                           mean(dataset$Dist2Coast,na.rm=T),
                           mean(dataset$U,na.rm=T),
                           mean(dataset$soilm,na.rm=T),
                           mean(dataset$Lat_diff,na.rm=T),
                           mean(dataset$vel_c,na.rm=T),
                           mean(dataset$Num,na.rm=T))
names(pred_data)=c("c_prop","temp","inv_hpbl","pm_c","rhum","Dist2Coast",
                   "U","soilm","Lat_diff","vel_c","Num")
pred_data$year=9
pred_data$city_state="FORT WORTH,TX"
pred_data$u_h=0
t<-predictInterval(model,newdata=pred_data,which = "fixed",level=0.9,n.sims=10000,stat="median")

index=which(names(fixef(model))==metrics[m])

inf=NULL
inf<-influence.ME::influence(model,group = "city_state")
t<-inf$alt.fixed
sd<-inf$alt.se
p<-abs(inf$alt.test)

#leave_est<-t[,index]
#leave_est_ci_u<-t[,index]+1.965*sd[,index]
#leave_est_ci_b<-t[,index]-1.965*sd[,index]

#lst<-unique(dataset[dataset[,metrics[m]]>0,"city_state"])

#dat<-cbind.data.frame(row.names(t),leave_est,leave_est_ci_b,leave_est_ci_u)
#names(dat)<-c("city","est","ci_b","ci_u")
#dat=dat%>%arrange(est)
#dat=dat%>%filter(city%in%lst$city_state)
#dat$city<-factor(dat$city,levels=unique(dat$city))
#ggplot2::ggplot(data=dat)+
#  geom_point(aes(x=100*est,y=city),color="red",size=2)+
#  geom_errorbarh(aes(xmin=100*ci_b,xmax=100*ci_u,y=city))+
#  geom_vline(xintercept = 100*basic_coef[index])+
#  ggtitle("Sensitivity of the contribution of upwind unconventional gas wells to monitor selection (50 km)")+
#  xlab("Increase of beta radiation per 100 additional upwind unconventional gas wells")+
#  theme(panel.border = element_rect(colour = "black", size=1,fill=NA),
#        panel.background = element_blank(),
#        panel.grid.major = element_blank(), 
#        panel.grid.minor = element_blank(),
#        panel.grid.major.x = element_blank(),
#        panel.grid.major.y = element_blank(),
#        plot.title = element_text(size=8,face="bold"),
#        axis.line = element_blank(),
#        axis.text = element_text(size=8,face="bold"),
#        axis.ticks = element_blank(),
#        axis.title = element_text(size=8,face="bold"),
#        legend.title = element_text(size =5, face = 'bold'),
#        legend.key.height = unit(0.75,"lines"),
#        legend.key.width=unit(1.25,"lines"),
#        legend.text = element_text(size=5,face='italic'),
#        legend.position = c(0.22, 0.1),
#        legend.direction="horizontal")
summary(t[,index])
summary(p[,index])

out_max=which.max(t[,index])
med=median(t[,index])
out_min=which.min(t[,index])
if(t[out_min,index]<0&t[out_max,index]>0){
  print(paste0("remove ",names(out_max)," due to huge leverage"))
  dataset=dataset[dataset$city_state!=names(out_max),]
  model=lme4::lmer(paste0(basic_formula,metrics[m],"+(1|city_state)"),data=dataset,REML=T)
  print(summary(model))
  inf<-influence.ME::influence(model,group = "city_state")
  t<-inf$alt.fixed
  p<-abs(inf$alt.test)
  print(summary(t[,index]))
  print(summary(p[,index]))
}else{
  print("GOOD! No single monitor to leverage the whole coefficient from significant to insignificant")
}

basic_coef<-summary(model)$coefficients
vcov<-vcov.merMod(model)

coef=basic_coef[index]
set.seed(12345)


bCI=confint.merMod(model,level=0.95,method="boot",boot.type = "perc",nsim=sim_times,re.form=~1|city_state)

dataset$vel_c=dataset$vel-3
model=lme4::lmer(paste0(basic_formula,metrics[m],"+(1|city_state)"),data=dataset,REML=T)
summary(model)
md_coef<-summary(model)$coefficients

bCI_md=confint.merMod(model,level=0.95,method="boot",boot.type = "perc",nsim=sim_times,re.form=~1|city_state)

range=as.data.frame(dataset)[,metrics[m]]
range=range[range>0]
range_metric=quantile(range,c(0.75,0.9,0.95,0.99,0.999),na.rm=T)
range_metric

radnet_basic=radnet_basic%>%left_join(radnet_sp@data[,c("city_state","Within")],by=c("city_state"="city_state"))
radnet_basic%>%group_by(Within)%>%summarise(m=mean(U),l=quantile(U,0.25),u=quantile(U,0.75))
dataset<-dataset%>%left_join(radnet_basic[,c("city_state","Within")],by=c("city_state"="city_state"))
pm_summ<-dataset%>%group_by(Within,year)%>%summarise(m=mean(pm),
                                                     l=quantile(pm,0.25),
                                                     u=quantile(pm,0.75))

beta_summ<- dataset%>%group_by(Within,year)%>%summarise(m=mean(gross_beta),
                                                        l=quantile(gross_beta,0.25),
                                                        u=quantile(gross_beta,0.75))
ggplot()+
  geom_point(data=pm_summ%>%filter(Within=="TRUE"),aes(x=year,y=m,color="OG Region"),size=3)+
  geom_line(data=pm_summ%>%filter(Within=="TRUE"),aes(x=year,y=m,color="OG Region"))+
  geom_errorbar(data=pm_summ%>%filter(Within=="TRUE"),aes(x=year,ymin=l,ymax=u,color="OG Region"),width=0.25)+
  geom_point(data=pm_summ%>%filter(Within=="FALSE"),aes(x=year+0.25,y=m,color="Other Region"),size=3)+
  geom_line(data=pm_summ%>%filter(Within=="FALSE"),aes(x=year+0.25,y=m,color="Other Region"))+
  geom_errorbar(data=pm_summ%>%filter(Within=="FALSE"),aes(x=year+0.25,ymin=l,ymax=u,color="Other Region"),width=0.25)+
  scale_color_manual(breaks=c("OG Region","Other Region"),values = c("#762a83","#01665e"))+
  scale_x_continuous(breaks = seq(2001,2017))+
  ylab("PM2.5 (ug/m3)")+
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(),
    legend.title = element_blank(),
    axis.title = element_text(size=16),
    axis.text = element_text(size=12),
    axis.title.x = element_blank(),
    legend.position = c(0.8,0.9),
    legend.text = element_text(size=12)
  )

ggplot()+
  geom_point(data=beta_summ%>%filter(Within=="TRUE"),aes(x=year,y=m,color="OG Region"),size=3)+
  geom_line(data=beta_summ%>%filter(Within=="TRUE"),aes(x=year,y=m,color="OG Region"))+
  geom_errorbar(data=beta_summ%>%filter(Within=="TRUE"),aes(x=year,ymin=l,ymax=u,color="OG Region"),width=0.25)+
  geom_point(data=beta_summ%>%filter(Within=="FALSE"),aes(x=year+0.25,y=m,color="Other Region"),size=3)+
  geom_line(data=beta_summ%>%filter(Within=="FALSE"),aes(x=year+0.25,y=m,color="Other Region"))+
  geom_errorbar(data=beta_summ%>%filter(Within=="FALSE"),aes(x=year+0.25,ymin=l,ymax=u,color="Other Region"),width=0.25)+
  scale_color_manual(breaks=c("OG Region","Other Region"),values = c("#762a83","#01665e"))+
  scale_x_continuous(breaks = seq(2001,2017))+
  ylab("Gross beta radiation (pCi/m3)")+
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(),
    legend.title = element_blank(),
    axis.title = element_text(size=16),
    axis.text = element_text(size=12),
    axis.title.x = element_blank(),
    legend.position = c(0.8,0.9),
    legend.text = element_text(size=12)
  )

cbind(mean(dataset$U,na.rm=T),quantile(dataset$U,0.25),quantile(dataset$U,0.75))
cbind(mean(dataset$gross_beta,na.rm=T),quantile(dataset$gross_beta,0.25),quantile(dataset$gross_beta,0.75))
cbind(mean(dataset$c_prop,na.rm=T),quantile(dataset$c_prop,0.25,na.rm=T),quantile(dataset$c_prop,0.75,na.rm=T))
cbind(mean(dataset$soilm,na.rm=T),quantile(dataset$soilm,0.25,na.rm=T),quantile(dataset$soilm,0.75,na.rm=T))
cbind(mean(dataset$pm,na.rm=T),quantile(dataset$pm,0.25,na.rm=T),quantile(dataset$pm,0.75,na.rm=T))
cbind(mean(dataset$rhum,na.rm=T),quantile(dataset$rhum,0.25,na.rm=T),quantile(dataset$rhum,0.75,na.rm=T))
cbind(mean(dataset$temp,na.rm=T),quantile(dataset$temp,0.25,na.rm=T),quantile(dataset$temp,0.75,na.rm=T))
cbind(mean(dataset$hpbl,na.rm=T),quantile(dataset$hpbl,0.25,na.rm=T),quantile(dataset$hpbl,0.75,na.rm=T))
cbind(mean(dataset$vel,na.rm=T),quantile(dataset$vel,0.25,na.rm=T),quantile(dataset$vel,0.75,na.rm=T))

dataset%>%group_by(Within)%>%summarise(m=mean(gross_beta,na.rm=T),
                                       l=quantile(gross_beta,0.25,na.rm=T),
                                       u=quantile(gross_beta,0.75,na.rm=T))

dataset%>%group_by(Within)%>%summarise(m=mean(c_prop,na.rm=T),
                                       l=quantile(c_prop,0.25,na.rm=T),
                                       u=quantile(c_prop,0.75,na.rm=T))
dataset%>%group_by(Within)%>%summarise(m=mean(rhum,na.rm=T),
                                       l=quantile(rhum,0.25,na.rm=T),
                                       u=quantile(rhum,0.75,na.rm=T))
dataset%>%group_by(Within)%>%summarise(m=mean(soilm,na.rm=T),
                                       l=quantile(soilm,0.25,na.rm=T),
                                       u=quantile(soilm,0.75,na.rm=T))
dataset%>%group_by(Within)%>%summarise(m=mean(temp,na.rm=T),
                                       l=quantile(temp,0.25,na.rm=T),
                                       u=quantile(temp,0.75,na.rm=T))%>%dplyr::mutate_if(is.numeric, format, 1)
dataset%>%group_by(Within)%>%summarise(m=mean(hpbl,na.rm=T),
                                       l=quantile(hpbl,0.25,na.rm=T),
                                       u=quantile(hpbl,0.75,na.rm=T))

dataset%>%group_by(Within)%>%summarise(m=mean(vel,na.rm=T),
                                       l=quantile(vel,0.25,na.rm=T),
                                       u=quantile(vel,0.75,na.rm=T))

dataset%>%group_by(Within)%>%summarise(m=mean(gross_beta),l=quantile(gross_beta,0.25),u=quantile(gross_beta,0.75))



