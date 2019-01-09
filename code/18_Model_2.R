library(dplyr)
library(lme4)
library(nlme)
library(pbkrtest)
library(influence.ME)
#random number from 0 to 151, use (1+as.int(sim/12))*25 as radius, use 1+sim%%38 as indicator of metric
sim<-as.numeric(Sys.getenv("Sim"))
radius<- (1+as.integer(sim/12))*25
j<- 1+sim%%12
options(dplyr.print_max = 1e9)
#' In this report, we first set the radius as 25km. If there's any oil/gas production within
#' this circle in the study period, this RadNet monitor is categorized as within gas/oil field.
#' Otherwise, this RadNet monitor is categorized as clean ones.
#+ Loading the Lead-210 and gas/oil production data within 25km, message=F, echo=F,warning=F 
load(here::here("data",paste0("beta_gas_oil_avg_wind_",radius,".RData")))
rad_all<-rad_all%>%filter(city_state!="EUREKA,CA")
rad_all<-rad_all%>%filter(city_state!="LUBBOCK,TX")
rad_all<-rad_all%>%filter(!is.na(mass))
rad_all$basin<-as.factor(rad_all$basin)
rad_all$log_pb<-log(rad_all$pb210)
rad_all$Oil_Field<-(rad_all$G_Oil_Num>0)
rad_all$Gas_Field<-(rad_all$G_Gas_Num>0)
rad_all$Play<-rad_all$Oil_Field|rad_all$Gas_Field
rad_all[,grep("Prod",names(rad_all))]=rad_all[,grep("Prod",names(rad_all))]/1e6
rad_all[,grep("Num",names(rad_all))]=rad_all[,grep("Num",names(rad_all))]/1e3
rad_all$G_Dist<-min(rad_all$H_Dist,rad_all$V_Dist)
#Current unit is aCi/L
rad_all$beta<-rad_all$beta*1000
rad_all$lbeta<-log(rad_all$beta)
test_data<-rad_all%>%group_by(city_state)%>%summarise(n=length(city_state))
summary(test_data$n)
test_data<-test_data%>%filter(n>70)
test_data<-rad_all%>%filter(city_state%in%test_data$city_state)
output<-matrix(0,nrow=1,ncol=13)
output<-as.data.frame(output)
variables<-names(test_data)[c(7:36,38:45,91)]
vars<-variables[grep("H_",variables)]
vars<- substr(vars,3,nchar(vars))
vars<-vars[1:12]

var=vars[j]
for(t in grep(var,names(test_data))){
  test_data[is.na(test_data[t]),t]=0 
}
output$metric<-var
output$radius<-radius
#Number of variables in the basic model
numbers=4
B=1000
#basic Model
basic_formula="lbeta~mass+vel+hpbl+MONTH+MONTH^2+(1|city_state)+(1|YEAR)"
m_basic<-lmer(as.formula(basic_formula),data=test_data,REML=F)
#gross Model
gross_formula<-paste0(basic_formula,"+G_",var)
m_gross<-lmer(as.formula(gross_formula),data=test_data,REML=F)
#Drilling Type Model
type_formula<-paste0(basic_formula,"+V_",var,"+H_",var,"+H_",var,":","V_",var)
m_type<-lmer(as.formula(type_formula),data=test_data,REML=F)

beta.hat=fixef(m_gross)
beta.hat
#slope of the gross metric
p1<-beta.hat[numbers+2]
p1
se=sqrt(diag(vcov(m_gross)))
se
Tstar=rep(0,B)
bench=abs(beta.hat[numbers+2]/se[numbers+2])
for(b in 1:B){
  ystar=drop(simulate(m_basic))
  sim_data<-test_data
  sim_data$lbeta<-ystar$sim_1
  clusters<-unique(m_basic@frame$city_state)
  city_list<-sample(clusters,length(clusters),replace = T)
  city_list<-as.data.frame(city_list)
  names(city_list)<-"city_state"
  sim_data<-left_join(city_list,sim_data,by="city_state")
  ostar=lmer(as.formula(gross_formula),data=sim_data)
  Tstar[b]=abs(fixef(ostar)[numbers+2]/sqrt(vcov(ostar)[numbers+2,numbers+2]))
}
#p-value of the gross metric
p2<-mean(Tstar>=bench)
p2
Tstar=rep(0,B)
for(b in 1:B){
  ystar=drop(simulate(m_gross))
  sim_data<-test_data
  sim_data$lbeta<-ystar$sim_1
  clusters<-unique(m_basic@frame$city_state)
  city_list<-sample(clusters,length(clusters),replace = T)
  city_list<-as.data.frame(city_list)
  names(city_list)<-"city_state"
  sim_data<-left_join(city_list,sim_data,by="city_state")
  ostar=lmer(as.formula(gross_formula),data=sim_data)
  Tstar[b]=(fixef(ostar)[numbers+2]-p1)/sqrt(vcov(ostar)[numbers+2,numbers+2])
}
tquant=quantile(Tstar,c(.025,.975))
tquant
#confidence interval of the gross metric
p3<-p1+tquant[1]*se[numbers+2]
p4<-p1+tquant[2]*se[numbers+2]

#bootstrap of the drilling type model
beta.hat<-fixed.effects(m_type)
se=sqrt(diag(vcov(m_type)))

tstar=matrix(0,ncol=3,nrow=B)
for(b in 1:B){
  ystar=drop(simulate(m_type))
  sim_data=test_data
  sim_data$lbeta<-ystar$sim_1
  clusters<-unique(m_basic@frame$city_state)
  city_list<-sample(clusters,length(clusters),replace = T)
  city_list<-as.data.frame(city_list)
  names(city_list)<-"city_state"
  sim_data<-left_join(city_list,sim_data,by="city_state")
  ostar=lmer(as.formula(m_type),data=sim_data)
  tstar[b,]=c((fixef(ostar)[numbers+2]-beta.hat[numbers+2])/sqrt(vcov(ostar)[numbers+2,numbers+2]),
             (fixef(ostar)[numbers+3]-beta.hat[numbers+3])/sqrt(vcov(ostar)[numbers+3,numbers+3]),
             (fixef(ostar)[numbers+4]-beta.hat[numbers+4])/sqrt(vcov(ostar)[numbers+4,numbers+4]))
}

#coefficient of the vertical metric
p5=beta.hat[numbers+2]
tquant=quantile(tstar[,1],c(.025,.975))
tquant
#confidence interval of the vertical metric
p6=beta.hat[numbers+2]+tquant[1]*se[numbers+2]
p7=beta.hat[numbers+2]+tquant[2]*se[numbers+2]

#coefficient of the horizontal metric
p8=beta.hat[numbers+3]
tquant=quantile(tstar[,2],c(.025,.975))
tquant
#confidence interval of the horizontal metric
p9=beta.hat[numbers+3]+tquant[1]*se[numbers+3]
p10=beta.hat[numbers+3]+tquant[2]*se[numbers+3]

#coefficient of the interaction term
p11=beta.hat[numbers+4]
tquant=quantile(tstar[,3],c(.025,.975))
tquant
#confidence interval of the interaction term
p12=beta.hat[numbers+4]+tquant[1]*se[numbers+4]
p13=beta.hat[numbers+4]+tquant[2]*se[numbers+4]


slopes<-matrix(0,ncol=3,length(clusters))
for(i in 1:length(clusters)){
  m<-exclude.influence(m_type,"city_state",as.character(clusters[i]))
  slopes[i,]=fixef(m)[(numbers+2):(numbers+4)]
}
slopes<-as.data.frame(slopes)
names(slopes)<-names(fixef(m)[(numbers+2):(numbers+4)])
slopes$city_state=clusters
#Spatial sensitivity up
#p5<-min(slopes)
#p6<-max(slopes)

#t_clusters<-unique(rad_all$YEAR)
#t_slopes<-rep(0,length(t_clusters))
#for(i in 1:length(t_clusters)){
#  m<-exclude.influence(m_test,"YEAR",t_clusters[i])
#  t_slopes[i]=fixef(m)[numbers+1]
#}
#Temporal sensitivity
#p7<-min(t_slopes)
#p8<-max(t_slopes)

output[1,1:13]<-c(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13)
names(output)[1:13]<-c("slope_gross","p","u_g_ci","b_p_ci","slope_v","u_v_ci","b_v_ci","slope_h","u_h_ci","b_h_ci","inter","u_inter","b_inter")

save(file=here::here("result",paste0("Simu_Result_",var,"_",radius,".RData")),output,slopes)
