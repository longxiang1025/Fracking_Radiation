library(dplyr)
library(lme4)
library(pbkrtest)
library(influence.ME)
options(dplyr.print_max = 1e9)
#' In this report, we first set the radius as 25km. If there's any oil/gas production within
#' this circle in the study period, this RadNet monitor is categorized as within gas/oil field.
#' Otherwise, this RadNet monitor is categorized as clean ones.
#+ Loading the Lead-210 and gas/oil production data within 25km, message=F, echo=F,warning=F 
load(here::here("data","beta_gas_oil_avg_wind_25.RData"))
rad_all<-rad_all%>%filter(city_state!="EUREKA,CA")
rad_all<-rad_all%>%filter(city_state!="LUBBOCK,TX")
rad_all<-rad_all%>%filter(!is.na(mass))
rad_all[is.na(rad_all$Radon),]$Radon=6
rad_all$basin<-as.factor(rad_all$basin)
#rad_all$log_dist<-log(rad_all$Coast_Dist)
rad_all$log_pb<-log(rad_all$pb210)
rad_all$Oil_Field<-(rad_all$G_Oil_Num>0)
rad_all$Gas_Field<-(rad_all$G_Gas_Num>0)
rad_all$Play<-rad_all$Oil_Field|rad_all$Gas_Field

rad_all[,grep("Prod",names(rad_all))]=rad_all[,grep("Prod",names(rad_all))]/1e6
rad_all[,grep("Num",names(rad_all))]=rad_all[,grep("Num",names(rad_all))]/1e3
#Current unit is aCi/L
rad_all$beta<-rad_all$beta*1000
rad_all$lbeta<-log(rad_all$beta)
rad_cross$pb210<-rad_cross$pb210*1000
test_data<-rad_all%>%group_by(city_state)%>%summarise(n=length(city_state))
test_data$beta_mass<-test_data$beta/test_data$mass
summary(test_data$n)
test_data<-test_data%>%filter(n>70)
test_data<-rad_all%>%filter(city_state%in%test_data$city_state)
output<-matrix(0,nrow=38,ncol=9)
output<-as.data.frame(output)
variables<-names(test_data)[c(7:36,38:45)]
names(output)<-c("metric","slope","p_value","l_ci","u_ci","#cities","max_p","l_slope","u_slope")
output$metric<-variables
for(j in 1:length(variables)){
  var=variables[j]
  #Number of variables in the larger model
  numbers=3
  small_formula="lbeta~mass+vel+(1|city_state)+(1|MONTH)+(1|YEAR)"
  m_basic<-lmer(as.formula(small_formula),data=test_data,REML=F)
  big_formula<-paste(small_formula,"+",var)
  m_test<-lmer(as.formula(big_formula),data=test_data,REML=F)
  p2<-PBmodcomp(m_test,m_basic,nsim=100)
  p2<-p2$test$p.value[2]
  if(p2<0.1){
    p1<-fixed.effects(m_test)[var]
    ci<-confint(m_test,parm=var,method="boot",nsim=250,boot.type="perc")
    infuence<-influence(m_test,group="city_state")
    cooks<-cooks.distance.estex(infuence)
    city_list<-names(cooks[cooks>0.10,])
    p3<-length(city_list)
    result<-matrix(0,nrow=length(city_list),ncol=numbers+2)
    for(i in 1:length(city_list)){
      b_model<-exclude.influence(m_basic,"city_state",city_list[i])
      c_model<-exclude.influence(m_test,"city_state",city_list[i])
      p<-PBmodcomp(c_model,b_model,nsim=100)
      p<-p$test$p.value[2]
      print(paste(p,city_list[i]))
      result[i,]=c(fixef(c_model),p)
    }
    p4<-max(result[,numbers+2])
    p5<-min(result[,numbers+1]/p1)
    p6<-max(result[,numbers+1]/p1)
    row<-c(p1,p2,ci,p3,p4,p5,p6)
    output[j,2:9]<-row
  }
  print(output[j,]) 
}