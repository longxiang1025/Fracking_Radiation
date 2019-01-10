#' ---
#' title: "Model the relation between oil/gas production and annual Lead-210 measurement"
#' author: "Longxiang Li"
#' date: "`r format(Sys.Date())`"
#' output: 
#'      pdf_document:
#'       toc: true
#'       fig_caption: yes
#' ---
#' 
#' # Introduction
#' In this report, we'll try to explore the correlation between Lead-210 with oil/gas drilling.
#' 
#' # Background
#' 
#' * Long-term exposure to low-level radon is dangerous. Because radon is carcinogenic.
#' 
#' * Direct long-term measurement of radon is rare.
#' 
#' * Lead-210 has been used as marker for radon exposure. This is applied in multiple studies.
#' 
#' * If there's an increase of Lead-210 in the particles, local residents are exposed to higher risk of lung cancer and other disease.
#' 
#' # Data 
#' 
#' * Study period: from 2014 to 2016. Because we don't have the annual Lead-210 measuremtn
#' before 2014 and after 2016. I've reached out to RadNet for data.
#' 
#' * Study region: Lower 48 states of the United States. There're 139 RedNet monitors. But
#' only a small fraction of these monitors have gas/oil activities nearby.
#' 
#' * Data sources: 
#' 
#' Lead-210 data: Lead-210 are determined by the analysis of annually composited samples (air filters) collected from the airborne particulate
#' samplers. Concentrations are determined by alpha-particle spectrometry following chemical separation. The total volume of air represented by
#'  all the samples received from one sampling location during a year typically ranges from 120,000 $m^3$ to 500,000 $m^3$.
#' 
#' Drilling data: We colleced drilling information from drillinginfo.com. From the database, we extracted the monthly gas/oil production during the study 
#' period, both horizontal and vertical drillings are included. Since the production data is reported by month, we need to aggregate them by year.
#' Then we have annual gas/oil production, number of active gas/oil wells, categorical and uncategorical within a radius of 25km from the assumed location of RadNet monitors.
#' 
#' Background radon data: We downloaded the EPA radon map data and join it to the assumed location of RadNet monitor. All counties of the U,S are catergorized
#' into three classes ranging from 1 with the highest radon level and 3 with the lowest. The background radon level is calculated based on soil type, weather and 
#' other information. 

#+ Loading all the necessary libraries, echo=F, message=F, warnings=F
library(dplyr)
library(lme4)
library(nlme)
library(lmerTest)
library(mgcv)
library(ggplot2)
library(spdep)
library(psych)
options(dplyr.print_max = 1e9)
#' In this report, we first set the radius as 25km. If there's any oil/gas production within
#' this circle in the study period, this RadNet monitor is categorized as within gas/oil field.
#' Otherwise, this RadNet monitor is categorized as clean ones.
#+ Loading the Lead-210 and gas/oil production data within 25km, message=F, echo=F,warning=F 
load(here::here("data","pb_gas_oil_50.RData"))
##############################################
#Massage the pb-210 dataset
rad_cross[is.na(rad_cross$Radon),]$Radon=6
rad_cross$basin<-as.factor(rad_cross$basin)
rad_cross$log_dist<-log(rad_cross$Coast_Dist)
rad_cross$log_pb<-log(rad_cross$pb210)

rad_cross$Oil_Field<-(rad_cross$G_Oil_Num>0)
rad_cross$Gas_Field<-(rad_cross$G_Gas_Num>0)
rad_cross$Play<-rad_cross$Oil_Field|rad_cross$Gas_Field

rad_cross[,grep("Prod",names(rad_cross))]=rad_cross[,grep("Prod",names(rad_cross))]/1e6
rad_cross[,grep("Num",names(rad_cross))]=rad_cross[,grep("Num",names(rad_cross))]/1e3
###################################################
#Massage the beta dataset

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

##################
#Descriptive Statistics
play_data<-rad_cross[rad_cross$Play,]
out_data<-rad_cross[!rad_cross$Play,]
play_beta_data<-rad_all[rad_all$Play,]
out_beta_data<-rad_all[!rad_all$Play,]
write_row<-function(play_dat,out_dat,name)
{
  t0<-t.test((rbind(play_dat,out_dat))[[name]])
  p0<-c(t0$estimate,t0$conf.int[1:2])
  t1<-t.test(play_dat[[name]])
  p1<-c(t1$estimate,t1$conf.int[1:2])
  t2<-t.test(out_dat[[name]])
  p2<-c(t2$estimate,t2$conf.int[1:2])
  p3<-(t.test(play_dat[[name]],out_dat[[name]]))$p.value
  return(as.numeric(c(p0,p1,p2,p3)))
}

write_cor_row<-function(play_dat,out_dat,name1,name2){
  c0<-cor.test((rbind(play_dat,out_dat))[[name1]],(rbind(play_dat,out_dat))[[name2]],conf.level=0.95)
  t<-paired.r(cor(play_dat[[name1]],play_dat[[name2]],use="complete.obs"),
              cor(out_dat[[name1]],out_dat[[name2]],use="complete.obs"),
           n=nrow(play_dat), n2=nrow(out_dat),twotailed=TRUE)
  c1<-cor.test(play_dat[[name1]],play_dat[[name2]],conf.level=0.95)
  c2<-cor.test(out_dat[[name1]],out_dat[[name2]],conf.level=0.95)
  return(as.numeric(c(c0$estimate,c0$conf.int[1:2],c1$estimate,c1$conf.int[1:2],c2$estimate,c2$conf.int[1:2],t$p)))
}

pb_row<-write_row(play_data,out_data,"pb210")
pb_Rn_row<-write_row(play_data,out_data,"Radon")
pb_mass_row<-write_row(play_data,out_data,"mass")
pb_U_row<-write_row(play_data,out_data,"Umeans")
pb_vel_row<-write_row(play_data,out_data,"vel")
pb_hpbl_row<-write_row(play_data,out_data,"hpbl")
pb_cstdist_row<-write_row(play_data,out_data,"Coast_Dist")
pb_oil_row<-write_row(play_data,out_data,"G_Oil_Prod")
pb_gas_row<-write_row(play_data,out_data,"G_Gas_Prod")

c_pb_radon_row<-write_cor_row(play_data,out_data,"pb210","Radon")
c_pb_U_row<-write_cor_row(play_data,out_data,"pb210","Umeans")
c_pb_mass_row<-write_cor_row(play_data,out_data,"pb210","mass")
c_pb_vel_row<-write_cor_row(play_data,out_data,"pb210","vel")
c_pb_hpbl_row<-write_cor_row(play_data,out_data,"pb210","hpbl")
c_pb_cstdist_row<-write_cor_row(play_data,out_data,"pb210","Coast_Dist")
c_pb_oil_row<-write_cor_row(play_data,out_data,"pb210","G_Oil_Prod")
c_pb_gas_row<-write_cor_row(play_data,out_data,"pb210","G_Gas_Prod")

beta_row<-write_row(play_beta_data,out_beta_data,"beta")
beta_Rn_row<-write_row(play_beta_data,out_beta_data,"Radon")
beta_mass_row<-write_row(play_beta_data,out_beta_data,"mass")
beta_U_row<-write_row(play_beta_data,out_beta_data,"Umeans")
beta_vel_row<-write_row(play_beta_data,out_beta_data,"vel")
beta_hpbl_row<-write_row(play_beta_data,out_beta_data,"hpbl")
beta_cstdist_row<-write_row(play_beta_data,out_beta_data,"Coast_Dist")
beta_oil_row<-write_row(play_beta_data,out_beta_data,"G_Oil_Prod")
beta_gas_row<-write_row(play_beta_data,out_beta_data,"G_Gas_Prod")

c_beta_pb_row<-write_cor_row(play_beta_data,out_beta_data,"beta","pb210")
c_beta_radon_row<-write_cor_row(play_beta_data,out_beta_data,"beta","Radon")
c_beta_U_row<-write_cor_row(play_beta_data,out_beta_data,"beta","Umeans")
c_beta_mass_row<-write_cor_row(play_beta_data,out_beta_data,"beta","mass")
c_beta_vel_row<-write_cor_row(play_beta_data,out_beta_data,"beta","vel")
c_beta_hpbl_row<-write_cor_row(play_beta_data,out_beta_data,"beta","hpbl")
c_beta_cstdist_row<-write_cor_row(play_beta_data,out_beta_data,"beta","Coast_Dist")
c_beta_oil_row<-write_cor_row(play_beta_data,out_beta_data,"beta","G_Oil_Prod")
c_beta_gas_row<-write_cor_row(play_beta_data,out_beta_data,"beta","G_Gas_Prod")

table_2<-rbind.data.frame(pb_row,pb_Rn_row,pb_mass_row,pb_U_row,pb_vel_row,pb_hpbl_row,pb_cstdist_row,
                          pb_oil_row,pb_gas_row,
                          c_pb_radon_row,c_pb_mass_row,c_pb_U_row,c_pb_vel_row,c_pb_hpbl_row,c_pb_cstdist_row,
                          c_pb_oil_row,c_pb_gas_row)
names(table_2)<-c("g_mean","gross_low","g_high","p_mean","p_low","p_high","o_mean",
                  "o_low","o_high","sig_dif")
row.names(table_2)<-c("Pb-210","Radon Index","PM2.5","U-238","Wind velocity","HPBL","Dist to Coast",
                      "Gross Oil Production","Gross Gas Production",
                      "Pb-210 & Radon","Pb-210 & PM2.5","Pb-210 & U238","Pb-210 & Wind velocity","Pb-210 & HPBL","PB-210 & Dist to Coast",
                      "Pb-210 & Oil Prod","Pb-210 & Gas Prod"
                      )

table_3<-rbind.data.frame(beta_row,beta_Rn_row,beta_mass_row,beta_U_row,beta_vel_row,beta_hpbl_row,beta_cstdist_row,
                          beta_oil_row,beta_gas_row,
                          c_beta_radon_row,c_beta_mass_row,c_beta_U_row,c_beta_vel_row,c_beta_hpbl_row,c_beta_cstdist_row,
                          c_beta_oil_row,c_beta_gas_row)
names(table_3)<-c("g_mean","gross_low","g_high","p_mean","p_low","p_high","o_mean",
                  "o_low","o_high","sig_dif")
row.names(table_3)<-c("beta","Radon Index","PM2.5","U-238","Wind velocity","HPBL","Dist to Coast",
                      "Gross Oil Production","Gross Gas Production",
                      "beta & Radon","beta & PM2.5","beta & U238","beta & Wind velocity","beta & HPBL","beta & Dist to Coast",
                      "beta & Oil Prod","beta & Gas Prod"
)
library(xtable)
xtable(table_2,digits = c(5,2,2,2,2,2,2,2,2,2,3))
xtable(table_3,digits = c(5,2,2,2,2,2,2,2,2,2,3))
#################
#Models
rad_cross[is.na(rad_cross$mass),]$mass<-mean(rad_cross$mass,na.rm=T)
rad_cross[is.na(rad_cross$vel),]$vel<-mean(rad_cross$vel,na.rm=T)
rad_cross$ppm<-rad_cross$pb210/rad_cross$mass
coords <- as.matrix(rad_cross[,c("Lon","Lat")])
col.knn <- knearneigh(coords, k=7)
W_dist<-dnearneigh(coords,0,1500,longlat = T)
W_dist<-nb2listw(W_dist, glist=NULL, style="W", zero.policy=NULL)

variables<-names(rad_cross)[c(9:11,13:45)]
result<-matrix(0,ncol=12,nrow=length(variables))
for(i in 1:length(variables)){
  var=variables[i]
  rad_cross[is.na(rad_cross[,var]),var]=0
  #gam comparison section
  small_formula="log_pb~Radon+mass+Coast_Dist+Umeans+vel+s(Lon,Lat,k=23)"
  large_fromula=paste0(small_formula,"+",var)
  g0<-gam(as.formula(small_formula),data=rad_cross)
  g_m<-gam(as.formula(large_fromula),data=rad_cross)
  lrtest<-anova.gam(g_m,g_0,test="F")
  #glm comparison section
  small_formula="log_pb~Radon+mass+Coast_Dist+Umeans+vel"
  large_formula=paste0(small_formula,"+",var)
  g_ls<-gls(as.formula(large_formula),correlation=corGaus(form=~Lon+Lat,nugget=TRUE),data=rad_cross)
  t<-coef(g_ls)[7]/sqrt(diag(vcov(g_ls)))[7]
  #summary(g_ls)
  sar_m<-lagsarlm(as.formula(large_formula),data=rad_cross,listw =  W_dist)
  t2<-coef(sar_m)[8]/sqrt(diag(vcov(sar_m)))[8]
  #summary(sar_m)
  result[i,]<-c(coef(g_m)[7],
                coef(g_m)[7]-1.96*sqrt(diag(vcov.gam(g_m)))[7],
                coef(g_m)[7]+1.96*sqrt(diag(vcov.gam(g_m)))[7],
                pnorm(-abs(coef(g_m)[7]/sqrt(diag(vcov.gam(g_m)))[7])),
                coef(g_ls)[7],
                coef(g_ls)[7]-1.96*sqrt(diag(vcov(g_ls)))[7],
                coef(g_ls)[7]+1.96*sqrt(diag(vcov(g_ls)))[7],
                pnorm(-abs(t)),
                coef(sar_m)[8],
                coef(sar_m)[8]-1.96*sqrt(diag(vcov(sar_m)))[8],
                coef(sar_m)[8]+1.96*sqrt(diag(vcov(sar_m)))[8],
                pnorm(-abs(t2))
  )
}
result<-as.data.frame(result)
names(result)<-c("coef","Low","Up","P","coef","Low","Up","P","coef","Low","Up","P")
result$metric<-variables

print(result)


