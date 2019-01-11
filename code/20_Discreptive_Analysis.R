library(dplyr)
library(psych)
library(xtable)
radius=75
load(here::here("data",paste0("pb_gas_oil_",radius,".RData")))
load(here::here("data",paste0("beta_gas_oil_avg_wind_",radius,".RData")))
#Massage the beta dataset
rad_all[is.na(rad_all$Radon),]$Radon=6
rad_all$basin<-as.factor(rad_all$basin)
#rad_all$log_dist<-log(rad_all$Coast_Dist)
rad_all$log_pb<-log(rad_all$pb210)
rad_all$Oil_Field<-(rad_all$G_Oil_Num>0)
rad_all$Gas_Field<-(rad_all$G_Gas_Num>0)
rad_all$Play<-rad_all$Oil_Field|rad_all$Gas_Field

#Current unit is aCi/L
rad_all$beta<-rad_all$beta*1000
rad_all$lbeta<-log(rad_all$beta)
rad_cross$pb210<-rad_cross$pb210*1000
rad_cross[is.na(rad_cross$Radon),]$Radon=6
rad_cross$basin<-as.factor(rad_cross$basin)
rad_cross$log_dist<-log(rad_cross$Coast_Dist)
rad_cross$log_pb<-log(rad_cross$pb210)

rad_cross$Oil_Field<-(rad_cross$G_Oil_Num>0)
rad_cross$Gas_Field<-(rad_cross$G_Gas_Num>0)
rad_cross$Play<-rad_cross$Oil_Field|rad_cross$Gas_Field
rad_cross[is.na(rad_cross$Play),"Play"]<-F
##################
#Descriptive Statistics
play_data<-rad_cross[rad_cross$Play,]
out_data<-rad_cross[!rad_cross$Play,]
out_data[is.na(out_data)]<-0
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
xtable(table_2,digits = c(5,2,2,2,2,2,2,2,2,2,3))
xtable(table_3,digits = c(5,2,2,2,2,2,2,2,2,2,3))
