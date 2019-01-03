#' ---
#' title: "Model the relation between oil/gas production and beta radiation"
#' author: "Longxiang Li"
#' date: "`r format(Sys.Date())`"
#' output: 
#'      pdf_document:
#'       toc: true
#'       fig_caption: yes
#' ---
#' 
#' # Introduction
#' In this report, we'll try to explore the correlation between $\beta$ with oil/gas drilling.
#' The particulate radioactivity (proxied by gross-beta radiation of particulate matters) has 
#' been investigated in Maggie's paper. It has the potential adverse effects on both systolic
#' and diastolic blood pressure. Mutliple ongoing studies in our lab also suggest its health effects 
#' 
#' # Background
#' 
#' * Thanks to the breakthrough of drilling techniques (directional drilling and hydraulic
#' fracking), the production cost of domestic oil has dropped remarkably in the past decade.
#' This makes drilling profitable at lower oil price, thus booming this industry. By Sep,2018,
#' U.S has passed Saudi Arabia and Russia as the top crude oil producer.
#' 
#' * These technique have existed for decades and were applied widely on off-shore drilling where
#' cost of pad construction is high. Directional drilling enables harvesting hydrocarbon from
#' formations unaccessible by vertical drilling. Fracking improve the absorption area thus has 
#' higher productivity.
#' 
#' * Both conventional and unconventional drilling may influence the local environment and public
#' health through multiple channels. Possible route includes: methane leakage from wellhead,
#' aquifer contaminated by fracking fluid, air pollutant emitted by drilling/production activities,
#' radioactive waste from drilling/production activities.
#' 
#' * Radon is a ubiquitous noble radioactive gas. It's the progeny of Thoronim-232 and 
#' Uranium-238. Radon represents a far smaller risk for lung cancer than cigarette smoking ,
#' but it is the second leading cause of lung cancer in the United States. Radon decays quickly,
#' giving off tiny radioactive particles. When inhaled, these radioactive particles can damage
#' the cells that line the lung. Long-term exposure to radon can lead to lung cancer, the only
#' cancer proven to be associated with inhaling radon.
#' 
#' * Currently, most studies about fracking-related radioactiviy focus on the 
#' radon level in drinking water. A small number of studies correlated indoor radon level with
#' nearby fracking activities in Marcellus Shale. This study will be the first one to
#' correlate conventional/unconventional drilling with radon. Since ambient radon is not measured
#' directly onsite, we use the gross $\beta$ of particles as a proxy to the ambient radon.   
#' 
#' # Data
#' 
#' Study period: from 2007 to 2017.
#' 
#' Study Extent: lower 48 states. 
#' 
#' $\beta$ radiation data: To measure the background radiation level, 130+ RadNet monitors were
#' constructed across the U.S. In every RadNet monitor, gross $\beta$ radiation from the 
#' particles on the filter is measured every 5 days. We downloaded this data directly from EnvrionFacts
#' run by EPA. To match the frequency of well production data, this is aggregated by month.
#' 
#' Drilling data: We collected drilling information from drillinginfo.com. From the database, we extracted
#' the monthly gas/oil production during the study period, both unconventional and conventional drillings are included. 
#' 
#' Background radon data: We downloaded the EPA radon map data and join it to the assumed location of RadNet monitor. All counties of the U,S are categorized
#' into three classes ranging from 1 with the highest radon level and 3 with the lowest. The background radon level is calculated based on soil type, weather and 
#' other information. 
#' 
#' Uranium-238 and Thorium-232 map: U.S. Geological Survey used aerial gamma-ray data to quantify and describe the radioactivity of surface rocks and soils.
#' Equivalent uranium (eU) is calculated from the counts received by the gamma-ray detector in the energy window. The same technique is also used for thorium.
#' These two background map helps us control for the large-scale spatial trend.

#+ Load libraries and data, echo=F, message=F
library(dplyr)
library(lme4)
library(nlme)
library(ggplot2)
library(mgcv)
library(rpostgis)
library(RPostgreSQL)
library(ggmap)
library(raster)
load(here::here("data","beta_gas_oil_avg_wind_100.RData"))
#+Data prep, echo=F,message=F
rad_all$city_state<-as.factor(rad_all$city_state)
rad_all$lbeta<-log(rad_all$beta)
rad_all$ebeta<-exp(rad_all$beta)
rad_all[is.na(rad_all$basin_region),"basin_region"]<-"Out"
rad_all$basin_region<-as.factor(rad_all$basin_region)

#' # Methods
#' 
#' ## Inverse Distance Weighted Production.
#' 
#' We assume that the contribution of conventional/unconventional drilling to local radon level is production-dependent. Possible mechanisms include:
#' open air storage of sludge, scale and produced water, low-pressure leaking in the cases, leakage during transportation and storage. In addition, the
#' contribution is negatively related with the distance due to diffusion.
#' 
#' So the contribution of a single well is:
#' $$C_{i,k}=\frac{Prod_{i}}{Dist(i,k)^{x}}$$
#' where $i$ and $k$ are index of well and RadNet monitor, and $x$ is the power for set. The larger the power, the faster the contribution decline alonr
#' distance. In this study, we calculate the first-order and second-order contribution.
#' For a RadNet monitor, the sum of these contribution within a radius is a proxy of the oil/gas production nearby.
#' $$ P_{k}=\sum_{i=1}{n}C_{i,k},Dist(i,k) < 50km$$
#' In this study, we calculate the sum contribution of oil production nearby with the unit of $\frac{bbl}{km}$ or $\frac{bbl}{km^{2}}$ and the contribution
#' of gas production nearby with tin the unit of $\frac{mcf}{km}$ or $\frac{mcf}{km^{2}}$. Then we disaggregate the contribution based on their drill type and 
#' get the contribution from conventional oil production, unconventional oil production, conventional gas production and unconventional gas production.
#' 
#' ## Generalized Additive Model
#' 
#' GAM models with random intercepts for each RadNet monitor are fit here to correlate $\beta$ radiation with local oil/gas production.
#' The basic model is:
#' $$m_{0}=Rn+Ur+Th+s(Month)+s(Year)+(1|city)$$
#' For every variable of interest,for example first order inverse distance sum of conventional oil production ,we can fit another model:
#' $$m_{1}=Rn+Ur+Th+V_Oil_Prod+s(Month)+s(Year)+(1|city)$$
#' Then we can compare two models with ANOVA, if $m_{1}$ is remarkably better than $m_{0}$, we can conclude the variable of interest is correlated with
#' $\beta$ radiation level.
#'
#'# Results
dataset<-rad_all
dataset=dataset[!is.na(dataset$mass),]
dataset=dataset[!is.na(dataset$radon),]
dataset$city_state<-as.factor(dataset$city_state)
dataset$inv_hpbl<-1/dataset$hpbl
summ<-dataset%>%group_by(city_state)%>%summarise(nbeta=length(beta),range=max(YEAR)-min(YEAR))
dataset<-left_join(dataset,summ)
dataset<-dataset%>%filter(range>7)

temp<-dataset
temp$H_Oil_Prod<-temp$H_Oil_Prod+1
temp$log_H_Oil_Prod<-log(temp$H_Oil_Prod/1000000)
l_0<-lmer(lbeta~hpbl+vel+radon+Thmeans+mass+Coast_Dist+(1|YEAR)+(1|MONTH)+(1|city_state/basin),data=temp)
l_b<-lmer(lbeta~hpbl+vel+radon+Thmeans+mass+Coast_Dist+(1|YEAR)+(1|MONTH)+(1|city_state/basin)+H_Oil_Prod,data=temp)
summary(l_b)
anova(l_0,l_b)

temp<-dataset
temp$V_Oil_Prod<-temp$V_Oil_Prod+1
temp$log_V_Oil_Prod<-log(temp$V_Oil_Prod/1000000)
l_0<-lmer(beta~hpbl+vel+radon+Thmeans+mass+Coast_Dist+(1|YEAR)+(1|MONTH)+
            (1|city_state/basin),data=temp)
l_b<-lmer(beta~hpbl+vel+radon+Thmeans+mass+Coast_Dist+(1|YEAR)+(1|MONTH)+
            (1|city_state/basin)+V_Oil_Prod,data=temp)
summary(l_b)
anova(l_0,l_b)

temp<-dataset
temp$H_Gas_Prod<-temp$H_Gas_Prod+1
temp$log_H_Gas_Prod<-log(temp$H_Gas_Prod/1000000)
l_0<-lmer(beta~hpbl+vel+radon+Thmeans+mass+Coast_Dist+(1|YEAR)+(1|MONTH)+(1|city_state/basin),data=temp)
l_b<-lmer(beta~hpbl+vel+radon+Thmeans+mass+Coast_Dist+(1|YEAR)+(1|MONTH)+(1|city_state/basin)+log_H_Gas_Prod,data=temp)
summary(l_b)
anova(l_0,l_b)

temp<-dataset
temp$V_Gas_Prod<-temp$V_Gas_Prod+1
temp$log_V_Gas_Prod<-log(temp$V_Gas_Prod/1000000)
l_0<-lmer(beta~hpbl+vel+radon+Thmeans+mass+Coast_Dist+(1|YEAR)+(1|MONTH)+(1|city_state/basin),data=temp)
l_b<-lmer(beta~hpbl+vel+radon+Thmeans+mass+Coast_Dist+(1|YEAR)+(1|MONTH)+(1|city_state/basin)+log_V_Gas_Prod,data=temp)
summary(l_b)
anova(l_0,l_b)


l_b<-lmer(beta~hpbl+vel+radon+Thmeans+mass+Coast_Dist+(1|YEAR)+(1|MONTH)+(1|city_state/basin)+V_Gas_Num,data=temp)
summary(l_b)

l_b<-lmer(beta~hpbl+vel+radon+Thmeans+mass+Coast_Dist+(1|YEAR)+(1|MONTH)+(1|city_state/basin)+H_Gas_Num,data=temp)
summary(l_b)

l_b<-lmer(beta~hpbl+vel+radon+Thmeans+mass+Coast_Dist+(1|YEAR)+(1|MONTH)+(1|city_state/basin)+V_Oil_Num,data=temp)
summary(l_b)

l_b<-lmer(beta~hpbl+vel+radon+Thmeans+mass+Coast_Dist+(1|YEAR)+(1|MONTH)+(1|city_state/basin)+H_Oil_Num,data=temp)
summary(l_b)














dataset$city_state<-as.character(dataset$city_state)
slope_summary<-matrix(nrow=length(unique(dataset$city_state)),ncol=2)
slope_summary<-as.data.frame(slope_summary)
names(slope_summary)<-c("city_state","slope")

slope_summary$city_state<-unique(dataset$city_state)
for(city in unique(dataset$city_state)){
  temp<-dataset[dataset$city_state==city,]
  g<-gam(data=temp,lbeta~inv_hpbl+vel+mass+V_Gas_Prod)
  est<-coef(g)["V_Gas_Prod"]
  slope_summary[slope_summary$city_state==city,]$slope=est
  print(city)
}
hist(slope_summary[slope_summary$slope!=0,]$slope)

leave_out_slope<-matrix(nrow=length(unique(dataset$city_state)),ncol=2)
leave_out_slope<-as.data.frame(leave_out_slope)
names(leave_out_slope)<-c("city_state","slope")
leave_out_slope$city_state<-unique(dataset$city_state)
for(city in unique(dataset$city_state)){
  temp<-dataset[dataset$city_state!=city,]
  l<-lmer(lbeta~hpbl+vel+radon+Thmeans+mass+Coast_Dist+(1|YEAR)+(1|MONTH)+(1|city_state)+V_Gas_Prod,data=temp)
  est<-fixed.effects(l)["V_Gas_Prod"]
  leave_out_slope[leave_out_slope$city_state==city,]$slope=est
  print(city)
}

ggplot(data=dataset,aes(x=V_Gas_Prod,y=beta,color=city_state))+geom_point()
ggplot(data=dataset[dataset$state=="TX",],aes(x=H_Oil_Prod,y=beta,color=city_state))+geom_point()
