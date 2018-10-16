#' ---
#' title: "Model the relation between oil/gas production and monthly beta measurement"
#' author: "Longxiang Li"
#' date: "`r format(Sys.Date())`"
#' output: 
#'      pdf_document:
#'       toc: true
#'       fig_caption: yes
#' ---
#' #Introduction
#' The primary goal of this study is to explore the correlation between monthly mean $\beta$
#' and the monthly production of natural gas and oil within 25km of the RadNet monitor.
#+ Load libraries and data, echo=F, message=F
library(dplyr)
library(lme4)
library(nlme)
library(ggplot2)
load(here::here("data","beta_gas_oil_25.RData"))
#' # Background
#' 
#' * $\beta$ level can also be used as a proxy to particulate radioactivity in Maggie's NAS paper.
#' 
#' * The relation between $\beta$ and radon is not as direct as that between Pb210 and radon.
#' 
#' * We want to use $\beta$ as another proxy to prove the influence of oil/gas production on local radon.
#' 
#' * Since we have monthly mean $\beta$ measurment (original frequency is every 5-7 days), we have more data
#' and power to detect the correlation.
#' 
#'# Data 
#'
#' ##Study Period: from 2010 to 2016:
#' 
#'  In this way, we can track the trend of oil/gas
#'production influenced by the viberation of price.
#'
#' ##Study Region: 
#' 
#' Lower 48 states of the U.S
#' 
#' ## Data Sources: 
#' 
#' ### Oil/Gas production data:
#' 
#' We colleced drilling information from drillinginfo.com. From the database, we extracted the monthly gas/oil production during the study 
#' period, both horizontal and vertical drillings are included. 
#' Then we have monthly gas/oil production, number of active gas/oil wells, categorical and uncategorical within a radius of 25km from the assumed location of RadNet monitors.
#' 
#' ### $\beta$ radiation data:
#' 
#' We got $\beta$ radiation from 139 RadNet monitors every 5-7 days. To match the 
#' frequency of oil/gas production data, we calculated the monthly mean.
#' 
#' 
#' ### USGS radiation potential map:
#' 
#' We also included USGS aeroradiometric data (including Potassium, Thorium and Uranium). 
#' These information can be applied as a proxy to the cosmic radiation source.
#' 
#' ### EPA radon zone map:
#' 
#' We downloaded the EPA radon map data and join it to the assumed location of RadNet monitor. All counties of the U,S are catergorized
#' into three classes ranging from 1 with the highest radon level and 3 with the lowest. The background radon level is calculated based on soil type, weather and 
#' other information. 
#' 
#+ Data preparation, echo=F, message=F, warning=F
rad_qs_zones<-rad_qs[rad_qs$YEAR>2010,]
rad_qs_zones<-rad_qs_zones[!is.na(rad_qs_zones$mass),]
rad_qs_zones$radon<-as.numeric(as.character(rad_qs_zones$radon))

summary_result<-rad_qs_zones%>%
  group_by(city_state)%>%
  summarise(min_gas_num=min(Gas_Num,na.rm = T),
            max_gas_num=max(Gas_Num,na.rm = T),
            min_gas_prod=min(Gas_Prod,na.rm=T),
            max_gas_prod=max(Gas_Prod,na.rm=T),
            min_gas_h_prod=min(H_Gas_Prod,na.rm=T),
            max_gas_h_prod=max(H_Gas_Prod,na.rm=T),
            min_gas_v_prod=min(V_Gas_Prod,na.rm=T),
            max_gas_v_prod=max(V_Gas_Prod,na.rm=T),
            min_gas_h_num=min(H_Gas_Num,na.rm = T),
            max_gas_h_num=max(H_Gas_Num,na.rm = T),
            min_gas_v_num=min(V_Gas_Num,na.rm = T),
            max_gas_v_num=max(V_Gas_Num,na.rm = T),
            min_oil_num=min(Oil_Num,na.rm = T),
            max_oil_num=max(Oil_Num,na.rm = T),
            min_oil_prod=min(Oil_Prod,na.rm=T),
            max_oil_prod=max(Oil_Prod,na.rm=T),
            min_oil_h_prod=min(H_Oil_Prod,na.rm=T),
            max_oil_h_prod=max(H_Oil_Prod,na.rm=T),
            min_oil_v_prod=min(V_Oil_Prod,na.rm=T),
            max_oil_v_prod=max(V_Oil_Prod,na.rm=T),
            min_oil_h_num=min(H_Oil_Num,na.rm = T),
            max_oil_h_num=max(H_Oil_Num,na.rm = T),
            min_oil_v_num=min(V_Oil_Num,na.rm = T),
            max_oil_v_num=max(V_Oil_Num,na.rm = T),
            min_num=min(Oil_Num,na.rm = T),
            max_num=max(Oil_Num,na.rm = T),
            mean_beta=mean(beta,na.rm=T),
            n_lead_210=length(Lead_210[!is.na(Lead_210)]),
            lead_range=paste(range(Date[!is.na(Lead_210)]),collapse = "~"),
            n_beta=length(beta),
            beta_range=paste(range(Date[!is.na(beta)]),collapse = "~"),
            gas_range=paste(range(Date[Gas_Num>0]),collapse = "~"),
            oil_range=paste(range(Date[Gas_Num>0]),collapse = "~")
  )

#+ Transform the variables, echo=F, message=F, warning=F
rad_qs_zones$log_beta<-log(rad_qs_zones$beta)
rad_qs_zones$btpm<-rad_qs_zones$beta/rad_qs_zones$mass
rad_qs_zones$logbtpm<-rad_qs_zones$log_beta/rad_qs_zones$mass
rad_qs_zones$radon<-as.numeric(as.character(rad_qs_zones$radon))
rad_qs_zones$gas_field<-rad_qs_zones$city_state%in%unique(summary_result[summary_result$max_gas_prod>0,]$city_state)
rad_qs_zones$oil_field<-rad_qs_zones$city_state%in%unique(summary_result[summary_result$max_oil_prod>0,]$city_state)

#'# Descriptive Analysis of the data
#' From the boxplot of $\beta$ against radon zones, we can find that $\beta$ is also
#' negatively related with radon zones. This's similar with Pb-210, but the variation of 
#' $\beta$ is wilder.
#' 
#' From the boxplotof $\beta$ against oil/gas field, we can find that $\beta$ in oil/gas
#' filed is higher than that non-field areas.
#' 
#' And similar with what we found in the Pb-210 dataset, the radon level of gas/oil field
#' is always lower than that of non-field areas.
#' 
#' So, we can guess that the production of oil/gas may change the local relation between
#' the EPA radon zone estimate and the $\beta$ (a proxy to the real radon level).
#' 
#+ Descriptive statistics of the data, echo=F, message=F, fig.width=16, fig.height=9
ggplot(data=rad_qs_zones,aes(x=radon,y=beta,group=radon))+geom_boxplot()
summary(lm(beta~oil_field, data=rad_qs_zones))
summary(lm(beta~gas_field, data=rad_qs_zones))


g1<-ggplot(data=rad_qs_zones,aes(x=oil_field,y=beta,group=oil_field))+geom_boxplot()
g2<-ggplot(data=rad_qs_zones,aes(x=gas_field,y=beta,group=gas_field))+geom_boxplot()
cowplot::plot_grid(g1,g2)
summary(lm(beta~oil_field,data=rad_qs_zones))

rad_qs_zones%>%group_by(oil_field)%>%summarise(mean_radon=mean(radon))
rad_qs_zones%>%group_by(gas_field)%>%summarise(mean_radon=mean(radon))

h1<-ggplot(data=rad_qs_zones,aes(x=Gas_Prod))+geom_histogram()
h2<-ggplot(data=rad_qs_zones,aes(x=Oil_Prod))+geom_histogram()
h3<-ggplot(data=rad_qs_zones,aes(x=beta))+geom_histogram()
h4<-ggplot(data=rad_qs_zones,aes(x=radon))+geom_histogram()
cowplot::plot_grid(h1,h2,h3,h4)

#'# Models
#'Mixed effects models are used in this report to model the correlation between our 
#'variable of interest and the $\beta$. Our variables of interest are always set as
#' fixed effect while random intercepts are assigned to each RadNet monitor. In addition,
#' radon zone is also set as fixed effect. To check the significance of our fixed effect,
#' a bootstrp confidence interval is calculated. In addition, a likelihood-ratio test is also
#' applied here. 
#' 
#' ## Gross Oil Production
#' 
#' Without log-transformation, gross oil is weakly correlated with local $\beta$ level.
#' After the log-tranformation, gross oil production is not significantly correlated with the 
#' local $\beta$ level.
#+ Check the effect of gross oil production,message=F
lm_basic<-lmer(beta~radon+Thmeans+MONTH+YEAR+(1|city_state),data=rad_qs_zones,REML = T)
lm_oil_prod<-lmer(beta~radon+Thmeans+Oil_Prod+MONTH+YEAR+(1|city_state),data=rad_qs_zones,REML = T)
confint(lm_oil_prod,parm ="Oil_Prod",method="boot",boot.type="perc")
anova(lm_basic,lm_oil_prod)

lm_log_basic<-lmer(log_beta~radon+Thmeans+MONTH+YEAR+(1|city_state),data=rad_qs_zones,REML = T)
lm_log_oil_prod<-lmer(log_beta~radon+Thmeans+Oil_Prod+MONTH+YEAR+(1|city_state),data=rad_qs_zones,REML = T)
confint(lm_log_oil_prod,parm="Oil_Prod",method="boot")
anova(lm_log_basic,lm_log_oil_prod)

#' ## Horizontal Oil Production
#' 
#' Without log-transformation, horizontal oil production is not correlated with local $\beta$ level.
#' After the log-tranformation, gross oil production is not significantly correlated with the 
#' local $\beta$ level.
#+ Check the effect of horizontal oil production,message=F
lm_h_oil_prod<-lmer(beta~radon+Thmeans+H_Oil_Prod+MONTH+YEAR+(1|city_state),data=rad_qs_zones,REML = T)
confint(lm_h_oil_prod,parm ="H_Oil_Prod",method="boot",boot.type="perc")
anova(lm_basic,lm_h_oil_prod)

lm_log_basic<-lmer(log_beta~radon+Thmeans+MONTH+YEAR+(1|city_state),data=rad_qs_zones,REML = T)
lm_log_oil_prod<-lmer(log_beta~radon+Thmeans+Oil_Prod+MONTH+YEAR+(1|city_state),data=rad_qs_zones,REML = T)
confint(lm_log_oil_prod,parm="Oil_Prod",method="boot")
anova(lm_log_basic,lm_log_oil_prod)

#' ## Vertical Oil Production
#' 
#' Without log-transformation, vertical oil is significantly correlated with local $\beta$ level.
#' After the log-tranformation, gross oil production is not significantly correlated with the 
#' local $\beta$ level.
#+ Check the effect of vertical oil production,message=F
lm_v_oil_prod<-lmer(beta~radon+Thmeans+V_Oil_Prod+MONTH+YEAR+(1|city_state),data=rad_qs_zones,REML = T)
confint(lm_v_oil_prod,parm ="V_Oil_Prod",method="boot",boot.type="perc")
anova(lm_basic,lm_v_oil_prod)

lm_log_v_oil_prod<-lmer(log_beta~radon+Thmeans+V_Oil_Prod+MONTH+YEAR+(1|city_state),data=rad_qs_zones,REML = T)
confint(lm_log_v_oil_prod,parm="V_Oil_Prod",method="boot")
anova(lm_log_basic,lm_log_v_oil_prod)
