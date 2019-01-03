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
#+ Load libraries and data, echo=F, message=F
library(dplyr)
library(lme4)
library(nlme)
library(ggplot2)
library(mgcv)
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
rad_all<-rad_all[rad_all$YEAR>2006,]
rad_all$radon<-as.numeric(as.character(rad_all$radon))


#+ Transform the variables, echo=F, message=F, warning=F
rad_all$log_beta<-log(rad_all$beta)
rad_all$btpm<-rad_all$beta/rad_all$mass
rad_all$logbtpm<-rad_all$log_beta/rad_all$mass
rad_all$radon<-as.numeric(as.character(rad_all$radon))

summary_result<-rad_all%>%
  group_by(city_state)%>%
  summarise(
    min_gas_prod=min(G_Gas_Prod,na.rm=T),
    max_gas_prod=max(G_Gas_Prod,na.rm=T),
    min_gas_h_prod=min(H_Gas_Prod,na.rm=T),
    max_gas_h_prod=max(H_Gas_Prod,na.rm=T),
    min_gas_v_prod=min(V_Gas_Prod,na.rm=T),
    max_gas_v_prod=max(V_Gas_Prod,na.rm=T),
    min_oil_prod=min(G_Oil_Prod,na.rm=T),
    max_oil_prod=max(G_Oil_Prod,na.rm=T),
    min_oil_h_prod=min(H_Oil_Prod,na.rm=T),
    max_oil_h_prod=max(H_Oil_Prod,na.rm=T),
    min_oil_v_prod=min(V_Oil_Prod,na.rm=T),
    max_oil_v_prod=max(V_Oil_Prod,na.rm=T),
    mean_beta=mean(beta,na.rm=T)
  )
rad_all$gas_field<-rad_all$city_state%in%unique(summary_result[summary_result$max_gas_prod>0,]$city_state)
rad_all$oil_field<-rad_all$city_state%in%unique(summary_result[summary_result$max_oil_prod>0,]$city_state)
rad_all$h_oil_field<-rad_all$city_state%in%unique(summary_result[summary_result$max_oil_h_prod>0,]$city_state)
rad_all$h_gas_field<-rad_all$city_state%in%unique(summary_result[summary_result$max_gas_h_prod>0,]$city_state)
rad_all$v_oil_field<-rad_all$city_state%in%unique(summary_result[summary_result$max_oil_v_prod>0,]$city_state)
rad_all$v_gas_field<-rad_all$city_state%in%unique(summary_result[summary_result$max_gas_v_prod>0,]$city_state)
rad_all<-left_join(rad_all,oil_price[,c("YEAR","MONTH","Price")])

#'# Descriptive Analysis of the data
#'Firstly, we plot the trend of monthly national average of $\beta$ radiation and monthly Brent Crude Oil price after 2006. We can see
#'they're loosely correlated. We use a GAM model to check the correlation. We need to control for the seasonal trend because it's related with both
#'oil price and $\beta$ radiation. After controlling for that, crude oil price is still significantly correlated with the national average $\beta$.
#'since domestic oil production is very sensitive to the global price 
#+ Descriptive statistics of the data, echo=F, message=F, fig.width=16, fig.height=9
ylim<-boxplot.stats(rad_all$beta)$stats[c(1, 5)]
ratio=8000
p<-ggplot(data=rad_all,aes(x=m_month))+
  geom_smooth(aes(x=m_month,y=beta,color="Beta"),formula = y ~ s(x, bs = "cs",k=10),method = gam)+
  geom_line(aes(y=Price/ratio,color="Oil Price"))+
  geom_smooth(aes(y=Price/ratio,color="Oil Price"))+
  scale_y_continuous(sec.axis = sec_axis(~.*ratio, name = "Brent Crude Oil Price [DPB]"))+
  scale_colour_manual(values = c("blue", "red"))+
  scale_x_continuous(breaks = seq(72, 203, 12),labels=2007:2017)+
  labs(y = "Beta Radiation [pCi/m3]",
              x = "Year",
              colour = "Parameter")+
  coord_cartesian(ylim=0.9*ylim)+
  theme(legend.position = c(0.8, 0.9))
p

summary(gam(beta~Price+s(MONTH,k=12),data=rad_all))

ggplot(data=rad_all,aes(x=radon,y=beta,group=radon))+geom_boxplot()
summary(lm(beta~oil_field, data=rad_all))
summary(lm(beta~gas_field, data=rad_all))


g1<-ggplot(data=rad_all,aes(x=oil_field,y=beta,group=oil_field))+geom_boxplot()
g2<-ggplot(data=rad_all,aes(x=gas_field,y=beta,group=gas_field))+geom_boxplot()
cowplot::plot_grid(g1,g2)

rad_all%>%group_by(oil_field)%>%summarise(mean_radon=mean(radon))
rad_all%>%group_by(gas_field)%>%summarise(mean_radon=mean(radon))

h1<-ggplot(data=rad_all,aes(x=G_Gas_Prod))+geom_histogram()
h2<-ggplot(data=rad_all,aes(x=G_Oil_Prod))+geom_histogram()
h3<-ggplot(data=rad_all,aes(x=beta))+geom_histogram()
h4<-ggplot(data=rad_all,aes(x=radon))+geom_histogram()
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
lm_basic<-lmer(beta~radon+Thmeans+Kmeans+MONTH+YEAR+(1|basin_region:city_state),data=rad_all,REML = T)
lm_oil_prod<-lmer(beta~radon+Thmeans+Kmeans+G_Oil_Prod+MONTH+YEAR+(1|basin_region:city_state),data=rad_all,REML = T)
confint(lm_oil_prod,parm ="G_Oil_Prod",method="boot",boot.type="perc")
anova(lm_basic,lm_oil_prod)

#' ## Horizontal Oil Production
#' 
#' Without log-transformation, horizontal oil production is not correlated with local $\beta$ level.
#' After the log-tranformation, gross oil production is not significantly correlated with the 
#' local $\beta$ level.
#+ Check the effect of horizontal oil production,message=F
lm_h_oil_prod<-lmer(beta~radon+Thmeans+Kmeans+H_Oil_Prod+MONTH+YEAR+(1|basin_region:city_state),data=rad_all,REML = T)
confint(lm_h_oil_prod,parm ="H_Oil_Prod",method="boot",boot.type="perc")
anova(lm_basic,lm_h_oil_prod)

#' ## Vertical Oil Production
#' 
#' Without log-transformation, vertical oil is significantly correlated with local $\beta$ level.
#' After the log-tranformation, gross oil production is not significantly correlated with the 
#' local $\beta$ level.
#+ Check the effect of vertical oil production,message=F
lm_v_oil_prod<-lmer(beta~radon+Thmeans+Kmeans+V_Oil_Prod+MONTH+YEAR+(1|basin_region:city_state),data=rad_all,REML = T)
confint(lm_v_oil_prod,parm ="V_Oil_Prod",method="boot",boot.type="perc")
anova(lm_basic,lm_v_oil_prod)

lm_gas_prod<-lmer(beta~radon+Thmeans+Kmeans+G_Gas_Prod+(1|MONTH)+(1|YEAR)+(1|basin_region:city_state),data=rad_all,REML = T)
confint(lm_gas_prod,parm ="G_Gas_Prod",method="boot",boot.type="perc")
anova(lm_basic,lm_gas_prod)

lm_h_gas_prod<-lmer(beta~radon+Thmeans+Kmeans+H_Gas_Prod+MONTH+YEAR+(1|basin_region:city_state),data=rad_all,REML = T)
confint(lm_h_gas_prod,parm ="H_Gas_Prod",method="boot",boot.type="perc")
anova(lm_basic,lm_h_gas_prod)

lm_v_gas_prod<-lmer(beta~radon+Thmeans+Kmeans+V_Gas_Prod+MONTH+YEAR+(1|basin_region:city_state),data=rad_all,REML = T)
confint(lm_v_gas_prod,parm ="V_Gas_Prod",method="boot",boot.type="perc")
anova(lm_basic,lm_v_gas_prod)


rad_all[is.na(rad_all$H_Dist),]$H_Dist<-10^6
rad_all[is.na(rad_all$V_Dist),]$V_Dist<-10^6
load(here::here("data","beta_gas_oil_25.RData"))

rad_all$lbeta<-log(rad_all$beta)
rad_all$city_state<-as.factor(rad_all$city_state)
rad_all$basin_region<-as.factor(rad_all$basin_region)

V_dataset<-rad_all[!is.na(rad_all$V_Dist),]
model_V_dist<-lmer(beta~radon+mass+Thmeans+Kmeans+V_Dist+(1|MONTH)+(1|YEAR)+(1|basin_region:city_state),data=V_dataset,REML = T)
model_V_basic<-lmer(beta~radon+mass+Thmeans+Kmeans+(1|MONTH)+(1|YEAR)+(1|basin_region:city_state),data=V_dataset,REML = T)
model_V_oil_prod<-lmer(beta~radon+mass+Thmeans+Kmeans+V_Dist+V_Oil_Prod+(1|MONTH)+(1|YEAR)+(1|basin_region:city_state),data=V_dataset,REML = T)
model_V_gas_prod<-lmer(beta~radon+mass+Thmeans+Kmeans+V_Dist+V_Gas_Prod+(1|MONTH)+(1|YEAR)+(1|basin_region:city_state),data=V_dataset,REML = T)
anova(model_V_basic,model_V_dist)
anova(model_V_oil_prod,model_V_dist)
anova(model_V_gas_prod,model_V_dist)
confint(model_V_gas_prod,method="boot",nsim=500)
confint(model_V_oil_prod,method="boot",nsim=500)

model_check<-model_V_oil_prod
g1<-ggplot(data=cbind.data.frame(res=resid(model_check),pred=fitted(model_check),color=model_check@frame$basin_region,name=model_check@frame$city),aes(y =res,x=pred,color=color))+
  geom_point()+geom_text(aes(label=ifelse(abs(res/pred)>1,as.character(name),'')))
g2<-ggplot(data.frame(lev=hatvalues(model_check),pearson=residuals(model_check,type="pearson"),basin=model_check@frame$basin_region,name=model_check@frame$city),aes(x=lev,y=pearson,color=basin))+
  geom_point()+geom_text(aes(label=ifelse(lev>0.035,as.character(name),'')))
cowplot::plot_grid(g1,g2)
levId <- which(hatvalues(model_check) >= .035)
V_dataset[levId,]
model_diag <-lmer( (model_check@call$formula),data=V_dataset[-levId,],REML = T)

LevCD <- data.frame(effect=fixef(model_check),
                    change=(fixef(model_diag) - fixef(model_check)),
                    se=sqrt(diag(vcov(model_check)))
)
rownames(LevCD) <- names(fixef(model_diag))
LevCD$multiples <- abs(LevCD$change / LevCD$se)
LevCD
confint(model_check,method="boot",nsim=500)

model_check<-model_V_gas_prod
g1<-ggplot(data=cbind.data.frame(res=resid(model_check),pred=fitted(model_check),color=model_check@frame$basin_region,name=model_check@frame$city),aes(y =res,x=pred,color=color))+
  geom_point()+geom_text(aes(label=ifelse(abs(res/pred)>1,as.character(name),'')))
g2<-ggplot(data.frame(lev=hatvalues(model_check),pearson=residuals(model_check,type="pearson"),basin=model_check@frame$basin_region,name=model_check@frame$city),aes(x=lev,y=pearson,color=basin))+
  geom_point()+geom_text(aes(label=ifelse(lev>0.035,as.character(name),'')))
cowplot::plot_grid(g1,g2)
levId <- which(hatvalues(model_check) >= .035)
rad_all[levId,]
model_diag <-lmer( (model_check@call$formula),data=V_dataset[-levId,],REML = T)

LevCD <- data.frame(effect=fixef(model_check),
                    change=(fixef(model_diag) - fixef(model_check)),
                    se=sqrt(diag(vcov(model_check)))
)
rownames(LevCD) <- names(fixef(model_diag))
LevCD$multiples <- abs(LevCD$change / LevCD$se)
LevCD
confint(model_diag,method="boot",nsim=500)


H_dataset<-rad_all[!is.na(rad_all$H_Dist),]
model_H_dist<-lmer(beta~radon+mass+Thmeans+Kmeans+H_Dist+(1|MONTH)+(1|YEAR)+(1|basin_region:city_state),data=H_dataset,REML = T)
model_H_basic<-lmer(beta~radon+mass+Thmeans+Kmeans+(1|MONTH)+(1|YEAR)+(1|basin_region:city_state),data=H_dataset,REML = T)
model_H_oil_prod<-lmer(beta~radon+mass+Thmeans+Kmeans+H_Dist+H_Oil_Prod+(1|MONTH)+(1|YEAR)+(1|basin_region:city_state),data=H_dataset,REML = T)
model_H_gas_prod<-lmer(beta~radon+mass+Thmeans+Kmeans+H_Dist+H_Gas_Prod+(1|MONTH)+(1|YEAR)+(1|basin_region:city_state),data=H_dataset,REML = T)
anova(model_H_basic,model_H_dist)
anova(model_H_oil_prod,model_H_dist)
anova(model_H_gas_prod,model_H_dist)
confint(model_H_gas_prod,method="boot",nsim=500)
confint(model_H_oil_prod,method="boot",nsim=500)






plot(gam(beta~radon+Thmeans+mass+V_Dist+s(V_Gas_Prod)+s(MONTH)+s(YEAR,k=10)+s(city_state,bs="re"),data=rad_all[!is.na(rad_all$V_Dist),]))


ggplot(data=rad_all[rad_all$city_state=="FORT WORTH,TX",])+
  geom_point(aes(x=V_Dist,y=beta))+geom_smooth(aes(x=V_Dist,y=beta),method=gam,color="blue")+
  geom_point(aes(x=H_Dist,y=beta))+geom_smooth(aes(x=H_Dist,y=beta),method=gam,color="red")

