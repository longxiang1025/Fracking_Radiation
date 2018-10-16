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
#' * Long-term exposure to low-level radon is dangerous. This has been proved in multiple studies.
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
library(ggplot2)
options(dplyr.print_max = 1e9)
#' In this report, we first set the radius as 25km. If there's any oil/gas production within
#' this circle in the study period, this RadNet monitor is categorized as within gas/oil field.
#' Otherwise, this RadNet monitor is categorized as clean ones.
#+ Loading the Lead-210 and gas/oil production data within 25km, message=F, echo=F,warning=F 
load(here::here("data","pb_gas_oil_25.RData"))
rad_all[rad_all$city_state=="MIAMI,FL",]$radon=1
rad_all$lpb<-log(rad_all$pb210)
rad_all$Oil_Num<-rad_all$H_Oil_Num+rad_all$V_Oil_Num
rad_all$Gas_Num<-rad_all$H_Gas_Num+rad_all$V_Gas_Num
rad_all$Oil_Prod<-rad_all$H_Oil_Prod+rad_all$V_Oil_Prod
rad_all$Gas_Prod<-rad_all$H_Gas_Prod+rad_all$V_Gas_Prod
summary_result<-rad_all%>%
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
            mean_beta=mean(beta,na.rm=T)
  )
#+ Select gas field dataset and oil field dataset, message=F, echo=F, fig.with=16, fig.height=9
rad_all$gas_field<-rad_all$city_state%in%unique(summary_result[summary_result$max_gas_prod>0,]$city_state)
rad_all$oil_field<-rad_all$city_state%in%unique(summary_result[summary_result$max_oil_prod>0,]$city_state)
rad_all$h_oil_field<-rad_all$city_state%in%unique(summary_result[summary_result$max_oil_h_prod>0,]$city_state)
rad_all$h_gas_field<-rad_all$city_state%in%unique(summary_result[summary_result$max_gas_h_prod>0,]$city_state)
rad_all$v_oil_field<-rad_all$city_state%in%unique(summary_result[summary_result$max_oil_v_prod>0,]$city_state)
rad_all$v_gas_field<-rad_all$city_state%in%unique(summary_result[summary_result$max_gas_v_prod>0,]$city_state)
#'# Descriptive statistic of the data
#' * Annual Pb210 is negatively related with EPA radon zone. Here we treat the ordered
#' categorical radon zone as continuous.
#+ Plot the negative correlation between EPA radon zone and Pb210, echo=F,warning=F, fig.width=16, fig.height=9
ggplot(data=rad_all,aes(x=radon,y=pb210,group=radon))+geom_boxplot()
#' * Even though oil fields has lower radon level, but they have higher Pb210 level.
#' This relation also applies to the natural gas drilling.
#+ Plot the contribution of oil field on Pb210, echo=F,fig.width=16,fig.height=9
rad_all%>%group_by(oil_field)%>%summarise(mean_radon=mean(radon))
rad_all%>%group_by(gas_field)%>%summarise(mean_radon=mean(radon))
p1<-ggplot(data=rad_all,aes(x=oil_field,y=pb210))+geom_boxplot()
p2<-ggplot(data=rad_all,aes(x=gas_field,y=pb210))+geom_boxplot()
cowplot::plot_grid(p1,p2)
#' * The oil/gas data is skewed. But the pb210 measurement is almost normal. Radon zone 
#' is almost evenly distributed.
#+ Plot the distribution of oil/gas prod and pb210, echo=F,message=F, fig.width=16, fig.height=16
h1<-ggplot(data=rad_all,aes(x=Gas_Prod))+geom_histogram()
h2<-ggplot(data=rad_all,aes(x=Oil_Prod))+geom_histogram()
h3<-ggplot(data=rad_all,aes(x=pb210))+geom_histogram()
h4<-ggplot(data=rad_all,aes(x=radon))+geom_histogram()
cowplot::plot_grid(h1,h2,h3,h4)
#'# Models
#'Mixed effects models are used in this report to model the correlation between our 
#'variable of interest and the Lead-210. Our variables of interest are always set as
#' fixed effect while random intercepts are assigned to each RadNet monitor. In addition,
#' radon zone is also set as fixed effect. To check the significance of our fixed effect,
#' a bootstrp confidence interval is calculated. In addition, a likelihood-ratio test is also
#' applied here.  
#'   
#'##Gross Oil Production
#'Oil production is the sum of monthly oil production from all wells within 25km away from the monitor.
#'Based on the summary of models and test, we can see that, without log-transformation, 
#' gross oil production is significantly correlated with the annual Lead-210. Adding oil
#' production doesn't influence the slope of radon remarkably. After log-transformation, the gross
#' oil production is weakly related with log(Pb210). The p-value is so close to cutoff that more
#' simulation is needed.
#+ Model the correlation between oil production and Lead-210,warning=F,message=F
model_basic<-lmer(pb210~radon+YEAR+(1|city_state),data=rad_all,REML=T)
fixed.effects(model_basic)
model_oil_prod<-lmer(pb210~radon+Oil_Prod+YEAR+(1|city_state),data=rad_all,REML=T)
confint(model_oil_prod,method="boot")
anova(model_basic,model_oil_prod)
fixed.effects(model_oil_prod)

model_log_basic<-lmer(lpb~radon+YEAR+(1|city_state),data=rad_all,REML=T)
model_log_oil_prod<-lmer(lpb~radon+Oil_Prod+YEAR+(1|city_state),data=rad_all,REML=T)
confint(model_log_oil_prod,method="boot")
anova(model_log_oil_prod,model_log_basic)
#' A tentative diagnostic based largely on leverage is applied here to check whether this
#' correlation is stable. Otherwise, it can be influenced by few influential measurements.
#' Based on the qqnorm and leverage plot, we can see that the residual of this model is largely
#' normally distributed except for some limit values. After removing the measurements with
#' very big leverage value, the slope of gross oil production doesn't change remarkbly, only 5%
#' of the standard deviation. The updated confidence interval after removing these measurements
#' still doesn't cover 0, meansing this correlation is stable and significant.
#+ Model diagonostic for oil production, fig.width=16, fig.height=9, echo=T,message=F,warning=F
g1<-ggplot(as.data.frame(resid(model_oil_prod)), aes(sample = resid(model_oil_prod)))+stat_qq() + stat_qq_line()
g2<-ggplot(data.frame(lev=hatvalues(model_oil_prod),pearson=residuals(model_oil_prod,type="pearson")),
           aes(x=lev,y=pearson)) +
  geom_point() +
  theme_bw()
cowplot::plot_grid(g1,g2)
levId <- which(hatvalues(model_oil_prod) >= .5)
rad_all[levId,c("pb210","radon","Oil_Prod","YEAR","city_state")]
model_oil_diag <- lmer(pb210 ~ radon + Oil_Prod + YEAR+ (1|city_state), data=rad_all[-c(levId),])
LevCD <- data.frame(effect=fixef(model_oil_prod),
                      change=(fixef(model_oil_diag) - fixef(model_oil_prod)),
                      se=sqrt(diag(vcov(model_oil_prod)))
)
rownames(LevCD) <- names(fixef(model_oil_diag))
LevCD$multiples <- abs(LevCD$change / LevCD$se)
LevCD
confint(model_oil_diag)

#'## Gross Gas Production
#'Gas production is the sum of monthly gas production from all wells within 25km away from the monitor.
#'Based on the summary of models and test, we can see that, without log-transformation, 
#' gross gas production is not significantly correlated with the annual Lead-210. After log-transformation, the gross
#' gas production is still not remarkably related with log(Pb210). Due to lack of significance, there's no need
#' to run diagnostic.
#+ Model the correlation between gas production and Lead-210,warning=F,message=F
model_gas_prod<-lmer(pb210~radon+Gas_Prod+YEAR+(1|city_state),data=rad_all,REML=T)
confint(model_gas_prod,method="boot")
anova(model_basic,model_gas_prod)

model_log_gas_prod<-lmer(lpb~radon+Gas_Prod+YEAR+(1|city_state),data=rad_all,REML=T)
confint(model_log_gas_prod,method="boot")
anova(model_log_gas_prod,model_log_basic)

#' ## Horizontal Oil Production
#'Horizontal oil production is the sum of monthly oil production from all horizontal wells within 25km away from the monitor.
#'Based on the summary of models and test, we can see that, without log-transformation, 
#' horizontal oil production is weakly correlated with the annual Lead-210. After log-transformation, the
#' horizontal oil production is not significantlyrelated with log(Pb210).
#+ Model the correlation between horizontal oil production and Lead-210,warning=F,message=F
model_h_oil_prod<-lmer(pb210~radon+H_Oil_Prod+YEAR+(1|city_state),data=rad_all,REML=T)
confint(model_h_oil_prod,method="boot")
anova(model_basic,model_h_oil_prod)
fixed.effects(model_h_oil_prod)

model_log_h_oil_prod<-lmer(lpb~radon+H_Oil_Prod+YEAR+(1|city_state),data=rad_all,REML=T)
confint(model_log_h_oil_prod,method="boot")
anova(model_log_h_oil_prod,model_log_basic)

#' ## Vertical Oil Production
#'Vertical oil production is the sum of monthly oil production from all vertical wells within 25km away from the monitor.
#'Based on the summary of models and test, we can see that, without log-transformation, 
#' vertical oil production is significantly correlated with the annual Lead-210. Adding oil
#' production influence the slope of radon and intercept remarkably. After log-transformation, the gross
#' oil production is significantly related with log(Pb210). 
#+ Model the correlation between vertical oil production and Lead-210,warning=F,message=F
model_v_oil_prod<-lmer(pb210~radon+V_Oil_Prod+YEAR+(1|city_state),data=rad_all,REML=T)
confint(model_v_oil_prod,method="boot")
anova(model_basic,model_v_oil_prod)
fixed.effects(model_v_oil_prod)

model_log_v_oil_prod<-lmer(lpb~radon+V_Oil_Prod+YEAR+(1|city_state),data=rad_all,REML=T)
confint(model_log_v_oil_prod,method="boot")
anova(model_log_v_oil_prod,model_log_basic)
#' A tentative diagnostic based largely on leverage is applied here to check whether this
#' correlation is stable. Otherwise, it can be influenced by few influential measurements.
#' Based on the qqnorm and leverage plot, we can see that the residual of this model is largely
#' normally distributed except for some limit values. After removing the measurements with
#' very big leverage value, the slope of gross oil production doesn't change remarkbly, only 5%
#' of the standard deviation. The updated confidence interval after removing these measurements
#' still doesn't cover 0, meansing this correlation is stable and significant.
#+ Model diagonostic for vertical oil production, fig.width=16, fig.height=9, echo=T,message=F,warning=F
g1<-ggplot(as.data.frame(resid(model_v_oil_prod)), aes(sample = resid(model_v_oil_prod)))+stat_qq() + stat_qq_line()
g2<-ggplot(data.frame(lev=hatvalues(model_v_oil_prod),pearson=residuals(model_v_oil_prod,type="pearson")),
           aes(x=lev,y=pearson)) +
  geom_point() +
  theme_bw()
cowplot::plot_grid(g1,g2)
levId <- which(hatvalues(model_v_oil_prod) >= .5)
rad_all[levId,c("pb210","radon","V_Oil_Prod","YEAR","city_state")]
model_v_oil_diag <- lmer(pb210 ~ radon + V_Oil_Prod + YEAR+ (1|city_state), data=rad_all[-c(levId),])
LevCD <- data.frame(effect=fixef(model_v_oil_prod),
                    change=(fixef(model_v_oil_diag) - fixef(model_v_oil_prod)),
                    se=sqrt(diag(vcov(model_v_oil_prod)))
)
rownames(LevCD) <- names(fixef(model_v_oil_diag))
LevCD$multiples <- abs(LevCD$change / LevCD$se)
LevCD
confint(model_v_oil_diag)

#' ## Horizontal Gas Production
#'Horizontal gas production is the sum of monthly gas production from all horizontal wells within 25km away from the monitor.
#'Based on the summary of models and test, we can see that, without log-transformation, 
#' horizontal gas production is weakly correlated with the annual Lead-210. After log-transformation, the
#' horizontal gas production is not significantlyrelated with log(Pb210).
#+ Model the correlation between horizontal gas production and Lead-210,warning=F,message=F
model_h_gas_prod<-lmer(pb210~radon+H_Gas_Prod+YEAR+(1|city_state),data=rad_all,REML=T)
confint(model_h_gas_prod,method="boot")
anova(model_basic,model_h_gas_prod)
fixed.effects(model_h_gas_prod)

model_log_h_gas_prod<-lmer(lpb~radon+H_Gas_Prod+YEAR+(1|city_state),data=rad_all,REML=T)
confint(model_log_h_gas_prod,method="boot")
anova(model_log_h_gas_prod,model_log_basic)

#' ## Vertical Gas Production
#'Vertical gas production is the sum of monthly gas production from all vertical wells within 25km away from the monitor.
#'Based on the summary of models and test, we can see that, without log-transformation, 
#' vertical gas production is weakly correlated with the annual Lead-210. After log-transformation, the
#' vertical gas production is not significantly related with log(Pb210).
#+ Model the correlation between vertical gas production and Lead-210,warning=F,message=F
model_v_gas_prod<-lmer(pb210~radon+V_Gas_Prod+YEAR+(1|city_state),data=rad_all,REML=T)
confint(model_v_gas_prod,method="boot")
anova(model_basic,model_v_gas_prod)
fixed.effects(model_v_gas_prod)

model_log_v_gas_prod<-lmer(lpb~radon+V_Gas_Prod+YEAR+(1|city_state),data=rad_all,REML=T)
confint(model_log_v_gas_prod,method="boot")
anova(model_log_v_gas_prod,model_log_basic)

#'# Conclusion
#'
#'Oil production especially oil production from vertical wells is significantly related
#'with local Lead-210. Natural gas production is not significantly correlated with local
#'local Lead-210. So, vertical oil drilling may significantly increase the local residents'
#'exposure to radon.
#'
#'# Tentative interperation
#'
#'Vertical wells were mostly completed before financial crisis. They're much older than the 
#'currently dorminant directional drilling. At the end of lifttime, the fraction of produced 
#'water is always higher than the new drill. Produced water (is not pumped back to the formation)
#'may serve as the medium for radon leakage.

#'# *Questions*
#'
#' *Q1: Are the models and diagnostic process 
#' valid?*
#' 
#' *Q2: Current models use all the data, but only 1/6 (25km case) of the RadNet monitors  
#' are located within oil/gas field. Do I need to model based on the "contaminated" area only? *
#' 
#' *Q3: To control for the spatial confounder, a random intercept is assigned to each RadNet monitor
#' . In additon, the EPA radon zone is also included. Is this sufficient, redundent or insufficient? 
#' Or can I use this as sensitivity analysis?*
#' 
#' *Q4: Concerning the significance part, can we state that we need to use the drilling information
#' to update the radon zone data?*
#' 
#' * Q5: The temporal confounding is only controlled for by adding the year in the model. In the context
#' of short study period, do you think this's sufficient?*


