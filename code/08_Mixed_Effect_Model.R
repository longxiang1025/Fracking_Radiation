#' ---
#' title: "Use mixed effect model to explore this longitudinal data"
#' author: "Longxiang Li"
#' date: "`r format(Sys.Date())`"
#' output: 
#'      html_document:
#'       toc: true
#'       fig_caption: yes
#' ---
#' 
#' # Introduction
#' In the past weeks, We have loaded all the data within our reach to the database and extract the data we need in our desired format.
#' Currently, we organize our information based on the RadNet monitors per month. The original measurement frequency of $\beta$ radiation is 3 days. But to
#' fit the data reporting frequency of drilling, we aggregated the beta radiation measurement (monthly average). Then we got the monthly $\beta$ data merged 
#' with gas/oil production data. This binding was based on the buffer around tha RadNet monitor. All gas/oil drilling information within this buffer was
#' aggregated (number of wells, sum production). Besides the general aggregation, we also did it by the drilling type (horizontal and vertical drilling). To 
#' find the optimal buffer radius, we created four cases: 25 km, 50 km, 75 km and 100 km.
#' 
#' These organized data files are stored in the shared dropbox folder now. In addition to the monthly $\beta$, categorized/overall production data, we
#' also 
#+ load-data, message = F, echo = F
library(tidyverse)
library(mgcv)
library(lme4)
library(lmerTest)
library(ggplot2)

#+ load the dataset of 75km
load(here::here("data","beta_gas_oil_75.RData"))

#+ rescale the production and number of wells data
rad_qs$H_Gas_Prod<-rad_qs$H_Gas_Prod/1000000
rad_qs$V_Gas_Prod<-rad_qs$V_Gas_Prod/1000000
rad_qs$Gas_Prod<-rad_qs$Gas_Prod/1000000

rad_qs$H_Gas_Num<-rad_qs$H_Gas_Num/100
rad_qs$V_Gas_Num<-rad_qs$V_Gas_Num/100
rad_qs$Gas_Num<-rad_qs$Gas_Num/100

rad_qs$H_Oil_Prod<-rad_qs$H_Oil_Prod/1000000
rad_qs$V_Oil_Prod<-rad_qs$V_Oil_Prod/1000000
rad_qs$Oil_Prod<-rad_qs$Oil_Prod/1000000

rad_qs$H_Oil_Num<-rad_qs$H_Oil_Num/100
rad_qs$V_Oil_Num<-rad_qs$V_Oil_Num/100
rad_qs$Oil_Num<-rad_qs$Oil_Num/100

#+ seperate the RadNet monitors into two classes: within/outside the basin zones
rad_qs_zones<-rad_qs[!is.na(rad_qs$basin_name),]
rad_qs_zones_a2007<-rad_qs_zones[rad_qs_zones$YEAR>2006&rad_qs_zones$YEAR<2018,]


(fm8 <- lmer(beta ~ 1 + radon+H_Oil_Num +(1|basin_name)+ (0 + H_Oil_Num|basin_name), rad_qs_zones,REML = 0))

fortworth_data<-rad_qs[rad_qs$city_state=="FORT WORTH,TX",]
fmf<-gam(beta~H_Gas_Prod+s(MONTH,k=12)+s(YEAR,k=9),data=fortworth_data)

head(ranef(fm8)[["basin_name"]])


