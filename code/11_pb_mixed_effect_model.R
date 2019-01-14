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
radius=50
load(here::here("data",paste0("pb_gas_oil_",radius,".RData")))
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
clusters<-unique(rad_cross$city_state)
sens_result<-list()
for(i in 1:length(variables)){
  var=variables[i]
  rad_cross[is.na(rad_cross[,var]),var]=0
  #gam comparison section
  small_formula="log_pb~Radon+mass+Coast_Dist+Umeans+vel+s(Lon,Lat,k=23)"
  large_gam_formula=paste0(small_formula,"+",var)
  g_0<-gam(as.formula(small_formula),data=rad_cross)
  g_m<-gam(as.formula(large_gam_formula),data=rad_cross)
  #glm comparison section
  small_formula="log_pb~Radon+mass+Coast_Dist+Umeans+vel"
  large_gls_formula=paste0(small_formula,"+",var)
  g_ls<-gls(as.formula(large_gls_formula),correlation=corGaus(form=~Lon+Lat,nugget=TRUE),data=rad_cross)
  t<-coef(g_ls)[7]/sqrt(diag(vcov(g_ls)))[7]
  #summary(g_ls)
  sar_m<-lagsarlm(as.formula(large_gls_formula),data=rad_cross,listw =  W_dist)
  t2<-coef(sar_m)[8]/sqrt(diag(vcov(sar_m)))[8]
  #summary(sar_m)
  result[i,]<-c(coef(g_m)[7],
                coef(g_m)[7]-1.96*sqrt(diag(vcov.gam(g_m)))[7],
                coef(g_m)[7]+1.96*sqrt(diag(vcov.gam(g_m)))[7],
                2*pnorm(-abs(coef(g_m)[7]/sqrt(diag(vcov.gam(g_m)))[7])),
                coef(g_ls)[7],
                coef(g_ls)[7]-1.96*sqrt(diag(vcov(g_ls)))[7],
                coef(g_ls)[7]+1.96*sqrt(diag(vcov(g_ls)))[7],
                2*pnorm(-abs(t)),
                coef(sar_m)[8],
                coef(sar_m)[8]-1.96*sqrt(diag(vcov(sar_m)))[8],
                coef(sar_m)[8]+1.96*sqrt(diag(vcov(sar_m)))[8],
                2*pnorm(-abs(t2))
  )
  cv_result<-matrix(0,nrow=length(clusters),ncol=4)
  for(j in 1:length(clusters)){
   temp_data<-rad_cross%>%filter(city_state!=clusters[j])
   gam_2<-gam(as.formula(large_gam_formula),data=temp_data)
   gls_2<-gls(as.formula(large_gls_formula),data=temp_data)
   cv_coords <- as.matrix(temp_data[,c("Lon","Lat")])
   cv_col.knn <- knearneigh(cv_coords, k=7)
   cv_W_dist<-dnearneigh(cv_coords,0,1500,longlat = T)
   cv_W_dist<-nb2listw(cv_W_dist, glist=NULL, style="W", zero.policy=NULL)
   sar_2<-lagsarlm(as.formula(large_gls_formula),data=temp_data,listw = cv_W_dist)
   cv_result[j,]<-c(coef(gam_2)[7],coef(gls_2)[7],coef(sar_2)[8],clusters[j])
  }
  cv_result<-as.data.frame(cv_result)
  names(cv_result)<-c("gam_coef","gls_coef","sar_coef","city_state")
  cv_result[,c("gam_coef")]<-as.numeric(as.character(cv_result[,c("gam_coef")]))
  cv_result[,c("gls_coef")]<-as.numeric(as.character(cv_result[,c("gls_coef")]))
  cv_result[,c("sar_coef")]<-as.numeric(as.character(cv_result[,c("sar_coef")]))
  cv_result$city_state<-as.character(cv_result$city_state)
  cv_result$metric=var
  sens_result[[i]]<-cv_result
}
result<-as.data.frame(result)
names(result)<-c("gam_coef","gam_Low","gam_Up","gam_P","gls_coef","gls_Low","gls_Up","gls_P","sar_coef","sar_Low","sar_Up","sar_P")
result$metric<-variables
sens_result<-do.call(rbind.data.frame,sens_result)
sens_range<-sens_result%>%group_by(metric)%>%summarise(max_gam=max(gam_coef),
                                                       min_gam=min(gam_coef),
                                                       max_gls=max(gls_coef),
                                                       min_gls=min(gls_coef),
                                                       max_sar=max(sar_coef),
                                                       min_sar=min(sar_coef))
result<-left_join(result,sens_range)
addtorow <- list()
addtorow$pos <- list(0, 0, 0)
addtorow$command <-c("& \\multicolumn{6}{c}{GAM}&\\multicolumn{6}{c}{GLS}&\\multicolumn{6}{c}{SAR} \\\\\n",
                     "\\hline",
                     "Metric & Slope & P-value & 2.5\\%CI & 97.5\\%CI&Sens\\_Low&Sens\\_Up& Slope & P-value & 2.5\\%CI & 97.5\\%CI&Sens\\_Low&Sens\\_Up& Slope & P-value & 2.5\\%CI & 97.5\\%CI&Sens\\_Low&Sens\\_Up \\\\\n")
table=xtable::xtable(result[,c(13,1,4,2,3,15,14,5,8,6,7,17,16,9,12,10,11,19,18)],caption=paste0("summary of three ways to adjust for spatial confounding (",radius ,"km case)"),digits=4)
xtable::align(table)<-"|ll|rrrrrr|rrrrrr|rrrrrr|"
print(table, floating = TRUE, floating.environment = "sidewaystable",include.rownames=F,scalebox = 0.85,add.to.row = addtorow,include.colnames=F,table.placement="htpb")
#(result[result$gam_P<0.05,grepl("gam",names(result))])
result%>%filter(gam_P<0.05,gls_P<0.05,sar_P<0.05)%>%select(gam_coef,gls_coef,sar_coef,metric)
save(file = here::here("result",paste0("Model_1_",radius,".RData")),result)
#print(result)


