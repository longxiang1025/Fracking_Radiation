library(dplyr)
library(mgcv)
library(lme4)
library(spdep)
library(caret)
library(xtable)
library(progress)
files<-list.files(here::here("result"))
result<-list()
sp_sens<-list()
metric<-list()
for(i in 1:length(files)){
  load(here::here("result",files[i]))
  result[[i]]<-output
  sp_sens[[i]]<-slopes
  metric[[i]]<-c(output$metric,output$radius)
}
result<-do.call(rbind,result)
metric<-do.call(rbind,metric)
metric<-as.data.frame(metric)
names(metric)<-c("metric","radius")

library(xtable)

moi="Gas_Num_Wind"

table1=xtable(result%>%select(slope_v,u_v_ci,b_v_ci,slope_h,u_h_ci,b_h_ci,inter,u_inter,b_inter,radius,metric)%>%
               filter(metric==moi,radius>25)%>%arrange((radius)),digits = c(4,4,4,4,4,4,4,4,4,4,0,0))
names(table1)=c("Slope Conv","Up CI","Low CI","Slope Unconv","Up CI","Low CI","Inter","Up CI","Low CI","R","Name")
row.names(table1)=NULL

indx<-metric[which(metric$metric==moi),]
table2<-list()
j=0
for(i in rownames(indx)){
  j=j+1
  radius<-indx[rownames(indx)==i,"radius"]
  case_slope=sp_sens[[as.numeric(i)]]
  v_range_sp_sens<-range(case_slope[,paste0("V_",moi)])
  h_range_sp_sens<-range(case_slope[,paste0("H_",moi)])
  i_range_sp_sens<-range(case_slope[,paste0("V_",moi,":","H_",moi)])
  v_u_ci=result[which(result$radius==radius&result$metric==moi),"u_v_ci"]
  v_b_ci=result[which(result$radius==radius&result$metric==moi),"b_v_ci"]
  h_u_ci=result[which(result$radius==radius&result$metric==moi),"u_h_ci"]
  h_b_ci=result[which(result$radius==radius&result$metric==moi),"b_h_ci"]
  i_u_ci<-result[which(result$radius==radius&result$metric==moi),"u_inter"]
  i_b_ci<-result[which(result$radius==radius&result$metric==moi),"b_inter"]
  case_table<-matrix(0,ncol=4,nrow=3)
  case_table[,1]=c(v_range_sp_sens[2],h_range_sp_sens[2],i_range_sp_sens[2])
  case_table[,2]=c(v_range_sp_sens[1],h_range_sp_sens[1],i_range_sp_sens[1])
  case_table[,3]=c(v_u_ci,h_u_ci,i_u_ci)
  case_table[,4]=c(v_b_ci,h_b_ci,i_b_ci)
  case_table<-as.data.frame(case_table)
  #names(case_table)<-paste0(radius,c("_max","_min","_u_ci","_b_ci"))
  names(case_table)<-c("max","min","u_ci","b_ci")
  rownames(case_table)<-paste0(c("V_","U_","I_"),moi)
  table2[[j]]=case_table
}
table2<-cbind.data.frame(table2[[3]],table2[[4]],table2[[1]])
addtorow <- list()
addtorow$pos <- list(0, 0)
addtorow$command <-c("& \\multicolumn{4}{c}{50km}&\\multicolumn{4}{c}{75km}&\\multicolumn{4}{c}{100km} \\\\\n",
                     "Metric & Max & Min & 2.5Ci & 97.5Ci& Max & Min & 2.5Ci & 97.5Ci& Max & Min & 2.5Ci & 97.5Ci \\\\\n")
table2<-xtable(table2,digits = 4)
print(table1,include.rownames=F)
print(table2,add.to.row = addtorow,include.colnames=F)
#################################################
#cross-sectional model summary table
results<-list()
pb <- progress_bar$new(total = 7*36)
f=0
for(r in c(50,75,100,125,150,175,200)){
  f=f+1
  load(here::here("data",paste0("pb_gas_oil_",r,".RData")))
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
  rad_cross$pb210<-rad_cross$pb210*1000
  rad_cross[is.na(rad_cross$mass),]$mass<-mean(rad_cross$mass,na.rm=T)
  rad_cross[is.na(rad_cross$vel),]$vel<-mean(rad_cross$vel,na.rm=T)
  rad_cross$ppm<-rad_cross$pb210/rad_cross$mass
  
  variables<-names(rad_cross)[c(9:11,13:45)]
  result<-matrix(0,ncol=12,nrow=length(variables))
  for(i in 1:length(variables)){
    pb$tick()
    var=variables[i]
    rad_cross[is.na(rad_cross[,var]),var]=0
    #lm comparison section
    small_formula="log_pb~Radon+mass+Coast_Dist+Umeans+vel"
    large_formula=paste0(small_formula,"+",var)
    lm_0<-lm(as.formula(small_formula),data=rad_cross)
    lm_m<-lm(as.formula(large_formula),data=rad_cross)
    t<-coef(lm_m)[7]/sqrt(diag(vcov(lm_m)))[7]
    result[i,1:6]<-c(coef(lm_m)[7],
                  sqrt(diag(vcov(lm_m)))[7],
                  2*pnorm(-abs(coef(lm_m)[7]/sqrt(diag(vcov(lm_m)))[7])),
                  sqrt(mean(residuals(lm_0)^2)),
                  sqrt(mean(residuals(lm_m)^2)),
                  1-var(lm_m$residuals)/var(rad_cross$log_pb)
    )
    #calculate RMSE through 10-fold cross validation
    dep_var<-rad_cross$log_pb
    ind_var<-rad_cross[,c("Radon","mass","Coast_Dist","Umeans","vel")]
    model_0 <- train(
      x=ind_var, y=dep_var,
      method = "lm",
      trControl = trainControl(
        method = "cv", number = 50
      )
    )
    ind_m_var<-rad_cross[,c("Radon","mass","Coast_Dist","Umeans","vel",var)]
    model_m<-train(
      x=ind_m_var, y=dep_var,
      method = "lm",
      trControl = trainControl(
        method = "cv", number = 50
      )
    )
    result[i,7:9]<-as.numeric(model_0$results[2:4])
    result[i,10:12]<-as.numeric(model_m$results[2:4])
  }
  result<-as.data.frame(result)
  names(result)<-c("slope","sd","p_value","RMSE_Basic","RMSE_Metric","Metric_RS",
                   "CV_Basic_RMSE","CV_Basic_RS","CV_Basic_MAE","CV_Metric_RMSE","CV_Metric_RS","CV_Metric_MAE")
  result$metric<-NA
  result[1:length(variables),]$metric<-variables
  result$radius<-r
  print(result)
  result$coef<-0
  results[[f]]=result
}
lm_data<-do.call(rbind,results)

metric_list<-unique(lm_data[lm_data$p_value<0.05,"metric"])
metric_list<-as.data.frame(metric_list)
names(metric_list)<-"metric"

variables<-as.data.frame(variables)
variables$rank=row.names(variables)
variables<-variables%>%filter(!grepl("G_",variables))

lm_data<-lm_data%>%filter(metric%in%metric_list$metric)
metric_list<-variables%>%inner_join(lm_data,by=c("variables"="metric"))

lm_min<-metric_list%>%group_by(variables)%>%summarise(which.max(Metric_RS))
names(lm_min)<-c("metric","min")
lm_min$min<-25*lm_min$min+25
ggplot(data=metric_list,aes(x=radius,y=reorder(variables,rank)))+
  geom_tile(aes(fill=Metric_RS),color="white")+
  scale_fill_distiller("Correlation",palette = "Spectral")+
  geom_tile(data=lm_min,aes(x=min,y=metric),fill=NA,color="black",size=2)+
  scale_x_continuous(breaks = c(50,75,100,125,150,175,200))+
  ylab("O&G Metric")

sar_metric<-sar_list%>%filter(metric%in%metric_list)
sar_metric<-sar_metric[,c("sar_metric_rmse","metric","radius")]
sar_metric<-variables%>%inner_join(sar_metric,by=c("variables"="metric"))
names(sar_metric)<-c("metric","rank","rmse","Radius")
sar_metric$rank<-as.numeric(sar_metric$rank)

sar_min<-sar_metric%>%group_by(metric)%>%summarise(which.min(rmse))
sar_min$`which.min(rmse)`<-25*sar_min$`which.min(rmse)`+25
names(sar_min)<-c("metric","min")
ggplot(data=sar_metric,aes(x=Radius,y=reorder(metric,rank)))+
  geom_tile(aes(fill=rmse),color="white")+scale_fill_distiller(palette = "Spectral")+
  geom_tile(data=sar_min,aes(x=min,y=metric),fill=NA,color="black",size=2)+
  scale_x_continuous(breaks = c(50,75,100,125,150,175,200))
  
table$Metric<-c("Gross Prod of Oil","Prod Dens of Oil From Unconv Drills","Prod Dens of Oil From All Wells",
                "Prod of Oil From Unconv Drills","Prod of Oil From Conv Drills","Prod of Oil From All Upwind Wells",
                "Num of Unconv Drills","Num of All Active Oil Wells","Prod Dens of Oil From Conv Drills","Number of Conv Drills")
names(table)<-c("Metric","Slope(50km)","Slope(75km)","Slope(100km)","Metric")
table_cross<-xtable(table[c(4,5,1,7,10,8,2,9,3,6),c(5,2,3,4)],digits = 4,method="compact",column.names=)
print(table_cross,include.rownames = FALSE,table.placement="htpb")

