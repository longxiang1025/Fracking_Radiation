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
  coords <- as.matrix(rad_cross[,c("Lon","Lat")])
  col.knn <- knearneigh(coords, k=7)
  W_dist<-dnearneigh(coords,0,1500,longlat = T)
  W_dist<-nb2listw(W_dist, glist=NULL, style="W", zero.policy=NULL)
  
  variables<-names(rad_cross)[c(9:11,13:45)]
  result<-matrix(0,ncol=15,nrow=length(variables))
  #clusters<-unique(rad_cross$city_state)
  #sens_result<-list()
  for(i in 1:length(variables)){
    pb$tick()
    var=variables[i]
    rad_cross[is.na(rad_cross[,var]),var]=0
    #gam comparison section
    small_gam_formula="log_pb~Radon+mass+Coast_Dist+Umeans+vel+s(Lon,Lat,k=23)"
    large_gam_formula=paste0(small_gam_formula,"+",var)
    g_0<-gam(as.formula(small_gam_formula),data=rad_cross)
    g_m<-gam(as.formula(large_gam_formula),data=rad_cross)
    #glm comparison section
    small_formula="log_pb~Radon+mass+Coast_Dist+Umeans+vel"
    large_formula=paste0(small_formula,"+",var)
    g_ls_0<-gls(as.formula(small_formula),correlation=corGaus(form=~Lon+Lat,nugget=TRUE,fixed = T),data=rad_cross)
    g_ls<-gls(as.formula(large_formula),correlation=corGaus(form=~Lon+Lat,nugget=TRUE,fixed = T),data=rad_cross)
    t<-coef(g_ls)[7]/sqrt(diag(vcov(g_ls)))[7]
    #summary(g_ls)
    sar_0<-lagsarlm(as.formula(small_formula),data=rad_cross,listw =  W_dist)
    sar_m<-lagsarlm(as.formula(large_formula),data=rad_cross,listw =  W_dist)
    t2<-coef(sar_m)[8]/sqrt(diag(vcov(sar_m)))[8]
    #summary(sar_m)
    result[i,]<-c(coef(g_m)[7],
                  sqrt(diag(vcov.gam(g_m)))[7],
                  2*pnorm(-abs(coef(g_m)[7]/sqrt(diag(vcov.gam(g_m)))[7])),
                  sqrt(mean(residuals(g_0)^2)),
                  sqrt(mean(residuals(g_m)^2)),
                  coef(g_ls)[7],
                  sqrt(diag(vcov(g_ls)))[7],
                  2*pnorm(-abs(coef(g_ls)[7]/sqrt(diag(vcov(g_ls)))[7])),
                  sqrt(mean(residuals(g_ls_0)^2)),
                  sqrt(mean(residuals(g_ls)^2)),
                  coef(sar_m)[8],
                  sqrt(diag(vcov(sar_m)))[8],
                  2*pnorm(-abs(coef(sar_m)[8]/sqrt(diag(vcov(sar_m)))[8])),
                  sqrt(mean(residuals(sar_0)^2)),
                  sqrt(mean(residuals(sar_m)^2))
    )
    #calculate RMSE through leave-one-out cross validation
    #this section was abolished due to tricky result
    #cv_result<-matrix(0,nrow=length(clusters),ncol=7)
    #for(c in 1:length(clusters)){
    #  temp_data<-rad_cross%>%filter(city_state!=clusters[c])
    #  gam_2<-gam(as.formula(large_gam_formula),data=temp_data)
      
    #  gls_2<-gls(as.formula(large_formula),
    #            correlation=corGaus(form=~Lon+Lat,nugget=TRUE,fixed = T),data=temp_data)
      
    #  cv_coords <- as.matrix(temp_data[,c("Lon","Lat")])
    #  cv_col.knn <- knearneigh(cv_coords, k=7)
    #  cv_W_dist<-dnearneigh(cv_coords,0,1500,longlat = T)
    #  cv_W_dist<-nb2listw(cv_W_dist, glist=NULL, style="W", zero.policy=NULL)
    #  sar_2<-lagsarlm(as.formula(large_formula),data=temp_data,listw = cv_W_dist)
    #  if(i==1){
    #    gam_2_0<-gam(as.formula(small_gam_formula),data=temp_data)
    #    gls_2_0<-gls(as.formula(small_formula),
    #                 correlation=corGaus(form=~Lon+Lat,nugget=TRUE,fixed = T),data=temp_data)
    #    sar_2_0<-lagsarlm(as.formula(small_formula),data=temp_data,listw = cv_W_dist)
    #  }
    #  cv_result[c,1:6]<-as.numeric(c(predict(gam_2,rad_cross%>%filter(city_state==clusters[c])),
    #                                 predict(gam_2_0,rad_cross%>%filter(city_state==clusters[c])),
    #                                 predict(gls_2,rad_cross%>%filter(city_state==clusters[c])),
    #                                 predict(gls_2_0,rad_cross%>%filter(city_state==clusters[c])),
    #                                 predict(sar_2,newdata=rad_cross,listw = W_dist)[c],
    #                                 predict(sar_2_0,newdata=rad_cross,listw = W_dist)[c]))
    #}
    #cv_result<-as.data.frame(cv_result)
    #names(cv_result)<-c("gam_pred","gam_bas_pred","gls_pred","gls_bas_pred","sar_pred",
    #                    "sar_bas_pred","log_pb")
    #cv_result$log_pb<-rad_cross$log_pb
    #cv_result$metric=var
    #result[i,4]<-sqrt(mean((rad_cross$log_pb-cv_result$gam_pred)^2))
    #result[i,8]<-sqrt(mean((rad_cross$log_pb-cv_result$gls_pred)^2))
    #result[i,12]<-sqrt(mean((rad_cross$log_pb-cv_result$sar_pred)^2))
    #if(i==length(variables)){
    #  result[i+1,4]=sqrt(mean((rad_cross$log_pb-cv_result$gam_bas_pred)^2))
    #  result[i+1,8]<-sqrt(mean((rad_cross$log_pb-cv_result$gls_bas_pred)^2))
    #  result[i+1,12]<-sqrt(mean((rad_cross$log_pb-cv_result$sar_bas_pred)^2))
    #}
  }
  result<-as.data.frame(result)
  names(result)<-c("gam_coef","gam_sd","gam_p","gam_basic_rmse","gam_metric_rmse",
                   "gls_coef","gls_sd","gls_p","gls_basic_rmse","gls_metric_rmse",
                   "sar_coef","sar_sd","sar_p","sar_basic_rmse","sar_metric_rmse")
  result$metric<-NA
  result[1:length(variables),]$metric<-variables
  print(result)
  result$coef<-0
  for(i in 1:nrow(result)){
    result[i,]$coef<-weighted.mean(x=c(result[i,]$gam_coef,result[i,]$gls_coef,result[i,]$sar_coef),w=c(1/result[i,]$gam_sd^2,1/result[i,]$gls_sd^2,1/result[i,]$sar_sd^2)) 
  }
  names(result)[17]<-paste0("coef_",r)
  results[[f]]=result
}
table<-full_join(results[[1]][,c(16,17)],results[[2]][,c(16,17)])
table<-full_join(table,results[[3]][,c(16,17)])
table<-full_join(table,results[[4]][,c(16,17)])
table<-full_join(table,results[[5]][,c(16,17)])

gam_list<-list()
gls_list<-list()
sar_list<-list()
for(i in 1:length(variables)){
  gam_data<-rbind.data.frame(results[[1]][i,c(1:5,16)],
        results[[2]][i,c(1:5,16)],
        results[[3]][i,c(1:5,16)],
        results[[4]][i,c(1:5,16)],
        results[[5]][i,c(1:5,16)],
        results[[6]][i,c(1:5,16)],
        results[[7]][i,c(1:5,16)])
  gam_data$radius<-c(50,75,100,125,150,175,200)
  gam_list[[i]]<-gam_data
  
  gls_data<-rbind.data.frame(results[[1]][i,c(6:10,16)],
                             results[[2]][i,c(6:10,16)],
                             results[[3]][i,c(6:10,16)],
                             results[[4]][i,c(6:10,16)],
                             results[[5]][i,c(6:10,16)],
                             results[[6]][i,c(6:10,16)],
                             results[[7]][i,c(6:10,16)])
  gls_data$radius<-c(50,75,100,125,150,175,200)
  gls_list[[i]]<-gls_data
  
  sar_data<-rbind.data.frame(results[[1]][i,c(11:15,16)],
                             results[[2]][i,c(11:15,16)],
                             results[[3]][i,c(11:15,16)],
                             results[[4]][i,c(11:15,16)],
                             results[[5]][i,c(11:15,16)],
                             results[[6]][i,c(11:15,16)],
                             results[[7]][i,c(11:15,16)])
  sar_data$radius<-c(50,75,100,125,150,175,200)
  sar_list[[i]]<-sar_data
}

gam_list<-do.call(rbind,gam_list)
gls_list<-do.call(rbind,gls_list)
sar_list<-do.call(rbind,sar_list)

metric_list<-unique(gam_list[gam_list$gam_p<0.05,"metric"])

variables<-as.data.frame(variables)
variables$rank=row.names(variables)
variables<-variables%>%filter(!grepl("G_",variables))

gam_metric<-gam_list%>%filter(metric%in%metric_list)
gam_metric<-gam_metric[,c("gam_metric_rmse","metric","radius")]
gam_metric<-variables%>%inner_join(gam_metric,by=c("variables"="metric"))
names(gam_metric)<-c("metric","rank","rmse","Radius")
gam_metric$rank<-as.numeric(gam_metric$rank)

gam_min<-gam_metric%>%group_by(metric)%>%summarise(which.min(rmse))
gam_min$`which.min(rmse)`<-25*gam_min$`which.min(rmse)`+25
names(gam_min)<-c("metric","min")
ggplot(data=gam_metric,aes(x=Radius,y=reorder(metric,rank)))+
  geom_tile(aes(fill=rmse),color="white")+scale_fill_distiller(palette = "Spectral")+
  geom_tile(data=gam_min,aes(x=min,y=metric),fill=NA,color="black",size=2)+
  scale_x_continuous(breaks = c(50,75,100,125,150,175,200))

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

