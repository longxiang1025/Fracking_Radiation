library(dplyr)
library(mgcv)
library(lme4)
library(spdep)
library(xtable)
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
j=0
for(r in c(50,75,100)){
  j=j+1
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
  result<-matrix(0,ncol=12,nrow=length(variables))
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
    g_ls<-gls(as.formula(large_gls_formula),correlation=corGaus(form=~Lon+Lat,nugget=TRUE,fixed = T),data=rad_cross)
    t<-coef(g_ls)[7]/sqrt(diag(vcov(g_ls)))[7]
    #summary(g_ls)
    sar_m<-lagsarlm(as.formula(large_gls_formula),data=rad_cross,listw =  W_dist)
    t2<-coef(sar_m)[8]/sqrt(diag(vcov(sar_m)))[8]
    #summary(sar_m)
    result[i,]<-c(coef(g_m)[7],
                  sqrt(diag(vcov.gam(g_m)))[7],
                  2*pnorm(-abs(coef(g_m)[7]/sqrt(diag(vcov.gam(g_m)))[7])),
                  sum(residuals(g_m)^2),
                  coef(g_ls)[7],
                  sqrt(diag(vcov(g_ls)))[7],
                  2*pnorm(-abs(coef(g_ls)[7]/sqrt(diag(vcov(g_ls)))[7])),
                  sum(residuals(g_ls)^2),
                  coef(sar_m)[8],
                  sqrt(diag(vcov(sar_m)))[8],
                  2*pnorm(-abs(coef(sar_m)[8]/sqrt(diag(vcov(sar_m)))[8])),
                  sum(residuals(sar_m)^2)
    )
  }
  result<-as.data.frame(result)
  names(result)<-c("gam_coef","gam_sd","gam_p","gam_res","gls_coef","gls_sd","gls_p","gls_res","sar_coef","sar_sd","sar_p","sar_res")
  result$metric<-variables
  result<-result%>%filter(gam_p<0.05,gls_p<0.05,sar_p<0.05)
  print(result)
  result$coef<-0
  for(i in 1:nrow(result)){
    result[i,]$coef<-weighted.mean(x=c(result[i,]$gam_coef,result[i,]$gls_coef,result[i,]$sar_coef),w=c(1/result[i,]$gam_sd^2,1/result[i,]$gls_sd^2,1/result[i,]$sar_sd^2)) 
  }
  names(result)[14]<-paste0("coef_",r)
  results[[j]]=result
}
table<-full_join(results[[1]][,c(13,14)],results[[2]][,c(13,14)])
table<-full_join(table,results[[3]][,c(13,14)])
table$Metric<-c("Gross Prod of Oil","Prod Dens of Oil From Unconv Drills","Prod Dens of Oil From All Wells",
                "Prod of Oil From Unconv Drills","Prod of Oil From Conv Drills","Prod of Oil From All Upwind Wells",
                "Num of Unconv Drills","Num of All Active Oil Wells","Prod Dens of Oil From Conv Drills","Number of Conv Drills")
names(table)<-c("Metric","Slope(50km)","Slope(75km)","Slope(100km)","Metric")
table_cross<-xtable(table[c(4,5,1,7,10,8,2,9,3,6),c(5,2,3,4)],digits = 4,method="compact",column.names=)
print(table_cross,include.rownames = FALSE,table.placement="htpb")

