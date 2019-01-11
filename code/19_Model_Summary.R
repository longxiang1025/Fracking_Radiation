library(dplyr)
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
