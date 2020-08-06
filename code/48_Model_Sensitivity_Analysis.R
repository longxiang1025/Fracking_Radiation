#The objective of this function is to run the sensitivity analysis for the model#
#################################################################################
sens_ana<-function(basic_formula,additional_term,index,times=10){
  sens_result<-matrix(0,ncol = 4,nrow=times)
  sens_result<-as.data.frame(sens_result)
  names(sens_result)<-c("run","min_slope","max_slope","max_p")
  for(bt in 1:times){
    assn<-sample(1:10,length(radnet),replace = T)
    assn<-cbind.data.frame(radnet$city_state,assn)
    names(assn)<-c("city_state","assn")
    bt_rad_all_data<-rad_all_data%>%left_join(assn)
    
    g<-bt_rad_all_data%>%group_by(assn)%>%
      do(mod=lmer(paste0(basic_formula,additional_term),
                  data=bt_rad_all_data[!bt_rad_all_data$assn%in%.$assn,],REML=F,
                  control =lmerCtrl.optx(method="nlminb")))
    g<-as.data.frame(g %>% do(data.frame(assn=.$assn,
                                         coef = fixef(.$mod)[index],
                                         warn=length(.$mod@optinfo$warnings),
                                         sd=sqrt(diag(vcov.merMod(.$mod))[index]),
                                         p=2*pnorm(-abs(fixef(.$mod)[index]/sqrt(diag(vcov.merMod(.$mod))[index]))))))
    min_slope<-min(g$coef)
    max_slope<-max(g$coef)
    max_p<-max(g$p)
    sens_result[bt,]=c(bt,min_slope,max_slope,max_p)
    print(sens_result[bt,])
  }
  rm(g)
  return(sens_result)
}