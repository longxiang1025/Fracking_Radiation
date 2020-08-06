##############Reshpae the production data##############################################
#The aim of this function is to convert the long form of production data into a wide  #
#form. Some columns should be conserved such as ID, collect_start,collect_end.        #
#Some columns should be reshaped into the metric calculation.                         #
#@para 1: metric. The metric of interest, must in n_oil,n_gas,oil_prod,gas_prod       #
#@para 2: prod_data. The production table, in this project, it's rad_prod             #
#@para 3: direct (default False). Whether only to summarise the upwind wells          #
#@para 4: dtype (default H). Which drilling type is of interested                     #
#@para 5: brk (defult=0). The bracket of interest                                     #
#######################################################################################
#Example Usage:
#t<-expand_prod(rad_table=rad[rad$city_state=="AUSTIN,TX",],
#               prod_table=rad_prod[rad_prod$city_state=="AUSTIN,TX",],metric="n_oil")
#t<-prod_reshape("n_oil",t,brk=c(0,1,2,3))
#######################################################################################
library(dplyr)
prod_reshape<-function(metric,prod_data,direct=FALSE,dtype="H",brk=c(0,1)){
  p=NULL
  if(metric %in% names(prod_data)){
    p<-prod_data[,c("city_state","collect_start","collect_end","Drill","brks",metric)]
    names(p)[ncol(p)]<-"metric"
    p<-p[p$Drill==dtype&p$brks%in%brk,]
    p<-p%>%
      filter(Drill==dtype,brks%in%brk)%>%
      dplyr::select(city_state,collect_start,collect_end,brks,metric)%>%
      spread(brks,metric)
    names(p)[4:(3+length(brk))]<-paste0("B",names(p)[4:(3+length(brk))],"_",metric,"_",dtype)
  }else{
    print("metric out of the extent")
  }
  return(p)
}

