#The objective of this function is to use random forest to  #
#estimate the drilling type of some unkown wells            #
#############################################################
library(randomForest)
estimate_d_type<-function(wells,Type_c="Drill_Type",Ukn_Type="U"){
  region_wells<-cbind(wells@data,coordinates(wells))
  region_wells$Basin<-as.factor(region_wells$Basin)
  u_set<-region_wells[region_wells[,Type_c]%in%Ukn_Type,]
  k_set<-region_wells[!region_wells[,Type_c]%in%Ukn_Type,]
  k_set$Drill_Type<-as.factor(k_set[,Type_c])
  rf = randomForest(DrillType~Closest_Type+H_Num+V_Num+Longitude+Latitude+Basin+Depth+First_Prod_Date+Last_Prod_Date+Well_Count+F6_Oil+F24_Oil+F6_Gas+F24_Gas,data=k_set,
                    ntree=100,mtry=10,nodesize=10,na.action=na.omit)
  print(rf$confusion)
  t<-predict(rf,u_set)
  u_set$Drill_Type<-as.character(t)
  k_set$Drill_Type<-as.character(k_set[,Type_c])
  return(bind_rows(k_set,u_set))
}
