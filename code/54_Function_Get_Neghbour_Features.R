#The objective of this function is to borrow features from  #
#nearby objects. For example, to predict the drilling type  #
#of an unknown well, we need to know the drilling type of   #
#its neighbours                                             #
#############################################################
ngb_features<-function(sp1,sp2=NULL,feat="DrillType",sel=c("H","V")){
  library(RANN)
  if(is.null(sp2)){
    print("Searching nearby points witin the dataset")
    sp2=sp1[as.vector(sp1@data[,feat]=="H"|(sp1@data[,feat]=="V")),]
    coord1<-sp1@coords
    coord2<-sp2@coords
    par=RANN::nn2(coord2,coord1,k=10)
    t<-sp2@data[par$nn.idx[,2],feat]
    ngb2<-matrix("H",nrow=length(sp1),ncol=9)
    for(c in 2:10){
      ngb2[,c-1]=sp2@data[par$nn.idx[,c],feat]
    }
    H_num=rowSums(ngb2=="H",na.rm=T)
    V_num=rowSums(ngb2=="V",na.rm=T)
    #table(as.factor(t),H_num)
    output=cbind.data.frame(t,H_num,V_num)
    return(output)
  }else{
    print("Searching nearby points in another dataset")
  }
}
