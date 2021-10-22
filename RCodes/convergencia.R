
covergencia<-function(base,objetivo_){
  if(objetivo_=="min"){
    X=base[order(base[,dim(base)[2]],decreasing=FALSE)[1],]
  }else{
    X=base[order(base[,dim(base)[2]],decreasing=TRUE)[1],]
  }
  return(X)
}
