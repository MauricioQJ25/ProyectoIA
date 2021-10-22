
muestreo_estocastico<-function(base,individuos_,porc_elitismo_,objetivo_){
  generacionnueva=matrix(0,individuos_, dim(base)[2])
  aptitud_=do.call(ranking,list(base,individuos_,objetivo_))
  y=runif(1)+seq(0,1,1/individuos_)[-(individuos_+1)]
  y[y>1]=y[y>1]-1
  for (j in 1:individuos_){
    generacionnueva[j,]= base[min((1:individuos_)[y[j]<cumsum(aptitud_)]),]
  }
  if(round(porc_elitismo_*individuos_)!=0){
    if(objetivo_=="min"){
      gen_elitismo=base[order(base[,dim(base)[2]],decreasing=FALSE)[1:round(porc_elitismo_*individuos_)],]
    }else if(objetivo_=="max"){
      gen_elitismo=base[order(base[,dim(base)[2]],decreasing=TRUE)[1:round(porc_elitismo_*individuos_)],]
    }
    
  }else{
    gen_elitismo="X"
  }
  return(list(generacionnueva,gen_elitismo))
}


