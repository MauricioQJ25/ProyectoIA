mutacion<-function(base,individuos_,pm_,x1_,x2_,y1_,y2_,z1_,z2_,parametros_,y,nhis,funcion_opt,parametros){
  m=matrix(runif(individuos_*parametros_)<pm_,nrow=individuos_)
  k=sum(m)
  for(w in 1:parametros_){
    if(w==1){
      base[,1:parametros_][m[,w]]=base[,1:parametros_][m[,w]]+runif(length(base[,1:parametros_][m[,w]]),x1_-base[,1:parametros_][m[,w]]+.00000000001,x2_-base[,1:parametros_][m[,w]])
    }else if(w==2){
      base[,1:parametros_][m[,w]]=base[,1:parametros_][m[,w]]+runif(length(base[,1:parametros_][m[,w]]),y1_-base[,1:parametros_][m[,w]],y2_-base[,1:parametros_][m[,w]])
      
    }else if(w==3){
      base[,1:parametros_][m[,w]]=base[,1:parametros_][m[,w]]+runif(length(base[,1:parametros_][m[,w]]),z1_-base[,1:parametros_][m[,w]],z2_-base[,1:parametros_][m[,w]])
      
    }
  }
  
  for(p in 1:individuos_){
    base[p,parametros+1]=do.call(funcion_opt,list(base[p,1],base[p,2],base[p,3],y,nhis))
  }

  
  return(base)
}
