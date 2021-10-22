
cruza<-function(base,parametros_,individuos_,x1_,x2_,y1_,y2_,z1_,z2_,y,nhis,funcion_opt,parametros){
  generacionnueva=matrix(0,individuos_, dim(base)[2])
  z= ifelse(individuos_%%2==0,individuos_/2,(individuos_-1)/2)
  for(j in 1:z){
    r=runif(2,-.25,1.25)
    generacionnueva[2*j-1,1:parametros_]=base[2*j-1,1:parametros_]+r[1]*(base[2*j,1:parametros_]-base[2*j-1,1:parametros_])
    generacionnueva[2*j,1:parametros_]=base[2*j,1:parametros_]+r[2]*(base[2*j-1,1:parametros_]-base[2*j,1:parametros_])
  }
  if(z<(individuos_/2)){
    generacionnueva[individuos_,]=base[individuos_,]
  }
  
  generacionnueva[generacionnueva[,1]>x2_]=x2_
  generacionnueva[generacionnueva[,1]<x1_]=.00000000001
  
  generacionnueva[generacionnueva[,2]>y2_]=y2_
  generacionnueva[generacionnueva[,2]<y1_]=y1_
  
  generacionnueva[generacionnueva[,3]>z2_]=z2_
  generacionnueva[generacionnueva[,3]<z1_]=z1_
  
  for(p in 1:individuos_){
    generacionnueva[p,parametros+1]=do.call(funcion_opt,list(generacionnueva[p,1],generacionnueva[p,2],generacionnueva[p,3],y,nhis))
  }
  
  
  return(generacionnueva)
}


