funcion_opt="fitness"
individuos=250
x1=0
x2=100
y1=-500
y2=500
z1=-500
z2=500
maxiter=1000
parametros=3
y=errores[[k]]
nhis=n_hist
objetivo="max"

evolucion_diferencial<-function(funcion_opt,individuos,x1,x2,y1,y2,z1,z2,maxiter,parametros,y,nhis,objetivo,R){
  
  X=matrix(0,individuos,parametros+1)
  V=matrix(0,individuos,parametros+1)
  X[,1:parametros]=matrix(runif(individuos*parametros,c(x1,y1,z1),c(x2,y2,z2)),nrow=individuos,ncol = parametros,TRUE)
  for(p in 1:individuos){
    X[p,parametros+1]=do.call(funcion_opt,list(X[p,1],X[p,2],X[p,3],y,nhis))
  }
  
  
  gBest=X[order(X[,parametros+1],decreasing =   ifelse(objetivo=="max",TRUE,FALSE))[1],]
  hist_gBest=gBest
  
  X1=matrix(0,individuos,parametros+1)
  iter=1
  time_inic=Sys.time()
  while(iter<maxiter){
    muestra=matrix(0,nrow = individuos,parametros+1)
    for(i in 1:individuos){
      muestra[i,]=sample((1:individuos)[-i],parametros+1)
    }
    
    V[,1:parametros]=X[muestra[,1],1:parametros]+runif(individuos,0,1)*(X[muestra[,2],1:parametros]-X[muestra[,3],1:parametros])
    V[,1][V[,1]>x2]=x2
    V[,1][V[,1]<=x1]=.00000000001
    V[,2][V[,2]>y2]=y2
    V[,2][V[,2]<y1]=y1
    V[,3][V[,3]>z2]=z2
    V[,3][V[,3]<z1]=z1
    
    for(p in 1:individuos){
      V[p,parametros+1]=do.call(funcion_opt,list(V[p,1],V[p,2],V[p,3],y,nhis))
    }
    
    if(objetivo=="min"){
      X1[V[,parametros+1]<X[,parametros+1],]=V[ V[,parametros+1]<X[,parametros+1],]
      X1[X[,parametros+1]<V[,parametros+1],]=X[X[,parametros+1]<V[,parametros+1],]
      X1[X[,parametros+1]==V[,parametros+1],]=X[X[,parametros+1]==V[,parametros+1],]
      gBest=X1[order(X1[,parametros+1],decreasing =FALSE )[1],]
      hist_gBest=rbind(hist_gBest,gBest)
      #print(gBest)
    }else{
      X1[V[,parametros+1]<X[,parametros+1],]=X[ V[,parametros+1]<X[,parametros+1],]
      X1[X[,parametros+1]<V[,parametros+1],]=V[X[,parametros+1]<V[,parametros+1],]
      X1[X[,parametros+1]==V[,parametros+1],]=X[X[,parametros+1]==V[,parametros+1],]
      gBest=X1[order(X1[,parametros+1],decreasing =TRUE )[1],]
      hist_gBest=rbind(hist_gBest,gBest)
      #print(gBest)
    }
    
    X=X1
    V=matrix(0,individuos,parametros+1)
    X1=matrix(0,individuos,parametros+1)
    iter=iter+1
   # print(iter)
  }
  time_final=Sys.time()
  return(list(X,hist_gBest,gBest,time_inic,time_final)) 
}
