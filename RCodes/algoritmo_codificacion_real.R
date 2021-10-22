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
R=.8

pm=.2
tipo_seleccion="muestreo_estocastico"
porc_elitismo=.2


nhis
funcion_opt

algoritmo_codificacion_real<-function(funcion_opt,individuos,x1,x2,y1,y2,z1,z2,maxiter,parametros,y,n_hist,objetivo,R,pm,tipo_seleccion,porc_elitismo){
  
  generacion=matrix(0,individuos,parametros+1)
  generacion[,1:parametros]=matrix(runif(individuos*parametros,c(x1,y1,z1),c(x2,y2,z2)),nrow=individuos,ncol = parametros,TRUE)
  for(p in 1:individuos){
    generacion[p,parametros+1]=do.call(funcion_opt,list(generacion[p,1],generacion[p,2],generacion[p,3],y,n_hist))
  }

  covergencia_=covergencia(generacion,objetivo)
  
  
  iter=1
  minaptitud=generacion[order(generacion[,parametros+1],decreasing = TRUE),][1,]
  time_inic=Sys.time()
  while(iter<maxiter){
    generacion=do.call(tipo_seleccion,list(generacion,individuos,porc_elitismo,objetivo))
    muestra_elitista=generacion[[2]]
    generacion=generacion[[1]]
    
    generacion=do.call(cruza,list(generacion,parametros,individuos,x1,x2,y1,y2,z1,z2,y,n_hist,funcion_opt,parametros))
    generacion=do.call(mutacion,list(generacion,individuos,pm,x1,x2,y1,y2,z1,z2,parametros,y,n_hist,funcion_opt,parametros))
    
    
    generacion=do.call(aplicar_elitismo,list(generacion,muestra_elitista,objetivo,individuos))
    covergencia_=rbind(covergencia_,covergencia(generacion,objetivo))
    
    minaptitud=generacion[order(generacion[,parametros+1],decreasing = TRUE),][1,]
    iter=iter+1
    
  } 
  time_final=Sys.time()
  return(list(generacion,covergencia_,minaptitud,time_inic,time_final))
}
