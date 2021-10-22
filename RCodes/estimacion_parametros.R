base=TR_historica
ndias_=n_dias
itermax=1000


estimacion_parametros<-function(base,ndias_,itermax){
  
  base=base[,-1]

  parametrosED=matrix(0,4,0)
  histED=matrix(0,itermax,0)
  parametrosPSO=matrix(0,4,0)
  histPSO=matrix(0,itermax,0)
  parametrosCR=matrix(0,4,0)
  histCR=matrix(0,itermax,0)
  historico_parametros=list()
  proy_reglin=matrix(0,ndias_,0)
  errores=list()
  dif_terminal=c()
    
  for(k in 1:dim(base)[2]){
    n_hist=sum(!is.na(base[k]))
    Sx=n_hist*(n_hist+1)/2
    Sy=sum(base[!is.na(base[k]),k])
    Sxx=sum((1:n_hist)^2)
    Syy=sum(base[!is.na(base[k]),k]*base[!is.na(base[k]),k])
    Sxy=sum(base[!is.na(base[k]),k]*(1:n_hist))
    
    b=(Sxy-(Sx*Sy)/n_hist)/(Sxx-(Sx^2)/n_hist)
    a=Sy/n_hist-b*Sx/n_hist
    errores[[k]]= ((a+b*(1:n_hist))-base[!is.na(base[k]),k])
    a1=a+base[!is.na(base[k]),k][n_hist]-(a+b*(n_hist))
    tasasregr=a1+b*((n_hist+1):(n_hist+ndias_))
    diasproy=((n_hist+1):(n_hist+ndias_))
   # plot(base[,1])
  
    ED=do.call("evolucion_diferencial",list("fitness",500,0,1000,-50,50,-5000,10000,itermax,3,errores[[k]],n_hist,"max",.8))
    PSO=do.call("pso",list("fitness",500,0,1000,-50,50,-5000,10000,itermax,3,errores[[k]],n_hist,"max",.75))
    CR=do.call("algoritmo_codificacion_real",list("fitness",500,0,1000,-50,50,-5000,10000,itermax,3,errores[[k]],n_hist,"max",.8,.2,"muestreo_estocastico",.2))
    
    parametrosED=cbind(parametrosED,ED[[3]])
    histED=cbind(histED,ED[[2]])
    parametrosPSO=cbind(parametrosPSO,PSO[[3]])
    histPSO=cbind(histPSO,PSO[[2]])
    parametrosCR=cbind(parametrosCR,CR[[3]])
    histCR=cbind(histCR,CR[[2]])
    proy_reglin=cbind(proy_reglin,tasasregr)
    historico_parametros[[k]]=list(unique(ED[[2]]),unique(PSO[[2]]),unique(CR[[2]]))
    dif_terminal=c(dif_terminal,errores[[k]][length(errores[[k]])])
    print(k)
    }
  
#  parametrosED_RL=matrix(0,4,0)
#  parametrosPSO_RL=matrix(0,4,0)
#  parametrosCR_RL=matrix(0,4,0)
#  historico_parametros_RL=list()
#  proy_reglin_RL=matrix(0,ndias_,0)
#  errores=list()
#  
#  for(k in 1:dim(base)[2]){
#    n_hist=sum(!is.na(base[k]))
#    Sx=sum(log(1:n_hist))
#    Sy=sum(base[!is.na(base[k]),k])
#    Sxx=sum(log(1:n_hist)*log(1:n_hist))
#    Syy=sum(base[!is.na(base[k]),k]*base[!is.na(base[k]),k])
#    Sxy=sum(base[!is.na(base[k]),k]*log((1:n_hist)))
#    
#    b=(Sxy-(Sx*Sy)/n_hist)/(Sxx-(Sx^2)/n_hist)
#    a=Sy/n_hist-b*Sx/n_hist
#    errores[[k]]= ((a+b*log(1:n_hist))-base[!is.na(base[k]),k])
#    a1=a+base[!is.na(base[k]),k][n_hist]-(a+b*(n_hist))
#    tasasregr=a1+b*((n_hist+1):(n_hist+ndias_))
#    diasproy=((n_hist+1):(n_hist+ndias_))
    
#    ED=do.call("evolucion_diferencial",list("fitness",250,0,500,-500,500,-500,500,itermax,3,errores[[k]],n_hist,"max",.8))
#    PSO=do.call("pso",list("fitness",250,0,500,-500,500,-500,500,itermax,3,errores[[k]],n_hist,"max",.8))
#    CR=do.call("algoritmo_codificacion_real",list("fitness",250,0,500,-500,500,-500,500,itermax,3,errores[[k]],n_hist,"max",.8,.2,"muestreo_estocastico",.2))
    
#    parametrosED_RL=cbind(parametrosED_RL,ED[[3]])
#    parametrosPSO_RL=cbind(parametrosPSO_RL,PSO[[3]])
#    parametrosCR_RL=cbind(parametrosCR_RL,CR[[3]])
#    proy_reglin_RL=cbind(proy_reglin_RL,tasasregr)
#    historico_parametros_RL[[k]]=list(unique(ED[[2]]),unique(PSO[[2]]),unique(CR[[2]]))


    
#  }
  
  
  
  
  
  
  return(list(parametrosED,parametrosPSO,parametrosCR,historico_parametros,proy_reglin,errores,dif_terminal,histED,histPSO,histCR))
}
