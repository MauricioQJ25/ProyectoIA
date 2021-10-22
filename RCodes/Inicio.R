setwd("C:/Users/actag/Desktop/Computo Evolutivo/Proyecto Final")

library("readxl")
library("dplyr")
library(xlsx)


diaval="30"
mesval="04"
añoval="2021"
saltos="diario"
nsim=1000

diasproy=seq(as.Date(paste(diaval,"/",mesval,"/",añoval,sep=""),"%d/%m/%Y"),as.Date(paste(diaval,"/",mesval,"/",as.numeric(añoval)+1,sep=""),"%d/%m/%Y"),by="day")
diasproy=diasproy[!weekdays(diasproy)%in%c("sábado","domingo")]
n_dias=length(diasproy)

Portafolio=data.frame(read.csv(paste("Portafolio.csv",sep=""),stringsAsFactors =FALSE,header=TRUE)%>%group_by(ISIN,TV,EMISORA,SERIE)%>%summarise(TITULOS=sum(as.numeric(TITULOS))))### Leemos el archivo donde se encuentra el portafolio
Portafolio$Llave=paste(Portafolio$TV,"_",Portafolio$EMISORA,"_",Portafolio$SERIE,sep="")
Portafolio=do.call("crear_portafolio",list(Portafolio))

TR_historica=crearhistorico_TR()

Precio_historica=TR_historica[[2]][(dim(TR_historica[[2]])[1]-260):dim(TR_historica[[2]])[1],]
TR_historica=TR_historica[[1]][(dim(TR_historica[[2]])[1]-260):dim(TR_historica[[2]])[1],]

ncurvas=dim(TR_historica)[2]-1

I=do.call(estimacion_parametros,list(TR_historica,n_dias,10000))

p=dim(TR_historica)[1]
SimCorrNorm=t(t(chol(cov(TR_historica[!is.na(rowSums(TR_historica[,-1])),-1])))%*%matrix(rnorm(n_dias*ncurvas,0,1),ncurvas,n_dias))
diaslinlogproy_Lineal=fechascalproy(as.Date(paste(diaval,"/",mesval,"/",añoval,sep=""),"%d/%m/%Y"),as.Date(paste(diaval,"/",mesval,"/",as.numeric(añoval)+1,sep=""),"%d/%m/%Y"),saltos)[[2]]
dimencionCurvasProy=length(diaslinlogproy_Lineal)+1

Parametros_ED=I[[1]]
Parametros_ED[2,]=abs(Parametros_ED[2,])
Parametros_PSO=I[[2]]
Parametros_PSO[2,]=abs(Parametros_PSO[2,])
Parametros_CR=I[[3]]
Parametros_CR[2,]=abs(Parametros_CR[2,])

colnames(Parametros_ED)=names(TR_historica)[-1]
colnames(Parametros_PSO)=names(TR_historica)[-1]
colnames(Parametros_CR)=names(TR_historica)[-1]
write.csv(Parametros_ED,"C:/Users/actag/Desktop/ED_3.csv")
write.csv(Parametros_PSO,"C:/Users/actag/Desktop/PSO_3.csv")
write.csv(Parametros_CR,"C:/Users/actag/Desktop/CR_3.csv")

hist_Convergencia=I[[4]]
proy_tasa=I[[5]]
Errores=I[[6]]
DifRegresion_Lineal=I[[7]]
Evolucion_ED=I[[8]]
Evolucion_PSO=I[[9]]
Evolucion_CR=I[[10]]

Final0=matrix(0,3,dim(Evolucion_CR)[2]/4)
iterador=seq(4,dim(Evolucion_CR)[2],4)
for( i in 1:(dim(Evolucion_CR)[2]/4)){
  Final0[1,i] =max((1:(dim(Evolucion_CR)[1]-1))[Evolucion_CR[2:dim(Evolucion_CR)[1],iterador[i]]!= Evolucion_CR[1:(dim(Evolucion_CR)[1]-1),iterador[i]]])
  Final0[2,i] =max((1:(dim(Evolucion_PSO)[1]-1))[Evolucion_PSO[2:dim(Evolucion_PSO)[1],iterador[i]]!= Evolucion_PSO[1:(dim(Evolucion_PSO)[1]-1),iterador[i]]])
  Final0[3,i] =max((1:(dim(Evolucion_ED)[1]-1))[Evolucion_ED[2:dim(Evolucion_ED)[1],iterador[i]]!= Evolucion_ED[1:(dim(Evolucion_ED)[1]-1),iterador[i]]])
}

Final1=matrix(0,3,dim(Evolucion_CR)[2]/4)
iterador=seq(4,dim(Evolucion_CR)[2],4)
for( i in 1:(dim(Evolucion_CR)[2]/4)){
  Final1[1,i] =max((1:(dim(Evolucion_CR)[1]-1))[(Evolucion_CR[2:dim(Evolucion_CR)[1],iterador[i]]- Evolucion_CR[1:(dim(Evolucion_CR)[1]-1),iterador[i]])>.0000001])
  Final1[2,i] =max((1:(dim(Evolucion_PSO)[1]-1))[(Evolucion_PSO[2:dim(Evolucion_PSO)[1],iterador[i]]- Evolucion_PSO[1:(dim(Evolucion_PSO)[1]-1),iterador[i]])>.0000001])
  Final1[3,i] =max((1:(dim(Evolucion_ED)[1]-1))[(Evolucion_ED[2:dim(Evolucion_ED)[1],iterador[i]]- Evolucion_ED[1:(dim(Evolucion_ED)[1]-1),iterador[i]])>.0000001])
}

write.csv(Evolucion_ED,"C:/Users/actag/Desktop/ED_Evolucion_3.csv")
write.csv(Evolucion_PSO,"C:/Users/actag/Desktop/PSO_Evolucion_3.csv")
write.csv(Evolucion_CR,"C:/Users/actag/Desktop/CR_Evolucion_3.csv")

Parametros_CR=read.csv("C:/Users/actag/Desktop/CR_Sol.csv",header=TRUE)
Parametros_PSO=read.csv("C:/Users/actag/Desktop/PSO_Sol.csv",header=TRUE)
Parametros_ED=read.csv("C:/Users/actag/Desktop/ED_Sol.csv",header=TRUE)

Inicio_Proyeccion=TR_historica[dim(TR_historica)[1],-1]


CurvasProy_Lineal_ED=array(data=0,dim=c(length(diaslinlogproy_Lineal)+1,dim(TR_historica)[2]-1,nsim))
CurvasProyST_Lineal_ED=array(data=0,dim=c(length(diaslinlogproy_Lineal)+1,dim(TR_historica)[2]-1,nsim))
CurvasProy_Lineal_ED[1,,]=matrix(rep(as.numeric(Inicio_Proyeccion),nsim),nsim,dim(TR_historica)[2]-1,F)
CurvasProyST_Lineal_ED[1,,]=matrix(rep(as.numeric(Inicio_Proyeccion),nsim),nsim,dim(TR_historica)[2]-1,F)
names(CurvasProy_Lineal_ED)=names(TR_historica)
names(CurvasProyST_Lineal_ED)=names(TR_historica)

CurvasProy_Lineal_PSO=CurvasProy_Lineal_ED
CurvasProy_Lineal_CR=CurvasProy_Lineal_ED
CurvasProyST_Lineal_PSO=CurvasProyST_Lineal_ED
CurvasProyST_Lineal_CR=CurvasProyST_Lineal_ED

t=1/252

for (i in 1:nsim){
  SimCorrNorm=t(t(chol(cov(TR_historica[!is.na(rowSums(TR_historica[,-1])),-1])))%*%matrix(rnorm(n_dias*ncurvas,0,1),ncurvas,n_dias))
  
  CurvasProyST_Lineal_ED[2:dimencionCurvasProy,,i]=((matrix(as.numeric(rep(Parametros_ED[3,],n_dias)),n_dias,ncurvas,byrow = TRUE)+  
                                                    matrix(rep(as.numeric(DifRegresion_Lineal-Parametros_ED[3,]) ,n_dias),n_dias,ncurvas,byrow = T)*
                                                    exp(-as.matrix(diaslinlogproy_Lineal,1,n_dias)%*%t(as.matrix(as.numeric(Parametros_ED[1,]),ncurvas,1))*t)
                                                  +(((matrix(rep(as.numeric(Parametros_ED[2,]) ,n_dias),n_dias,ncurvas,byrow = T)*
                                                        (((1-exp(-as.matrix(diaslinlogproy_Lineal,1,n_dias)%*%t(as.matrix(as.numeric(Parametros_ED[1,]),ncurvas,1))*(2*t)))^(1))/
                                                           (matrix(rep(((2*as.numeric(Parametros_ED[1,]))^1),n_dias),n_dias,ncurvas,byrow = T)))^(.5)))*SimCorrNorm)))
  
  CurvasProyST_Lineal_ED[2:dimencionCurvasProy,,i]=t(t(CurvasProyST_Lineal_ED[2:dimencionCurvasProy,,i])-CurvasProyST_Lineal_ED[2,,i])
  
  CurvasProyST_Lineal_PSO[2:dimencionCurvasProy,,i]=((matrix(as.numeric(rep(Parametros_PSO[3,],n_dias)),n_dias,ncurvas,byrow = TRUE)+  
                                                        matrix(rep(as.numeric(DifRegresion_Lineal-Parametros_PSO[3,]) ,n_dias),n_dias,ncurvas,byrow = T)*
                                                        exp(-as.matrix(diaslinlogproy_Lineal,1,n_dias)%*%t(as.matrix(as.numeric(Parametros_PSO[1,]),ncurvas,1))*t)
                                                      +(((matrix(rep(as.numeric(Parametros_PSO[2,]) ,n_dias),n_dias,ncurvas,byrow = T)*
                                                            (((1-exp(-as.matrix(diaslinlogproy_Lineal,1,n_dias)%*%t(as.matrix(as.numeric(Parametros_PSO[1,]),ncurvas,1))*(2*t)))^(1))/
                                                               (matrix(rep(((2*as.numeric(Parametros_PSO[1,]))^1),n_dias),n_dias,ncurvas,byrow = T)))^(.5)))*SimCorrNorm)))
  
  CurvasProyST_Lineal_PSO[2:dimencionCurvasProy,,i]=t(t(CurvasProyST_Lineal_PSO[2:dimencionCurvasProy,,i])-CurvasProyST_Lineal_PSO[2,,i])
  
  CurvasProyST_Lineal_CR[2:dimencionCurvasProy,,i]=((matrix(as.numeric(rep(Parametros_CR[3,],n_dias)),n_dias,ncurvas,byrow = TRUE)+  
                                                       matrix(rep(as.numeric(DifRegresion_Lineal-Parametros_CR[3,]) ,n_dias),n_dias,ncurvas,byrow = T)*
                                                       exp(-as.matrix(diaslinlogproy_Lineal,1,n_dias)%*%t(as.matrix(as.numeric(Parametros_CR[1,]),ncurvas,1))*t)
                                                     +(((matrix(rep(as.numeric(Parametros_CR[2,]) ,n_dias),n_dias,ncurvas,byrow = T)*
                                                           (((1-exp(-as.matrix(diaslinlogproy_Lineal,1,n_dias)%*%t(as.matrix(as.numeric(Parametros_CR[1,]),ncurvas,1))*(2*t)))^(1))/
                                                              (matrix(rep(((2*as.numeric(Parametros_CR[1,]))^1),n_dias),n_dias,ncurvas,byrow = T)))^(.5)))*SimCorrNorm)))
  
  CurvasProyST_Lineal_CR[2:dimencionCurvasProy,,i]=t(t(CurvasProyST_Lineal_CR[2:dimencionCurvasProy,,i])-CurvasProyST_Lineal_CR[2,,i])
  

  print(i)
}

CurvasProy_Lineal_ED[2:dimencionCurvasProy,,]=array(rep(proy_tasa,nsim),dim=c(n_dias,ncurvas,nsim))+CurvasProyST_Lineal_ED[2:dimencionCurvasProy,,]
CurvasProyST_Lineal_ED[2:dimencionCurvasProy,,]=array(rep(matrix(rep(as.numeric(Inicio_Proyeccion),n_dias),n_dias,ncurvas,T),nsim),dim=c(n_dias,ncurvas,nsim))+CurvasProyST_Lineal_ED[2:dimencionCurvasProy,,]

CurvasProy_Lineal_PSO[2:dimencionCurvasProy,,]=array(rep(proy_tasa,nsim),dim=c(n_dias,ncurvas,nsim))+CurvasProyST_Lineal_PSO[2:dimencionCurvasProy,,]
CurvasProyST_Lineal_PSO[2:dimencionCurvasProy,,]=array(rep(matrix(rep(as.numeric(Inicio_Proyeccion),n_dias),n_dias,ncurvas,T),nsim),dim=c(n_dias,ncurvas,nsim))+CurvasProyST_Lineal_PSO[2:dimencionCurvasProy,,]

CurvasProy_Lineal_CR[2:dimencionCurvasProy,,]=array(rep(proy_tasa,nsim),dim=c(n_dias,ncurvas,nsim))+CurvasProyST_Lineal_CR[2:dimencionCurvasProy,,]
CurvasProyST_Lineal_CR[2:dimencionCurvasProy,,]=array(rep(matrix(rep(as.numeric(Inicio_Proyeccion),n_dias),n_dias,ncurvas,T),nsim),dim=c(n_dias,ncurvas,nsim))+CurvasProyST_Lineal_CR[2:dimencionCurvasProy,,]


periodo=c(1,7,28,60,91,180,360,720,1080,1420,1800,2160,2520,2880,3240,3600,5400,7200,9000,10800)
plot(periodo,as.numeric(TR_historica[260,-c(1,2,3,4,5,6,7)]),type="l",ylim=c(-2,12), main="Curva UMS",xlab="Nodos",ylab="Tasa")
for(i in 1:260){
  lines(periodo,TR_historica[i,-c(1,2,3,4,5,6,7)],col="GREEN")
}

for(i in 1:100){
  lines(periodo,CurvasProy_Lineal_ED[20,-c(1,2,3,4,5,6),i],col="GREEN")
  lines(periodo,CurvasProy_Lineal_PSO[20,-c(1,2,3,4,5,6),i],col="ROYALBLUE4")
  lines(periodo,CurvasProy_Lineal_CR[20,-c(1,2,3,4,5,6),i],col="RED")
}
lines(periodo,as.numeric(TR_historica[260,-c(1,2,3,4,5,6,7)]),col="WHITE",lty=5,lwd=2)

Activo=1
order(c(as.numeric(Parametros_PSO[4,Activo]),as.numeric(Parametros_ED[4,Activo]),as.numeric(Parametros_CR[4,Activo])),decreasing = TRUE)

plot(as.numeric(TR_historica[1:261,Activo+1]),type="l",main="Tipo de Cambio USD",xlab="Historia",ylab="Tasa",ylim=c(19,23),xlim=c(1,321))


for(i in 1:1000){
  lines(c(rep(NA,261),CurvasProy_Lineal_ED[1:60,Activo,i]),col="GREEN")
  lines(c(rep(NA,261),CurvasProy_Lineal_PSO[1:60,Activo,i]),col="ROYALBLUE")
  lines(c(rep(NA,261),CurvasProy_Lineal_CR[1:60,Activo,i]),col="RED")

 
}

hist(CurvasProy_Lineal_CR[60,Activo,],border="RED",breaks=100,col="WHITE",xlim=c(19,20.5))
hist(CurvasProy_Lineal_PSO[60,Activo,],border="ROYALBLUE4",breaks=100,add=TRUE,col="WHITE")


Activo=2

plot(as.numeric(TR_historica[160:261,Activo+1]),type="l",main="Rendimiento D1 MEX039 710419",xlab="Historia",ylab="Tasa",xlim=c(1,160))


for(i in 1:1000){
  lines(c(rep(NA,100),CurvasProy_Lineal_ED[1:60,Activo,i]),col="GREEN")
  lines(c(rep(NA,100),CurvasProy_Lineal_PSO[1:60,Activo,i]),col="ROYALBLUE")
  lines(c(rep(NA,100),CurvasProy_Lineal_CR[1:60,Activo,i]),col="RED")
  
  
}

hist(CurvasProy_Lineal_CR[60,Activo,],border="RED",breaks=100,col="WHITE",xlim=c(4.20,4.40))
hist(CurvasProy_Lineal_PSO[60,Activo,],border="ROYALBLUE4",breaks=100,add=TRUE,col="WHITE")
