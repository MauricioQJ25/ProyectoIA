crearhistorico_TR<-function(){
  archivos=dir("Historia")
  hist=data.frame(as.Date(matrix(read.csv(paste("Historia/",archivos[1],sep=""))$FECHA,ncol =1 ),"%d/%m/%Y"))
  precios=data.frame(matrix(read.csv(paste("Historia/",archivos[1],sep=""))$FECHA,ncol =1 ))
  nombre="FECHA"
  
  for(i in 1:length(archivos)){
    x=read.csv(paste("Historia/",archivos[i],sep=""))
    fechas=as.Date(x$FECHA,tryFormats = c("%Y-%m-%d", "%d/%m/%Y"))
    if(x$TIPO.VALOR[1]!="CURVA"){
      nombre=c(nombre,paste(x$TIPO.VALOR[1],"_",x$EMISORA[1],"_",x$SERIE[1],sep=""))
    }else{
      nombre=c(nombre,substr(archivos[i],1,9))
    }
    
    
    if(x$TIPO.VALOR[1]=="*C"){
      hist=cbind(hist,x$PRECIO.SUCIO[match(hist[,1],fechas)])
      precios=cbind(precios,x$PRECIO.SUCIO[match(hist[,1],fechas)])
    }else{
      hist=cbind(hist,x$RENDIMIENTO[match(hist[,1],fechas)])
      precios=cbind(precios,x$PRECIO.SUCIO[match(hist[,1],fechas)])
    }
    
    names(hist)=nombre
    names(precios)=nombre
    
  }
  
 
  return(list(hist,precios))
}
