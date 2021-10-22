

ranking<-function(generacion,individuos_,objetivo_){
  max=2
  #xi=numeric(individuos_)
  tabla=data.frame(table(generacion[,dim(generacion)[2]]))
  aux=tabla$Var1
  xi=numeric(length(aux))
  
  if(objetivo_=="min"){
    xi[order(aux,decreasing=TRUE)]=1:length(aux)
  }else{
    xi[order(aux,decreasing=FALSE)]=1:length(aux)
  }
  aptitudaux=2-max+2*(max-1)*(xi-1)/(length(aux)-1)
  aptitudaux=aptitudaux/sum(aptitudaux)
  
  tabla$NUEVO=aptitudaux[xi[order(xi,decreasing = FALSE)]]
  tabla$Real=tabla$NUEVO/tabla$Freq
  salida=tabla$Real[match(generacion[,dim(generacion)[2]],tabla$Var1)]

  return(salida)
}
