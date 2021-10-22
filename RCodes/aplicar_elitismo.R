aplicar_elitismo<-function(base,muestra_elitista_,objetivo_,individuos_){
  #if(elitismo_==TRUE){
    if(muestra_elitista_[1]!="X"){
      xi=numeric(individuos_)
      if(objetivo_=="min"){
        xi=order(base[,dim(base)[2]],decreasing=TRUE)
      }else{
        xi=order(base[,dim(base)[2]],decreasing=FALSE)
      }
      if(is.null(dim(muestra_elitista_)[1])){
        base[xi[1],]=muestra_elitista_
      }else{
        base[xi[1:dim(muestra_elitista_)[1]],]=muestra_elitista_
      }
    }
  #}
  return(base)
}


