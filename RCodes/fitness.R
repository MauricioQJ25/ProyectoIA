alpha=X[p,1]
sigma=X[p,2]
re=X[p,3]
ndias_=n_hist


fitness<-function(alpha,sigma,re,y,ndias_){
  t_=1/252
  aux=(-ndias_*(log(2*pi)+log((sigma^2)*(1-exp(-2*alpha*t_))/(2*alpha)))/2
  -sum(((y[2:ndias_]-(re+(y[1:(ndias_-1)]-re)*exp(-alpha*t_)))^2)/((sigma^2)*(1-exp(-2*alpha*t_))/(2*alpha)))/2)


  return(aux)
}

