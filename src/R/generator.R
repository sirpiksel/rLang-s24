
#White Noise
make_WN<-function(n,sigma=1){
  WN<-rnorm(n,mean=0,sd=sigma)
  return(WN)
}

#ar erzeuger variante 1, phi gegeben , p=length(phi), t Länge der Zeitreihe
make_ar<-function(t,phi,sigma=1,start=numeric(length(phi))){

  stopifnot("phi has to be an vector of numeric or complex values"=(is.numeric(phi)|is.complex(phi)),
            "t has to be positive"=(t>0),
            "t has to be greater than the length of phi and start"=(t>length(phi)&t>length(start)),
            "the polynomial phi has to have no roots in the unit circle"=(all(abs(polyroot(c(1,-phi)))!=1)))
  p<-length(phi)
  X<-c(start,numeric(t-length(start)))
  Z<- rnorm(t,mean = 0,sd=sigma)
  
  for (i in (p + 1):t) {
    X[i] <- sum(phi * X[(i-1):(i-p)]) + Z[i]
  }
  return(X)
}


#ma erzeuger var 1, theta gegeben q=length(theta)
make_ma<-function(t,theta,sigma=1){
 
  stopifnot("theta has to be an vector of numeric or complex values"=(is.numeric(theta)|is.complex(theta)),
            "t has to be positive"=(t>0),
            "t has to be greater than the length of theta"=(t>length(theta)),
            "sigma has to be positive"=(sigma>0),
            "sigma has to be a number"=(!is.na(sigma)&!is.infinite(sigma)),
            "t has to be a number"=(!is.na(t)&!is.infinite(t)),
            "theta has to be a number"=(!is.na(theta)&!is.infinite(theta)))
  q<-length(theta)
  X<-numeric(t)
  Z<-rnorm(t+q,mean=0,sd=sigma)
  
  for (i in (q + 1):(t + q)) {
    X[i - q] <- sum(theta * Z[(i-1):(i-q)]) + Z[i]
  }
  return(X)
}
