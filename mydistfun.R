library(philentropy)
#Minkowsky distance
mydistfun<- function(element1,element2,p){
  # this function returns the distace between the element1 and element2
  # according to the metricf

  dimensions=length(element1)
  sqd<-matrix(,dimensions,1)
  
  #Minkowsky
  for(i in seq(along=element1)){
    sqd[i]<-abs(element1[i]-element2[i])^p
  }
  dist<-colSums(sqd)^(1/p)

  #juhul kui Inf, kasutatakse Chebyshevi kaugust
  if(p==Inf){
    for(i in seq(along=element1)){
      sqd[i]<-abs(element1[i]-element2[i])
   }
  dist<-max(sqd)
}  
 

  
  return(dist)
}

#Canberra distance
mydistfun2<- function(element1,element2,p){
  # this function returns the distace between the element1 and element2
  # according to the metricf
  # lehvenstein, mahlonomis, cosine (levenshtein on vabatahtlik)
  dimensions=length(element1)
  sqd<-matrix(,dimensions,1)

  #Canberra
  for(i in seq(along=element1)){
    first<-abs(element1[i]-element2[i])
    second<-abs(element1[i])+abs(element2[i]) 
    sqd[i]<-first/second
  }

  dist<-colSums(sqd)

  return(dist)
}


#Mahalanobis distance
#mydistfun3 <- function(element1, element2, C){
  
  #dist <- sqrt(t(element1-element2)%*%solve(C)%*%(element1-element2))
  #return(dist)
#}


  

