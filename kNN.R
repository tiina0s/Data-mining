source("C:/Users/Lenovo/Documents/kool/andmekaeve/kodutoo1/kMeans.R")
source("C:/Users/Lenovo/Documents/kool/andmekaeve/kodutoo1/mydistfun.R")


kNN<- function(point, data, clusters,k){

  print("Punkt, millele otsitakse klastrit:")
  print(point)
  dimensions=length(data)
  t<-dimensions/2
  elt1<-matrix(,2,1)
  elt2<-matrix(,2,1)
  # maatriks, kuhu lisatakse koik punkti kaugused koikidest teistest punktidest
  dist2<-matrix(,2,t)
  
  sqd<-matrix(,dimensions/2,1)
  sqd2<-matrix(,dimensions/2,1)

  
  for(i in 1:dimensions){
    if (i <= t){
      sqd[i]<-data[i]
      
    }
    else {
      j = i-t
      sqd2[j]<-data[i]
    }
  }
  elt1[1,1] <- point[1]
  elt1[2,1] <- point[2]
  # kauguste leidmine
  for (i in 1:t){
    elt2[1,1]<-sqd[i]
    elt2[2,1]<-sqd2[i]
    min <- mydistfun(elt1,elt2,2)
    t <- i
    dist2[1,i]<- t
    dist2[2,i]<- min

  }
  print("Kauguste maatriks: ")
  print(dist2)
  kPoints <- list()
  # k l2himate punktide leidmine
  for (i in 1:k){
    min <- 999
    point1 <- 0
    
    for (i in 1:t){
      if (dist2[2,i] < min){
        if (!dist2[1,i] %in% kPoints){
          min <- dist2[2,i]
          point1 <- dist2[1,i]
        }
      }
    }
    kPoints <- c(kPoints,point1)
    
  }
print("k l2himad punktid: ")
print(unlist(kPoints))
  
  clusterCount <- length(clusters)
  allClusters <- list()
  # k l2himate punktide klastrite leidmine, koik punktid on enne klastritesse jagatud
  for (i in 1:clusterCount){
    cluster <- clusters[i]
    cl <- as.list(unlist(cluster))
    for (j in 1:length(kPoints)){
      if (kPoints[j] %in% cl){
        allClusters <- c(allClusters,i)
      }
    }
  }
  print("klastrid")
  print((unlist(allClusters)))
 
  # klastri leidmine, kuhu kuulub koige rohkem k punkte
  rightClust <- (names(which.max(table(unlist(allClusters)))))
  print("Kuulub klastrisse: ")
  print(rightClust)
                 
}


#data <- get(load("C:/Users/Lenovo/Documents/kool/andmekaeve/homeworkData5.RData"))


#k-keskmiste klasterdamine 
#clusters <- kmeans(data,10)  

#p<-matrix(,2,1)
#p[1,1] <- 1
#p[2,1] <- 2

#kNN(p, data, clusters,20)