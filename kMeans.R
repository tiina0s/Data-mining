source("C:/Users/Lenovo/Documents/kool/andmekaeve/kodutoo1/mydistfun.R")
library(cluster)
library(fpc)

# funktsioon, mis kontrollib l2bi koik punktid, kas kuuluvad klastrisse, mille keskpunkt nende jaoks l2him
separateAgain<- function(v,k,sqd,sqd2,t){
  # separating points into 2 clusters
  v0 <- v
  elt1<-matrix(,2,1)
  for (i in 1:t){
    elt1[1,1]<-sqd[i]
    elt1[2,1]<-sqd2[i]
    # finding max distances for every middle point of clusters
    clusterNr <-0
    minNow <- 99999
    
      for (j in 1:k){
        elt2<-getMiddlePoint(v,j,sqd,sqd2)
        min<-mydistfun(elt1,elt2,2)
        if (min < minNow){
          minNow<-min
          clusterNr<-j
        }
      }
    
      #votab vastava klastri valja klastrite hulgast
        clus<-v[clusterNr]
        for (s in 1:k){

          cl<-v[s]  
          e<-as.list(unlist(v[s]))
          if (i %in% e){
            if (s==clusterNr){
           #   print("on oiges klastris")
            }
            else{
            #  print("ei ole sama klaster")
              toRem <- c(i)
           #   c6 <- c(subset(cl[[1]],!(cl[[1]] %in% toRem)))
              
              #print("kustutada" )
            #  cat(sprintf("kustutada: ", i, " lisada: ", clusterNr))
              message("kust: ", i, " lisada: ", clusterNr)
              e12<-as.list(unlist(v[clusterNr]))
              e13<-as.list(unlist(v[s]))
              c6 <- setdiff(e13,toRem)
              e12<-c(e12,i)

              cl <- c6
              v[[s]]<-c(cl)
              clus<-c(i)
              
              #lisab klastri uuendustega tagasi klastrite hulka
              v[[clusterNr]] <-c(v[[clusterNr]],clus)
           }
          }
        }
  }
  return(v)
}


# uue keskmise leidmine andes ette koik sinna klassi kuuluvad punktid, mille jargi uus keskmine arvutatakse
findNewMean<- function(v,sqd,sqd2){

  elt<-matrix(,2,1)
  kesk1 <- 0
  kesk2 <- 0
  max <- 0
  for (i in seq(along=v)){
    t <- v[i]
   
    k1 <- sqd[t]
    k2 <- sqd2[t]

    kesk1 <- kesk1 + k1
    kesk2 <- kesk2 + k2
    max <- max + 1
  }#}
  elt[1,1] <- kesk1/max
  elt[2,1] <- kesk2/max

  return(elt)
}

getMiddlePoint<-function(cluster,nr,sqd,sqd2){

  elt<-matrix(,2,1)
  kesk1 <- 0
  kesk2 <- 0
  max <- 0
  #votame valja klastri, millele leiame keskpunkti
  clust = cluster[nr]
  list<-as.list(unlist(clust[1]))
  l<-length(list)
  if (l==1){
    a<-clust[1]
    a<-as.numeric(unlist(a))
    elt[1,1]<-sqd[a]
    elt[2,1]<-sqd2[a]
  }
  else{
    kesk1 <- 0
    kesk2 <- 0
    for (i in 1:l){
      s<-as.numeric(unlist(clust[[1]]))
      k1 <- sqd[s[i]]
      k2 <- sqd2[s[i]]

      kesk1 <- kesk1 + k1
      kesk2 <- kesk2 + k2
      max <- l
    }
    elt[1,1] <- kesk1/max
    elt[2,1] <- kesk2/max
  }
  
  return(elt)
}

mean<- function(data,k){

  dimensions=length(data)
  t<-dimensions/2
  sqd<-matrix(,dimensions/2,1)
  sqd2<-matrix(,dimensions/2,1)
  z1<- matrix(data,t,2)
  #z2<-matrix(,60,2)
  elt1<-matrix(,2,1)
  elt2<-matrix(,2,1)
  elt3<-matrix(,2,1)
  elt4<-matrix(,2,1)
  rows<-c(1:t)
  rownames(z1) <- rows
  

  v1<-numeric()
  v2<-numeric()
  
  elt1[1,1]=0
  elt1[2,1]=0
  
  elt3[1,1]=0
  elt3[2,1]=0
  
  elt4[1,1]=0
  elt4[2,1]=0
  
  for(i in 1:dimensions){
    if (i <= dimensions/2){
      sqd[i]<-data[i]
      
    }
    else {
      j = i-dimensions/2
      sqd2[j]<-data[i]
    }
  }
  rownames(sqd) <- rows
  rownames(sqd2)<-rows

  #suvaliste punktide leidmine replace=F, siis ei tagasta samu punkte:
  
  x2 <- sample(1:t, k, replace=F)
#  print(x2)
  #kui vorrelda omavahel kauguse funktsioone, kasutada valmis antud punkte
#  x2<-list()
#  x2<-c(1,4)
  print(x2) 

  v <- list()
  # vectors as clusters
  for (i in 1:k){

    v1<-list()
    v1<-c(x2[i])
    v[i]<-c(v1)
  }

  # separating points into 2 clusters

  for (i in 1:t){
    elt1[1,1]<-sqd[i]
    elt1[2,1]<-sqd2[i]
    # finding max distances for every middle point of clusters
    clusterNr <-0
    minNow <- 99999
    #kontroll, et punkt ei oleks algne keskpunkt
    if (!i %in% x2){
      for (j in 1:k){
        elt2<-getMiddlePoint(v,j,sqd,sqd2)
        min<-mydistfun(elt1,elt2,2)
        if (min < minNow){
          minNow<-min
          clusterNr<-j
        }
      }
      #votab vastava klastri valja klastrite hulgast
      clus<-v[clusterNr]
      #uue punkti lisamine klastrisse, kui see veel sinna ei kuulu

      clus<-c(clus,i)
        
        #lisab klastri uuendustega tagasi klastrite hulka
      v[[clusterNr]] <-c(clus)
    #  print(v)
  }
    
  }
  l1<-as.numeric(unlist(v))
  a<-as.numeric(unlist(v[1]))
  b<-as.numeric(unlist(v[2]))
  c<-as.numeric(unlist(v[3]))
  d<-as.numeric(unlist(v[4]))
  e<-as.numeric(unlist(v[5]))

  l<-length(l1)

  print("Algsed klastrid: ")
  list <- list()
  for (i in 1:k){
    list[i] <-list(as.numeric(unlist(v[i])))
  }
  print(list)
  print("now")
  
  v2<-separateAgain(list,k,sqd,sqd2,t)

    # nii kaua sorteerib uuesti kuni enam punkt ei kuulu teise klastrisse

  list2 <- list()
  while(!identical(all.equal(v2,list),TRUE)){
    list <- v2
    
    v2 <- separateAgain(list,k,sqd,sqd2,t)
    print("Uued klastrid:")
    for (i in 1:k){
      print(as.numeric(unlist(v2[i])))
      list2[i] <-list(as.numeric(unlist(v2[i])))
    }
  }
  
  clusterCount <- length(list2)
  allClusters <- integer()
  # k l2himate punktide klastrite leidmine, koik punktid on enne klastritesse jagatud
  for (i in 1:t){
    
    for (j in 1:length(list2)){
      list3 <- list2[j]
      cl <- as.list(unlist(list3))
      if (i %in% cl){
        allClusters <- c(allClusters,j)
      }
    }
  }
  #print("klastrid")
  #print((unlist(allClusters)))
  allCenters1 <- matrix()
  allCenters2 <- matrix()
  for (i in 1:k){
    elt1<-matrix(,2,1)
    elt1 <- getMiddlePoint(v,i,sqd,sqd2)

    p1 <- elt1[1]
    p2 <- elt1[2]
    allCenters1 <- c(allCenters1,p1)
    allCenters2 <- c(allCenters2,p2)
  }
  allCenters1 <- allCenters1[-1]
  allCenters2 <- allCenters2[-1]
  allCenters <- cbind(allCenters1,allCenters2)
  #allCenters <- allCenters[[-1]]
  #print(allCenters)
  #print(allCenters)
  
  myls <- list("list", length = 3)
  myls[["cluster"]] <- allClusters
  myls[["centers"]] <- allCenters
  myls[["klastrid"]] <- list2
#  print(myls)
  return(myls)
  
}


