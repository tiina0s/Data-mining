source("C:/Users/Lenovo/Documents/kool/andmekaeve/kodutoo1/kMeans.R")
source("C:/Users/Lenovo/Documents/kool/andmekaeve/kodutoo1/kNN.R")
source("C:/Users/Lenovo/Documents/kool/andmekaeve/kodutoo1/performance.R")

# clear everything
#rm(list=ls())

library(shotGroups)
data <- get(load("C:/Users/Lenovo/Documents/kool/andmekaeve/kodutoo1/homeworkData5.RData"))

load(file="C:/Users/Lenovo/Documents/kool/andmekaeve/kodutoo1/homeworkData5.RData")

#split the data in proportion 70/30 for training and validation purposes.
sample_size <- floor(0.7 * nrow(xx))

set.seed(123) # seed is necessary to make it reproducible
train_ind <- sample(seq_len(nrow(xx)), size = sample_size)

train_set <- xx[train_ind, ]
test <- xx[-train_ind, ]
train <- train_set[,1:2] # the data we used was initially prepared for the classificatrion example please remove third column

#k-keskmiste klasterdamine 

results <- mean(train,4)  
idx = results[["cluster"]]

p<-matrix(,2,1)
p[1,1] <- 1
p[2,1] <- 2


for (i in seq(along=idx)){
  a<-switch(idx[i],"red","green","blue","yellow") 
  plot(train[i,1],train[i,2], col=a,type="p",xlim=c(-10,20),ylim=c(-10,20))
  par(new=TRUE)
}

# select cluster Nr 1 (you may select any other cluster)
cluster_1 = train[idx == 1,]

cov_cluster_1 = cov(cluster_1)
print(results[[1]])

#DrawEllipse(results$centers[1,1], results$centers[1,2], a = sqrt(eigen_values[1]), b = sqrt(eigen_values[2]), angle = theta,  border = NULL, col = "r")
drawEllipse(results$centers[1,], cov_cluster_1, radius=3, nv = 100, axes = FALSE, fg = par('fg'), bg = NA, colCtr = "red", lty = par('lty'), lwd = par('lwd'), pch = par('pch'), cex = par('cex'))
p<-matrix(,2,1)
p[1,1] <- 1
p[2,1] <- 2


#leiame lähima klastri kNN meetodi abil punktile p
kNN(p, data, results[["klastrid"]],5)
