# clear everything
rm(list=ls())

# activate libs for plotting in 3D 
library("scatterplot3d")
library("car")
library("rgl")
library('lattice')
source("C:/Users/Lenovo/Documents/kool/andmekaeve/kodutoo1/mydistfun.R")
# let us prepare some data, x on vektor -50 kuni 50 arvudest
x<- c(-50:50)
x<-x*0.1
y=x

#imagine 10 by 10 square with the center at origine
# compute the distance between the origin and each internal point ofthe square
elt1<-matrix(,2,1)
elt2<-matrix(,2,1)

elt1[1,1]=0
elt1[2,1]=0

z1<-matrix(,101,101)
z2<-matrix(,101,101)
z3<-matrix(,101,101)

for (i in 1:101){
  elt2[1,1]=x[i]
  for  (j in 1:101){
    elt2[2,1]=y[j]
    z1[i,j]=mydistfun(elt1,elt2,2)
    z2[i,j]=mydistfun(elt1,elt2,1)
    z3[i,j]=mydistfun(elt1,elt2,Inf) 

  }
}



#persp(x,y,z1)
#par(new=TRUE)
#open3d()
#surface3d(x,y,z2)


persp3d(x, y, z1,alpha=0.5, col="skyblue")
persp3d(x, y, z2,alpha=0.5, col="brown3",add=T)
persp3d(x, y, z3,alpha=0.5, col="green",add=T)
