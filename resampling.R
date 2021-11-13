#import
install.packages("HistData")
rm(list=ls()) ###初始化编程环境
set.seed(123) ##设置随机种子
library(HistData)
data(GaltonFamilies)
galton0=GaltonFamilies
summary(galton0)
#preprocessing
galton1=galton0
galton1[,c(2,3,8)]=2.54*galton0[,c(2,3,8)] ##原始数据中的英寸转为厘米
galton1[,3]=1.08*galton1[,3];
galton1[galton1[,7]==c("female"),8]=1.08*galton1[galton1[,7]==c("female"),8] ##女性数据乘以1.08消除性别影响
summary(galton1)
#model
X<-(galton1[,2]+galton1[,3])/2 # 解释变量
Y<-galton1[,8] # 响应变量
X1<-matrix(1,nrow=nrow(galton0),ncol=2)
X1[,2]=X
lmobj=lm(Y~X)
obj2<-lm(Y~galton1[,2]+galton1[,3]) 
#resamling
##Leave One Out
n=dim(galton1)[1]
er=rep(0,n)
x=c(1,1,1)
for(i in 1:n){
  obj=lm(Y[-i]~galton1[-i,2]+galton1[-i,3])
  x[2]=galton1[i,2]
  x[3]=galton1[i,3]
  error=Y[i]-obj$coefficients%*%x
  er[i]=error^2
}
aim=mean(er)
print(aim)
##Leave One Out plus
K=10
n=dim(galton1)[1]
x=c(1,1,1)
aim=rep(0,10)
for(k in 1:K){
X1=galton1[[2]]^{k}
X2=galton1[[3]]^{k}
er=rep(0,n)
for(i in 1:n){
  obj=lm(Y[-i]~X1[-i]+X2[-i])
  x[2]=X1[i]
  x[3]=X2[i]
  error=Y[i]-obj$coefficients%*%x
  er[i]=error^2
}
aim[k]=mean(er)
}
##K-fold
install.packages("caret")
library(caret)
K=10
n=dim(galton1)[1]
er=rep(0,n)
x=c(1,1,1)
Y_fold=createFolds(Y, k = K, list = TRUE, returnTrain = FALSE)
X1_fold=createFolds(galton1[,2], k = K, list = TRUE, returnTrain = FALSE)
X2_fold=createFolds(galton1[,3], k = K, list = TRUE, returnTrain = FALSE)
for(i in 1:K){
  l=c(1:K)
  y=0
  x1=0
  x2=0
  for(j in l[-i]){
    y=c(y,(Y_fold[[j]]))
    x1=c(x1,(X1_fold[[j]]))
    x2=c(x2,X2_fold[[j]])
  }
  obj=lm(y~x1+x2)
  error=Y_fold[[i]]-obj$coefficients%*%x
  er[i]=error%*%error
  
}
# a change

