

```r
setwd("C:\\Users\\12758\\Desktop\\target_oriented_BO\\1D\\specific\\PureExp")
source("function_y.R")

estimator=2########PureEXP

##############create data
total.data.x <- as.data.frame(seq(0, 1, , 200))
colnames(total.data.x)="x"
total.data.y <- as.data.frame(f11_xiong(total.data.x))
colnames(total.data.y)="es"
order.data=as.data.frame(rep(1:nrow(total.data.x)))
colnames(order.data)="order.num"
data.training=cbind(total.data.x,total.data.y,order.data)

############Set a target
tar=24
tar.points=data.training[which(abs(data.training[,"es"]-data.training[tar,"es"])<=0.01*(max(data.training[,"es"])-min(data.training[,"es"]))),]
tar_data=data.training[tar,"es"]
write.csv(tar_data,"tar_data.csv")

print(paste("The targeted y:", tar_data))
```

```
## [1] "The targeted y: -0.109781026409803"
```

```r
#############iteration number
iter=seq(0,10)
i=7

#########sample the training data
set.seed(11)
sample.num0<-vector()
for(ii in 1:1000){
  s <-  sample(1:(nrow(data.training)),40,replace=F,prob=NULL) 
  sample.num0<-as.data.frame(rbind(sample.num0,s))
}

size=c(round(0.02*nrow(data.training)))
sample.num<-sample.num0[,1:(size)]

split=sample.num[i,]
split=unlist(split)
training.data <- data.training[split,]
write.csv(training.data,"0train.csv")
training.data.x <- data.frame(training.data[,"x"]) 
colnames(training.data.x)="x"
training.data.y <-  data.frame(training.data[,"es"]) 
colnames(training.data.y)="es"

############virtual data
vir.total.data.x=data.frame(data.training[,"x"]) 
colnames(vir.total.data.x)="x"
data.all=data.frame(data.training) 

virtual.data <- data.training[-split,]
rownames(virtual.data)<-1:nrow(virtual.data)
virtual.data.x=as.data.frame(virtual.data[,"x"])
colnames(virtual.data.x)="x"

##########km model prediction
gp = fn.gp(training.data.x, training.data.y,virtual.data.x)
```

```
## 
## optimisation start
## ------------------
## * estimation method   : MLE 
## * optimisation method : BFGS 
## * analytical gradient : used
## * trend model : ~1
## * covariance model : 
##   - type :  matern5_2 
##   - noise variances :
## [1] 1e-05 1e-05 1e-05 1e-05
##   - parameters lower bounds :  1e-10 
##   - parameters upper bounds :  1.005025 
##   - variance bounds :  0.002404243 0.3060814 
##   - best initial criterion value(s) :  2.460662 
## 
## N = 2, M = 5 machine precision = 2.22045e-16
## At X0, 0 variables are exactly at the bounds
## At iterate     0  f=      -2.4607  |proj g|=      0.96275
## At iterate     1  f =      -2.6674  |proj g|=       0.91789
## At iterate     2  f =      -2.7745  |proj g|=      0.097813
## At iterate     3  f =      -2.7792  |proj g|=       0.28285
## At iterate     4  f =      -2.7822  |proj g|=      0.093416
## At iterate     5  f =      -2.7825  |proj g|=       0.02129
## At iterate     6  f =      -2.7825  |proj g|=     0.0025827
## At iterate     7  f =      -2.7825  |proj g|=    0.00023432
## 
## iterations 7
## function evaluations 11
## segments explored during Cauchy searches 8
## BFGS updates skipped 0
## active bounds at final generalized Cauchy point 0
## norm of the final projected gradient 0.000234318
## final function value -2.78246
## 
## F = -2.78246
## final  value -2.782457 
## converged
```

```r
gp.p = fn.gp(training.data.x, training.data.y,vir.total.data.x)
```

```
## 
## optimisation start
## ------------------
## * estimation method   : MLE 
## * optimisation method : BFGS 
## * analytical gradient : used
## * trend model : ~1
## * covariance model : 
##   - type :  matern5_2 
##   - noise variances :
## [1] 1e-05 1e-05 1e-05 1e-05
##   - parameters lower bounds :  1e-10 
##   - parameters upper bounds :  1.005025 
##   - variance bounds :  0.002404243 0.3060814 
##   - best initial criterion value(s) :  2.460662 
## 
## N = 2, M = 5 machine precision = 2.22045e-16
## At X0, 0 variables are exactly at the bounds
## At iterate     0  f=      -2.4607  |proj g|=      0.96275
## At iterate     1  f =      -2.6674  |proj g|=       0.91789
## At iterate     2  f =      -2.7745  |proj g|=      0.097813
## At iterate     3  f =      -2.7792  |proj g|=       0.28285
## At iterate     4  f =      -2.7822  |proj g|=      0.093416
## At iterate     5  f =      -2.7825  |proj g|=       0.02129
## At iterate     6  f =      -2.7825  |proj g|=     0.0025827
## At iterate     7  f =      -2.7825  |proj g|=    0.00023432
## 
## iterations 7
## function evaluations 11
## segments explored during Cauchy searches 8
## BFGS updates skipped 0
## active bounds at final generalized Cauchy point 0
## norm of the final projected gradient 0.000234318
## final function value -2.78246
## 
## F = -2.78246
## final  value -2.782457 
## converged
```

```r
mean=as.data.frame(gp.p[,1])
mean=unlist(mean)
t=total.data.x
t=unlist(t)
yy0=cbind(total.data.x,as.data.frame(mean),as.data.frame(gp.p[,1]-gp.p[,2]),as.data.frame(gp.p[,1]+gp.p[,2]))
colnames(yy0)=c("x","mean","lower","upper")
write.csv(yy0,"yy0.csv")
head(yy0)
```

```
##                      x      mean      lower     upper
## gp.p[, 1]1 0.000000000 0.2140459 0.06028518 0.3678065
## gp.p[, 1]2 0.005025126 0.2140240 0.06026335 0.3677846
## gp.p[, 1]3 0.010050251 0.2139998 0.06023925 0.3677604
## gp.p[, 1]4 0.015075377 0.2139732 0.06021265 0.3677337
## gp.p[, 1]5 0.020100503 0.2139438 0.06018332 0.3677042
## gp.p[, 1]6 0.025125628 0.2139113 0.06015098 0.3676716
```

```r
##########data frame
data.OC.SD=matrix(,length(iter)-1,length(selector))
data.dis=matrix(,length(iter)-1,length(selector))
SD=matrix(0,length(iter),1)
dis=matrix(0,length(iter),1)

########utility value
EI= fn.utility(data.training,estimator,training.data,gp.p,tar_num = tar)
EI[split,]=-1
EI=na.omit(EI)
ee0=cbind(total.data.x,EI)
write.csv(ee0,"ee0.csv")

EI=as.data.frame(EI)

SD[1,1]= fn.OC(data.training,training.data)
dis[1,1]= fn.OC(data.training,training.data)

######recommend the option
num=which(EI[,"EI"]==min(EI[-which(EI[,"EI"]==-1),"EI"]))

u=1
pic0=rep(1:10)
pic0=as.character(pic0)
pic1=rep("csv",10)
pic=paste(pic0,pic1,sep=".")
er0=rep(1:10)
er0=as.character(er0)
er1=rep("csv",10)
er=paste(er0,er1,sep="ee.")
train0=rep(1:10)
train0=as.character(train0)
train1=rep("csv",10)
train=paste(train0,train1,sep="train.")

num.count=c(split,num)
training.data0=training.data
virtual.data0=virtual.data

####start iteration
u=1
repeat{
  ########updated training data and virtual data
  training.data0[nrow(training.data0)+1,]=virtual.data0[which(virtual.data0[,"order.num"]==num),]
  virtual.data0=virtual.data0[-which(virtual.data0[,"order.num"]==num),]
  rownames(training.data0)<-1:nrow(training.data0)
  rownames(virtual.data0)<-1:nrow(virtual.data0)
  
  training.data0.x <- data.frame(training.data0[,"x"])
  colnames(training.data0.x)="x"
  training.data0.y <-  data.frame(training.data0[,"es"]) 
  colnames(training.data0.y)="es"
  virtual.data0.x=as.data.frame(virtual.data0[,"x"])
  colnames(virtual.data0.x)="x"
  
  ##########km model prediction
  gp0 = fn.gp(training.data0.x, training.data0.y,virtual.data0.x)
  gp0.p = fn.gp(training.data0.x, training.data0.y,vir.total.data.x)
  
  
  ########utility value
  EI=fn.utility(data.training,estimator,training.data0,gp0.p,tar_num = tar)
  EI=as.data.frame(EI)
  EI[num.count,]=-1
  EI=na.omit(EI)

  
  ######recommend the option
  num= which(EI[,"EI"]==min(EI[-which(EI[,"EI"]==-1),"EI"]))
  mean=as.data.frame(gp0.p[,1])
  mean=unlist(mean)
  t=total.data.x
  t=unlist(t)
  
  SD[u+1,]=fn.OC(data.training,training.data0)
  dis[u+1,]=fn.dis(data.training, training.data0[nrow(training.data0),"es"])
  
  if(max(EI)==0)
  {break}
  
  if (u==1|u==2|u==3|u==4|u==5|u==6)
  { 
    yy0=cbind(total.data.x,as.data.frame(mean),as.data.frame(gp0.p[,1]-gp0.p[,2]),as.data.frame(gp0.p[,1]+gp0.p[,2]))
    colnames(yy0)=c("x","mean","lower","upper")
    write.csv(yy0,pic[u])
    ee0=cbind(total.data.x,EI)
    write.csv(ee0,er[u])
    write.csv(training.data0,train[u])
  }
  num.count=c(num.count,num)
  #######stopping criteria
  if(u>=length(iter)-1)
    break
  
  #######points in the the tolerance of t
  if(min(abs(training.data0[,"es"]-data.training[tar,"es"]))<=0.01*(max(data.training[,"es"])-min(data.training[,"es"])))
  {
    repeat{ 
      u=u+1
      yy0=cbind(total.data.x,as.data.frame(mean),as.data.frame(gp0.p[,1]-gp0.p[,2]),as.data.frame(gp0.p[,1]+gp0.p[,2]))
      colnames(yy0)=c("x","mean","lower","upper")
      write.csv(yy0,pic[u])
      ee0=cbind(total.data.x,EI)
      write.csv(ee0,er[u])
      training.data0=rbind(training.data0,training.data0[nrow(training.data0),])
      write.csv(training.data0,train[u])
      if(u>6)
        break
    }
  }
  u=u+1
  
}
```

```
## 
## optimisation start
## ------------------
## * estimation method   : MLE 
## * optimisation method : BFGS 
## * analytical gradient : used
## * trend model : ~1
## * covariance model : 
##   - type :  matern5_2 
##   - noise variances :
## [1] 1e-05 1e-05 1e-05 1e-05 1e-05
##   - parameters lower bounds :  1e-10 
##   - parameters upper bounds :  1.005025 
##   - variance bounds :  0.002536986 0.2843334 
##   - best initial criterion value(s) :  6.724987 
## 
## N = 2, M = 5 machine precision = 2.22045e-16
## At X0, 0 variables are exactly at the bounds
## At iterate     0  f=       -6.725  |proj g|=      0.27022
## At iterate     1  f =      -6.9289  |proj g|=       0.90767
## At iterate     2  f =      -7.0022  |proj g|=       0.90542
## At iterate     3  f =      -7.0568  |proj g|=       0.90202
## At iterate     4  f =      -7.0644  |proj g|=       0.89958
## At iterate     5  f =      -7.0706  |proj g|=       0.89747
## At iterate     6  f =      -7.0876  |proj g|=       0.25887
## At iterate     7  f =      -7.0883  |proj g|=       0.25825
## At iterate     8  f =      -7.0883  |proj g|=      0.051025
## At iterate     9  f =      -7.0883  |proj g|=     0.0059251
## At iterate    10  f =      -7.0883  |proj g|=     8.861e-05
## 
## iterations 10
## function evaluations 14
## segments explored during Cauchy searches 11
## BFGS updates skipped 0
## active bounds at final generalized Cauchy point 0
## norm of the final projected gradient 8.86103e-05
## final function value -7.08832
## 
## F = -7.08832
## final  value -7.088323 
## converged
## 
## optimisation start
## ------------------
## * estimation method   : MLE 
## * optimisation method : BFGS 
## * analytical gradient : used
## * trend model : ~1
## * covariance model : 
##   - type :  matern5_2 
##   - noise variances :
## [1] 1e-05 1e-05 1e-05 1e-05 1e-05
##   - parameters lower bounds :  1e-10 
##   - parameters upper bounds :  1.005025 
##   - variance bounds :  0.002536986 0.2843334 
##   - best initial criterion value(s) :  6.724987 
## 
## N = 2, M = 5 machine precision = 2.22045e-16
## At X0, 0 variables are exactly at the bounds
## At iterate     0  f=       -6.725  |proj g|=      0.27022
## At iterate     1  f =      -6.9289  |proj g|=       0.90767
## At iterate     2  f =      -7.0022  |proj g|=       0.90542
## At iterate     3  f =      -7.0568  |proj g|=       0.90202
## At iterate     4  f =      -7.0644  |proj g|=       0.89958
## At iterate     5  f =      -7.0706  |proj g|=       0.89747
## At iterate     6  f =      -7.0876  |proj g|=       0.25887
## At iterate     7  f =      -7.0883  |proj g|=       0.25825
## At iterate     8  f =      -7.0883  |proj g|=      0.051025
## At iterate     9  f =      -7.0883  |proj g|=     0.0059251
## At iterate    10  f =      -7.0883  |proj g|=     8.861e-05
## 
## iterations 10
## function evaluations 14
## segments explored during Cauchy searches 11
## BFGS updates skipped 0
## active bounds at final generalized Cauchy point 0
## norm of the final projected gradient 8.86103e-05
## final function value -7.08832
## 
## F = -7.08832
## final  value -7.088323 
## converged
## 
## optimisation start
## ------------------
## * estimation method   : MLE 
## * optimisation method : BFGS 
## * analytical gradient : used
## * trend model : ~1
## * covariance model : 
##   - type :  matern5_2 
##   - noise variances :
## [1] 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05
##   - parameters lower bounds :  1e-10 
##   - parameters upper bounds :  1.015075 
##   - variance bounds :  0.002164632 0.3044879 
##   - best initial criterion value(s) :  10.85878 
## 
## N = 2, M = 5 machine precision = 2.22045e-16
## At X0, 0 variables are exactly at the bounds
## At iterate     0  f=      -10.859  |proj g|=      0.29082
## At iterate     1  f =      -11.032  |proj g|=       0.91577
## At iterate     2  f =      -11.118  |proj g|=        0.9133
## At iterate     3  f =      -11.165  |proj g|=       0.90432
## At iterate     4  f =      -11.206  |proj g|=       0.90644
## At iterate     5  f =      -11.218  |proj g|=       0.90454
## At iterate     6  f =      -11.278  |proj g|=       0.51749
## At iterate     7  f =      -11.291  |proj g|=       0.27822
## At iterate     8  f =      -11.295  |proj g|=       0.27665
## At iterate     9  f =      -11.295  |proj g|=       0.27608
## At iterate    10  f =      -11.295  |proj g|=      0.014857
## At iterate    11  f =      -11.295  |proj g|=    0.00014015
## At iterate    12  f =      -11.295  |proj g|=    4.2076e-07
## 
## iterations 12
## function evaluations 15
## segments explored during Cauchy searches 13
## BFGS updates skipped 0
## active bounds at final generalized Cauchy point 0
## norm of the final projected gradient 4.20756e-07
## final function value -11.2952
## 
## F = -11.2952
## final  value -11.295211 
## converged
## 
## optimisation start
## ------------------
## * estimation method   : MLE 
## * optimisation method : BFGS 
## * analytical gradient : used
## * trend model : ~1
## * covariance model : 
##   - type :  matern5_2 
##   - noise variances :
## [1] 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05
##   - parameters lower bounds :  1e-10 
##   - parameters upper bounds :  1.015075 
##   - variance bounds :  0.002164632 0.3044879 
##   - best initial criterion value(s) :  10.85878 
## 
## N = 2, M = 5 machine precision = 2.22045e-16
## At X0, 0 variables are exactly at the bounds
## At iterate     0  f=      -10.859  |proj g|=      0.29082
## At iterate     1  f =      -11.032  |proj g|=       0.91577
## At iterate     2  f =      -11.118  |proj g|=        0.9133
## At iterate     3  f =      -11.165  |proj g|=       0.90432
## At iterate     4  f =      -11.206  |proj g|=       0.90644
## At iterate     5  f =      -11.218  |proj g|=       0.90454
## At iterate     6  f =      -11.278  |proj g|=       0.51749
## At iterate     7  f =      -11.291  |proj g|=       0.27822
## At iterate     8  f =      -11.295  |proj g|=       0.27665
## At iterate     9  f =      -11.295  |proj g|=       0.27608
## At iterate    10  f =      -11.295  |proj g|=      0.014857
## At iterate    11  f =      -11.295  |proj g|=    0.00014015
## At iterate    12  f =      -11.295  |proj g|=    4.2076e-07
## 
## iterations 12
## function evaluations 15
## segments explored during Cauchy searches 13
## BFGS updates skipped 0
## active bounds at final generalized Cauchy point 0
## norm of the final projected gradient 4.20756e-07
## final function value -11.2952
## 
## F = -11.2952
## final  value -11.295211 
## converged
## 
## optimisation start
## ------------------
## * estimation method   : MLE 
## * optimisation method : BFGS 
## * analytical gradient : used
## * trend model : ~1
## * covariance model : 
##   - type :  matern5_2 
##   - noise variances :
## [1] 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05
##   - parameters lower bounds :  1e-10 
##   - parameters upper bounds :  1.035176 
##   - variance bounds :  0.00188776 0.3500049 
##   - best initial criterion value(s) :  14.74078 
## 
## N = 2, M = 5 machine precision = 2.22045e-16
## At X0, 0 variables are exactly at the bounds
## At iterate     0  f=      -14.741  |proj g|=      0.92863
## At iterate     1  f =      -15.197  |proj g|=       0.16965
## At iterate     2  f =      -15.203  |proj g|=      0.069555
## At iterate     3  f =      -15.213  |proj g|=       0.31847
## At iterate     4  f =      -15.213  |proj g|=       0.10908
## At iterate     5  f =      -15.213  |proj g|=       0.02924
## At iterate     6  f =      -15.213  |proj g|=    0.00049805
## At iterate     7  f =      -15.213  |proj g|=     1.739e-05
## 
## iterations 7
## function evaluations 11
## segments explored during Cauchy searches 8
## BFGS updates skipped 0
## active bounds at final generalized Cauchy point 0
## norm of the final projected gradient 1.73904e-05
## final function value -15.2134
## 
## F = -15.2134
## final  value -15.213356 
## converged
## 
## optimisation start
## ------------------
## * estimation method   : MLE 
## * optimisation method : BFGS 
## * analytical gradient : used
## * trend model : ~1
## * covariance model : 
##   - type :  matern5_2 
##   - noise variances :
## [1] 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05
##   - parameters lower bounds :  1e-10 
##   - parameters upper bounds :  1.035176 
##   - variance bounds :  0.00188776 0.3500049 
##   - best initial criterion value(s) :  14.74078 
## 
## N = 2, M = 5 machine precision = 2.22045e-16
## At X0, 0 variables are exactly at the bounds
## At iterate     0  f=      -14.741  |proj g|=      0.92863
## At iterate     1  f =      -15.197  |proj g|=       0.16965
## At iterate     2  f =      -15.203  |proj g|=      0.069555
## At iterate     3  f =      -15.213  |proj g|=       0.31847
## At iterate     4  f =      -15.213  |proj g|=       0.10908
## At iterate     5  f =      -15.213  |proj g|=       0.02924
## At iterate     6  f =      -15.213  |proj g|=    0.00049805
## At iterate     7  f =      -15.213  |proj g|=     1.739e-05
## 
## iterations 7
## function evaluations 11
## segments explored during Cauchy searches 8
## BFGS updates skipped 0
## active bounds at final generalized Cauchy point 0
## norm of the final projected gradient 1.73904e-05
## final function value -15.2134
## 
## F = -15.2134
## final  value -15.213356 
## converged
## 
## optimisation start
## ------------------
## * estimation method   : MLE 
## * optimisation method : BFGS 
## * analytical gradient : used
## * trend model : ~1
## * covariance model : 
##   - type :  matern5_2 
##   - noise variances :
## [1] 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05
##   - parameters lower bounds :  1e-10 
##   - parameters upper bounds :  1.065327 
##   - variance bounds :  0.001674575 0.3348516 
##   - best initial criterion value(s) :  18.26903 
## 
## N = 2, M = 5 machine precision = 2.22045e-16
## At X0, 0 variables are exactly at the bounds
## At iterate     0  f=      -18.269  |proj g|=      0.95568
## At iterate     1  f =      -18.971  |proj g|=       0.18169
## At iterate     2  f =      -18.974  |proj g|=      0.035211
## At iterate     3  f =      -18.977  |proj g|=       0.22657
## At iterate     4  f =      -18.977  |proj g|=       0.30112
## At iterate     5  f =      -18.977  |proj g|=      0.020442
## At iterate     6  f =      -18.977  |proj g|=    0.00033543
## At iterate     7  f =      -18.977  |proj g|=    3.3024e-05
## 
## iterations 7
## function evaluations 10
## segments explored during Cauchy searches 8
## BFGS updates skipped 0
## active bounds at final generalized Cauchy point 0
## norm of the final projected gradient 3.30239e-05
## final function value -18.9772
## 
## F = -18.9772
## final  value -18.977228 
## converged
## 
## optimisation start
## ------------------
## * estimation method   : MLE 
## * optimisation method : BFGS 
## * analytical gradient : used
## * trend model : ~1
## * covariance model : 
##   - type :  matern5_2 
##   - noise variances :
## [1] 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05
##   - parameters lower bounds :  1e-10 
##   - parameters upper bounds :  1.065327 
##   - variance bounds :  0.001674575 0.3348516 
##   - best initial criterion value(s) :  18.26903 
## 
## N = 2, M = 5 machine precision = 2.22045e-16
## At X0, 0 variables are exactly at the bounds
## At iterate     0  f=      -18.269  |proj g|=      0.95568
## At iterate     1  f =      -18.971  |proj g|=       0.18169
## At iterate     2  f =      -18.974  |proj g|=      0.035211
## At iterate     3  f =      -18.977  |proj g|=       0.22657
## At iterate     4  f =      -18.977  |proj g|=       0.30112
## At iterate     5  f =      -18.977  |proj g|=      0.020442
## At iterate     6  f =      -18.977  |proj g|=    0.00033543
## At iterate     7  f =      -18.977  |proj g|=    3.3024e-05
## 
## iterations 7
## function evaluations 10
## segments explored during Cauchy searches 8
## BFGS updates skipped 0
## active bounds at final generalized Cauchy point 0
## norm of the final projected gradient 3.30239e-05
## final function value -18.9772
## 
## F = -18.9772
## final  value -18.977228 
## converged
## 
## optimisation start
## ------------------
## * estimation method   : MLE 
## * optimisation method : BFGS 
## * analytical gradient : used
## * trend model : ~1
## * covariance model : 
##   - type :  matern5_2 
##   - noise variances :
## [1] 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05
##   - parameters lower bounds :  1e-10 
##   - parameters upper bounds :  1.266332 
##   - variance bounds :  0.006351528 0.8524333 
##   - best initial criterion value(s) :  11.01935 
## 
## N = 2, M = 5 machine precision = 2.22045e-16
## At X0, 0 variables are exactly at the bounds
## At iterate     0  f=      -11.019  |proj g|=      0.81341
## At iterate     1  f =      -11.371  |proj g|=        1.1709
## At iterate     2  f =      -13.647  |proj g|=        1.1173
## At iterate     3  f =      -14.522  |proj g|=        1.0738
## At iterate     4  f =      -14.751  |proj g|=        1.0413
## At iterate     5  f =      -14.756  |proj g|=       0.55317
## At iterate     6  f =      -14.757  |proj g|=       0.59457
## At iterate     7  f =      -14.763  |proj g|=       0.58017
## At iterate     8  f =      -14.769  |proj g|=       0.56062
## At iterate     9  f =      -14.775  |proj g|=       0.25345
## At iterate    10  f =      -14.776  |proj g|=      0.076558
## At iterate    11  f =      -14.776  |proj g|=     0.0070226
## At iterate    12  f =      -14.776  |proj g|=     0.0057425
## At iterate    13  f =      -14.776  |proj g|=    1.6323e-05
## At iterate    14  f =      -14.776  |proj g|=    1.3274e-07
## 
## iterations 14
## function evaluations 17
## segments explored during Cauchy searches 15
## BFGS updates skipped 0
## active bounds at final generalized Cauchy point 0
## norm of the final projected gradient 1.32736e-07
## final function value -14.7757
## 
## F = -14.7757
## final  value -14.775671 
## converged
## 
## optimisation start
## ------------------
## * estimation method   : MLE 
## * optimisation method : BFGS 
## * analytical gradient : used
## * trend model : ~1
## * covariance model : 
##   - type :  matern5_2 
##   - noise variances :
## [1] 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05
##   - parameters lower bounds :  1e-10 
##   - parameters upper bounds :  1.266332 
##   - variance bounds :  0.006351528 0.8524333 
##   - best initial criterion value(s) :  11.01935 
## 
## N = 2, M = 5 machine precision = 2.22045e-16
## At X0, 0 variables are exactly at the bounds
## At iterate     0  f=      -11.019  |proj g|=      0.81341
## At iterate     1  f =      -11.371  |proj g|=        1.1709
## At iterate     2  f =      -13.647  |proj g|=        1.1173
## At iterate     3  f =      -14.522  |proj g|=        1.0738
## At iterate     4  f =      -14.751  |proj g|=        1.0413
## At iterate     5  f =      -14.756  |proj g|=       0.55317
## At iterate     6  f =      -14.757  |proj g|=       0.59457
## At iterate     7  f =      -14.763  |proj g|=       0.58017
## At iterate     8  f =      -14.769  |proj g|=       0.56062
## At iterate     9  f =      -14.775  |proj g|=       0.25345
## At iterate    10  f =      -14.776  |proj g|=      0.076558
## At iterate    11  f =      -14.776  |proj g|=     0.0070226
## At iterate    12  f =      -14.776  |proj g|=     0.0057425
## At iterate    13  f =      -14.776  |proj g|=    1.6323e-05
## At iterate    14  f =      -14.776  |proj g|=    1.3274e-07
## 
## iterations 14
## function evaluations 17
## segments explored during Cauchy searches 15
## BFGS updates skipped 0
## active bounds at final generalized Cauchy point 0
## norm of the final projected gradient 1.32736e-07
## final function value -14.7757
## 
## F = -14.7757
## final  value -14.775671 
## converged
## 
## optimisation start
## ------------------
## * estimation method   : MLE 
## * optimisation method : BFGS 
## * analytical gradient : used
## * trend model : ~1
## * covariance model : 
##   - type :  matern5_2 
##   - noise variances :
##  [1] 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05
##   - parameters lower bounds :  1e-10 
##   - parameters upper bounds :  1.266332 
##   - variance bounds :  0.005927537 0.6847249 
##   - best initial criterion value(s) :  11.72809 
## 
## N = 2, M = 5 machine precision = 2.22045e-16
## At X0, 0 variables are exactly at the bounds
## At iterate     0  f=      -11.728  |proj g|=      0.65122
## At iterate     1  f =      -12.739  |proj g|=        1.1718
## At iterate     2  f =      -15.085  |proj g|=         1.131
## At iterate     3  f =      -16.741  |proj g|=         1.076
## At iterate     4  f =      -17.133  |proj g|=        1.0378
## At iterate     5  f =       -17.16  |proj g|=       0.47849
## At iterate     6  f =      -17.196  |proj g|=       0.46825
## At iterate     7  f =      -17.288  |proj g|=       0.43185
## At iterate     8  f =      -17.402  |proj g|=       0.37491
## At iterate     9  f =       -17.48  |proj g|=       0.31659
## At iterate    10  f =      -17.521  |proj g|=       0.31868
## At iterate    11  f =      -17.539  |proj g|=       0.32343
## At iterate    12  f =      -17.543  |proj g|=        0.3508
## At iterate    13  f =      -17.547  |proj g|=       0.12156
## At iterate    14  f =      -17.548  |proj g|=      0.012928
## At iterate    15  f =      -17.548  |proj g|=    0.00062453
## At iterate    16  f =      -17.548  |proj g|=    0.00044647
## At iterate    17  f =      -17.548  |proj g|=     4.319e-06
## 
## iterations 17
## function evaluations 20
## segments explored during Cauchy searches 18
## BFGS updates skipped 0
## active bounds at final generalized Cauchy point 0
## norm of the final projected gradient 4.31903e-06
## final function value -17.5477
## 
## F = -17.5477
## final  value -17.547684 
## converged
## 
## optimisation start
## ------------------
## * estimation method   : MLE 
## * optimisation method : BFGS 
## * analytical gradient : used
## * trend model : ~1
## * covariance model : 
##   - type :  matern5_2 
##   - noise variances :
##  [1] 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05
##   - parameters lower bounds :  1e-10 
##   - parameters upper bounds :  1.266332 
##   - variance bounds :  0.005927537 0.6847249 
##   - best initial criterion value(s) :  11.72809 
## 
## N = 2, M = 5 machine precision = 2.22045e-16
## At X0, 0 variables are exactly at the bounds
## At iterate     0  f=      -11.728  |proj g|=      0.65122
## At iterate     1  f =      -12.739  |proj g|=        1.1718
## At iterate     2  f =      -15.085  |proj g|=         1.131
## At iterate     3  f =      -16.741  |proj g|=         1.076
## At iterate     4  f =      -17.133  |proj g|=        1.0378
## At iterate     5  f =       -17.16  |proj g|=       0.47849
## At iterate     6  f =      -17.196  |proj g|=       0.46825
## At iterate     7  f =      -17.288  |proj g|=       0.43185
## At iterate     8  f =      -17.402  |proj g|=       0.37491
## At iterate     9  f =       -17.48  |proj g|=       0.31659
## At iterate    10  f =      -17.521  |proj g|=       0.31868
## At iterate    11  f =      -17.539  |proj g|=       0.32343
## At iterate    12  f =      -17.543  |proj g|=        0.3508
## At iterate    13  f =      -17.547  |proj g|=       0.12156
## At iterate    14  f =      -17.548  |proj g|=      0.012928
## At iterate    15  f =      -17.548  |proj g|=    0.00062453
## At iterate    16  f =      -17.548  |proj g|=    0.00044647
## At iterate    17  f =      -17.548  |proj g|=     4.319e-06
## 
## iterations 17
## function evaluations 20
## segments explored during Cauchy searches 18
## BFGS updates skipped 0
## active bounds at final generalized Cauchy point 0
## norm of the final projected gradient 4.31903e-06
## final function value -17.5477
## 
## F = -17.5477
## final  value -17.547684 
## converged
## 
## optimisation start
## ------------------
## * estimation method   : MLE 
## * optimisation method : BFGS 
## * analytical gradient : used
## * trend model : ~1
## * covariance model : 
##   - type :  matern5_2 
##   - noise variances :
##  [1] 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05
##   - parameters lower bounds :  1e-10 
##   - parameters upper bounds :  1.266332 
##   - variance bounds :  0.00512627 0.5286628 
##   - best initial criterion value(s) :  19.10837 
## 
## N = 2, M = 5 machine precision = 2.22045e-16
## At X0, 0 variables are exactly at the bounds
## At iterate     0  f=      -19.108  |proj g|=       1.2086
## At iterate     1  f =      -23.493  |proj g|=       0.47048
## At iterate     2  f =       -24.95  |proj g|=        1.1012
## At iterate     3  f =      -25.301  |proj g|=        1.0875
## At iterate     4  f =      -25.861  |proj g|=       0.37547
## At iterate     5  f =      -26.118  |proj g|=       0.33435
## At iterate     6  f =        -26.3  |proj g|=       0.28485
## At iterate     7  f =      -26.388  |proj g|=        1.0052
## At iterate     8  f =      -26.505  |proj g|=       0.19187
## At iterate     9  f =      -26.563  |proj g|=        0.3188
## At iterate    10  f =      -26.586  |proj g|=       0.33093
## At iterate    11  f =      -26.603  |proj g|=       0.33965
## At iterate    12  f =      -26.606  |proj g|=       0.14216
## At iterate    13  f =      -26.606  |proj g|=     0.0060768
## At iterate    14  f =      -26.606  |proj g|=    3.4956e-06
## At iterate    15  f =      -26.606  |proj g|=    3.4942e-06
## 
## iterations 15
## function evaluations 26
## segments explored during Cauchy searches 16
## BFGS updates skipped 0
## active bounds at final generalized Cauchy point 1
## norm of the final projected gradient 3.49416e-06
## final function value -26.6058
## 
## F = -26.6058
## final  value -26.605838 
## converged
## 
## optimisation start
## ------------------
## * estimation method   : MLE 
## * optimisation method : BFGS 
## * analytical gradient : used
## * trend model : ~1
## * covariance model : 
##   - type :  matern5_2 
##   - noise variances :
##  [1] 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05
##   - parameters lower bounds :  1e-10 
##   - parameters upper bounds :  1.266332 
##   - variance bounds :  0.00512627 0.5286628 
##   - best initial criterion value(s) :  19.10837 
## 
## N = 2, M = 5 machine precision = 2.22045e-16
## At X0, 0 variables are exactly at the bounds
## At iterate     0  f=      -19.108  |proj g|=       1.2086
## At iterate     1  f =      -23.493  |proj g|=       0.47048
## At iterate     2  f =       -24.95  |proj g|=        1.1012
## At iterate     3  f =      -25.301  |proj g|=        1.0875
## At iterate     4  f =      -25.861  |proj g|=       0.37547
## At iterate     5  f =      -26.118  |proj g|=       0.33435
## At iterate     6  f =        -26.3  |proj g|=       0.28485
## At iterate     7  f =      -26.388  |proj g|=        1.0052
## At iterate     8  f =      -26.505  |proj g|=       0.19187
## At iterate     9  f =      -26.563  |proj g|=        0.3188
## At iterate    10  f =      -26.586  |proj g|=       0.33093
## At iterate    11  f =      -26.603  |proj g|=       0.33965
## At iterate    12  f =      -26.606  |proj g|=       0.14216
## At iterate    13  f =      -26.606  |proj g|=     0.0060768
## At iterate    14  f =      -26.606  |proj g|=    3.4956e-06
## At iterate    15  f =      -26.606  |proj g|=    3.4942e-06
## 
## iterations 15
## function evaluations 26
## segments explored during Cauchy searches 16
## BFGS updates skipped 0
## active bounds at final generalized Cauchy point 1
## norm of the final projected gradient 3.49416e-06
## final function value -26.6058
## 
## F = -26.6058
## final  value -26.605838 
## converged
## 
## optimisation start
## ------------------
## * estimation method   : MLE 
## * optimisation method : BFGS 
## * analytical gradient : used
## * trend model : ~1
## * covariance model : 
##   - type :  matern5_2 
##   - noise variances :
##  [1] 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05
## [14] 1e-05
##   - parameters lower bounds :  1e-10 
##   - parameters upper bounds :  1.266332 
##   - variance bounds :  0.004244466 0.4692356 
##   - best initial criterion value(s) :  27.59962 
## 
## N = 2, M = 5 machine precision = 2.22045e-16
## At X0, 0 variables are exactly at the bounds
## At iterate     0  f=        -27.6  |proj g|=       1.2086
## At iterate     1  f =      -31.883  |proj g|=       0.41914
## At iterate     2  f =      -33.965  |proj g|=        1.0993
## At iterate     3  f =      -34.352  |proj g|=         1.085
## At iterate     4  f =       -35.01  |proj g|=        0.3229
## At iterate     5  f =      -35.284  |proj g|=       0.28619
## At iterate     6  f =      -35.496  |proj g|=        0.2722
## At iterate     7  f =      -35.516  |proj g|=        1.0189
## At iterate     8  f =      -35.793  |proj g|=       0.97443
## At iterate     9  f =      -35.844  |proj g|=       0.31385
## At iterate    10  f =      -35.875  |proj g|=       0.33047
## At iterate    11  f =      -35.893  |proj g|=       0.33429
## At iterate    12  f =      -35.895  |proj g|=       0.11802
## At iterate    13  f =      -35.895  |proj g|=    0.00030535
## At iterate    14  f =      -35.895  |proj g|=    7.9062e-08
## 
## iterations 14
## function evaluations 17
## segments explored during Cauchy searches 15
## BFGS updates skipped 0
## active bounds at final generalized Cauchy point 1
## norm of the final projected gradient 7.9062e-08
## final function value -35.8953
## 
## F = -35.8953
## final  value -35.895250 
## converged
## 
## optimisation start
## ------------------
## * estimation method   : MLE 
## * optimisation method : BFGS 
## * analytical gradient : used
## * trend model : ~1
## * covariance model : 
##   - type :  matern5_2 
##   - noise variances :
##  [1] 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05
## [14] 1e-05
##   - parameters lower bounds :  1e-10 
##   - parameters upper bounds :  1.266332 
##   - variance bounds :  0.004244466 0.4692356 
##   - best initial criterion value(s) :  27.59962 
## 
## N = 2, M = 5 machine precision = 2.22045e-16
## At X0, 0 variables are exactly at the bounds
## At iterate     0  f=        -27.6  |proj g|=       1.2086
## At iterate     1  f =      -31.883  |proj g|=       0.41914
## At iterate     2  f =      -33.965  |proj g|=        1.0993
## At iterate     3  f =      -34.352  |proj g|=         1.085
## At iterate     4  f =       -35.01  |proj g|=        0.3229
## At iterate     5  f =      -35.284  |proj g|=       0.28619
## At iterate     6  f =      -35.496  |proj g|=        0.2722
## At iterate     7  f =      -35.516  |proj g|=        1.0189
## At iterate     8  f =      -35.793  |proj g|=       0.97443
## At iterate     9  f =      -35.844  |proj g|=       0.31385
## At iterate    10  f =      -35.875  |proj g|=       0.33047
## At iterate    11  f =      -35.893  |proj g|=       0.33429
## At iterate    12  f =      -35.895  |proj g|=       0.11802
## At iterate    13  f =      -35.895  |proj g|=    0.00030535
## At iterate    14  f =      -35.895  |proj g|=    7.9062e-08
## 
## iterations 14
## function evaluations 17
## segments explored during Cauchy searches 15
## BFGS updates skipped 0
## active bounds at final generalized Cauchy point 1
## norm of the final projected gradient 7.9062e-08
## final function value -35.8953
## 
## F = -35.8953
## final  value -35.895250 
## converged
```

```r
write.csv(dis,"dis_pre.csv")
write.csv(SD,"OC_pre.csv")
```

