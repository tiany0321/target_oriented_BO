setwd("C:\\Users\\12758\\Desktop\\manuscript\\target-submit\\code available\\SMA\\SMA prediction")

source("utility function.R")
library("DiceKriging")

fn.gp = function(training.data.x, training.data.y,virtual.data.x)
{ 
  set.seed(23)
  noise=rep(10^(-5),nrow(training.data.x))
  m<-  km(formula<-~.,design=training.data.x, response=training.data.y , noise.var=noise, gr = TRUE,covtype = "exp")
  set.seed(23)
  p <- predict(m, virtual.data.x, "UK")
  sd=as.data.frame(p$sd)
  mean=as.data.frame(p$mean)
  pr=cbind(mean,sd)
  colnames(pr)=c("mean","sd")
  return(pr)
}


data.training=read.csv("traindata_scale.csv")
data.training0=data.training

data.training_fea=data.training0[,c("anum","energy1","YM")]

training.data.x=data.training_fea
training.data.y=as.data.frame(data.training[,"hp"])
colnames(training.data.y)="es"
data.train=cbind(training.data.x,training.data.y)

keep=c("anum","energy1","YM")

vir=read.csv("virtualdata_scale.csv")
vir=vir[,-1]

data=vir


data.vir_fea=data[,c("anum","energy1","YM")]

gp_result=fn.gp(training.data.x, training.data.y,data.vir_fea)

tar=c(440)
i=1
for(tar_num in tar) 
{
  pre=cbind(vir,gp_result)
  for(estimator in 2:3)
  {
    EI=fn.utility(estimator,data.train,gp_result,tar_num)
    EI=as.data.frame(EI)
    pre=cbind(pre,EI)
  }
  colnames(pre)[20]="Pure_exploitation"
  colnames(pre)[21]="T_EGO"
  write.csv(pre,paste0(tar[i],".",sep="csv"))
  i=i+1
}
data=read.csv("440.csv")
da=data[,2:12]

data0=data[order(-data[,"T_EGO"]),]
data0=data0[1:10000,]
write.csv(data0,"440_T_EGO.csv")
data0=data[order(-data[,"Pure_exploitation"]),]
data0=data0[1:10000,]
write.csv(data0,"440_Pure exploitation.csv")
