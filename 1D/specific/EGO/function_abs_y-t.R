library("DiceKriging")
library(rPref)
library(dplyr)
######km model
fn.gp = function(training.data.x, training.data.y,virtual.data.x){ 
  set.seed(123) 
  noise=rep(10^(-5),nrow(training.data.x))
  m <- km(design=training.data.x, response=training.data.y, noise.var=noise)
  set.seed(123) 
  p <- predict(m, data.frame(virtual.data.x), "SK")
  sd=as.data.frame(p$sd)
  mean=as.data.frame(p$mean)
  pr=cbind(mean,sd)
  colnames(pr)=c("mean","sd")
  return(pr)
}

########Acquisition function
fn.utility=function(data.training,estimator,training.data,infj,tar_num){
 
   #EGO
  if (estimator==1){ 
    fn.ego.ei = function(training.data,infj)
    {
      ego = (min(training.data[,"es"]) - infj[,"mean"])/infj[,"sd"]
      z = ego
      ei.ego = infj[,"sd"]*z*pnorm(z) + infj[,"sd"]*dnorm(z) 
      ei.ego = data.frame(ei.ego)
      return (ei.ego)
    }
    EI= fn.ego.ei(training.data,infj)
    EI[which(EI[,]=="NaN"),]=0
    EI=as.data.frame(EI)
    colnames(EI)="EI"
  }

   #  PI
  if (estimator==2){ 
    fn.PI.ei = function(training.data,infj)
    {
      PI = (min(training.data[,"es"]) - infj[,"mean"])/infj[,"sd"]
      z = PI
      ei.PI = pnorm(z) 
      ei.PI = data.frame(ei.PI)
      return (ei.PI)
    }
    EI= fn.PI.ei(training.data,infj)
    EI[which(EI[,]=="NaN"),]=0
    EI=as.data.frame(EI)
    colnames(EI)="EI"
  }
  
  # LCB
  if (estimator==3){ 
    fn.LCB = function(training.data,infj,f=20,delta=0.1)
    {
      LCB = infj[,"mean"] -(2*f*log10(nrow(training.data)*(nrow(training.data)-size+1)^2*3.14^2/(6*delta)))^0.5*infj[,"sd"]
      #  LCB = infj[,"mean"] -1*infj[,"sd"]
      LCB = data.frame(LCB)
      return (LCB)
    }
    LCB.v= fn.LCB(training.data,infj,f=20,delta=0.1)################parameter delta
    LCB.v=as.data.frame(LCB.v)
    LCB.v[which(LCB.v[,]=="NaN"),]=0
    
    EI=as.data.frame(-LCB.v)
    colnames(EI)="EI"
  }
  
  # EQI
  if (estimator==4){ 
    fn.EQI.ei = function(training.data,infj,eqi.beta=0.75,epsilon=(10^(-5))^0.5)
    {
      s1=infj[,"sd"]^2/(epsilon^2+infj[,"sd"]^2)^(1/2)#
      s2=infj[,"sd"]*epsilon/(epsilon^2+infj[,"sd"]^2)^(1/2)#
      q.min=min(infj[,"mean"]+qnorm(eqi.beta)*infj[,"sd"])#
      
      EQI = (q.min-(infj[,"mean"] + qnorm(eqi.beta)*s2))/s1#
      z=EQI
      ei.EQI = s1*z*pnorm(z) + s1*dnorm(z)  #
      
      ei.EQI = data.frame(ei.EQI)
      return (ei.EQI)
    }
    EI= fn.EQI.ei(training.data,infj)
    EI[which(EI[,]=="NaN"),]=0
    EI=as.data.frame(EI)
    colnames(EI)="EI"
  }
  
  # MOAF
  if (estimator==5){ 
    fn.ego.ei = function(training.data,infj)
    {
      ego = (min(training.data[,"es"]) - infj[,"mean"])/infj[,"sd"]
      z = ego
      ei.ego = infj[,"sd"]*z*pnorm(z) + infj[,"sd"]*dnorm(z) 
      ei.ego = data.frame(ei.ego)
      return (ei.ego)
    }
    ego= fn.ego.ei(training.data,infj)
    ego[which(ego[,]=="NaN"),]=0
    ego=as.data.frame(ego)
    colnames(ego)="ego"
    
    fn.PI.ei = function(training.data,infj)
    {
      PI = (min(training.data[,"es"]) - infj[,"mean"])/infj[,"sd"]
      z = PI
      ei.PI = pnorm(z) 
      ei.PI = data.frame(ei.PI)
      return (ei.PI)
    }
    PI= fn.PI.ei(training.data,infj)
    PI[which(PI[,]=="NaN"),]=0
    PI=as.data.frame(PI)
    colnames(PI)="PI"
    
    fn.LCB = function(training.data,infj,f=20,delta=0.1)
    {
      LCB = infj[,"mean"] -(2*f*log10(nrow(training.data)*(nrow(training.data)-size+1)^2*3.14^2/(6*delta)))^0.5*infj[,"sd"]
      #  LCB = infj[,"mean"] -1*infj[,"sd"]
      LCB = data.frame(LCB)
      return (LCB)
    }
    LCB.v= fn.LCB(training.data,infj,f=20,delta=0.1)################parameter delta
    LCB.v=as.data.frame(LCB.v)
    LCB.v[which(LCB.v[,]=="NaN"),]=0
    
    LCB=as.data.frame(-LCB.v)
    colnames(LCB)="LCB"
    
    M_score=cbind(ego,PI,LCB)
    
    # 计算 Pareto 前沿（目标是最大化）
    pareto_front <- psel(M_score, high(ego) * high(PI) * high(LCB))
    
    # 计算 Pareto 前沿上每个点的目标值的平均值
    pareto_front <- pareto_front %>%
      mutate(mean_value = (ego + PI + LCB) / 3)
    
    # 找到与平均值最接近的点
    middle_point <- pareto_front %>%
      slice(which.min(abs(mean_value - median(mean_value))))
    
    # 输出最中间点的索引
    middle_index <- which(M_score$ego == middle_point$ego &
                            M_score$PI == middle_point$PI &
                            M_score$LCB == middle_point$LCB)
    EI=as.data.frame(rep(0,nrow(infj)))
    colnames(EI)="EI"
    EI[middle_index,]=1
  }
  
  return(EI)
}

#########select the option with the max(EI)
fn.selector = function(EI){
  num=which(EI[,]==max(EI[,]))
  return(num)
}

########OC function: calculate the current minimum distance
fn.OC = function(realdata,traindata){
  OC=min(abs(realdata[tar,"es"]-traindata[,"es"]))
  return(OC)
}

########dis function: calculate the distance between the selected one and the target
fn.dis=function(realdata,pre_actual){
  dis=min(abs(realdata[tar,"es"]-pre_actual))
  return(dis)
}

###########1D function
f11_xiong <- function(x){ 
  return( sin(4 * (x - 1.3)^4) * cos( (x - 1.4)) + (x - 0.9) / 2)
}