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
  #constrained EGO
  if (estimator==1){ 
    fn.tcei = function(data.training,training.data,infj,tar_num)
    {
      #Calculate the current closet distance
      diff_values <- abs(training.data[,"es"] - data.training[tar_num,"es"])
      closest_value <- training.data[which.min(diff_values),"es"]
      
      #give constrain
      upper_bound = data.training[tar_num,"es"]+abs(min(diff_values))
      lower_bound = data.training[tar_num,"es"]-abs(min(diff_values))
      
      # Calculate EI, the current best is set as upper_bound
      z = (upper_bound - infj[,"mean"])/infj[,"sd"]
      ei = infj[,"sd"]*z*pnorm(z) + infj[,"sd"]*dnorm(z) 
      
      # Calculate feasibility probability
      p_feasible <- pnorm((upper_bound - infj[,"mean"]) / infj[,"sd"]) - pnorm((lower_bound - infj[,"mean"]) / infj[,"sd"])
      
      # Calculate tcEI
      tcEI <- ei * p_feasible
      
      ei.ego = data.frame(tcEI)
      return (ei.ego)
    }
    EI= fn.tcei(data.training,training.data,infj,tar_num)
    EI[which(EI[,]=="NaN"),]=0
    EI=as.data.frame(EI)
    colnames(EI)="EI"
  }
  
  #max u
  if(estimator==2)
  { 
    fn.max.u.ei = function(infj)
    {
      #max.u.ei = infj[,"mean"]
      
      max.u.ei = abs(infj[,"mean"]- data.training[tar_num,"es"])
      return (max.u.ei)
    }
    EI= fn.max.u.ei(infj)
    EI=as.data.frame(EI)
    EI[which(EI[,]=="NaN"),]=0
    colnames(EI)="EI"
  }
  
  #t-EGO
  if (estimator==3)
  { 
    fn.ego.ei = function(data.training,training.data,infj,para=  data.training[tar_num,"es"])
    {
      if(tar_num%in%which(data.training[,"es"]==max(data.training[,"es"])))
      {
        para=Inf
        yit=max(training.data[,"es"])
        w1=((2*para-yit) - infj[,"mean"])/infj[,"sd"]
        w2=(infj[,"mean"] - yit  )/infj[,"sd"]
        w3=( para - infj[,"mean"] )/infj[,"sd"]
        w4=( infj[,"mean"] - para )/infj[,"sd"]
        
        # w1_ego=infj[,"sd"]*w1*pnorm(w1) + infj[,"sd"]*dnorm(w1) 
        w2_ego=infj[,"sd"]*w2*pnorm(w2) + infj[,"sd"]*dnorm(w2) 
        #  w3_ego=infj[,"sd"]*w1*pnorm(w3) + infj[,"sd"]*dnorm(w3) 
        #   w4_ego=infj[,"sd"]*w2*pnorm(w4) + infj[,"sd"]*dnorm(w4) 
        EI=w2_ego
        #ei.ego=w1_ego-w3_ego+w2_ego-w4_ego
        # print((para-min(abs(training.data[,"es"]-para)))%in%training.data[,"es"])
      }else if(tar_num%in%which(data.training[,"es"]==min(data.training[,"es"]))){
        {
          para=Inf
          yit=min(training.data[,"es"])
          w1=( yit - infj[,"mean"] )/infj[,"sd"]
          w2=(infj[,"mean"] - (2*para-yit))/infj[,"sd"]
          w3=( para - infj[,"mean"] )/infj[,"sd"]
          w4=( infj[,"mean"] - para )/infj[,"sd"]
          
          # w1_ego=infj[,"sd"]*w1*pnorm(w1) + infj[,"sd"]*dnorm(w1) 
          w1_ego=infj[,"sd"]*w1*pnorm(w1) + infj[,"sd"]*dnorm(w1) 
          #  w3_ego=infj[,"sd"]*w1*pnorm(w3) + infj[,"sd"]*dnorm(w3) 
          #   w4_ego=infj[,"sd"]*w2*pnorm(w4) + infj[,"sd"]*dnorm(w4) 
          EI=w1_ego
          #ei.ego=w1_ego-w3_ego+w2_ego-w4_ego
          # print((para-min(abs(training.data[,"es"]-para)))%in%training.data[,"es"])
        }
      }else{
        if(signif((min(abs(training.data[,"es"]-para))+para),10) %in% signif(training.data[,"es"],10))
        {
          yit=signif((min(abs(training.data[,"es"]-para))+para),10)
          w1=( yit - infj[,"mean"] )/infj[,"sd"]
          w2=(infj[,"mean"] - (2*para-yit))/infj[,"sd"]
          w3=( para - infj[,"mean"] )/infj[,"sd"]
          w4=( infj[,"mean"] - para )/infj[,"sd"]
          
          w1_ego=infj[,"sd"]*w1*pnorm(w1) + infj[,"sd"]*dnorm(w1) 
          w2_ego=infj[,"sd"]*w2*pnorm(w2) + infj[,"sd"]*dnorm(w2) 
          w3_ego=infj[,"sd"]*w1*pnorm(w3) + infj[,"sd"]*dnorm(w3) 
          w4_ego=infj[,"sd"]*w2*pnorm(w4) + infj[,"sd"]*dnorm(w4) 
          
          EI=w1_ego-w3_ego+w2_ego-w4_ego
          # print((min(abs(training.data[,"es"]-para))+para)%in%training.data[,"es"])
        }
        if(signif((para-min(abs(training.data[,"es"]-para))),10) %in% signif(training.data[,"es"],10))
        {
          yit=signif((para-min(abs(training.data[,"es"]-para))),10)
          w1=((2*para-yit) - infj[,"mean"])/infj[,"sd"]
          w2=(infj[,"mean"] - yit  )/infj[,"sd"]
          w3=( para - infj[,"mean"] )/infj[,"sd"]
          w4=( infj[,"mean"] - para )/infj[,"sd"]
          
          w1_ego=infj[,"sd"]*w1*pnorm(w1) + infj[,"sd"]*dnorm(w1) 
          w2_ego=infj[,"sd"]*w2*pnorm(w2) + infj[,"sd"]*dnorm(w2) 
          w3_ego=infj[,"sd"]*w1*pnorm(w3) + infj[,"sd"]*dnorm(w3) 
          w4_ego=infj[,"sd"]*w2*pnorm(w4) + infj[,"sd"]*dnorm(w4) 
          
          EI=w1_ego-w3_ego+w2_ego-w4_ego
          # print((para-min(abs(training.data[,"es"]-para)))%in%training.data[,"es"])
        }
      }
      ei.ego = data.frame(EI)
      return (ei.ego)
    }
    EI= fn.ego.ei(data.training,training.data,infj,para=  data.training[tar_num,"es"])
    EI[which(EI[,]=="NaN"),]=0
    colnames(EI)="EI"
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