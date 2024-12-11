fn.utility=function(estimator,training.data,infj,tar_num){
  
  ###pure exploitation
  if(estimator==2)
  { 
    fn.max.u.ei = function(infj)
    {
      max.u.ei = -abs(infj[,"mean"]- tar_num)
      return (max.u.ei)
    }
    EI= fn.max.u.ei(infj)
    EI=as.data.frame(EI)
    EI[which(EI[,]=="NaN"),]=0
    colnames(EI)="EI"
  }
  ###T-EGO
  if (estimator==3)
  { 
    fn.ego.ei = function(training.data,infj,para= tar_num)
    {
      
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
      }
      
      ei.ego = data.frame(EI)
      return (ei.ego)
    }
    EI= fn.ego.ei(training.data,infj,para=tar_num)
    EI[which(EI[,]=="NaN"),]=0
    colnames(EI)="EI"
  }
  return(EI)
}
