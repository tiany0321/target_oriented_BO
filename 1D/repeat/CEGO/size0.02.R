setwd("C:\\Users\\12758\\Desktop\\target_oriented_BO\\1D\\repeat\\CEGO")
start_time<-Sys.time()

library("DiceKriging")

fn.utility=function(data.training,estimator,training.data,infj,tar_num){
  #Constrained EGO
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
  
  #PureExp
  if(estimator==2)
  { 
    fn.max.u.ei = function(infj)
    {
      max.u.ei = -abs(infj[,"mean"]- data.training[tar_num,"es"])
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

fn.gp = function(training.data.x, training.data.y,virtual.data.x){
  set.seed(123)
  noise=rep(10^(-5),nrow(training.data.x))
 # noise=rep(0.02*(max(training.data.y)-min(training.data.y)),nrow(training.data.x))
  m<-  km(formula<-~.,design=training.data.x,noise.var = noise, response=training.data.y)
  set.seed(123)
  p <- predict(m, virtual.data.x, "UK")
  sd=as.data.frame(p$sd)
  mean=as.data.frame(p$mean)
  pr=cbind(mean,sd)
  colnames(pr)=c("mean","sd")
  return(pr)
}

f11_xiong <- function(x){ 
  return( sin(4 * (x - 1.3)^4) * cos( (x - 1.4)) + (x - 0.9) / 2)
}

tar=24
total.data.x <- as.data.frame(seq(0, 1, , 200))
colnames(total.data.x)="x"
y_range=max(f11_xiong(total.data.x))-min(f11_xiong(total.data.x))
total.data.y <- as.data.frame(f11_xiong(total.data.x))
colnames(total.data.y)="es"
order.data=as.data.frame(rep(1:nrow(total.data.x)))
colnames(order.data)="order.num"
data.training=cbind(total.data.x,total.data.y,order.data)
keep.x="x"


train.num=200#200
iter=seq(0,50)


fn.selector = function(EI){
  num=which(EI[,]==max(EI[,]))
  return(num)
}

fn.OC = function(realdata,traindata){
  OC=min(abs(realdata[tar_num,"es"]-traindata[,"es"]))
  return(OC)
}
fn.dis=function(realdata,pre_actual){
  dis=min(abs(realdata[tar_num,"es"]-pre_actual))
  return(dis)
}
iter=as.data.frame(iter)
SD=matrix(0,nrow(iter)-1,1)
dis=matrix(0,nrow(iter)-1,1)
data.OC.SD=matrix(0,nrow(iter)-1,train.num)
data.dis=matrix(0,nrow(iter)-1,train.num)
data.iter.num.max=matrix(,train.num,1)


iter_total=matrix(,1,6)
iter_mean=matrix(0,train.num,1)
iter_mean_w=matrix(0,train.num,1)
ave.OC=matrix(,nrow(iter)-1,6)
ave.dis=matrix(,nrow(iter)-1,6)
ave.OC_total=matrix(,nrow(iter)-1,6)
ave.dis_total=matrix(,nrow(iter)-1,6)
ave.OC_mean=matrix(0,nrow(iter)-1,1)
ave.dis_mean=matrix(0,nrow(iter)-1,1)


set.seed(11)
sample.num0<-vector()
for(ii in 1:1000){
  s <-  sample(1:(nrow(data.training)),40,replace=F,prob=NULL) 
  sample.num0<-as.data.frame(rbind(sample.num0,s))
}

size.i=c(round(0.02*nrow(data.training)))
sample.num<-sample.num0[,1:(size.i)]


estimator=1

repeat{
  SD=matrix(0,nrow(iter)-1,1)
  dis=matrix(0,nrow(iter)-1,1)
  data.OC.SD=matrix(0,nrow(iter)-1,train.num)
  data.dis=matrix(0,nrow(iter)-1,train.num)
  tar_num = tar

  for(size in size.i)
  {
    sample.num<-sample.num0[,1:(size)]
    for(i in 1:train.num)
    {
      
      split=sample.num[i,]
      split=unlist(split)
      training.data <- data.training[split,]
      training.data.x <- data.frame(training.data[,keep.x]) 
      colnames(training.data.x)="x"
      training.data.y <-  data.frame(training.data[,"es"]) 
      colnames(training.data.y)="es"
      vir.total.data.x=data.frame(data.training[,keep.x]) 
      colnames(vir.total.data.x)="x"
      data.all=data.frame(data.training) 
     
      virtual.data <- data.training[-split,]
      rownames(virtual.data)<-1:nrow(virtual.data)
      virtual.data.x=as.data.frame(virtual.data[,keep.x])
      colnames(virtual.data.x)="x"
      gp = fn.gp(training.data.x, training.data.y,virtual.data.x)
      
      EI0= fn.utility(data.training,estimator,training.data,gp,tar_num)
      EI0=as.data.frame(EI0)
      ######
      
      if(min(abs(training.data[,"es"]-data.training[tar_num,"es"]))<=0.01*y_range)
      {
        SD[c(1:(nrow(iter)-1)),]=0 
        dis[c(1:(nrow(iter)-1)),]=0 
        u=0
      }else{
        
        SD[1,1]= fn.OC(data.training,training.data)
        dis[1,1]= fn.OC(data.training,training.data)
        
        num=max(virtual.data[which(EI0[,"EI"]==max(EI0[,"EI"])),"order.num"])
        
        training.data0=training.data
        virtual.data0=virtual.data
        
        ####
        u=1
        repeat{
          training.data0[nrow(training.data0)+1,]=virtual.data0[which(virtual.data0[,"order.num"]==num),]
          virtual.data0=virtual.data0[-which(virtual.data0[,"order.num"]==num),]
          rownames(training.data0)<-1:nrow(training.data0)
          rownames(virtual.data0)<-1:nrow(virtual.data0)
          
          training.data0.x <- data.frame(training.data0[,keep.x])
          colnames(training.data0.x)="x"
          training.data0.y <-  data.frame(training.data0[,"es"]) 
          colnames(training.data0.y)="es"
          virtual.data0.x=as.data.frame(virtual.data0[,keep.x])
          colnames(virtual.data0.x)="x"
          gp0 = fn.gp(training.data0.x, training.data0.y,virtual.data0.x)
          
          EI=fn.utility(data.training,estimator,training.data0,gp0,tar_num)
          EI=as.data.frame(EI)
          
          
          num= max(virtual.data0[which(EI[,"EI"]==max(EI[,"EI"])),"order.num"])
          if(u>=nrow(iter)-1)
            break
          SD[u+1,]=fn.OC(data.training,training.data0)
          dis[u+1,]=fn.dis(data.training, training.data0[nrow(training.data0),"es"])
          
          if(sum(EI)==0||(nrow(unique(EI))==1&nrow(EI)>1))
          {
            SD[c((u+1):(nrow(iter)-1)),]=SD[u+1,]
            dis[c((u+1):(nrow(iter)-1)),]=dis[u+1,]
            break
          }else{ 
            if(min(abs(training.data0[,"es"]-data.training[tar_num,"es"]))<=0.01*y_range)
            {
              if(u>=nrow(iter)-2){
                break
              }else{
                SD[c((u+1):(nrow(iter)-1)),]=0
                dis[c((u+1):(nrow(iter)-1)),]=0
                break
              }
            }
            u=u+1
          }
        }
        data.OC.SD[,i]=SD[,1]
        data.dis[,i]=dis[,1]
        
      } 
      data.iter.num.max[i,1]=u
    } 
  }
  data.iter.num.max=as.data.frame(data.iter.num.max)
  iter_mean=cbind(iter_mean,data.iter.num.max)
  iter_mean_w=cbind(iter_mean_w,data.iter.num.max)
  iter_mean=as.data.frame(iter_mean[,-1])
  iter_total[1,2*(estimator-1)+1]=mean(unlist(iter_mean))
  iter_total[1,2*(estimator-1)+2]=t.test(iter_mean)$conf.int[2]-t.test(iter_mean)$estimate
  
  
  for(j in 1:nrow(data.OC.SD))
  {
    ave.OC[j,2*(estimator)-1]=mean(data.OC.SD[j,])
    if (length(unique(data.OC.SD[j,]))==1)
      ave.OC[j,2*(estimator)]=0 else
        ave.OC[j,2*(estimator)]=t.test(data.OC.SD[j,])$conf.int[2]-t.test(data.OC.SD[j,])$estimate
  }

  
  for(j in 1:nrow(data.dis))
  {
    ave.dis[j,2*(estimator)-1]=mean(data.dis[j,])
    if (length(unique(data.dis[j,]))==1)
      ave.dis[j,2*(estimator)]=0 else
        ave.dis[j,2*(estimator)]=t.test(data.dis[j,])$conf.int[2]-t.test(data.dis[j,])$estimate
  }

  estimator=estimator+1
  if(estimator>1)
    break
}
colnames(ave.dis)=c("cego","cego.sd","pre_value","pre_value.sd","Tego","Tego.sd")
write.csv(ave.dis,"ave.dis.csv")
colnames(ave.OC)=c("cego","cego.sd","pre_value","pre_value.sd","Tego","Tego.sd")
write.csv(ave.OC,"ave.OC.csv")
colnames(iter_total)=c("cego_u","cego_u.sd","pre_value","pre_value.sd","Tego","Tego.sd")
write.csv(iter_total,"iter_total.csv")
write.csv(iter_mean_w,"iter_mean_w.csv")

end_time<- Sys.time()
elapsed_time<-end_time-start_time
cat("运行时长：",elapsed_time,"\n")
write.csv(elapsed_time,"elapsed_time.csv")