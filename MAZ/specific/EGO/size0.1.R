setwd("C:\\Users\\12758\\Desktop\\manuscript\\target-submit\\code available\\MAZ\\1data\\specific\\num131-size0.1-0.01-final\\EGO")
fn.utility=function(data.training,estimator,training.data,infj,tar_num){
  #Efficient Global optimization
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
  
  if(estimator==2)
  { 
    fn.max.u.ei = function(infj)
    {
      #max.u.ei = infj[,"mean"]
      
      max.u.ei = -abs(infj[,"mean"]- data.training[tar_num,"es"])
      return (max.u.ei)
    }
    EI= fn.max.u.ei(infj)
    EI=as.data.frame(EI)
    EI[which(EI[,]=="NaN"),]=0
    colnames(EI)="EI"
  }
  
  if (estimator==3)
  { 
    fn.ego.ei = function(data.training,training.data,infj,para=  data.training[tar_num,"es"])
    {
      if(tar_num==which(data.training[,"es"]==max(data.training[,"es"])))
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
      }else if(tar_num==which(data.training[,"es"]==min(data.training[,"es"]))){
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

library("DiceKriging")
data.3D=read.csv("MA2Z4_all.csv")
data.3D=na.omit(data.3D)
data.training=data.3D
colnames(data.training)[3]="es"
order.data=as.data.frame(rep(1:nrow(data.training)))
colnames(order.data)="order.num"
data.training=cbind(data.training,order.data)
keep=c("es", "AverW_Boi","Aver_Ros","Max_Homo","Aver_Pc","Min_Lumo")
keep.x=c("AverW_Boi","Aver_Ros","Max_Homo","Aver_Pc","Min_Lumo")
data.training=data.frame(data.training[,keep]) 

data.training_order=data.training[order(-data.training[,"es"]),]
data.training=cbind(data.training_order,order.data)
y_range=max(data.training[,"es"])-min(data.training[,"es"])

tar=c(112)
data.training[,"es"]=abs(data.training[,"es"]-data.training[tar,"es"])



fn.gp = function(training.data.x, training.data.y,virtual.data.x)
{
  set.seed(23)
  noise=rep(0.02*(max(training.data.y)-min(training.data.y)),nrow(training.data.x))
  m<-  km(formula<-~.,design=training.data.x,noise.var = noise, response=training.data.y,covtype = "exp",optim.method="BFGS")
  set.seed(23)
  p <- predict(m, virtual.data.x, "UK")
  sd=as.data.frame(p$sd)
  mean=as.data.frame(p$mean)
  pr=cbind(mean,sd)
  colnames(pr)=c("mean","sd")
  return(pr)
}


train.num=200
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
size.i=c(round(0.1*nrow(data.training)))

iter_total=matrix(,1,6)
iter_mean=matrix(0,train.num,1)
iter_mean_w=matrix(0,train.num,1)
ave.OC=matrix(,nrow(iter)-1,6)
ave.dis=matrix(,nrow(iter)-1,6)
ave.OC_total=matrix(,nrow(iter)-1,6)
ave.dis_total=matrix(,nrow(iter)-1,6)
ave.OC_mean=matrix(0,nrow(iter)-1,1)
ave.dis_mean=matrix(0,nrow(iter)-1,1)
EI.iter=matrix(0,nrow(iter)-1,1)
mea.iter=matrix(0,nrow(iter)-1,1)
pre.iter=matrix(0,nrow(iter)-1,1)

set.seed(11*size.i)
sample.num0<-vector()
for(ii in 1:1000){
  s <-  sample(c(1:106,118:(nrow(data.training))),80,replace=F,prob=NULL) 
  sample.num0<-as.data.frame(rbind(sample.num0,s))
}

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
    for(i in 131:131)
    {
      
      split=sample.num[i,]
      split=unlist(split)
      training.data <- data.training[split,]
      training.data.x <- data.frame(training.data[,keep.x]) 
      training.data.y <-  data.frame(training.data[,"es"]) 
      colnames(training.data.y)="es"
      vir.total.data.x=data.frame(data.training[,keep.x]) 
      data.all=data.frame(data.training) 
     
      virtual.data <- data.training[-split,]
      rownames(virtual.data)<-1:nrow(virtual.data)
      virtual.data.x=as.data.frame(virtual.data[,keep.x])
      gp = fn.gp(training.data.x, training.data.y,virtual.data.x)
      
      EI0= fn.utility(data.training,estimator,training.data,gp,tar_num)
      EI0=as.data.frame(EI0)
      ######
      
      if(min(abs(training.data[,"es"]-data.training[tar_num,"es"]))<=0.01*(y_range))
      {
        SD[c(1:(nrow(iter)-1)),]=0 
        dis[c(1:(nrow(iter)-1)),]=0 
        u=0
      }else{
        
        SD[1,1]= fn.OC(data.training,training.data)
        dis[1,1]= fn.OC(data.training,training.data)
        
        num=min(virtual.data[which(EI0[,"EI"]==max(EI0[,"EI"])),"order.num"])
        EI.iter[1,1]=max(EI0[,"EI"])
        pre.iter[1,1]=gp[which(EI0[,"EI"]==max(EI0[,"EI"])),1]
        mea.iter[1,1]=virtual.data[which(virtual.data[,"order.num"]==num),"es"]
       
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
          training.data0.y <-  data.frame(training.data0[,"es"]) 
          colnames(training.data0.y)="es"
          virtual.data0.x=as.data.frame(virtual.data0[,keep.x])
          gp0 = fn.gp(training.data0.x, training.data0.y,virtual.data0.x)
          
          EI=fn.utility(data.training,estimator,training.data0,gp0,tar_num)
          EI=as.data.frame(EI)
          
          
          num= min(virtual.data0[which(EI[,"EI"]==max(EI[,"EI"])),"order.num"])
          EI.iter[u+1,1]=max(EI[,"EI"])
          mea.iter[u+1,1]=virtual.data0[which(virtual.data0[,"order.num"]==num),"es"]
          pre.iter[u+1,1]=gp[which(EI[,"EI"]==max(EI[,"EI"])),1]
          
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
            if(min(abs(mea.iter[u+1,1]-data.training[tar_num,"es"]))<=0.01*(y_range))
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

  estimator=estimator+1
  if(estimator>1)
    break
}

write.csv(mea.iter,"mea.iter.csv")
write.csv(EI.iter,"EI.iter.csv")
write.csv(pre.iter,"pre.iter.csv")

