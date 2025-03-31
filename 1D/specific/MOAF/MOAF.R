setwd("C:\\Users\\12758\\Desktop\\target_oriented_BO\\1D\\specific\\MOAF")
source("function_abs_y-t.R")

estimator=5########MOAF

##############create data
total.data.x <- as.data.frame(seq(0, 1, , 200))
colnames(total.data.x)="x"
write.csv(total.data.x,"total.data.x.csv")

total.data.y <- as.data.frame(f11_xiong(total.data.x))
colnames(total.data.y)="es"
order.data=as.data.frame(rep(1:nrow(total.data.x)))
colnames(order.data)="order.num"
data.training=cbind(total.data.x,total.data.y,order.data)

############Set a target
tar=24
tar.points=data.training[which(abs(data.training[,"es"]-data.training[tar,"es"])<=0.01*(max(data.training[,"es"])-min(data.training[,"es"]))),]
tar_data=data.training[tar,"es"]
data.training$pro.es=abs(data.training$es-tar_data)
write.csv(data.training$pro.es,"total.data.y.csv")
tar_data=min(data.training$pro.es)
write.csv(tar_data,"tar_data.csv")

print(paste("The targeted y:", tar_data))

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
training.data.y <-  data.frame(training.data[,"pro.es"]) 
colnames(training.data.y)="pro.es"

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
gp.p = fn.gp(training.data.x, training.data.y,vir.total.data.x)
mean=as.data.frame(gp.p[,1])
mean=unlist(mean)
t=total.data.x
t=unlist(t)
yy0=cbind(total.data.x,as.data.frame(mean),as.data.frame(gp.p[,1]-gp.p[,2]),as.data.frame(gp.p[,1]+gp.p[,2]))
colnames(yy0)=c("x","mean","lower","upper")
write.csv(yy0,"yy0.csv")
head(yy0)

##########data frame
data.OC.SD=matrix(,length(iter)-1,length(selector))
data.dis=matrix(,length(iter)-1,length(selector))
EI=as.data.frame(EI)
SD=matrix(0,length(iter),1)
dis=matrix(0,length(iter),1)

########utility value
EI= fn.utility(data.training,estimator,training.data,gp.p)
EI[split,]=0
EI=na.omit(EI)
ee0=cbind(total.data.x,EI)
write.csv(ee0,"ee0.csv")

SD[1,1]= fn.OC(data.training,training.data)
dis[1,1]= fn.OC(data.training,training.data)

######recommend the option
num= which(EI[,"EI"]==max(EI[,"EI"]))

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
  training.data0.y <-  data.frame(training.data0[,"pro.es"]) 
  colnames(training.data0.y)="pro.es"
  virtual.data0.x=as.data.frame(virtual.data0[,"x"])
  colnames(virtual.data0.x)="x"
  
  ##########km model prediction
  gp0 = fn.gp(training.data0.x, training.data0.y,virtual.data0.x)
  gp0.p = fn.gp(training.data0.x, training.data0.y,vir.total.data.x)
  
  ########utility value
  EI=fn.utility(data.training,estimator,training.data0,gp0.p)
  EI=as.data.frame(EI)
  EI[num.count,]=0
  EI=na.omit(EI)
  
  ######recommend the option
  num= which(EI[,"EI"]==max(EI[,"EI"]))

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
    print("training.data:")
    print(training.data0)
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

write.csv(dis,"dis_pre.csv")
write.csv(SD,"OC_pre.csv")


