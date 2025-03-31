knitr::opts_chunk$set(fig.width = 10, fig.height = 5.5,out.width="100%", dpi = 300)
#print(knitr::opts_chunk$get("fig.width"))  # 
#print(knitr::opts_chunk$get("fig.height"))  # 

setwd("C:\\Users\\12758\\Desktop\\target_oriented_BO\\1D\\specific\\t-EGO")
library(RColorBrewer)

###########Read data
aa0=read.csv("yy0.csv")
aa1=read.csv("1.csv")
aa5=read.csv("2.csv")
aa15=read.csv("3.csv")
aa25=read.csv("4.csv")
aa50=read.csv("5.csv")
bb0=read.csv("ee0.csv")
bb1=read.csv("1ee.csv")
bb5=read.csv("2ee.csv")
bb15=read.csv("3ee.csv")
bb25=read.csv("4ee.csv")
bb50=read.csv("5ee.csv")
cc0=read.csv("0train.csv")
cc1=read.csv("1train.csv")
cc2=read.csv("2train.csv")
cc3=read.csv("3train.csv")
cc4=read.csv("4train.csv")
cc5=read.csv("5train.csv")
cc6=read.csv("6train.csv")
tar_data=read.csv("tar_data.csv", row.names = 1)
total.data.x=read.csv("total.data.x.csv", row.names = 1)
total.data.y=read.csv("total.data.y.csv", row.names = 1)

ym=0.16########ylim of utility plot


#display.brewer.all(type = "seq")
#pdf("T-EGO.pdf",width=11.7,height=6)
#Plot 1-1
par(mfrow = c(2,6),mai=c(0.07,0.05,0.07,0.05),omi=c(1.5,0.3,1.5,0.1))
plot(aa0[,"x"], aa0[,"mean"],xaxt="n", type="l",  ylab="y",xlab="x",col="black", lwd=1.5, tck=0.01,font=2,font.lab=2,ylim=c(-1,0.65),cex.axis=1.2,cex.lab=1.2)
polygon(c(aa0[,"x"],rev(aa0[,"x"])), c(aa0[,"lower"], rev(aa0[,"upper"])), col=brewer.pal(9, "Purples")[3], border = brewer.pal(9, "Purples")[3])
lines(aa0[,"x"], aa0[,"mean"], type="l",  xlab="x", ylab="y", col=brewer.pal(9, "Purples")[8], lwd=1.5)
text(x = 0.93, y = -0.9, labels = "#=0",cex = 1.4,font=1.7 )
text(x = 0.5, y = -0.9, labels = "t-EGO",cex = 1.4,font=1.7 )
par(new=T)
plot(unlist(total.data.x),unlist(total.data.y), xaxt="n",type="l", lty=2, ylab="y",xlab="x",col="red", lwd=1, tck=0.01,font=2,font.lab=2,ylim=c(-1,0.65),cex.axis=1.2,cex.lab=1.2)
points(unlist(cc0[,2]),unlist(cc0[,3]), pch=20,col=brewer.pal(9, "Oranges")[5],cex=1.7)
abline(h=unlist(tar_data),type="l", lty=3, ylab="y",xlab="x",col=brewer.pal(9, "Oranges")[8], lwd=1, tck=0.01,font=2,font.lab=2,ylim=c(-1,0.65),cex.axis=1.2,cex.lab=1.2)

#Plot 1-2
plot(aa1[,"x"], aa1[,"mean"],xaxt="n", yaxt="n", type="l",  ylab="y",xlab="x",col="blue", lwd=1.5, tck=0.01,font=2,font.lab=2,ylim=c(-1,0.65),cex.axis=1.2,cex.lab=1.2)
polygon(c(aa1[,"x"],rev(aa1[,"x"])), c(aa1[,"lower"], rev(aa1[,"upper"])), col=brewer.pal(9, "Purples")[3], border = brewer.pal(9, "Purples")[3])
lines(aa1[,"x"], aa1[,"mean"], type="l",  xlab="x", ylab="y", col=brewer.pal(9, "Purples")[8], lwd=1.5)
text(x = 0.93, y = -0.9, labels = "#=1",cex = 1.4,font=1.7 )
par(new=T)
plot(unlist(total.data.x),unlist(total.data.y), xaxt="n", yaxt="n",type="l", lty=2, ylab="y",xlab="x",col="red", lwd=1, tck=0.01,font=2,font.lab=2,ylim=c(-1,0.65),cex.axis=1.2,cex.lab=1.2)
points(unlist(cc0[,2]),unlist(cc0[,3]), pch=20,col=brewer.pal(9, "Oranges")[5],cex=1.7)
points(unlist(cc1[5,2]),unlist(cc1[5,3]), pch=20,col="red",cex=1.7)
abline(h=unlist(tar_data),type="l", lty=3, ylab="y",xlab="x",col=brewer.pal(9, "Oranges")[8], lwd=1, tck=0.01,font=2,font.lab=2,ylim=c(-1,0.65),cex.axis=1.2,cex.lab=1.2)

#Plot 1-3
plot(aa5[,"x"], aa5[,"mean"],xaxt="n", yaxt="n", type="l",  ylab="y",xlab="x",col="blue", lwd=1.5, tck=0.01,font=2,font.lab=2,ylim=c(-1,0.65),cex.axis=1.2,cex.lab=1.2)
polygon(c(aa5[,"x"],rev(aa5[,"x"])), c(aa5[,"lower"], rev(aa5[,"upper"])), col=brewer.pal(9, "Purples")[3], border = brewer.pal(9, "Purples")[3])
lines(aa5[,"x"], aa5[,"mean"], type="l",  xlab="x", ylab="y", col=brewer.pal(9, "Purples")[8], lwd=1.5)
text(x = 0.93, y = -0.9, labels = "#=2",cex = 1.4,font=1.7 )
par(new=T)
plot(unlist(total.data.x),unlist(total.data.y), xaxt="n", yaxt="n",type="l", lty=2, ylab="y",xlab="x",col="red", lwd=1, tck=0.01,font=2,font.lab=2,ylim=c(-1,0.65),cex.axis=1.2,cex.lab=1.2)
points(unlist(cc1[,2]),unlist(cc1[,3]), pch=20,col=brewer.pal(9, "Oranges")[5],cex=1.7)
points(unlist(cc2[6,2]),unlist(cc2[6,3]), pch=20,col="red",cex=1.7)
abline(h=unlist(tar_data),type="l", lty=3, ylab="y",xlab="x",col=brewer.pal(9, "Oranges")[8], lwd=1, tck=0.01,font=2,font.lab=2,ylim=c(-1,0.65),cex.axis=1.2,cex.lab=1.2)

#Plot 1-4
plot(aa15[,"x"], aa15[,"mean"], xaxt="n", yaxt="n",type="l",  ylab="y",xlab="x",col="blue", lwd=1.5, tck=0.01,font=2,font.lab=2,ylim=c(-1,0.65),cex.axis=1.2,cex.lab=1.2)
polygon(c(aa15[,"x"],rev(aa15[,"x"])), c(aa15[,"lower"], rev(aa15[,"upper"])), col=brewer.pal(9, "Purples")[3], border = brewer.pal(9, "Purples")[3])
lines(aa15[,"x"], aa15[,"mean"], type="l",  xlab="x", ylab="y", col=brewer.pal(9, "Purples")[8], lwd=1.5)
text(x = 0.93, y = -0.9, labels = "#=3",cex = 1.4,font=1.7 )
par(new=T)
plot(unlist(total.data.x),unlist(total.data.y), xaxt="n", yaxt="n",type="l", lty=2, ylab="y",xlab="x",col="red", lwd=1, tck=0.01,font=2,font.lab=2,ylim=c(-1,0.65),cex.axis=1.2,cex.lab=1.2)
points(unlist(cc2[,2]),unlist(cc2[,3]), pch=20,col=brewer.pal(9, "Oranges")[5],cex=1.7)
points(unlist(cc3[7,2]),unlist(cc3[7,3]), pch=20,col="red",cex=1.7)
abline(h=unlist(tar_data),type="l", lty=3, ylab="y",xlab="x",col=brewer.pal(9, "Oranges")[8], lwd=1, tck=0.01,font=2,font.lab=2,ylim=c(-1,0.65),cex.axis=1.2,cex.lab=1.2)

#Plot 1-5
plot(aa25[,"x"], aa25[,"mean"],xaxt="n", yaxt="n", type="l",  ylab="y",xlab="x",col="blue", lwd=1.5, tck=0.01,font=2,font.lab=2,ylim=c(-1,0.65),cex.axis=1.2,cex.lab=1.2)
polygon(c(aa25[,"x"],rev(aa25[,"x"])), c(aa25[,"lower"], rev(aa25[,"upper"])), col=brewer.pal(9, "Purples")[3], border = brewer.pal(9, "Purples")[3])
lines(aa25[,"x"], aa25[,"mean"], type="l",  xlab="x", ylab="y", col=brewer.pal(9, "Purples")[8], lwd=1.5)
text(x = 0.93, y = -0.9,labels = "#=4",cex = 1.4,font=1.7 )
par(new=T)
plot(unlist(total.data.x),unlist(total.data.y), xaxt="n", yaxt="n",type="l", lty=2, ylab="y",xlab="x",col="red", lwd=1, tck=0.01,font=2,font.lab=2,ylim=c(-1,0.65),cex.axis=1.2,cex.lab=1.2)
points(unlist(cc3[,2]),unlist(cc3[,3]), pch=20,col=brewer.pal(9, "Oranges")[5],cex=1.7)
points(unlist(cc4[8,2]),unlist(cc4[8,3]), pch=20,col="red",cex=1.7)
abline(h=unlist(tar_data),type="l", lty=3, ylab="y",xlab="x",col=brewer.pal(9, "Oranges")[8], lwd=1, tck=0.01,font=2,font.lab=2,ylim=c(-1,0.65),cex.axis=1.2,cex.lab=1.2)

#Plot 1-6
plot(aa50[,"x"], aa50[,"mean"],xaxt="n", yaxt="n", type="l",  ylab="y",xlab="x",col="blue", lwd=1.5, tck=0.01,font=2,font.lab=2,ylim=c(-1,0.65),cex.axis=1.2,cex.lab=1.2)
polygon(c(aa50[,"x"],rev(aa50[,"x"])), c(aa50[,"lower"], rev(aa50[,"upper"])), col=brewer.pal(9, "Purples")[3], border = brewer.pal(9, "Purples")[3])
lines(aa50[,"x"], aa50[,"mean"], type="l",  xlab="x", ylab="y", col=brewer.pal(9, "Purples")[8], lwd=1.5)
text(x = 0.93, y = -0.9, labels = "#=5",cex = 1.4,font=1.7 )
par(new=T)
plot(unlist(total.data.x),unlist(total.data.y), xaxt="n", yaxt="n",type="l", lty=2, ylab="y",xlab="x",col="red", lwd=1, tck=0.01,font=2,font.lab=2,ylim=c(-1,0.65),cex.axis=1.2,cex.lab=1.2)
points(unlist(cc5[,2]),unlist(cc5[,3]), pch=20,col=brewer.pal(9, "Oranges")[5],cex=1.7)
points(unlist(cc6[9,2]),unlist(cc6[9,3]), pch=20,col="red",cex=1.7)
abline(h=unlist(tar_data),type="l", lty=3, ylab="y",xlab="x",col=brewer.pal(9, "Oranges")[8], lwd=1, tck=0.01,font=2,font.lab=2,ylim=c(-1,0.65),cex.axis=1.2,cex.lab=1.2)

#Plot 2-1
plot(bb0[,2],bb0[,3],type="l", xlab="x", ylab="y",col="purple",ylim=c(0,ym), lwd=1,tck=0.01,font=2,font.lab=2,cex.axis=1.2,cex.lab=1.2)
points(bb0[which(bb0[,3]==max(bb0[,3])),2],max(bb0[,3])+0.1*ym,pch=25,bg="red") 
polygon(c(bb0[,2],rev(bb0[,2])), c(rep(0,nrow(bb0)), rev(bb0[,3])), col=brewer.pal(9, "Greens")[2], border = brewer.pal(9, "Greens")[2])
par(new=T)
plot(bb0[,2],bb0[,3],type="l", xlab="x", ylab="y",col=brewer.pal(9, "Greens")[8],ylim=c(0,ym), lwd=1,tck=0.01,font=2,font.lab=2,cex.axis=1.2,cex.lab=1.2)

#Plot 2-2
plot(bb1[,2],bb1[,3],xaxt="n", yaxt="n",type="l", xlab="x", ylab="y",col="purple",ylim=c(0,ym), lwd=1,tck=0.01,font=2,font.lab=2,cex.axis=1.2,cex.lab=1.2)
points(bb1[which(bb1[,3]==max(bb1[,3])),2],max(bb1[,3])+0.1*ym,pch=25,bg="red") 
polygon(c(bb1[,2],rev(bb1[,2])), c(rep(0,nrow(bb1)), rev(bb1[,3])), col=brewer.pal(9, "Greens")[2], border = brewer.pal(9, "Greens")[2])
par(new=T)
plot(bb1[,2],bb1[,3],xaxt="n", yaxt="n",type="l", xlab="x", ylab="y",col=brewer.pal(9, "Greens")[8],ylim=c(0,ym), lwd=1,tck=0.01,font=2,font.lab=2,cex.axis=1.2,cex.lab=1.2)

#Plot 2-3
plot(bb5[,2],bb5[,3],xaxt="n", yaxt="n",type="l", xlab="x", ylab="y",col="purple",ylim=c(0,ym), lwd=1,tck=0.01,font=2,font.lab=2,cex.axis=1.2,cex.lab=1.2)
points(bb5[which(bb5[,3]==max(bb5[,3])),2],max(bb5[,3])+0.1*ym,pch=25,bg="red") 
polygon(c(bb5[,2],rev(bb5[,2])), c(rep(0,nrow(bb5)), rev(bb5[,3])), col=brewer.pal(9, "Greens")[2], border = brewer.pal(9, "Greens")[2])
par(new=T)
plot(bb5[,2],bb5[,3],xaxt="n", yaxt="n",type="l", xlab="x", ylab="y",col=brewer.pal(9, "Greens")[8],ylim=c(0,ym), lwd=1,tck=0.01,font=2,font.lab=2,cex.axis=1.2,cex.lab=1.2)

#Plot 2-4
plot(bb15[,2],bb15[,3],xaxt="n", yaxt="n",type="l", xlab="x", ylab="y",col="purple",ylim=c(0,ym), lwd=1,tck=0.01,font=2,font.lab=2,cex.axis=1.2,cex.lab=1.2)
points(bb15[which(bb15[,3]==max(bb15[,3])),2],max(bb15[,3])+0.1*ym,pch=25,bg="red") 
polygon(c(bb15[,2],rev(bb15[,2])), c(rep(0,nrow(bb15)), rev(bb15[,3])), col=brewer.pal(9, "Greens")[2], border = brewer.pal(9, "Greens")[2])
par(new=T)
plot(bb15[,2],bb15[,3],xaxt="n", yaxt="n",type="l", xlab="x", ylab="y",col=brewer.pal(9, "Greens")[8],ylim=c(0,ym), lwd=1,tck=0.01,font=2,font.lab=2,cex.axis=1.2,cex.lab=1.2)

#Plot 2-5
plot(bb25[,2],bb25[,3],xaxt="n", yaxt="n",type="l", xlab="x", ylab="y",col="purple",ylim=c(0,ym), lwd=1,tck=0.01,font=2,font.lab=2,cex.axis=1.2,cex.lab=1.2)
points(bb25[which(bb25[,3]==max(bb25[,3])),2],max(bb25[,3])+0.1*ym,pch=25,bg="red") 
polygon(c(bb25[,2],rev(bb25[,2])), c(rep(0,nrow(bb25)), rev(bb25[,3])), col=brewer.pal(9, "Greens")[2], border = brewer.pal(9, "Greens")[2])
par(new=T)
plot(bb25[,2],bb25[,3],xaxt="n", yaxt="n",type="l", xlab="x", ylab="y",col=brewer.pal(9, "Greens")[8],ylim=c(0,ym), lwd=1,tck=0.01,font=2,font.lab=2,cex.axis=1.2,cex.lab=1.2)

#Plot 2-6
plot(bb50[,2],bb50[,3],xaxt="n", yaxt="n",type="l", xlab="x", ylab="y",col="purple",ylim=c(0,ym), lwd=1,tck=0.01,font=2,font.lab=2,cex.axis=1.2,cex.lab=1.2)
points(bb50[which(bb50[,3]==max(bb50[,3])),2],max(bb50[,3])+0.1*ym,pch=25,bg="red") 
polygon(c(bb50[,2],rev(bb50[,2])), c(rep(0,nrow(bb50)), rev(bb50[,3])), col=brewer.pal(9, "Greens")[2], border = brewer.pal(9, "Greens")[2])
par(new=T)
plot(bb50[,2],bb50[,3],xaxt="n", yaxt="n",type="l", xlab="x", ylab="y",col=brewer.pal(9, "Greens")[8],ylim=c(0,ym), lwd=1,tck=0.01,font=2,font.lab=2,cex.axis=1.2,cex.lab=1.2)
#dev.off()


####################################t-EI varies as the iteration

par(mfrow = c(1,3),mai=c(0.07,0.8,0.07,0.05),omi=c(1.5,0.3,1.5,0.1))

###################iteration 1
# Draw the mean curve and confidence interval.
plot(aa0[,"x"], aa0[,"mean"], type="l", ylab="y", xlab="x", col="black", lwd=1.5, tck=0.01, font=2, font.lab=2, ylim=c(-1, 0.65), cex.axis=1.2, cex.lab=1.2)
polygon(c(aa0[,"x"], rev(aa0[,"x"])), c(aa0[,"lower"], rev(aa0[,"upper"])), col=brewer.pal(9, "Purples")[3], border = brewer.pal(9, "Purples")[3])
lines(aa0[,"x"], aa0[,"mean"], type="l", xlab="x", ylab="y", col=brewer.pal(9, "Purples")[8], lwd=1.5)

# Add text labels.
text(x = 0.93, y = -0.9, labels = "#=0", cex = 1.4, font=1.7)
text(x = 0.5, y = -0.9, labels = "t-EGO", cex = 1.4, font=1.7)

# Plot Gaussian distribution
sel=bb0[which(bb0[,3]==max(bb0[,3])),2]
sel.num=which(bb0[,3]==max(bb0[,3]))
y_gaussian <- seq(-1, 0.65, length.out = 100)  # The y-range of the Gaussian distribution.
mean_gaussian <- aa0[sel.num,"mean"]  # The mean (central position) of the Gaussian distribution.
sd_gaussian <- 2*(aa0[sel.num,"mean"]-aa0[sel.num,"lower"])  #The standard deviation of the Gaussian distribution.
x_gaussian <- dnorm(y_gaussian, mean = mean_gaussian, sd = sd_gaussian)  # The probability density function of the Gaussian distribution.
x_gaussian <- x_gaussian / max(x_gaussian) * 0.2  # 

# 
lines(x_gaussian + sel, y_gaussian, col="blue", lwd=1.5, lty=2)  # 

# Add shading to a portion of the Gaussian distribution.
up=which.min(abs(unlist(cc0[,3])-unlist(tar_data)))
Dis_min=abs(cc0[up,3]-tar_data[,1])
y_shaded <- seq(tar_data[,1]-Dis_min, tar_data[,1]+Dis_min, length.out = 50)   # limits
x_shaded <- dnorm(y_shaded, mean = mean_gaussian, sd = sd_gaussian)  # Calculate the Gaussian distribution value of the shaded region.
x_shaded <- x_shaded / max(x_shaded) * 0.2  # 调整宽度
polygon(c(x_shaded + sel, rep(sel, length(y_shaded))), c(y_shaded, rev(y_shaded)), col=rgb(0, 0, 1, 0.3), border = NA)  #

# 
par(new=T)
plot(unlist(total.data.x), unlist(total.data.y), xaxt="n", type="l", lty=2, ylab="y", xlab="x", col="red", lwd=1, tck=0.01, font=2, font.lab=2, ylim=c(-1, 0.65), cex.axis=1.2, cex.lab=1.2)
points(unlist(cc0[,2]), unlist(cc0[,3]), pch=20, col=brewer.pal(9, "Oranges")[5], cex=1.7)
abline(h=unlist(tar_data), type="l", lty=3, ylab="y", xlab="x", col=brewer.pal(9, "Oranges")[8], lwd=1, tck=0.01, font=2, font.lab=2, ylim=c(-1, 0.65), cex.axis=1.2, cex.lab=1.2)
abline(h=tar_data[,1]+Dis_min, type="l", lty=3, ylab="y", xlab="x", col=brewer.pal(9, "Oranges")[8], lwd=1, tck=0.01, font=2, font.lab=2, ylim=c(-1, 0.65), cex.axis=1.2, cex.lab=1.2)
abline(h=tar_data[,1]-Dis_min, type="l", lty=3, ylab="y", xlab="x", col=brewer.pal(9, "Oranges")[8], lwd=1, tck=0.01, font=2, font.lab=2, ylim=c(-1, 0.65), cex.axis=1.2, cex.lab=1.2)
text(x = 0.02, y = tar_data[,1]+0.035, labels = "t", cex = 1.4, font=1.7)
text(x = 0.04, y = tar_data[,1]+Dis_min+0.035, expression(y[t.min]), cex = 1.4, font=1.7)
text(x = 0.08, y = tar_data[,1]-Dis_min+0.035, expression(t-y[t.min]),  cex = 1.4, font=1.7)





###################iteration 2
# Draw the mean curve and confidence interval.
plot(aa1[,"x"], aa1[,"mean"], type="l",  ylab="y",xlab="x",col="blue", lwd=1.5, tck=0.01,font=2,font.lab=2,ylim=c(-1,0.65),cex.axis=1.2,cex.lab=1.2)
polygon(c(aa1[,"x"],rev(aa1[,"x"])), c(aa1[,"lower"], rev(aa1[,"upper"])), col=brewer.pal(9, "Purples")[3], border = brewer.pal(9, "Purples")[3])
lines(aa1[,"x"], aa1[,"mean"], type="l",  xlab="x", ylab="y", col=brewer.pal(9, "Purples")[8], lwd=1.5)
text(x = 0.93, y = -0.9, labels = "#=1",cex = 1.4,font=1.7 )
text(x = 0.5, y = -0.9, labels = "t-EGO", cex = 1.4, font=1.7)
par(new=T)
plot(unlist(total.data.x),unlist(total.data.y), xaxt="n", yaxt="n",type="l", lty=2, ylab="y",xlab="x",col="red", lwd=1, tck=0.01,font=2,font.lab=2,ylim=c(-1,0.65),cex.axis=1.2,cex.lab=1.2)
points(unlist(cc0[,2]),unlist(cc0[,3]), pch=20,col=brewer.pal(9, "Oranges")[5],cex=1.7)
points(unlist(cc1[5,2]),unlist(cc1[5,3]), pch=20,col="red",cex=1.7)
abline(h=unlist(tar_data),type="l", lty=3, ylab="y",xlab="x",col=brewer.pal(9, "Oranges")[8], lwd=1, tck=0.01,font=2,font.lab=2,ylim=c(-1,0.65),cex.axis=1.2,cex.lab=1.2)

# Plot Gaussian distribution
sel=bb1[which(bb1[,3]==max(bb1[,3])),2]
sel.num=which(bb1[,3]==max(bb1[,3]))
y_gaussian <- seq(-1, 0.65, length.out = 100)  # The y-range of the Gaussian distribution.
mean_gaussian <- aa1[sel.num,"mean"]  # The mean (central position) of the Gaussian distribution.
sd_gaussian <-  2*(aa1[sel.num,"mean"]-aa1[sel.num,"lower"])  #The standard deviation of the Gaussian distribution.
x_gaussian <- dnorm(y_gaussian, mean = mean_gaussian, sd = sd_gaussian)  # The probability density function of the Gaussian distribution.
x_gaussian <- x_gaussian / max(x_gaussian) * 0.2  #

# 
lines(x_gaussian + sel, y_gaussian, col="blue", lwd=1.5, lty=2)  # 

# Add shading to a portion of the Gaussian distribution.
up=which.min(abs(unlist(cc1[,3])-unlist(tar_data)))
Dis_min=abs(cc1[up,3]-tar_data[,1])
y_shaded <- seq(tar_data[,1]-Dis_min, tar_data[,1]+Dis_min, length.out = 50)   # limits
x_shaded <- dnorm(y_shaded, mean = mean_gaussian, sd = sd_gaussian)  # Calculate the Gaussian distribution value of the shaded region.
x_shaded <- x_shaded / max(x_shaded) * 0.2  # justify the width
polygon(c(x_shaded + sel, rep(sel, length(y_shaded))), c(y_shaded, rev(y_shaded)), col=rgb(0, 0, 1, 0.3), border = NA)  # add shadow


# 
par(new=T)
plot(unlist(total.data.x), unlist(total.data.y), xaxt="n", type="l", lty=2, ylab="y", xlab="x", col="red", lwd=1, tck=0.01, font=2, font.lab=2, ylim=c(-1, 0.65), cex.axis=1.2, cex.lab=1.2)
points(unlist(cc1[,2]), unlist(cc1[,3]), pch=20, col=brewer.pal(9, "Oranges")[5], cex=1.7)
abline(h=unlist(tar_data), type="l", lty=3, ylab="y", xlab="x", col=brewer.pal(9, "Oranges")[8], lwd=1, tck=0.01, font=2, font.lab=2, ylim=c(-1, 0.65), cex.axis=1.2, cex.lab=1.2)
abline(h=tar_data[,1]+Dis_min, type="l", lty=3, ylab="y", xlab="x", col=brewer.pal(9, "Oranges")[8], lwd=1, tck=0.01, font=2, font.lab=2, ylim=c(-1, 0.65), cex.axis=1.2, cex.lab=1.2)
abline(h=tar_data[,1]-Dis_min, type="l", lty=3, ylab="y", xlab="x", col=brewer.pal(9, "Oranges")[8], lwd=1, tck=0.01, font=2, font.lab=2, ylim=c(-1, 0.65), cex.axis=1.2, cex.lab=1.2)
text(x = 0.02, y = tar_data[,1]+0.035, labels = "t", cex = 1.4, font=1.7)
text(x = 0.04, y = tar_data[,1]+Dis_min+0.035, expression(y[t.min]), cex = 1.4, font=1.7)
text(x = 0.08, y = tar_data[,1]-Dis_min+0.035, expression(t-y[t.min]),  cex = 1.4, font=1.7)



####################################iteration 3
# Draw the mean curve and confidence interval.
plot(aa5[,"x"], aa5[,"mean"],xaxt="n", yaxt="n", type="l",  ylab="y",xlab="x",col="blue", lwd=1.5, tck=0.01,font=2,font.lab=2,ylim=c(-1,0.65),cex.axis=1.2,cex.lab=1.2)
polygon(c(aa5[,"x"],rev(aa5[,"x"])), c(aa5[,"lower"], rev(aa5[,"upper"])), col=brewer.pal(9, "Purples")[3], border = brewer.pal(9, "Purples")[3])
lines(aa5[,"x"], aa5[,"mean"], type="l",  xlab="x", ylab="y", col=brewer.pal(9, "Purples")[8], lwd=1.5)
text(x = 0.93, y = -0.9, labels = "#=2",cex = 1.4,font=1.7 )
text(x = 0.5, y = -0.9, labels = "t-EGO", cex = 1.4, font=1.7)
par(new=T)
plot(unlist(total.data.x),unlist(total.data.y), xaxt="n", yaxt="n",type="l", lty=2, ylab="y",xlab="x",col="red", lwd=1, tck=0.01,font=2,font.lab=2,ylim=c(-1,0.65),cex.axis=1.2,cex.lab=1.2)
points(unlist(cc1[,2]),unlist(cc1[,3]), pch=20,col=brewer.pal(9, "Oranges")[5],cex=1.7)
points(unlist(cc2[6,2]),unlist(cc2[6,3]), pch=20,col="red",cex=1.7)
abline(h=unlist(tar_data),type="l", lty=3, ylab="y",xlab="x",col=brewer.pal(9, "Oranges")[8], lwd=1, tck=0.01,font=2,font.lab=2,ylim=c(-1,0.65),cex.axis=1.2,cex.lab=1.2)



# Plot Gaussian distribution
sel=bb5[which(bb5[,3]==max(bb5[,3])),2]
sel.num=which(bb5[,3]==max(bb5[,3]))
y_gaussian <- seq(-1, 0.65, length.out = 100)  # The y-range of the Gaussian distribution.
mean_gaussian <- aa1[sel.num,"mean"]  # The mean (central position) of the Gaussian distribution.
sd_gaussian <-  2*(aa1[sel.num,"mean"]-aa1[sel.num,"lower"])  #The standard deviation of the Gaussian distribution.
x_gaussian <- dnorm(y_gaussian, mean = mean_gaussian, sd = sd_gaussian) # The probability density function of the Gaussian distribution.
x_gaussian <- x_gaussian / max(x_gaussian) * 0.2  # justify the width

# 
lines(x_gaussian + sel, y_gaussian, col="blue", lwd=1.5, lty=2)  # 

# Add shading to a portion of the Gaussian distribution.
up=which.min(abs(unlist(cc2[,3])-unlist(tar_data)))
Dis_min=abs(cc2[up,3]-tar_data[,1])
y_shaded <- seq(tar_data[,1]-Dis_min, tar_data[,1]+Dis_min, length.out = 50)  # limits
x_shaded <- dnorm(y_shaded, mean = mean_gaussian, sd = sd_gaussian)  # Calculate the Gaussian distribution value of the shaded region.
x_shaded <- x_shaded / max(x_shaded) * 0.2  # justify the width
polygon(c(x_shaded + sel, rep(sel, length(y_shaded))), c(y_shaded, rev(y_shaded)), col=rgb(0, 0, 1, 0.3), border = NA)  # add shadow


# 
par(new=T)
plot(unlist(total.data.x), unlist(total.data.y), xaxt="n", type="l", lty=2, ylab="y", xlab="x", col="red", lwd=1, tck=0.01, font=2, font.lab=2, ylim=c(-1, 0.65), cex.axis=1.2, cex.lab=1.2)
points(unlist(cc2[,2]), unlist(cc2[,3]), pch=20, col=brewer.pal(9, "Oranges")[5], cex=1.7)
abline(h=unlist(tar_data), type="l", lty=3, ylab="y", xlab="x", col=brewer.pal(9, "Oranges")[8], lwd=1, tck=0.01, font=2, font.lab=2, ylim=c(-1, 0.65), cex.axis=1.2, cex.lab=1.2)
abline(h=tar_data[,1]+Dis_min, type="l", lty=3, ylab="y", xlab="x", col=brewer.pal(9, "Oranges")[8], lwd=1, tck=0.01, font=2, font.lab=2, ylim=c(-1, 0.65), cex.axis=1.2, cex.lab=1.2)
abline(h=tar_data[,1]-Dis_min, type="l", lty=3, ylab="y", xlab="x", col=brewer.pal(9, "Oranges")[8], lwd=1, tck=0.01, font=2, font.lab=2, ylim=c(-1, 0.65), cex.axis=1.2, cex.lab=1.2)
text(x = 0.02, y = tar_data[,1]+0.005, labels = "t", cex = 1.4, font=1.7)
text(x = 0.04, y = tar_data[,1]+Dis_min+0.035, expression(y[t.min]), cex = 1.4, font=1.7)
text(x = 0.08, y = tar_data[,1]-Dis_min-0.035, expression(t-y[t.min]),  cex = 1.4, font=1.7)


