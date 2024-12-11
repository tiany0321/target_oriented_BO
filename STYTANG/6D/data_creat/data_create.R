setwd("C:\\Users\\12758\\Desktop\\manuscript\\target-submit\\code available\\STYTANG\\6D\\data_creat")
start_time<-Sys.time()
##########library
library("lhs")
library("DiceKriging")
###############Latin Hypercube Sampling
set.seed(123)
 
sample_size <- 60000  
dimensions <-   6
lhs_sample_standard <- t(randomLHS(dimensions, sample_size))

min_val <- -5  
max_val <- 5  
lhs_sample_scaled <- lhs_sample_standard * (max_val - min_val) + min_val 

total.data.x <- lhs_sample_scaled



############### 6D-STYBLINSKI-TANG function
stybtang <- function(xx)
{
  ##########################################################################
  #
  # STYBLINSKI-TANG FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2, ..., xd)
  #
  ##########################################################################
  
  sum <- sum(xx^4 - 16*xx^2 + 5*xx)
  
  y <- sum/2
  return(y)
}
total.data.y <- apply(total.data.x, 1, stybtang) 
###############database
data.all=cbind(total.data.y,total.data.x)
colnames(data.all)=c("y","x1","x2","x3","x4","x5","x6")

write.csv(data.all,"6Dstybtang_data.all.csv")

end_time<- Sys.time()
elapsed_time<-end_time-start_time
cat("运行时长：",elapsed_time,"\n")
write.csv(elapsed_time,"elapsed_time.csv")