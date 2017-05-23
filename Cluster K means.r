F <- read.csv("MV FandB1.csv",header=TRUE)

############## Basic Scatterplot Matrix ###########
pairs(~Energy+Protein+Fat+Calcium+Iron,data=F, 
   main="Simple Scatterplot Matrix")

library(car)
scatterplot.matrix(~Energy+Protein+Fat+Calcium+Iron, data=F,
  	main="Simple Scatterplot Matrix")

########## Clustering data##########
########### work with the quantitative variables 3 through 7
F1 <- F[,3:7]

########## Determine number of clusters ############
set.seed(712345711)
wss <- matrix(0,nrow=14,ncol=1)
for (i in 2:15) wss[i] <- sum(kmeans(F1, 
  	 centers=i)$withinss)
wss <- wss[-1] # reove 1-st element of wss
n <- 1:14
plot(n, wss, type="b", xlab="Number of Clusters",
  ylab="Within groups sum of squares")

########### K means #####################
km <-  kmeans(F1, 5) # 5 cluster solution
aggregate(F1,by=list(km$cluster),FUN=mean)
aggregate(F1,by=list(km$cluster),FUN=sd)
aggregate(F1,by=list(km$cluster),FUN=length)
# append cluster number 
F1C <- data.frame(F$Food,F1,km$cluster)

############ sort by cluster ##############
F1Csorted <- F1C[order(F1C$km.cluster),] 

############# NOTE: Running this code (without providing a seed) #############
############# will give different results, since kmeans uses a ###############
############# randomly initial clustering assignment #########################

######################################################
#  perform hierarchical clustering    ################
######################################################

d <- dist(as.matrix(F1))
# perform hierarchical clustering    
hc <- hclust(d) 
# plot clustering results as a dendrogram               
plot(hc) 

######################################################
#  Partitioning Around Medoids    ####################
######################################################

#Largest Silhouette
library(cluster)
pam2E <- pam(F1, 2, metric="euclidean")
summary(pam2E)
pamx3 <- pam(F1, 3,metric="euclidean")
summary(pamx3)
pamx4 <- pam(F1, 4,metric="euclidean")
summary(pamx4)
pamx5 <- pam(F1, 5,metric="euclidean")  
summary(pamx5)

################# Optimal number of Clusters ############
# pamk in library fpc determines optimum number of clusters
install.packages("fpc")
library(fpc)
pk1 <- pamk(F,krange=1:6,criterion="asw",critout=TRUE)
pk1
siF <- silhouette(pamx4)
plot(siF, col = c("red", "green", "blue", "purple"))

