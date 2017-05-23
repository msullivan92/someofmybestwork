################## PCA of Wine Quality Data #######################
setwd("F:/DRSINGH/Teach2016/HOA732")
W <- read.csv("winequality.csv",header=TRUE)
#colnames(W) <- c("FA","VA","CA","RS","CH","FS","TS","D","pH","Sulph","Alc",
                 "quality","Wine")         
# data frame of quantitative variables
WQ <- W[,1:11]  

pca1 <- princomp(WQ,cor=TRUE)
print(pca1)

summary(pca1)
pca1$loadings
plot(pca1, type = "l")

biplot(pca1,cex=.8)

# biplot in ggplot2
install.packages('ggfortify')
library(ggfortify)
#fit1 <- prcomp(WQ)

autoplot(pca1,data=W,colour="Wine",loadings = TRUE, 
         loadings.colour="blue",loadings.label = TRUE, 
         loadings.label.size = 3)
         
#################################################################
################### Principal Components Analysis ###############
#################################################################
P<-read.csv("~/Desktop/Rstats/Places Related Almanac data.csv")
P1<-P[,2:9]#useful dataframe
fit1 <- princomp(F1, cor=TRUE)
summary(fit1) # print variance accounted for 
loadings(fit1) # pc loadings
PCScores<-as.data.frame(fit1$scores)



#par(mfrow=c(2,2))
plot(Comp.1~Comp.2, xlab = 'PC2', ylab = 'PC1', data = PCScores)
with(PCScores, text(Comp.1~Comp.2,labels = row.names(PCScores), cex=.8, pos = 1))

plot(Comp.1~Comp.3, xlab = 'PC3', ylab = 'PC1', data = PCScores)
with(PCScores, text(Comp.1~Comp.2,labels = row.names(PCScores), cex=.8,pos = 1))

plot(Comp.2~Comp.3, xlab = 'PC3', ylab = 'PC2', data = PCScores)
with(PCScores, text(Comp.1~Comp.2,labels = row.names(PCScores), cex=.8,pos = 3))

# Better plots are obtained by ggplot2
library(ggplot2)
P12 <- ggplot(PC, aes(x=Comp.2, y=Comp.1),size=3)+ geom_point(size=1)+
ggtitle("Plot of PC1 vs PC2")+
xlab("PC2") + ylab("PC1")+
geom_text(aes(label=row.names(P.Climate)))

P13 <- ggplot(PC, aes(x=Comp.3, y=Comp.1),size=3)+ geom_point(size=1)+
ggtitle("Plot of PC1 vs PC3")+
xlab("PC3") + ylab("PC1")+
geom_text(aes(label=row.names(P.Climate)))

P23 <- ggplot(PC, aes(x=Comp.3, y=Comp.2),size=3)+ geom_point(size=1)+
ggtitle("Plot of PC2 vs PC3")+
xlab("PC3") + ylab("PC2")+
geom_text(aes(label=row.names(P.Climate)))

library(gridExtra)
grid.arrange(P12,P13,P23,ncol=2, nrow =2)





