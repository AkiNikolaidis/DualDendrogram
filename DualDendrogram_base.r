library(viridis)
library(gplots)

setwd("/Volumes/data/Research/CDB/RA Didactics/Programming_Jan2017/Week 6/")
##Import Data
Quotient_All <- read.csv("BiCluster2.csv")
Quotient_All <- na.omit(Quotient_All)
ADHD_Dx <- read.csv("ADHD_1.csv")
##Select Subgroup for clustering
Q_Older <- Quotient_All[which(Quotient_All$Age>=11.000),]
##Separate demos
Q_Older_Demos<-Q_Older[1:3]
##Remove demos from data for clustering
Q_Older<-Q_Older[,c(1,4:27)]
#Separate ID column
rnames3 <- Q_Older[,1]
Q_Older2 <- data.matrix(Q_Older[, c(2:25)])
rownames(Q_Older2) <- rnames3

##Center and scale the data
Qscale3 <- scale(Q_Older2)
#Cluster rows and columns
hr <- hclust(as.dist(1-cor(t(Qscale3), method="spearman")), method="ward.D2")
hc <- hclust(as.dist(1-cor(Qscale3, method="spearman")), method="ward.D2")
##Create cut tree for rows and columns
mycl <- cutree(hr, h=max(hr$height)/1.5); mycolhc <- rainbow(length(unique(mycl)), start=0.1, end=0.9);
mycolhc <- mycolhc[as.vector(mycl)]
mycl2 <- cutree(hc, h=max(hr$height)/5.0); mycolhr <- rainbow(length(unique(mycl2)), start=0.1, end=0.9); 
mycolhr <- mycolhr[as.vector(mycl2)]
##Create heatmap
heatmap.2(Qscale3,
          Rowv=as.dendrogram(hr),
          Colv=as.dendrogram(hc),
          col="viridis",
          scale="row",
          density.info="none",
          trace="none",
          margins = c(9, 2),
          srtRow = 0,
          srtCol = 60,
          keysize = 1,
          key.title = NA,
          main = NULL,
          RowSideColors=mycolhc,
          ColSideColors = mycolhr)
##Create DX/Demos heatmap
##Gather Data
cluster_groups <- cbind(rnames3, Qscale3, clusterID=mycl)
dxinorder3 <- merge(cluster_groups, ADHD_Dx, by.x="rnames3", by.y="Rand_ID")
##Reorder data to match subject clustering
dxinorder3 <- dxinorder3[,c(1,27:30)]
dxinorder3 <- dxinorder3[hr$order,]
dxinorder3 <- (dxinorder3[ nrow(dxinorder3):1, ])
##Separate ID Column
rownames3 <- dxinorder3[,1]
dxinorder3 <- data.matrix(dxinorder3[, c(2:5)])
rownames(dxinorder3) <- rownames3
#Create heatmap
heatmap.2(dxinorder3,
          dendrogram="none",
          Rowv=FALSE,
          Colv=FALSE,
          trace="none",
          col="viridis",
          na.color="gray",
          margins = c(11, 30),
          keysize = 1,
          key.title = NA,
          main = NULL)

