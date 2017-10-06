
#biclustering examples
install.packages("Rcmdr")
install.packages("RcmdrPlugin.BiclustGUI")
library(RcmdrPlugin.BiclustGUI)
library("biclust") 
library("viridis")

CBCL<-read.csv('/Users/aki.nikolaidis/Dropbox/1_Projects/1_Research/2_FosterCare/Data/CBCL_Full.csv',row.names = 1, header = TRUE, sep = ",");
IVs<-read.csv('/Users/aki.nikolaidis/Dropbox/1_Projects/1_Research/2_FosterCare/Data/IVs_Full.csv',row.names = 1, header = TRUE, sep = ",");


#RealTimeCC<-read.csv('/Users/Aki/Dropbox/1_POSTDOC/1_Research/fMRIRealtimeCC.csv',row.names = 1, header = TRUE, sep = ",");
#RealTimeCCMat<-as.matrix(RealTimeCC)
#d<-dist(RealTimeCCMat, method = 'euclidean');
#dc <-dist(t(RealTimeCCMat), method = 'euclidean');
#hr <- hclust(d, method = "complete"); hc <- hclust(dc, method = "complete");
#mycl <- cutree(hr, h = max(hr$height)/1.05); mycolhc <- rainbow(length(unique(mycl)), start = 0.1, end = 0.9); mycolhc <- mycolhc[as.vector(mycl)]
#heatmap(RealTimeCCMat, Rowv = as.dendrogram(hr), Colv = as.dendrogram(hc), col = inferno(256), scale = "row", density.info = "none", trace = "none", RowSideColors = mycolhc)


FullCBCL<-CBCL[complete.cases(CBCL),];
Full_IVs<-IVs[complete.cases(IVs),];

CBCLMat<-as.matrix(FullCBCL);
IVsMat<-as.matrix(Full_IVs);

CBCLMat<-scale(CBCLMat)
IVsMat<-scale(IVsMat)

CBCLMat<-CBCLMat[ , colSums(is.na(CBCLMat)) == 0]

#CBCLMat<-t(CBCLMat)
CBCLBiclust <- biclust(CBCLMat, method = BCBimax(), minr=10, minc=10, number=5)
CBCLBiclust <- biclust(CBCLMat, method=BCQuest(), ns=5, nd=10, sd=5, alpha=0.05, number=10)
CBCLBiclust <- biclust(CBCLMat, method=BCQuestord(), ns=5, nd=10, sd=5, alpha=0.05, number=10)
CBCLBiclust <- biclust(CBCLMat, method=BCQuestmet(), quant=0.25, vari=1, ns=20, nd=30, sd=5, alpha=0.0005, number=100)
CBCLBiclust<- biclust(CBCLMat, method=BCCC(), delta = .25, alpha=1.5, number=100)
IVsBiclust <- biclust(IVsMat, method=BCQuest(), ns=5, nd=10, sd=5, alpha=0.05, number=10)


heatmapBC(CBCLMat,CBCLBiclust)
heatmapBC(IVsMat,IVsBiclust)

drawHeatmap(CBCLMat,CBCLBiclust, number=1)
drawHeatmap(IVsMat,IVsBiclust, number=3)

biclustbarchart(CBCLMat, CBCLBiclust, which=NULL)

diagnoseColRow(CBCLMat,CBCLBiclust, number = 1, nResamplings = 999, replace = TRUE)


#EXAMPLE USAGES
xmat<-matrix(rnorm(20*50,0,0.25),50,50) # background noise only
rowSize <- 20 #number of rows in a bicluster
colSize <- 10 #number of columns in a bicluster
a1<-rnorm(rowSize,1,0.1) #sample row effect from N(0,0.1) #adding a coherent values bicluster: b1<-rnorm((colSize),2,0.25) #sample column effect from N(0,0.05)
mu<-0.01 #constant value signal
for ( i in 1 : rowSize){
  for(j in 1: (colSize)){
    xmat[i,j] <- xmat[i,j] + mu + a1[i] + b1[j]
  }
}
TempBiclust <- biclust(xmat, method=BCQuestord(), ns=5, nd=10, sd=5, alpha=0.05, number=10)

s2=matrix(rnorm(5000),100,50)
s2[11:20,11:20]=rnorm(100,3,0.3)
set.seed(1)
bics <- biclust(s2,BCPlaid(), back.fit = 2, shuffle = 3, fit.model = ~m + a + b,
                iter.startup = 5, iter.layer = 30,  verbose = TRUE)
drawHeatmap(s2,bics)
# Example Usage
# data(BicatYeast)
# x <- discretize(BicatYeast)                                                    
# Xmotif <- biclust(x, method=BCXmotifs(), number = 50, alpha = 0.5, nd = 20, ns = 20, sd = 5)  
# writeBiclusterResults("results.txt", Xmotif,"CC with delta 1.5", dimnames(BicatYeast)[1][[1]], dimnames(BicatYeast)[2][[1]])


#https://www.youtube.com/watch?v=VcUQZDccdHc
#https://ibiostat.be/online-resources/online-resources/biclustgui
#https://cran.r-project.org/web/packages/RcmdrPlugin.BiclustGUI/vignettes/GuideBiclustGUI.pdf

#hierarchical Clustering example
hr <- hclust(as.dist(1 - cor(t(Std_CBCL_Full), method = "spearman")), method = "ward.D2");
hc <- hclust(as.dist(1 - cor(Std_CBCL_Full, method = "spearman")), method = "ward.D2");
mycl <- cutree(hr, h = max(hr$height)/1.5); mycolhc <- rainbow(length(unique(mycl)), start = 0.1, end = 0.9); 
mycolhc <- mycolhc[as.vector(mycl)]
heatmap(Std_CBCL_Full, Rowv = as.dendrogram(hr), Colv = as.dendrogram(hc), col = inferno(256), scale = "row", density.info = "none", trace = "none", RowSideColors = mycolhc)


NewMat <- as.matrix(print(mycl))

Group1<-mycl[ which(mycl=='1')]
Group1Labels<-as.matrix(print(Group1))
Group1Labels<-(rownames(Group1Labels))
Group1Data<-Std_CBCL_Full[Group1Labels,]
Group1Data<-data.frame(Group1Data)

Group2<-mycl[ which(mycl=='2')]
Group2Labels<-as.matrix(print(Group2))
Group2Labels<-(rownames(Group2Labels))
Group2Data<-Std_CBCL_Full[Group2Labels,]
Group2Data<-data.frame(Group2Data)

Group3<-mycl[ which(mycl=='3')]
Group3Labels<-as.matrix(print(Group3))
Group3Labels<-(rownames(Group3Labels))
#now test whether 1 group is significantly different than another based on their cluster assignment
Group3Data<-Std_CBCL_Full[Group3Labels,]
Group3Data<-data.frame(Group3Data)
#Group 1 Orange, Group 2 Teal- Group 3 Pink

Group1Data <- Group1Data[complete.cases(Group1Data),]
Group2Data <- Group2Data[complete.cases(Group2Data),]
Group3Data <- Group3Data[complete.cases(Group3Data),]

Grp1Means <- colMeans(Group1Data)
Grp2Means <- colMeans(Group2Data)
Grp3Means <- colMeans(Group3Data)

DVsGroup3Data<-StdDVsFull[Group3Labels,]
DVsGroup2Data<-StdDVsFull[Group2Labels,]
DVsGroup1Data<-StdDVsFull[Group1Labels,]

DVsGrp1Means <- colMeans(DVsGroup1Data)
DVsGrp2Means <- colMeans(DVsGroup2Data)
DVsGrp3Means <- colMeans(DVsGroup3Data)
AllGroupIVMeans <- rbind(Grp1Means, Grp2Means, Grp3Means)
CBCL_Subs_Names <- rownames(Std_CBCL_Full)
L = rownames(DVs_Full) == CBCL_Subs_Names
CBCLGroup_DVScores <- DVs_Full[L,]
Ordered_CBCL_Subs_Names<-sort(CBCL_Subs_Names)
# Doing the same but for the IVs
IVs_Group4 <- mycl[ which(mycl == '4')]
IVs_Group4Labels <- as.matrix(print(IVs_Group4))
IVs_Group4Labels <- (rownames(IVs_Group4Labels))
IVs_Group4Data <- StdIVsFull[IVs_Group4Labels,]
IVs_Group4Data <- data.frame(IVs_Group4Data)




radarchart( IVsForPredAllGroupMeans  , axistype=0 , maxmin=F,
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
            #custom labels
            vlcex=0.8 
)

##########
#Creating Profiles for the groups-

library(fmsb)

# Create data: note in High school for several students
set.seed(99)
data=as.data.frame(matrix( sample( 0:20 , 15 , replace=F) , ncol=5))
colnames(data)=c("math" , "english" , "biology" , "music" , "R-coding" )
rownames(data)=paste("mister" , letters[1:3] , sep="-")

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data=rbind(rep(20,5) , rep(0,5) , data)


#Predicting group membership with IVs
CBCLGroup <- c(Group1, Group2, Group3)
IVsForPredCBCLGroup<-Std_IVs_CBCL_Full[,1:13]
OverlapCBCL_IV_Subs <- c(rownames(IVsForPredCBCLGroup),rownames(CBCLGroup))
OverlapCBCL_IV_Subs <- OverlapCBCL_IV_Subs[duplicated(OverlapCBCL_IV_Subs)]
IVsForPredCBCLGroup <- IVsForPredCBCLGroup[OverlapCBCL_IV_Subs,]
CBCLGroup <- CBCLGroup[OverlapCBCL_IV_Subs,] 

CBCLGroup1List <- CBCLGroup == 1
CBCLGroup2List <- CBCLGroup == 2
CBCLGroup3List <- CBCLGroup == 3

CBCLGroupList <- replace(CBCLGroup, CBCLGroup1List, 'Group1')
CBCLGroupList <- replace(CBCLGroupList, CBCLGroup2List, 'Group2')
CBCLGroupList <- replace(CBCLGroupList, CBCLGroup3List, 'Group3')

CBCL_TwoGroupList <- replace(CBCLGroupList, CBCLGroup3List, 'Group2')
CBCL_TwoGroupFactors <- factor(x = CBCL_TwoGroupList)


CBCLGroupFactors <- factor(x = CBCLGroup)
IVsForPredCBCLGroup_noAge <- IVsForPredCBCLGroup[,2:13]

RF_FirstRun <- randomForest(IVsForPredCBCLGroup,CBCLGroupFactors)
RF_SecondRun <- randomForest(IVsForPredCBCLGroup,CBCLGroupFactors, nodesize = 5, importance = TRUE, ntree = 10000)
RF_ThirdRun <- randomForest(IVsForPredCBCLGroup,CBCLGroupFactors, nodesize = 10, importance = TRUE, ntree = 10000, replace = TRUE)
RF_FourthRun <- randomForest(IVsForPredCBCLGroup,CBCLGroupFactors, nodesize = 10, importance = TRUE, ntree = 10000, replace = TRUE, norm.votes = TRUE)
RF_TwoGroup_FirstRun_noAge <- randomForest(IVsForPredCBCLGroup_noAge,CBCL_TwoGroupFactors, nodesize = 10, importance = TRUE, ntree = 100000, replace = TRUE)
RF_ThreeGroup_FirstRun_noAge <- randomForest(IVsForPredCBCLGroup_noAge,CBCLGroupFactors, nodesize = 10, importance = TRUE, ntree = 100000, replace = TRUE)



RF_VarImportance.FirstRun <- importance(RF_FirstRun)
RF_VarImportance.SecondRun <- importance(RF_SecondRun)
RF_VarImportance.ThirdRun <- importance(RF_ThirdRun)
RF_VarImportance.FourthRun <- importance(RF_FourthRun)


IVsForPredGroup1<-IVsForPredCBCLGroup[CBCLGroup1List,]
IVsForPredGroup2<-IVsForPredCBCLGroup[CBCLGroup2List,]
IVsForPredGroup3<-IVsForPredCBCLGroup[CBCLGroup3List,]

IVsForPredGroup1Means <- colMeans(IVsForPredGroup1)
IVsForPredGroup2Means <- colMeans(IVsForPredGroup2)
IVsForPredGroup3Means <- colMeans(IVsForPredGroup3)

IVsForPredAllGroupMeans <- rbind(IVsForPredGroup1Means, IVsForPredGroup2Means, IVsForPredGroup3Means)
IVsForPredAllGroupMeans <- as.data.frame(IVsForPredAllGroupMeans)

#==================
# Plot 1: Default radar chart proposed by the library:
radarchart(data)


#==================
# Plot 2: Same plot with custom features
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
radarchart( data  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)
legend(x=0.7, y=1, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)



#=================
# Plot3: If you remove the 2 first lines, the function compute the max and min of each variable with the available data:
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
radarchart( data[-c(1,2),]  , axistype=0 , maxmin=F,
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
            #custom labels
            vlcex=0.8 
)
legend(x=0.7, y=1, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)


#Testing Clusters for Significance
library(pvclust); library(gplots) # Loads the required packages.
pv <- pvclust(scale(t(mydata)), method.dist="correlation", method.hclust="complete", nboot=10) 
   # Perform the hierarchical cluster analysis. Due to time resrictions, we are using here only 10 bootstrap repetitions. 
   # Usually, one should use at least 1000 repetitions.
plot(pv, hang=-1); pvrect(pv, alpha=0.95) 
   # Plots result as a dendrogram where the significant clusters are highlighted with red rectangles.
clsig <- unlist(pvpick(pv, alpha=0.95, pv="au", type="geq", max.only=TRUE)$clusters) # Retrieve members of significant clusters.
source("http://faculty.ucr.edu/~tgirke/Documents/R_BioCond/My_R_Scripts/dendroCol.R") # Import tree coloring function.
dend_colored <- dendrapply(as.dendrogram(pv$hclust), dendroCol, keys=clsig, xPar="edgePar", bgr="black", fgr="red", pch=20) 
   # Create dendrogram object where the significant clusters are labeled in red.
heatmap(mydata, Rowv=dend_colored, Colv=as.dendrogram(hc), col=my.colorFct(), scale="row", RowSideColors=mycolhc) 
   # Plot the heatmap from above, but with the significant clusters in red and the cluster bins from the tree cutting step in 
   # the color bar.
x11(height=12); heatmap.2(mydata, Rowv=dend_colored, Colv=as.dendrogram(hc), col=my.colorFct(), scale="row", trace="none", RowSideColors=mycolhc) # Plot heatmap with heatmap.2() function which scales better for many entries.
mydatasort <- mydata[pv$hclust$labels[pv$hclust$order], hc$labels[hc$order]] # Sort rows in data table by 'dend_colored' and its colums by 'hc'.
x11(height=16, width=12); par(mfrow=c(1,2)); plot(dend_colored, horiz=T, yaxt="n"); image(scale(t(mydatasort)), col=my.colorFct(), xaxt="n",yaxt="n") # Plot heatmap with bootstrap tree in larger format using instead of heatmap the image function.
pdf("pvclust.pdf", height=21, width=6); plot(dend_colored, horiz=T, yaxt="n"); dev.off(); pdf("heatmap.pdf", height=20, width=6); image(scale(t(mydatasort)), col=my.colorFct(), xaxt="n",yaxt="n"); dev.off() 
   # Save graphical results to two PDF files: 'pvclust.pdf' and'heatmap.pdf'.