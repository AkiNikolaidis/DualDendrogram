#Functions 

library("biclust") 
library("fmsb")
library("viridis")
library("ggplot2")
library("purrr")
library("plyr")
library("randomForest")


#Actions:
# DONE 1- Go through the script and refamiliarize myself with different parts
# DONE 2- Mark sections that would be good for transformation into functions
# DONE 3- Organize this script a bit more- more commenting- put in a large scale structure
# 4 - Begin working on transformign sections into functions- Write Pseudocode
# 5 - Begin running through Action points on this function page 
###FUNCTION## Apply Radar Chart- using grouping from the first dataset and the sorting from the second.
###FUNCTION###- Testing variable differences between cluster groups.
##FUNCTION## Predict Cluster Membership with a matrix of data
##FUNCTION## Apply clustering to data matrix. Output clustering variables and visualization.

#_______________________________________________________________________________________________________________________
#_______________________________________________________________________________________________________________________
#Table Of Contents
# - Scope and Purpose -
# - Importing and Cleaning Data for Data1 - 
# - Importing and Cleaning Data for Data2 - 
# - Function- Two-Way Clustering on Data1 - 
# - Function- Assess Quality of Clustering on Data1 - 
# - Function- Testing Clusters for Differences in Data2 - 
# - Function- Predict Data1 Cluster Membership with Data2 - 
# - Function- Visualization of Data2 by Data1 Clusters - 
# - Miscellaneous - 

#_______________________________________________________________________________________________________________________
#_______________________________________________________________________________________________________________________

# _______________________________________ - Scope and Purpose - __________________________________________________________

# _______________________________________ - Importing and Cleaning Data for Data1 - __________________________________

#Steps 1- Import Data1
#what should data1 look like? binary or continuous variables only
#Data1 should be standardized within variables, but not across subjects
dataset_path <- '~/Dropbox/1_Projects/1_Research/2_FosterCare/Data/CBCL_Full.csv'
# 1 - Take in data
if (is.character(dataset_path)==TRUE) {
  UnprocessedData <- read.csv(dataset_path,row.names = 1, header = TRUE, sep = ",");
} else {
  UnprocessedData <- dataset_path
}

UnprocessedData <- UnprocessedData[complete.cases(UnprocessedData),];
#UnprocessedData1 <- UnprocessedData[duplicated(UnprocessedData[,1])==FALSE , ]
sum(is.na(UnprocessedData))
Data1 <- data.frame(UnprocessedData)


# 2 - Clean Data
Data1 <- Data1[ , colSums(is.na(Data1)) == 0]
Data1 <- Data1[complete.cases(Data1),];
Data1 <- as.matrix(Data1, na.rm = TRUE);


#Scaling is Optional

Data1 <- scale(Data1)  
sum(is.na(Data1))
Data1 <- Data1[, colSums(!is.na(Data1)) > 0]
sum(is.na(Data1))
Data1 <- scale(Data1) 
Data1 <- Data1[, colSums(!is.na(Data1)) > 0]
sum(is.na(Data1))
OutlierID <- abs(Data1)<4
subs <- apply(OutlierID, 1, min)
Data1 <- Data1[subs==1,]
sum(is.na(Data1))


# _______________________________________ - Importing and Cleaning Data for Data2 - __________________________________
dataset_path<- '~/Dropbox/1_Projects/1_Research/2_FosterCare/Data/IVs_Full.csv'     
# 1 - Take in data
if (is.character(dataset_path)==TRUE) {
  UnprocessedData <- read.csv(dataset_path,row.names = 1, header = TRUE, sep = ",");
} else {
  UnprocessedData <- dataset_path
}

UnprocessedData <- UnprocessedData[complete.cases(UnprocessedData),];
#UnprocessedData1 <- UnprocessedData[duplicated(UnprocessedData[,1])==FALSE , ]
sum(is.na(UnprocessedData))
Data2 <- data.frame(UnprocessedData)


# 2 - Clean Data
Data2 <- Data2[ , colSums(is.na(Data2)) == 0]
Data2 <- Data2[complete.cases(Data2),];
Data2 <- as.matrix(Data2, na.rm = TRUE);


#Scaling is Optional

Data2 <- scale(Data2)  
sum(is.na(Data2))
Data2 <- Data2[, colSums(!is.na(Data2)) > 0]
sum(is.na(Data2))
Data2 <- scale(Data2) 
Data2 <- Data2[, colSums(!is.na(Data2)) > 0]
sum(is.na(Data2))
OutlierID <- abs(Data2)<4
subs <- apply(OutlierID, 1, min)
Data2 <- Data2[subs==1,]
sum(is.na(Data2))

#Merging Data1 and Data2 to extract the same row names from each.
#Action- check that Data1 and Data2 have rownames in them (probably when we first import data1 and data2)
#merges datasets according to row0 of both
temp_orig <- merge(Data1, Data2, by=0, all=TRUE)
temp <- temp_orig[rowSums(!is.na(temp_orig)) == ncol(temp_orig), ]

sum(is.na(temp))

temp1 <- temp[,1:(ncol(Data1)+1)]
colnumber<-(ncol(Data1)+2)
colnumberall<-ncol(temp)
temp2 <- temp[,colnumber:colnumberall]
nrow(temp1)/nrow(Data1) # of subjects retained from Data1
nrow(temp1)/nrow(Data2) # of subjects retained from Data2

Data1 <- temp1
Data2 <- temp2

#Action- Implement Multiple Imputation in Data1 and Data2 in preprocessing/cleaning step. (look at Giovanni's Code)
#Imputation Methods- 
#Categorical Variables. Simply add a new category indicating missing data.
#Multiple Imputation by Chained Equations (MICE) package in R
# https://www.analyticsvidhya.com/blog/2016/03/tutorial-powerful-packages-imputing-missing-values/
# https://www.jstatsoft.org/article/view/v045i03
# From Giovanni ## hbn_inputation<-mice(hbn,m=1,maxit=1,meth='polr',seed=500); hbn_inp<-complete(hbn_inputation)
A_inputation<-mice(A,m=1,maxit=1,meth='polr',seed=500)

# _______________________________________ - Function- Two-Way Clustering on Data1 - __________________________________

#dataset_path <- '/Users/aki.nikolaidis/Dropbox/1_Projects/1_Research/2_FosterCare/Data/CBCL_Full.csv'
outputdir <- '~/Dropbox/1_Projects/1_Research/2_FosterCare/Results'
#group_thresh = 1.7
#variable_thresh = 2   

##FUNCTION## Apply clustering to data matrix. Output clustering variables and visualization.
TwoWayclust <- function(Data1, group_thresh = 1.5, variable_thresh = 1.5, outputdir) {
  #takes in a csv dataset, cleans it up, standardizes it, and calculates the twowayclust on the data at the threshold defined by the user
  #saves the heatmap, and outputs the mycl for the group differences function
  
  #change so that if variable is already loaded into workspace the can call it
  #if variable is a path then do the following, else just set Clusterset equal to the dataset_path variable

  # 3.1 Euclidean + Ward Clustering of Subjects
  #r_dist <- dist(Clusterset, method = "euclidean")
  #hr <- hclust(r_dist, method = "ward.D2");
  r_dist <- dist(Data1, method = "binary")
  hr <- hclust(r_dist, method = "complete");
  
  # 3.2 Spearman + Complete Clustering of Variables
  #c_dist <- dist(t(Clusterset), method = "euclidean")
  #hc <- hclust(c_dist, method = "ward.D2")
  c_dist <- dist(t(Data1), method = "binary")
  hc <- hclust(c_dist, method = "complete")
  
  # 4 Subject Group Assignment
  Sub_Group <- cutree(hr, h = max(hr$height)/group_thresh)
  mycolhr <- rainbow(length(unique(Sub_Group)), start = 0.1, end = 0.9); 
  mycolhr <- mycolhr[as.vector(Sub_Group)]
  
  # 5 Variable Group Assignment
  Var_Group <- cutree(hc, h = max(hc$height)/variable_thresh)
  mycolhc <- rainbow(length(unique(Var_Group)), start = 0.1, end = 0.9); 
  mycolhc <- mycolhc[as.vector(Var_Group)]
  
  # 6 Visualization
  h<-heatmap(Data1, Rowv = as.dendrogram(hr), Colv = as.dendrogram(hc), col = inferno(256), scale = "none", RowSideColors = mycolhr, ColSideColors = mycolhc)
  h
  #Action- Figure out how to export the visualization to disk-
  #Action- Figure out how to save all relevant variables- Var-Group, Sub_group, mycolhr, hr, hc
  }


# _______________________________________ - Function- Assess Quality of Clustering on Data1 - __________________________________
# Action- Take a look at Silhouette plots, and the clValid package

#Pseudocode

# _______________________________________ - Function- Testing Clusters for Differences in Data2 - __________________________________

#organize Data2 variables by Data1 clusters

#if Data2=continuous then ANOVAs
#For loop through the variables in Data2 and test for group differences across Data1 Clusters
# if Data2=binary then Chi-Squared
#For loop through variables in Data2


# _______________________________________ - Function- Predict Data1 Cluster Membership with Data2 - __________________________________

#Use data 2 as feature sets to predict group


# 1.1 process the IVs through clustering to get sorted IVs 
UnprocessedData<-MergedData_IVs
UnprocessedData <- TestIVs
# 1.2 run through Two-Way Clustering
#1.3 reclaim variable order and apply to IVs for later plotting
ClusteredIVOrder <- hc$order
SortedIVs <- MergedData_IVs[, ClusteredIVOrder]
SortedIVs <- scale(SortedIVs)

#
#3.1 assign CBCL grouping to the IVs
#assign the secondary group to the subject group

#
IVs_Group1 <- Sub_Group[ which(Sub_Group == '1')]

IVs_Group1Labels <- as.matrix(print(IVs_Group1))

IVs_Group1Labels <- (rownames(IVs_Group1Labels))

IVs_Group1Data <- SortedIVs[IVs_Group1Labels,]

IVs_Group1Data <- data.frame(IVs_Group1Data)

Grp1Means <- colMeans(IVs_Group1Data)

AllGroupIVMeans <- rbind(Grp1Means, Grp2Means, Grp3Means)

AllGroupIVMeans <- as.data.frame(AllGroupIVMeans)


# _______________________________________ - Function- Visualization of Data2 by Data1 Clusters - __________________________________

##FUNCTION## Apply Radar Chart- using grouping from the first dataset and the sorting from the second.

# 4.1 Apply CBCL grouping, and IV means, and IV sorting, to RadarChart
#colors_border<-unique(mycolhr[hr$order])
colors_border<-c("#FF990099", "#00FFFF99", "#FF009999")
colors_in<- c("#FF990050", "#00FFFF50", "#FF009950")

radarchart( AllGroupIVMeans  , axistype=0 , maxmin=F,
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
            #custom labels
            vlcex=0.8 
)

# _______________________________________ - Miscellaneous- ________________________________________________________________


#ANALYZING THE IV DATA FOR RELATIONSHIP TO THE CBCL CLUSTERING


UnprocessedData_df <- as.data.frame(UnprocessedData)
Clusterset2_df <- as.data.frame(Clusterset2)
MergedData_All <- merge(UnprocessedData_df, Clusterset2_df, by="row.names")

DVs_Full <- read.csv(DVs_Full,row.names = 1, header = TRUE, sep = ",");
DVs_Full_df <- as.data.frame(DVs_Full)
AllData<-MergedData_All
row.names(AllData) <- MergedData_All[,1]
AllData<-AllData[,2:113]
Merged_DVs_IVs_CBCL <- merge(AllData,DVs_Full_df, by="row.names")



MergedData_CBCL<-MergedData_All[,2:100]
MergedData_IVs<-MergedData_All[,101:113]
row.names(MergedData_CBCL) <-MergedData_All[,1]
row.names(MergedData_IVs) <-MergedData_All[,1]

# 1.1 process the IVs through clustering to get sorted IVs 
UnprocessedData<-MergedData_IVs
UnprocessedData <- TestIVs
# 1.2 run through Two-Way Clustering
#1.3 reclaim variable order and apply to IVs for later plotting
ClusteredIVOrder <- hc$order
SortedIVs <- MergedData_IVs[, ClusteredIVOrder]
SortedIVs <- scale(SortedIVs)

#
# 2.1 process the CBCL through clustering to get subject grouping
UnprocessedData<-MergedData_CBCL
#2.2 run through two-way clustering


##TRYING CCA-
#Test of Canonical Dimension
# X can be DVs or MergedData_CBCL, and Y can be IVs
x <- DVs; y <- TempIVs
cc <- cancor(x, y)
library(reshape2)
library(lattice)
library(CCP)
N = dim(x)[1]
p = dim(x)[2]
q = dim(y)[2]
p.asym(rho = cc$cor, N, p, q, tstat = "Wilks")
round(cc$cor, 4)


#3.1 assign CBCL grouping to the IVs
#assign the secondary group to the subject group
IVs_Group1 <- Sub_Group[ which(Sub_Group == '1')]
IVs_Group2 <- Sub_Group[ which(Sub_Group == '2')]
IVs_Group3 <- Sub_Group[ which(Sub_Group == '3')]

IVs_Group1Labels <- as.matrix(print(IVs_Group1))
IVs_Group2Labels <- as.matrix(print(IVs_Group2))
IVs_Group3Labels <- as.matrix(print(IVs_Group3))

IVs_Group1Labels <- (rownames(IVs_Group1Labels))
IVs_Group2Labels <- (rownames(IVs_Group2Labels))
IVs_Group3Labels <- (rownames(IVs_Group3Labels))

IVs_Group1Data <- SortedIVs[IVs_Group1Labels,]
IVs_Group2Data <- SortedIVs[IVs_Group2Labels,]
IVs_Group3Data <- SortedIVs[IVs_Group3Labels,]

IVs_Group1Data <- data.frame(IVs_Group1Data)
IVs_Group2Data <- data.frame(IVs_Group2Data)
IVs_Group3Data <- data.frame(IVs_Group3Data)

Grp1Means <- colMeans(IVs_Group1Data)
Grp2Means <- colMeans(IVs_Group2Data)
Grp3Means <- colMeans(IVs_Group3Data)

AllGroupIVMeans <- rbind(Grp1Means, Grp2Means, Grp3Means)

AllGroupIVMeans <- as.data.frame(AllGroupIVMeans)

#Pink: FF9900FF, Orange: FF0099FF, Teal: 00FFFFFF
#colors_border=c( rgb(1.0,1.0,0.0,0.9), rgb(1.0,0.0,1.0,0.9) , rgb(0.0,1.0,1.0,0.9) )

#colors_in=c( rgb(.8,0.8,0.3,0.4), rgb(0.8,0.3,0.8,0.4) , rgb(0.3,.8,.8,0.4) )

#"#FF9900FF" "#00FFFFFF" "#FF0099FF"

# GET GROUP COLOR ASSIGNMENTS
#CBCL GROUP 1 = Orange
#CBCL GROUP 2 = Teal
#CBCL GROUP 3 = Pink

##FUNCTION## Apply Radar Chart- using grouping from the first dataset and the sorting from the second.

# 4.1 Apply CBCL grouping, and IV means, and IV sorting, to RadarChart
#colors_border<-unique(mycolhr[hr$order])
colors_border<-c("#FF990099", "#00FFFF99", "#FF009999")
colors_in<- c("#FF990050", "#00FFFF50", "#FF009950")

radarchart( AllGroupIVMeans  , axistype=0 , maxmin=F,
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
            #custom labels
            vlcex=0.8 
)

#5.1 Plot the different groups in other charts
p <- ggplot(MergedData_IVs, aes(factor(Sub_Group), CPAC_Raw$duration)) + geom_violin(aes(fill = Thread_Est_Error))
p + geom_jitter(height = 0, shape = 20) #+ theme(axis.text.x = element_text(vjust = grid::unit(c(-2, 0, 2), "points")))#+ geom_point(shape=1)

p <- ggplot(MergedData_IVs, aes(factor(Sub_Group), MergedData_IVs$IV.Adopted)) + geom_violin(aes(factor(Sub_Group)))
p + geom_jitter(height = 0, shape = 20)

p <- ggplot(TempIVs, aes(factor(Sub_Group), IV.Adopted)) + geom_boxplot(aes(factor(Sub_Group)))
p + geom_jitter(height = 0, shape = 20)


p <- ggplot(SortedIVs, aes(factor(Sub_Group), SortedIVs$IV.Adopted)) + geom_smooth(aes(factor(Sub_Group)))
p + geom_jitter(height = 0, shape = 20)





#Testing differences in IVs between CBCL groups with Chi-squared test and Anovas

###FUNCTION###- Testing variable differences between cluster groups.

TestIVs <- as.data.frame(MergedData_IVs)
newdata <- mydata[c(-3,-5)]
TestIVs <- TestIVs[c(-1, -3)]
TestIVs[TestIVs>=1] <-1


tbl = table(TestIVs$IV.PriorPlacements, Sub_Group)
tbl
chisq.test(tbl)

tbl = table(TestIVs$IV.PhysicalAbuse, Sub_Group)
tbl
chisq.test(tbl)

tbl = table(TestIVs$IV.SexualAbuse, Sub_Group)
tbl
chisq.test(tbl)

tbl = table(TestIVs$IV.Neglect, Sub_Group)
tbl
chisq.test(tbl)

tbl = table(TestIVs$IV.ParentalIncarceration, Sub_Group)
tbl
chisq.test(tbl)

tbl = table(TestIVs$IV.ParentSubstanceAbuse, Sub_Group)
tbl
chisq.test(tbl)

tbl = table(TestIVs$IV.Dependency, Sub_Group)
tbl
chisq.test(tbl)

tbl = table(TestIVs$IV.DomesticViolence, Sub_Group)
tbl
chisq.test(tbl)

tbl = table(TestIVs$IV.ParentMentalHealth, Sub_Group)
tbl
chisq.test(tbl)

tbl = table(TestIVs$IV.Related, Sub_Group)
tbl
chisq.test(tbl)

tbl = table(TestIVs$IV.Adopted, Sub_Group)
tbl
chisq.test(tbl)


####
#IVBinClusterAvg <- Sub_Group
#IVBinClusterComp <- Sub_Group
# Testing chi squared overlap between CBCL groups and IV groups
tbl = table (IVClusterGroups4, Sub_Group)
tbl
chisq.test(tbl)

tbl = table (IVClusterGroups2, Sub_Group)
tbl
chisq.test(tbl)

####
ChildAge_lm <- lm(ChildCurrentAge ~ Sub_Group, data = TempIVs)
anova(ChildAge_lm)




# 5.2 Creating Colormap by CBCL Grouping
CBCL_ItemNames<-colnames(MergedData_CBCL)
ER<-grep("ER", CBCL_ItemNames)
AD<-grep("AD", CBCL_ItemNames)
SC<-grep("SC", CBCL_ItemNames)
W<-grep("W", CBCL_ItemNames)
SP<-grep("SP", CBCL_ItemNames)
AP<-grep("AP", CBCL_ItemNames)
AB<-grep("AB", CBCL_ItemNames)
OP<-grep("OP", CBCL_ItemNames)
CBCL_CategoryIndex <- CBCL_ItemNames
CBCL_CategoryIndex[ER] <- 1 
CBCL_CategoryIndex[AD] <- 2 
CBCL_CategoryIndex[SC] <- 3 
CBCL_CategoryIndex[W] <- 4 
CBCL_CategoryIndex[SP] <- 5 
CBCL_CategoryIndex[AP] <- 6 
CBCL_CategoryIndex[AB] <- 7 
CBCL_CategoryIndex[OP] <- 8 
CBCL_CategoryIndex<-as.integer(CBCL_CategoryIndex)


New_VarColors <- c("deepskyblue", "dodgerblue", "blue4", "skyblue", "yellow", "red", "indianred1", "gray63" )
New_VarColors <- New_VarColors[as.vector(CBCL_CategoryIndex)]



# 5.3 Looking at differences in DV scores between groups

AllData<- Merged_DVs_IVs_CBCL
AllData$SubGroups<-Sub_Group
tests <- c("emot_reactive_tscore", "anxious_depressed_tscore", "somatic_tscore")
for (test in tests){
  temp_lm <- lm(test ~ SubGroups, data = AllData )
}

ER_lm <- lm(emot_reactive_tscore ~ IVBinClusterComp, data = AllData )
AD_lm <- lm(anxious_depressed_tscore ~ IVBinClusterComp, data = AllData )
SC_lm <- lm(somatic_tscore ~ IVBinClusterComp, data = AllData )
W_lm <- lm(withdrawn_tscore ~ IVBinClusterComp, data = AllData )
SP_lm <- lm(sleep_tscore ~ IVBinClusterComp, data = AllData )
AtP_lm <- lm(attention_probs_tscore ~ IVBinClusterComp, data = AllData )
AB_lm <- lm(aggressive_beh_tscore ~ IVBinClusterComp, data = AllData )
AnxP_lm <- lm(anxiety_probs_tscore ~ IVBinClusterComp, data = AllData )
pdd_lm <- lm(pdd_probs_tscore ~ IVBinClusterComp, data = AllData )
adhd_lm <- lm(adhd_probs_tscore ~ IVBinClusterComp, data = AllData )
OP_lm <- lm(odd_probs_tscore ~ IVBinClusterComp, data = AllData )
Int_lm <- lm(internal_probs_tscore ~ IVBinClusterComp, data = AllData )
Ext_lm <- lm(external_probs_tscore ~ IVBinClusterComp, data = AllData )

models <- c(ER_lm, AD_lm, SC_lm, W_lm, SP_lm, AtP_lm, AB_lm, AnxP_lm, pdd_lm, adhd_lm, OP_lm, Int_lm, Ext_lm)

anova(ER_lm)
anova(AD_lm)
anova(SC_lm)
anova(W_lm)
anova(SP_lm)
anova(AtP_lm)
anova(AB_lm)
anova(AnxP_lm)
anova(pdd_lm)
anova(adhd_lm)
anova(OP_lm)
anova(Int_lm)
anova(Ext_lm)


#Each IV with Externalizing and Internalizing Scores



IntProb_lm<- lm(internal_probs_tscore ~ AllData$ChildCurrentAge, data = AllData )
anova(IntProb_lm)
IntProb_lm <- lm(internal_probs_tscore ~ AllData$IV.PriorPlacements, data = AllData )
anova(IntProb_lm)
IntProb_lm <- lm(internal_probs_tscore ~ AllData$IV.AgeRemovedfromBio, data = AllData )
anova(IntProb_lm)
IntProb_lm <- lm(internal_probs_tscore ~ AllData$IV.PhysicalAbuse, data = AllData )
anova(IntProb_lm)
IntProb_lm <- lm(internal_probs_tscore ~ AllData$IV.SexualAbuse, data = AllData )
anova(IntProb_lm)
IntProb_lm <- lm(internal_probs_tscore ~ AllData$IV.Neglect, data = AllData )
anova(IntProb_lm)
IntProb_lm <- lm(internal_probs_tscore ~ AllData$IV.ParentalIncarceration, data = AllData )
anova(IntProb_lm)
IntProb_lm <- lm(internal_probs_tscore ~ AllData$IV.ParentSubstanceAbuse, data = AllData )
anova(IntProb_lm)
IntProb_lm <- lm(internal_probs_tscore ~ AllData$IV.Dependency, data = AllData )
anova(IntProb_lm)
IntProb_lm <- lm(internal_probs_tscore ~ AllData$IV.DomesticViolence, data = AllData )
anova(IntProb_lm)
IntProb_lm <- lm(internal_probs_tscore ~ AllData$IV.ParentMentalHealth, data = AllData )
anova(IntProb_lm)
IntProb_lm <- lm(internal_probs_tscore ~ AllData$IV.Related, data = AllData )
anova(IntProb_lm)
IntProb_lm <- lm(internal_probs_tscore ~ AllData$IV.Adopted, data = AllData )
anova(IntProb_lm)

ExtProb_lm <- lm(external_probs_tscore ~ AllData$ChildCurrentAge, data = AllData )
anova(ExtProb_lm)
ExtProb_lm <- lm(external_probs_tscore ~ AllData$IV.PriorPlacements, data = AllData )
anova(ExtProb_lm)
ExtProb_lm <- lm(external_probs_tscore ~ AllData$IV.AgeRemovedfromBio, data = AllData )
anova(ExtProb_lm)
ExtProb_lm <- lm(external_probs_tscore ~ AllData$IV.PhysicalAbuse, data = AllData )
anova(ExtProb_lm)
ExtProb_lm <- lm(external_probs_tscore ~ AllData$IV.SexualAbuse, data = AllData )
anova(ExtProb_lm)
ExtProb_lm <- lm(external_probs_tscore ~ AllData$IV.Neglect, data = AllData )
anova(ExtProb_lm)
ExtProb_lm <- lm(external_probs_tscore ~ AllData$IV.ParentalIncarceration, data = AllData )
anova(ExtProb_lm)
ExtProb_lm <- lm(external_probs_tscore ~ AllData$IV.ParentSubstanceAbuse, data = AllData )
anova(ExtProb_lm)
ExtProb_lm <- lm(external_probs_tscore ~ AllData$IV.Dependency, data = AllData )
anova(ExtProb_lm)
ExtProb_lm <- lm(external_probs_tscore ~ AllData$IV.DomesticViolence, data = AllData )
anova(ExtProb_lm)
ExtProb_lm <- lm(external_probs_tscore ~ AllData$IV.ParentMentalHealth, data = AllData )
anova(ExtProb_lm)
ExtProb_lm <- lm(external_probs_tscore ~ AllData$IV.Related, data = AllData )
anova(ExtProb_lm)
ExtProb_lm <- lm(external_probs_tscore ~ AllData$IV.Adopted, data = AllData )
anova(ExtProb_lm)

#Followups
ER_lm <- lm(emot_reactive_tscore ~ AllData$IV.ParentMentalHealth, data = AllData )
anova(ER_lm)
AD_lm <- lm(anxious_depressed_tscore ~ AllData$IV.ParentMentalHealth, data = AllData )
anova(IntProb_lm)
SC_lm <- lm(somatic_tscore ~ AllData$IV.ParentMentalHealth, data = AllData )
anova(SC_lm)
IntProb_lm <- lm(withdrawn_tscore ~ AllData$IV.ParentMentalHealth, data = AllData )
anova(W_lm)

AtP_lm <- lm(attention_probs_tscore ~ AllData$IV.Adopted, data = AllData )
anova(AtP_lm)
AB_lm <- lm(aggressive_beh_tscore ~ AllData$IV.Adopted, data = AllData )
anova(AB_lm)

#Plotting

ggplot(AllData, aes(x = factor(IV.ParentMentalHealth), y =internal_probs_tscore)) + geom_violin() + geom_jitter( shape = 1, size = 3, alpha = 0.5)

ggplot(AllData, aes(x = factor(IV.ParentMentalHealth), y =emot_reactive_tscore)) + geom_violin() + geom_jitter( shape = 1, size = 3, alpha = 0.5)
ggplot(AllData, aes(x = factor(IV.ParentMentalHealth), y =anxious_depressed_tscore)) + geom_violin() + geom_jitter( shape = 1, size = 3, alpha = 0.5)
ggplot(AllData, aes(x = factor(IV.ParentMentalHealth), y =somatic_tscore)) + geom_violin() + geom_jitter( shape = 1, size = 3, alpha = 0.5)
ggplot(AllData, aes(x = factor(IV.ParentMentalHealth), y =withdrawn_tscore)) + geom_violin() + geom_jitter( shape = 1, size = 3, alpha = 0.5)

ggplot(AllData, aes(x = factor(IV.Adopted), y = external_probs_tscore)) + geom_violin() + geom_jitter(shape = 1, size = 3, alpha = 0.5)

ggplot(AllData, aes(x = factor(IV.Adopted), y = aggressive_beh_tscore)) + geom_violin() + geom_jitter(shape = 1, size = 3, alpha = 0.5)
ggplot(AllData, aes(x = factor(IV.Adopted), y = attention_probs_tscore)) + geom_violin() + geom_jitter(shape = 1, size = 3, alpha = 0.5)

external_probs_tscore

# Grouped Bar Plot
counts <- table(mtcars$vs, mtcars$gear)
barplot(counts, main="Car Distribution by Gears and VS",
        xlab="Number of Gears", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)

# SHADED LINE PLOTS
install.packages("gcookbook")
library(gcookbook) # For the data set

# Grab a subset of the climate data
clim <- subset(climate, Source == "Berkeley",
               select=c("Year", "Anomaly10y", "Unc10y"))

clim


# Shaded region
ggplot(clim, aes(x=Year, y=Anomaly10y)) +
  geom_ribbon(aes(ymin=Anomaly10y-Unc10y, ymax=Anomaly10y+Unc10y),
              alpha=0.2) +
  geom_line()


DV_Names <-c(Merged_DVs_IVs_CBCL$emot_reactive_tscore,Merged_DVs_IVs_CBCL$anxious_depressed_tscore)
Merged_DVs_IVs_CBCL$emot_reactive_tscore
Merged_DVs_IVs_CBCL$anxious_depressed_tscore
Merged_DVs_IVs_CBCL$somatic_tscore
Merged_DVs_IVs_CBCL$withdrawn_tscore
Merged_DVs_IVs_CBCL$sleep_tscore
Merged_DVs_IVs_CBCL$attention_probs_tscore
Merged_DVs_IVs_CBCL$aggressive_beh_tscore
Merged_DVs_IVs_CBCL$affective_probs_tscore
Merged_DVs_IVs_CBCL$anxiety_probs_tscore
Merged_DVs_IVs_CBCL$pdd_probs_tscore
Merged_DVs_IVs_CBCL$adhd_probs_tscore
Merged_DVs_IVs_CBCL$odd_probs_tscore
Merged_DVs_IVs_CBCL$internal_probs_tscore
Merged_DVs_IVs_CBCL$external_probs_tscore



#6.1 Apply IV data to predict CBCL group assignment.
##FUNCTION## Predict Cluster Membership with a matrix of data
GroupFactors <-  factor(x = Sub_Group)

TwoSubGroups <- Sub_Group
TwoSubGroups[TwoSubGroups=="3"] <- 2

TwoFactors <- factor(x = TwoSubGroups)

RemovedBad_IVPred <- subset(MergedData_IVs, select = -c(IV.ParentalIncarceration, IV.PhysicalAbuse, IV.ParentSubstanceAbuse, IV.Related, IV.SexualAbuse))

set.seed(415)

RF_FourthRun <- randomForest(MergedData_IVs,TwoFactors, nodesize = 1, importance = TRUE, ntree = 10000, replace = TRUE, norm.votes = TRUE)

RF_fit <- randomForest(GroupFactors ~. , data = Data, nodesize = 1, importance = TRUE, ntree = 10000, replace = TRUE, norm.votes = TRUE)
#NOTES- one important thing is to figure out how to put the X and Y in the same matrix,
#and then change the random forests code above accordingly to predict the Y from the rest of the data matrix

Data <- c(MergedData_IVs,Sub_Group)



model <- svm(GroupFactors ~ MergedData_IVs , MergedData_IVs)

predictedY <- predict(model, MergedData_IVs)

points(data$X, predictedY, col = "red", pch=4)



#Try tuning the RF parameters!

print(RF_FourthRun)
varImpPlot(RF_FourthRun)
heatmap(RF_FourthRun$confusion)

###Example
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                      Embarked + Title + FamilySize + FamilyID2,
                    data=train, 
                    importance=TRUE, 
                    ntree=2000)


#_______________________________________________________________________________________________________________________

#dataset_path <- '/Users/aki.nikolaidis/Dropbox/1_Projects/1_Research/2_FosterCare/Data/CBCL_Full.csv'
dataset_path <- '~/Dropbox/1_Projects/1_Research/2_FosterCare/Data/CBCL_Full.csv'
outputdir <- '~/Dropbox/1_Projects/1_Research/2_FosterCare/Results'
second_dataset <- '~/Dropbox/1_Projects/1_Research/2_FosterCare/Data/IVs_Full.csv'     
DVs_Full <- '~/Dropbox/1_Projects/1_Research/2_FosterCare/Data/DVs_Full.csv' 
group_thresh = 1.7
variable_thresh = 2   

##FUNCTION## Apply clustering to data matrix. Output clustering variables and visualization.
TwoWayclust <- function(dataset_path, group_thresh = 1.5, variable_thresh = 1.5, outputdir) {
  #takes in a csv dataset, cleans it up, standardizes it, and calculates the twowayclust on the data at the threshold defined by the user
  #saves the heatmap, and outputs the mycl for the group differences function
  
  #change so that if variable is already loaded into workspace the can call it
  #if variable is a path then do the following, else just set Clusterset equal to the dataset_path variable
  
  # 1 - Take in data
  if (is.character(dataset_path)==TRUE) {
    UnprocessedData <- read.csv(dataset_path,row.names = 1, header = TRUE, sep = ",");
  } else {
    UnprocessedData <- dataset_path
  }
  
  UnprocessedData <- UnprocessedData[complete.cases(UnprocessedData),];
  #UnprocessedData1 <- UnprocessedData[duplicated(UnprocessedData[,1])==FALSE , ]
  sum(is.na(UnprocessedData))
  Clusterset <- data.frame(UnprocessedData)
  
  
  # 2 - Clean Data
  Clusterset <- Clusterset[ , colSums(is.na(Clusterset)) == 0]
  Clusterset <- Clusterset[complete.cases(Clusterset),];
  Clusterset <- as.matrix(Clusterset, na.rm = TRUE);
  
  #Scaling is Optional
  
  #Clusterset <- scale(Clusterset)  
  #sum(is.na(Clusterset))
  #Clusterset <- Clusterset[, colSums(!is.na(Clusterset)) > 0]
  #sum(is.na(Clusterset))
  #Clusterset <- scale(Clusterset) 
  #Clusterset <- Clusterset[, colSums(!is.na(Clusterset)) > 0]
  #sum(is.na(Clusterset))
  #OutlierID <- abs(Clusterset)<4
  #subs <- apply(OutlierID, 1, min)
  #Clusterset <- Clusterset[subs==1,]
  #sum(is.na(Clusterset))
  
  
  # 3.1 Euclidean + Ward Clustering of Subjects
  #r_dist <- dist(Clusterset, method = "euclidean")
  #hr <- hclust(r_dist, method = "ward.D2");
  r_dist <- dist(Clusterset, method = "binary")
  hr <- hclust(r_dist, method = "complete");
  
  # 3.2 Spearman + Complete Clustering of Variables
  #c_dist <- dist(t(Clusterset), method = "euclidean")
  #hc <- hclust(c_dist, method = "ward.D2")
  c_dist <- dist(t(Clusterset), method = "binary")
  hc <- hclust(c_dist, method = "complete")
  
  # 4 Subject Group Assignment
  Sub_Group <- cutree(hr, h = max(hr$height)/group_thresh)
  mycolhr <- rainbow(length(unique(Sub_Group)), start = 0.1, end = 0.9); 
  mycolhr <- mycolhr[as.vector(Sub_Group)]
  
  # 5 Variable Group Assignment
  Var_Group <- cutree(hc, h = max(hc$height)/variable_thresh)
  mycolhc <- rainbow(length(unique(Var_Group)), start = 0.1, end = 0.9); 
  mycolhc <- mycolhc[as.vector(Var_Group)]
  
  # 6 Visualization
  heatmap(Clusterset, Rowv = as.dendrogram(hr), Colv = as.dendrogram(hc), col = inferno(256), scale = "none", RowSideColors = mycolhr, ColSideColors = mycolhc)
}


#_______________________________________________________________________________________________________________________

GroupDifferences <- function(dataset_path, second_dataset,mycl) {
  
  Clusterset2<-read.csv(second_dataset,row.names = 1, header = TRUE, sep = ",");
  Clusterset2<-Clusterset2[ , colSums(is.na(Clusterset2)) == 0]
  Clusterset2<-Clusterset2[complete.cases(Clusterset2),];
  Clusterset2<-as.matrix(Clusterset2);
  #Clusterset2<-scale(t(Clusterset2))
  #Clusterset2<-scale(t(Clusterset2))
  
  
}
#____________

#_______________________________________________________________________________________________________________________



for (i in max(Sub_Group)) {
  
  GroupSubs <- assign(paste("ClustGroup_", i, sep=""),Sub_Group[ which(Sub_Group==i)] )
  GroupSubs <- as.matrix(GroupSubs)
  as.matrix(assign(paste("ClustGroup_", i, "Data", sep=""), Clusterset[GroupSubs,] ))
  
  
  
  
  
  #  Group_i_Labels<-as.matrix(print(Group_i))
  #  assign(paste("ClustGroup_", i, "Labels", sep=""))<-(colnames(as.name(paste("ClustGroup_", i, sep=""))))
  
  #  <-Clusterset[as.name(paste("ClustGroup_", i, "Labels", sep="")),]
  #  Group_i_Data<-data.frame(Group_i_Data)
}
#_______________________________________________________________________________________________________________________





#_______________________________________________________________________________________________________________________

hr <- hclust(as.dist(1 - cor(t(Std_CBCL_Full), method = "spearman")), method = "ward.D2");
hc <- hclust(as.dist(1 - cor(Std_CBCL_Full, method = "spearman")), method = "ward.D2");

Sub_Group <- cutree(hr, h = max(hr$height)/group_thresh)
mycolhr <- rainbow(length(unique(Sub_Group)), start = 0.1, end = 0.9); 
mycolhr <- mycolhr[as.vector(Sub_Group)]

mycl <- cutree(hr, h = max(hr$height)/1.5); 
mycolhc <- rainbow(length(unique(mycl)), start = 0.1, end = 0.9); 
mycolhc <- mycolhc[as.vector(mycl)]


heatmap(Std_CBCL_Full, Rowv = as.dendrogram(hr), Colv = as.dendrogram(hc), col = inferno(256), scale = "row", density.info = "none", trace = "none", RowSideColors = mycolhc)


NewMat <- as.matrix(print(mycl))

Group1<-mycl[ which(mycl=='1')]
Group1Labels<-as.matrix(print(Group1))
Group1Labels<-(rownames(Group1Labels))
Group1Data<-Std_CBCL_Full[Group1Labels,]
Group1Data<-data.frame(Group1Data)


#_______________________________________________________________________________________________________________________

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}




