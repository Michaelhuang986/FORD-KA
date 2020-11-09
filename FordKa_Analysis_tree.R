###############################################################################
# Script: FordKa_Analysis_tree.R
#
# R script to compute decision tree
# Requires the excel spreadsheet with the data (FordKaData.xlsx).
###############################################################################



###############################################################################
### setup the environment
###############################################################################

# load in additional packages to extend R functionality
if (!require(lattice)) {install.packages("lattice"); library(lattice)}
if (!require(ggplot2)) {install.packages("ggplot2"); library(ggplot2)}
if (!require(reshape2)) {install.packages("reshape2"); library(reshape2)}
if (!require(openxlsx)) {install.packages("openxlsx"); library(openxlsx)}
if (!require(gmodels)) {install.packages("gmodels"); library(gmodels)}
if (!require(psych)) {install.packages("psych"); library(psych)}
# packages for trees
if (!require(tree)) {install.packages("tree"); library(tree)}
if (!require(rpart)) {install.packages("rpart"); library(rpart)}
if (!require(rattle)) {install.packages("rattle"); library(rattle)}
if (!require(rpart.plot)) {install.packages("rpart.plot"); library(rpart.plot)}
if (!require(RColorBrewer)) {install.packages("RColorBrewer"); library(RColorBrewer)}
if (!require(party)) {install.packages("party"); library(party)}
if (!require(partykit)) {install.packages("partykit"); library(partykit)}

# set to your correct working directory
setwd("C:\\Users\\vhao\\Desktop\\BA\\Google") # only works in Rstudio scripts



###############################################################################
### input data
###############################################################################

# read in Ford Ka datasets from the Excel file
forddemo <- read.xlsx("FordKaData.xlsx",sheet = 1,startRow = 7,colNames = T,rowNames = F,cols = 2:10)# read the demographic data
fordpsyc=read.xlsx("FordKaData.xlsx",sheet=4,startRow=7,colNames=T,rowNames=F,cols=2:63)  # read the psychographic data
fordquest=read.xlsx("FordKaData.xlsx",sheet=5,startRow=7,colNames=T,rowNames=F,cols=2)  # read the question list
fordseg=read.xlsx("FordKaData.xlsx",sheet=1,startRow=7,colNames=T,rowNames=F,cols=11:12)  # read the segments that ford created (do not use in cluster)

# if you have problems with read.xlsx you can read in the data from CSV files (make sure you uncomment lines below and download CSV files)
#forddemo=read.csv("FordKaDemographicData.csv",row.names=1)  # just the demographic data
#fordpsyc=read.csv("FordKaPsychographicData.csv",row.names=1)  # just the psychographic data
fordquest=scan("FordKaQuestions.txt",what='a',sep='\n')  # question list, which is read as a vector
#fordseg=read.csv("FordKaSegmentData.csv")  # these are the segments that Ford came up with

# transform the data to make it easier to use
fordquest=paste0(1:62,',',fordquest$Statement)  # convert the question list into a character string to make it easier to work with
afordquest=strtrim(fordquest,30) # truncate the strings to the first 30 characters since some questions are quite long
fordseg$SegName=as.factor(fordseg$SegmentName)  # convert the segment names into a factor for easier use as a classification variable
fordseg$SegmentName=NULL  # remove this variable
ford = cbind(forddemo,fordpsyc)  # create a new dataframe with both demogrpahic and psychographic data

# create some lists of variables which we will use later in the script
nqlist=1:62  # sequence of numbers from 1 to 62
qlist=paste0('Q',nqlist)
# let's try to cluster our questions by transposing the question data
nshortqlist=c(30,57,53,1,4,12)  # short list of questions
shortqlist=paste0('Q',nshortqlist)  # append Q in front of the numbers to generate a list of questions to match variable names
shortqname=strtrim(fordquest[nshortqlist],30) # the first 30 characters of the strings
nvars=match(qlist,colnames(ford))
nvars# define list of numeric variables

# create new standardized datasets using the scale function (set the mean of the new variable to 0 and stddev to 1)
xforddemo=scale(forddemo)
xfordpsyc=scale(fordpsyc)
xford=scale(ford)

# create a transposed version of the data
qford=t(ford[,qlist])
qford
# create factorized versions of the data (so R interprets variables as discrete)
nford=ford
nford$StrongPref = (ford$PreferenceGroup==1)   # this is our target: Is the consumer interested in Ka (Preference Group #1)
nford$PreferenceGroup = NULL
nford$Gender = as.factor(ford$Gender)
nford$MaritalStatus = as.factor(ford$MaritalStatus)
nford$AgeCategory = NULL
nford$ChildrenCategory = as.factor(ford$ChildrenCategory)
nford$IncomeCategory = as.factor(ford$IncomeCategory)
nford$FirstTimePurchase = as.factor(ford$FirstTimePurchase)

# check our data
head(nford)
str(nford)



###############################################################################
## create a segmentation scheme based upon a decision tree
###############################################################################

# estimate a decision tree
ctree = rpart(StrongPref~., data=nford, control=rpart.control(minsplit=20,minbucket=40,cp=.01))
summary(ctree)

# plot the results
plot(ctree)
text(ctree)

# alternative plots
prp(ctree)  # gives a tree
fancyRpartPlot(ctree)  # colorizes the tree

# compare the tree classifications
padopter = predict(ctree,newdata=nford,type='vector')  # generate probability
cadopter = as.vector((padopter>.5)+0)  # classify the predictions as adopters or not

# compute a k-means cluster with k=4 using just the psychographics
set.seed(1248765792)
(grpA=kmeans(xford[,qlist],centers=4))
xtabs(~fordseg$SegName+grpA$cluster)

# compare decision tree "cluster" with psychographic clusters
xtabs(~fordseg$SegName+cadopter)

# estimate an alternative tree (more complexity)
ctree = rpart(StrongPref~., data=nford, control=rpart.control(cp=.02))
summary(ctree)
prp(ctree)

