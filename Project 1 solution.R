# K-Mean Clustering on Ford KA
# Haoyuan Huang

#Set working Environment
if(!require('tidyverse')){install.packages("tidyverse");library(tidyverse)}
if(!require("cluster")){install.packages("cluster");library("cluster")}
if(!require("factoextra")){install.packages("factoextra");library("factoextra")}
if(!require("corrplot")){install.packages("corrplot");library("corrplot")}
if(!require("gplots")){install.packages("gplots");library("gplots")}

#Import Data
setwd("C:\\Users\\vhao\\Desktop\\BA\\Google\\Project 1 solution")
psy <- read.csv(file = "Ford.csv")


# Cluster the Data(select only 'extreme' questions)
newpsy=psy
questions <- c('Q1','Q15','Q21','Q22','Q24','Q25','Q26','Q28','Q42','Q54',	'Q55','Q56','Respondent')
newpsy <- newpsy %>%
   select(questions)

#Correlation Matrix
CorrMatrix <- cor(newpsy)
corrplot(CorrMatrix,'number')

#Distance Matrix
distance <- get_dist(newpsy)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


#Determine K (Elbow Method)
fviz_nbclust(newpsy,kmeans,method = "wss")+geom_vline(xintercept = 3 , linetype = 2)
#Silhouette Method
fviz_nbclust(newpsy, kmeans, method = "silhouette")

set.seed(125)
# compute multiple cluster solutions
grpA2=kmeans(newpsy,centers=2)
grpA3=kmeans(newpsy,centers=3)
grpA4=kmeans(newpsy,centers=4)
grpA5=kmeans(newpsy,centers=5)
grpA6=kmeans(newpsy,centers=6)
grpA7=kmeans(newpsy,centers=7)
grpA8=kmeans(newpsy,centers=8)
grpA9=kmeans(newpsy,centers=9)
grpA10=kmeans(newpsy,centers=10)
grpA15=kmeans(newpsy,centers=15)
grpA20=kmeans(newpsy,centers=20)
grpA30=kmeans(newpsy,centers=30)
# compute between and within SS
kclust=c(2:10,15,20,30)
bss=c(grpA2$betweenss,
      grpA3$betweenss,grpA4$betweenss,grpA5$betweenss,grpA6$betweenss,
      grpA7$betweenss,grpA8$betweenss,grpA9$betweenss,grpA10$betweenss,
      grpA15$betweenss,grpA20$betweenss,grpA30$betweenss)
wss=c(grpA2$tot.withinss,
      grpA3$tot.withinss,grpA4$tot.withinss,grpA5$tot.withinss,grpA6$tot.withinss,
      grpA7$tot.withinss,grpA8$tot.withinss,grpA9$tot.withinss,grpA10$tot.withinss,
      grpA15$tot.withinss,grpA20$tot.withinss,grpA30$tot.withinss)
# plot the results and look for the "Hockey-Stick" effect
par(mfrow=c(1,1))
plot(kclust,bss,type="l",main="Between SS for k-means")
points(kclust,bss)
plot(kclust,wss,type="l",main="Within SS for k-means")
points(kclust,wss)
plot(kclust,bss/(wss+bss),type="l",main="R-Squared for k-means")
points(kclust,bss/(wss+bss))


#K-Mean Clustering
set.seed(1248765792)
kc = kmeans(newpsy,centers = 3, nstart = 10) 
str(kc)
print(kc$centers)

#Visualization
fviz_cluster(kc, data = newpsy)

clusplot(newpsy,kc$cluster,main = '2D Representation of the Cluster Solution',shade = T, color = T, lines = 1, labels = 2)

#Cross-Tabulation
setwd("C:\\Users\\vhao\\Desktop\\BA\\Google")
ford <- read.csv(file="FordKaDemographicData.csv")
balloonplot(table(ford$PreferenceGroup,kc$cluster),ylab = "Clusters",xlab = "PreferenceGroup")

#Visuilation
install.packages("lattice")
library(lattice)

parallelplot(newpsy[2:63,],groups=newpsy$Q1,main="Q1",auto.key=list(space="top",columns=3,lines=T))
parallelplot(newpsy[2:63,],groups=kc$cluster,main="Clusters",auto.key=list(space="top",columns=3,lines=T))
parallelplot(kc$centers,main="Clusters",auto.key=list(text=c("1","2","3"),space="top",columns=3,lines=T))
parallelplot(kc$centers,main="Clusters",common.scale=TRUE,auto.key=list(text=c("1","2","3"),space="top",columns=3,lines=T))






             