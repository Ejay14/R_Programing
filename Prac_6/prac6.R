protein <- read.csv("protein.csv")
X <- as.matrix (protein[,-1])   # or standardised data:  X <- as.matrix(scale(datasetname))
rownames(X) <- protein[,1]
require(cluster)

# CLUSTER ANALYSIS: Single linkage
out.single.euc <- hclust(daisy(X,metric="euclidean"),method="single")
plot(out.single.euc)
 # decide to cut the tree at height 9
out.single.euc <- cutree(out.single.euc, h=9)
 # view cluster allocation
names (out.single.euc) <- protein[,1]
sort(out.single.euc)

out.single.city <- hclust(daisy(X,metric="manhattan"),method="single")
plot(out.single.city)
# decide to cut the tree into 6 clusters
out.single.city <- cutree(out.single.city, k=6)
# view cluster allocation
names (out.single.city) <- protein[,1]
sort(out.single.city)

out.single.cor <- hclust(as.dist(1-cor(t(X))),method="single")
plot(out.single.cor)

# Complete linkage
out.complete.euc <- hclust(daisy(X,metric="euclidean"),method="complete")
plot(out.complete.euc)
# decide to cut the tree into 5 clusters
out.complete.euc <- cutree(out.complete.euc, k=5)
# view cluster allocation
names (out.complete.euc) <- protein[,1]
sort(out.complete.euc)

out.complete.city <- hclust(daisy(X,metric="manhattan"),method="complete")
plot(out.complete.city)
# decide to cut the tree at a height of 40
out.complete.city <- cutree(out.complete.city, h=40)
# view cluster allocation
names (out.complete.city) <- protein[,1]
sort(out.complete.city)

out.complete.cor <- hclust(as.dist(1-cor(t(X))),method="complete")
plot(out.complete.cor)
# decide to cut the tree into 5 clusters
out.complete.cor <- cutree(out.complete.cor, k=5)
# view cluster allocation
names (out.complete.cor) <- protein[,1]
sort(out.complete.cor)

# Centroid clustering
out.centroid.euc <- hclust(daisy(X,metric="euclidean"),method="centroid")
plot(out.centroid.euc)
out.centroid.city <- hclust(daisy(X,metric="manhattan"),method="centroid")
plot(out.centroid.city)
out.centroid.cor <- hclust(as.dist(1-cor(t(X))),method="centroid")
plot(out.centroid.cor)

# set number of clusters
r <- 6

out.kmeans <- kmeans(X,centers=r)$cluster
# view cluster allocation
names (out.kmeans) <- protein[,1]
sort(out.kmeans)

# CLUSTER PROFILING
clusvec <- out.complete.euc

# calculate means
class.means <- apply(X, 2, function(x) tapply (x, clusvec, mean))
class.means

# plot means
plot (c(1,ncol(X)),range(class.means),type="n",xlab="",ylab="Average proportion of protein intake",xaxt="n")
axis (side=1, 1:ncol(X), colnames(X), las=2)
#ensure you list enough colours for the number of clusters
colvec <- c("green","gold","blue","red","black")
for (i in 1:nrow(class.means))
  lines (1:ncol(X),class.means[i,],col=colvec[i])

#MULTIDIMENSIONAL SCALING

# REMEMBER: if you want to represent the output of a clustering algorithm with colours, the distances used for the
#           MDS must correspond to that used for the clustering

# Classical scaling
out.Euc <- cmdscale(daisy(X,metric="euclidean"))
# create empty plot
plot (out.Euc, type="n", xlab="", ylab="", xaxt="n", yaxt="n", asp=1)
#ensure you list enough colours for the number of clusters
colvec <- c("green","gold","blue","red","black")
for (i in 1:nrow(X))
  text (out.Euc[i,1],out.Euc[i,2],rownames(X)[i],col=colvec[clusvec[i]],cex=0.85)

# Metric SMACOF
require(smacof)
out.Euc <- smacofSym(daisy(X,metric="euclidean"),type="ratio")$conf
# create empty plot
plot (out.Euc, type="n", xlab="", ylab="", xaxt="n", yaxt="n", asp=1)
#ensure you list enough colours for the number of clusters
colvec <- c("green","gold","blue","red","black")
for (i in 1:nrow(X))
  text (out.Euc[i,1],out.Euc[i,2],rownames(X)[i],col=colvec[clusvec[i]],cex=0.85)

# Non-metric SMACOF
require(smacof)
out.Euc <- smacofSym(daisy(X,metric="euclidean"),type="ordinal")$conf
# create empty plot
plot (out.Euc, type="n", xlab="", ylab="", xaxt="n", yaxt="n", asp=1)
#ensure you list enough colours for the number of clusters
colvec <- c("green","gold","blue","red","black")
for (i in 1:nrow(X))
  text (out.Euc[i,1],out.Euc[i,2],rownames(X)[i],col=colvec[clusvec[i]],cex=0.85)

# Sammon
require(MASS)
out.Euc <- sammon(daisy(X,metric="euclidean"))$points
# create empty plot
plot (out.Euc, type="n", xlab="", ylab="", xaxt="n", yaxt="n", asp=1)
#ensure you list enough colours for the number of clusters
colvec <- c("green","gold","blue","red","black")
for (i in 1:nrow(X))
  text (out.Euc[i,1],out.Euc[i,2],rownames(X)[i],col=colvec[clusvec[i]],cex=0.85)

# Kruskal's non-metric MDS
require(MASS)
out.Euc <- isoMDS(daisy(X,metric="euclidean"))$points
# create empty plot
plot (out.Euc, type="n", xlab="", ylab="", xaxt="n", yaxt="n", asp=1)
#ensure you list enough colours for the number of clusters
colvec <- c("green","gold","blue","red","black")
for (i in 1:nrow(X))
  text (out.Euc[i,1],out.Euc[i,2],rownames(X)[i],col=colvec[clusvec[i]],cex=0.85)

### -------------------- DONOR DATA ANALYSIS

X <- derived
# distance calculation for numeric data
numeric.dat <- X[,-ncol(derived)]
head(numeric.dat)
numeric.D <- daisy(scale(numeric.dat), metric="euclidean")
# distance calculation for categorical data
cat.dat <- with(derived, data.frame(substring(MDMAUD,1,1), substring(MDMAUD,2,2), substring(MDMAUD,3,3)))
colnames(cat.dat) <- c("Recency","Freq","Amount")
# ensure numeric category level indicators are not confused with numerical variables
cat.dat[,1] <- factor(cat.dat[,1])
cat.dat[,2] <- factor(cat.dat[,2])
cat.dat[,3] <- factor(cat.dat[,3])
head(cat.dat)
categorical.D <- daisy(cat.dat,metric="gower")
# combine two distances with appropriate scaling
Dmat <- categorical.D/ncol(cat.dat)*ncol(X) + numeric.D/max(numeric.D)*ncol(X)

