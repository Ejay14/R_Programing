# donor <- read.csv("~/Teaching/STA3022/donor/donor.csv")


Eurojobs <- read.table("Eurojobs.txt",sep="\t", header=TRUE)
X <- as.matrix (Eurojobs[,-1])
rownames(X) <- Eurojobs[,1]

# PCA: scree plot and proportion of variance
# decide whether to standardise the data or not. 
my.eigen <- svd(cor(X))$d  #  or svd(var(X))$d
plot(my.eigen, type="b")
cumsum(my.eigen)/sum(my.eigen)


# SET NUMBER OF COMPONENTS, needs to change, depending on data set
r <- 3

# PCA: loadings and scores
out <- princomp(X, cor=TRUE)  # or out <- princomp (X)
out$loadings[,1:r]
out$scores[,1:r]

# PCA: biplot
PCAbiplot(X, scaled.mat=T)


### -------------------- DONOR DATA PCA

# Extract data from data frame. Using with means that donor$ need not appear before each variable name
PCA.dat <- with (donor, cbind (AGE, INCOME, WEALTH1, NUMCHLD, NUMPROM, RAMNTALL, AVGGIFT, RFA_2F))
# Get an idea of the first few lines of the data
head(PCA.dat)
# Size of data matrix
dim(PCA.dat)
# Remove missing values, rowwise, i.e. remove the whole row if there is at least one missing observation
PCA.dat <- na.omit(PCA.dat)
# Size of data matrix without missing values
dim(PCA.dat)
# Construct scree plot with code similar to lines 4 to 11
# Report proportion of variation explained by first two principal components
# Construct PCA biplot. Since there are many observations, swith off sample labels by 
#    adding samples=list(label=F), 
# inside call PCAbiplot ()
# Discuss relationships between donor behaviour and demographics by interpreting PCA biplot
PCAbiplot (PCA.dat, scaled.mat=T, samples=list(label=F))
