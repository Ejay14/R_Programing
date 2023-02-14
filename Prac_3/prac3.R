# Factor Analysis - Example Data Set


# ------------------------------------------------------------------------------------------------

airman <- read.csv("airman.csv", header=TRUE)
X <- airman[,-1]
p <- dim.data.frame(X)[2]
n <- dim.data.frame(X)[1]
# FA: decide whether to standardise the data or not.
head(X)


# decide whether to standardise the data or not.
my.eigen <- svd(cor(X))$d
plot(my.eigen, type="b") # determine the number of Factors with the Elbow rule or Unity rule
cumsum(my.eigen)/sum(my.eigen)

# SET NUMBER OF FACTORS, needs to change, depending on data set
q <- 5
                
# FA: Maximum likelihood
cov.list <- list (cov = cor(X), center = rep(0,p), n.obs=n)
faMLE <- factanal(covmat = cov.list, factors = q, rotation="varimax")
faMLE
communalitiesMLE <- 1-matrix(faMLE$uniquenesses,p,1)
row.names(communalitiesMLE) = names(X)
communalitiesMLE

# FA: PCA estimation
svd.out <- svd(cor(X))
gamma.mat <- svd.out$v[,1:q] %*% diag(sqrt(svd.out$d[1:q]))
row.names(gamma.mat) <- names(X)
gamma.mat
sqgammas <- gamma.mat^2
communalitiesPCA <- matrix(rowSums(sqgammas), p,1)
row.names(communalitiesPCA) = names(X)
communalitiesPCA

### -------------------- DONOR DATA FA
# Extract data from two data frames, donor and derived.
donor <- read.csv("donor.csv", header=TRUE)
derived <- read.csv("derived.csv", header=TRUE)

major <- donor[donor$MDMAUD != 28,]
FA.dat <- cbind(with(major, cbind(HIT, CARDPROM, NUMPROM, CARDPM12, NUMPRM12, RAMNTALL, NGIFTALL, CARDGIFT, MINRAMNT, MAXRAMNT, LASTGIFT, AVGGIFT)), with(derived, cbind(freq94, freq95, freq96, freqT1, freqT2, freqT3, freqT4, freqT5, freqT6, freqT7, freqT8, amount94, amount95, amount96, amountT1, amountT2, amountT3, amountT4, amountT5, amountT6, amountT7, amountT8)))
# Get an idea of the first few lines of the data
head(FA.dat)
#Size of data matrix
dim(FA.dat)
X <- data.frame(FA.dat)
# SET NUMBER OF FACTORS, needs to change, depending on data set
q <- 4
# Perform FA with maximum likelihood and PCA estimation

