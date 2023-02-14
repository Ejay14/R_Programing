install.packages("ca") #Do not need the FactoMineR package
library(ca)
library(readr)

tea <- read.csv("tea.csv", header = TRUE)
datatea = tea[,c("Tea", "How", "how", "sugar", "where", "always")]
head(datatea)
# Create two-way frequency table between Tea and Where variables
tabletea = table(datatea$Tea, datatea$where)
tabletea
addmargins(tabletea)

# Example CA
simpleCAdata = data.frame(datatea[,c(1,5)])
table.CA = table(simpleCAdata) # Create two-way frequency table
# Chi-squared test
chisq.test(table.CA)
chisq.test(table.CA)$expected
# CA map
require(ca)
ca(table.CA)
plot(ca(table.CA))
# USE THE FREQUENCY TABLE IN THE CHISQ.TEST() and CA() functions

# ------------------------------------------------------------------------------------------------
#### Example MCA ####
# perform MCA
require(ca)
teaMCA = mjca(datatea,lambda="JCA")
teaMCA$sv # singular values
teaMCA$sv^2 # eigen values
teaMCA$sv^2/sum(teaMCA$sv^2) #Percentages of explained inertia
# mailordermca$inertia.e #Percentages of explained inertia
plot(teaMCA)
#ask helpfile of mjca function to see under Value how to obtain the column coordinates for plotting
?mjca
names(teaMCA)
teaMCA$rowpcoord

### -------------------- DONOR DATA CA
donor <- read_csv("donor.csv")
dim(donor) # check number of donors in data set
#remove donors with missing domain information
ca.dat <- donor[!is.na(donor$DOMAIN),]
# check number of donors after removing missing values
dim(ca.dat)
interests <- with(ca.dat, cbind (tapply(COLLECT1, DOMAIN, function(x)sum(x==2)),
                                  tapply(VETERANS, DOMAIN, function(x)sum(x==2)),
                                  tapply(BIBLE, DOMAIN, function(x)sum(x==2)),
                                  tapply(CATLG, DOMAIN, function(x)sum(x==2)),
                                  tapply(HOMEE, DOMAIN, function(x)sum(x==2)),
                                  tapply(PETS, DOMAIN, function(x)sum(x==2)),
                                  tapply(CDPLAY, DOMAIN, function(x)sum(x==2)),
                                  tapply(STEREO, DOMAIN, function(x)sum(x==2)),
                                  tapply(PCOWNERS, DOMAIN, function(x)sum(x==2)),
                                  tapply(PHOTO, DOMAIN, function(x)sum(x==2)),
                                  tapply(CRAFTS, DOMAIN, function(x)sum(x==2)),
                                  tapply(FISHER, DOMAIN, function(x)sum(x==2)),
                                  tapply(GARDENIN, DOMAIN, function(x)sum(x==2)),
                                  tapply(BOATS, DOMAIN, function(x)sum(x==2)),
                                  tapply(WALKER, DOMAIN, function(x)sum(x==2)),
                                  tapply(KIDSTUFF, DOMAIN, function(x)sum(x==2)),
                                  tapply(CARDS, DOMAIN, function(x)sum(x==2)),
                                  tapply(PLATES, DOMAIN, function(x)sum(x==2))))
colnames(interests) <- c("Collect","Veterans","Bible","Catelogue","HomeE","Pets","CDplay","Stereo","PCowners","Photo","Crafts","Fisher","Gardening","Boats","Walker","Kidstuff","Cards","Plates")
interests

# perform chi-squared test and CA and construct a CA map

### -------------------- DERIVED DATA MCA

MCA.dat <- as.data.frame(with(derived, cbind(substring(MDMAUD,1,1), substring(MDMAUD,2,2), substring(MDMAUD,3,3))))
colnames(MCA.dat) <- c("Recency","Freq","Amount")
head(MCA.dat)

#hint: in the plot command set labels=c(1,1) to prevent labels from being too far out.

