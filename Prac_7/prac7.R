# Classification Trees by Dr. Sebnem Er ####
library(tree)
library(rpart)
library(partykit)



# Load data into your working directory.####
attach()
names()
# Define the formula and data
formula=
datatowork=
yvariable=
minsplitnumber=         # USE 10% of the DATA (Round DOWN to the next integer, if 24.6 then 24, if 24.1 then 24)
# USING RPART package for CART Algorithm####
rpartfit <- rpart(formula, data=datatowork, method="class", control = rpart.control(minsplit = minsplitnumber))
rpartfit

# plot the tree obtained by CART Algorithm
plot(rpartfit, margin=.12)
text(rpartfit, use.n=TRUE)

# Nicer view of the plot using as.party() function in partykit
partyfit=as.party(rpartfit)
# here the complexity parameter is 0.1 and minimum number of objects in a node is 20 (default)
print(partyfit)
plot(partyfit, type = "simple")

# Classification matrix
p1=predict(partyfit, datatowork)
table(yvariable,p1)

# end of Prac8