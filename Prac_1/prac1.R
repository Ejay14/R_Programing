# Analysis of Variance-Covariance by Jabulani Mavodze ####
# TO BEGIN, WE READ IN THE DATA: workspace > import dataset
datatowork = houseprices
attach(datatowork)
names(datatowork)
head(datatowork)

y = price
x1 = area
x2 = security

# Calculating the means for each categories
tapply(y,list(x1), mean)
tapply(y,list(x2), mean)
tapply(y,list(x1, x2), mean)

# ONE WAY ANOVA ####
# One way ANOVA with the treatment variable AREA
anova1way.x1=aov(y~x1,data=datatowork)
summary(anova1way.x1)

# One way ANOVA with the treatment variable security
anova1way.x2=aov(y~x2,data=datatowork)
summary(anova1way.x2)

# POST HOC TEST FOR MEAN DIFFERENCES
# Scheffe Function for PostHoc Tests
# source from scheffe.R file : Code > Source File > scheffe.R

# POST HOC TEST FOR MEAN DIFFERENCES of Areas
scheffe(datatowork,y,x1,0.05)
# POST HOC TEST FOR MEAN DIFFERENCES of Security
scheffe(datatowork,y,x2,0.05)

# TWO WAY ANOVA ####
anova2way=aov(y~x1+x2,data=datatowork)
drop1(anova2way, ~., test="F")

# TWO WAY ANOVA WITH INTERACTIONS ####
options(contrasts=c("contr.sum", "contr.poly"))
anovaInteract=aov(y~x1*x2,data=datatowork)
drop1(anovaInteract,~., test="F" )

# ANCOVA ####
x3 = size
ancova1 = aov(y~x3+x1+x2,data=datatowork)
drop1(ancova1,~.,test="F")

# Multiple Linear Regression (MLR) Analysis by Dr. Sebnem Er ####
# lm() function for MLR

cor(datatowork[,c(1,2,3,4)])

# Adding Dummy Variables for Categorical Variables like Area
options(contrasts=c("contr.treatment", "contr.poly"))
x1f = factor(x1)
dummyx1 = model.matrix(~x1f)
head(dummyx1)

# Adding Dummy Variables for Categorical Variables like Security
x2f = factor(x2)
dummyx2 = model.matrix(~x2f)
head(dummyx2)

# Using all variables together - Simultaneous
x3 = size
x4 = roomnumber
x5 = bathnumber

modelAll=lm(y~x1f+x2f+x3+x4+x5,data=datatowork)
summary(modelAll)


# For the standardized Regression Coefficients (Betas)
modelAllStandardized = lm(scale(y) ~ scale(x3) + scale(x4)+
                            scale(x5),
                          data=datatowork)
summary(modelAllStandardized)

# To predict price when size=100, roomnumber=2, bathnumber=2, area="CityBowl", security="24hour"
predict(modelAll, list(x3=100, x4=2,x5=2, x1f="CityBowl", 
                       x2f="24hour"))

#For stepwise regression
step(modelAll, direction="backward", test="F")
modelStepwise= update(modelAll, .~.-x4-x5-x2f)
summary(modelStepwise)


predict(modelStepwise, list(x3=100,x1f="CityBowl")
