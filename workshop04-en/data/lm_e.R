# Name: 
# Date:
# Description: Linear models, t-test, ANOVA, and ANCOVA
# Dataset: File names is "birdsdiet.csv" and "dickcissel.csv"
# Notes: Material in R script obtained from Brian McGill (http://brianmcgill.org/614) class material
#***************************************************************************************#

#### Loading the data ####
rm(list=ls())
install.packages("e1071")
install.packages("MASS")
library(e1071)
library(MASS)
setwd("~/Dropbox/CSBQ:QCBS R Ateliers:Workshops (1)/Workshop 4-lm/English Final")
# Note that this path will NOT work on your computer! You must provide the path of where your data
# is saved, or set up an R Project as shown in earlier workshops
bird<-read.csv("birdsdiet.csv")

#### Data exploration ####
names(bird)
str(bird)
head(bird)
summary(bird) 

# Explore basic relationships
plot(bird)  # note: pairs(bird) does the same thing

#***************************************************************************************#

#### Linear Regression ####

# Regression allows to model a linear relationship between a response (Y) and a dependent variable (X)
# Let's create some fake data to see a simple example:
X <- 1:100
Y <- X + rnorm(100,sd=5) # where rnorm(100) creates a 100 random data points from a normal distribution. See ?runif for more info
lm1 <- lm(Y~X) # this is our regression model where Y ~ X means Y "as a function of" X
plot(X,Y) # to visualize the data we just created

# We can run through the diagnostic plots of this fake data
opar <- par(mfrow=c(2,2)) # this is to draw subsequent figures in a 2-by-2 array because the plot(lm1) command will produce 4 figures
plot(lm1)
par(opar) # resets the window to one graph only

# Now, let's try this with the birds dataset!
# Our main hypothesis is that Maximum Abundance is a linear function of Mass
lm1 <- lm(bird$MaxAbund ~ bird$Mass) # Regression of Maximum Abundance on Mass

# or, to be more consice, you can write data=bird instead of data$variable.name 
lm1 <- lm(MaxAbund ~ Mass, data=bird)

# Let's run through the diagnostic plots (to do BEFORE looking at p-values)
opar <- par(mfrow=c(2,2)) 
plot(lm1)
par(opar) 
# What do the residuals suggest?
# Note: The residuals can also be extracted from lm1 using
hist(resid(lm1))

# Plot MaxAbund ~ Mass and the regression line
plot(bird$MaxAbund ~ bird$Mass, pch=19, col="coral", ylab="Maximum Abundance", xlab="Mass")
abline(lm1, lwd=2) 
# see colours() for list of colours

# Are the data normally distributed?
hist(bird$MaxAbund,col="coral", main="Untransformed data", xlab="Maximum Abundance")
hist(bird$Mass, col="coral", main="Untransformed data", xlab="Mass")

# Let's verify using the Shapiro and Skewness tests:
shapiro.test(bird$MaxAbund) 
shapiro.test(bird$Mass)
?shapiro.test   # Tests the null hypothesis that the sample came from a normally distributed population 
# If p < 0.05 --> not-normal
# if p > 0.05 --> normal
skewness(bird$MaxAbund)
skewness(bird$Mass) 
?skewness # Measure of symmetry: positive values indicate that the data distribution is left-skewed,
# negative skewness indicates a right-skewed distribution.

# Variables need to be transformed to normalize. Apply a log10() transformation and add to the dataframe
bird$logMaxAbund <- log10(bird$MaxAbund)
bird$logMass <- log10(bird$Mass)
names(bird) # verify that it's there

hist(bird$logMaxAbund,col="yellowgreen", main="Log transformed", xlab=expression("log"[10]*"(Maximum Abundance)"))
hist(bird$logMass,col="yellowgreen", main="Log transformed",xlab=expression("log"[10]*"(Mass)"))
shapiro.test(bird$logMaxAbund); skewness(bird$logMaxAbund)
shapiro.test(bird$logMass); skewness(bird$logMass)

# Re-run your analysis with the appropriate transformations
lm2 <- lm(logMaxAbund ~ logMass, data=bird)

# Are there remaining problems with the diagnostics (heteroscedasticity, non-independence, high leverage)?
opar <- par(mfrow=c(2,2))
plot(lm2, pch=19, col="gray")
par(opar)

# Aside: You can verify the high-leveraged points if you know the row # (indicated by Cook's distance)
bird[c(32,21,50),]
# If valid, you can drop the high-leverage points and re-run the model
bird2 <- bird[-c(32,21,50),]
lm2.rmlev <- lm(logMaxAbund ~ logMass, data=bird2)
abline(lm2.rmlev, lty=2, lwd=3) 

# If you are satisfied with the diagnostic plots, we may now look at the model coefficients and p-values
summary(lm2)

# You can also just call up the coefficients of the model
lm2$coef

# What else?
str(summary(lm2))
summary(lm2)$coefficients # Std. Error = Standard Error of the estimate; t-value = test for no difference from zero
summary(lm2)$r.squared # R2 (aka coefficient of determination; SSreg/ SStotal)
# Note: the simple regression is the only case where squared correlation coefficients are equivalent to regression coefficients: see (cor.test(bird$logMaxAbund,bird$logMass)$estimate)^2
summary(lm2)$adj.r.squared # Adjusted R2
summary(lm2)$sigma # Residual standard error (square root of Residual Mean Squared)
# etc...

# You can also check for yourself the equation for R2:
SSE = sum(resid(lm2)^2)
SST = sum((bird$logMaxAbund - mean(bird$logMaxAbund))^2)
R2 = 1 - ((SSE)/SST)
R2

# Finally, let's plot logMaxAbund ~ logMass and the regression line
plot(logMaxAbund ~ logMass, data=bird, pch=19,col="yellowgreen", xlim=c(min(bird$logMass),max(bird$logMass)),ylab = expression("log"[10]*"(Maximum Abundance)"), xlab = expression("log"[10]*"(Mass)"))
abline(lm2, lwd=2) 
# For your own interest, flag the high-leveraged points
points(bird$logMass[32], bird$logMaxAbund[32], pch=19, col="violet")
points(bird$logMass[21], bird$logMaxAbund[21], pch=19, col="violet")
points(bird$logMass[50], bird$logMaxAbund[50], pch=19, col="violet")

# We can also obtain the confidence intervals (CIs)
confit<-predict(lm2,interval="confidence")
head(confit)
head(confit[,2]) # get a preview of the lower intervale (or "lwr")
head(confit[,3]) # get a preview of the upper intervale (or "upr")

# And plot the CIs
points(bird$logMass,confit[,2]) 
points(bird$logMass,confit[,3])

#***************************************************************************************#

# Does the model improve if we only analyze terrestrial birds?

# Recall that you can exclude objects using "!" 
# We can analyze a subset of this data using this subset command in lm() 
?lm # if you want to see more (argument "subset")

lm3 <- lm(logMaxAbund ~ logMass, data=bird, subset =! bird$Aquatic) # removing the Aquatic birds

# or equivalently
lm3 <- lm(logMaxAbund ~ logMass, data=bird, subset=bird$Aquatic == 0)

# Examine the diagnostic plots
opar <- par(mfrow=c(2,2))
plot(lm3)
summary(lm3)
par(opar)

# Compare the two datasets
opar <- par(mfrow=c(1,2))
plot(logMaxAbund ~ logMass, data=bird, main="All birds", ylab = expression("log"[10]*"(Maximum Abundance)"), 
     xlab = expression("log"[10]*"(Mass)"), pch=19, col="darkgreen")
abline(lm2,lwd=2)

plot(logMaxAbund ~ logMass, data=bird, subset=!bird$Aquatic, main="Terrestrial birds",ylab = expression("log"[10]*"(Maximum Abundance)"), 
     xlab = expression("log"[10]*"(Mass)"), pch=19, col="skyblue")
abline(lm3,lwd=2)
par(opar)
#***************************************************************************************#

#### Challenge 1 ####
# Examine the relationship between log(Maximum Abundance) and log(Mass) for passerine birds
# HINT: Passerine is coded 0 and 1 just like Aquatic. You can verify this by viewing the structure:
str(bird) 
# (Answer below)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
#
# 
# 
# Answer
lm4 <- lm(logMaxAbund ~ logMass, data=bird, subset=bird$Passerine == 1)

# Examine the diagnostic plots
par(mfrow=c(2,2))
plot(lm4)
summary(lm4)

# Compare variance explained by lm2, lm3 and lm4
str(summary(lm4)) # Recall: we want adj.r.squared
summary(lm4)$adj.r.squared # R2-adj = -0.02
summary(lm2)$adj.r.squared # R2-adj = 0.05
summary(lm3)$adj.r.squared # R2-adj = 0.25

# Visually compare the three models
opar <- par(mfrow=c(1,3))
plot(logMaxAbund ~ logMass, data=bird, main="All birds", ylab = expression("log"[10]*"(Maximum Abundance)"), 
     xlab = expression("log"[10]*"(Mass)"), pch=19, col="green")
abline(lm2,lwd=2)

plot(logMaxAbund ~ logMass, subset=Passerine == 1, data=bird, main="Passerine birds",ylab = expression("log"[10]*"(Maximum Abundance)"), 
     xlab = expression("log"[10]*"(Mass)"), pch=19, col="red")
abline(lm4,lwd=2)

plot(logMaxAbund ~ logMass, data=bird, subset=!bird$Aquatic, main="Terrestrial birds",ylab = expression("log"[10]*"(Maximum Abundance)"), 
     xlab = expression("log"[10]*"(Mass)"), pch=19, col="skyblue")
abline(lm3,lwd=2)
par(opar)

#**********************************************************************************************#

#### t-test ####

# Hypothesis: Aquatic birds are heavier than non aquatic birds
# Explanatory factor has only two levels 
boxplot(logMass ~ Aquatic, data=bird, ylab=expression("log"[10]*"(Bird Mass)"), names=c("Non-Aquatic", "Aquatic"), col=c("yellowgreen","skyblue"))

# First, let's test the assumption of equal variance 
# Note: we do not need to test the assumption of normally distributed data since 
# we already log transformed the data above 
tapply(bird$logMass,bird$Aquatic,var)
var.test(logMass~Aquatic,data=bird)
# the ratio of variances is not statistically different from 1, therefore variances
# are equal 

# We are now ready to run the t-test, and specify that variances are equal
?t.test
ttest1 <- t.test(logMass ~ Aquatic,  var.equal=TRUE, data=bird)

# or equivalently
ttest1 <- t.test(x=bird$logMass[bird$Aquatic==0], y=bird$logMass[bird$Aquatic==1], var.equal=TRUE)
ttest1

# Note: instead of t.test(), the lm() command can also be used
ttest.lm1 <- lm(logMass ~ Aquatic, data=bird)
anova(ttest.lm1)
# Check that t-squared = F
ttest1$statistic^2 # 60.3845
anova(ttest.lm1)$F # 60.3845

# Unilateral t-test 
uni.ttest1 <- t.test(logMass~Aquatic, var.equal=TRUE, data=bird, alternative="less")
uni.ttest1 # note this test evaluates whether aquatic birds are heavier than terrestrial birds

# What if we asked whether terrestrial birds were heavier than aquatic birds?
uni.ttest2 <- t.test(logMass~Aquatic,  var.equal=TRUE, data=bird, alternative="greater")
uni.ttest2

#**********************************************************************************************#

#### ANOVA ####

# Now let's switch to an ANOVA to examine the effect of Diet (categorical variable) on MaxAbund
# Hypothesis: Maximum abundance is a function of Diet
# Explanatory factor has many levels and we may also be interested in other factors

# First visualize the data
boxplot(logMaxAbund ~ Diet, data=bird)
# By default, R will order you groups in alphabetical order

# We can also reorder according to the median of each Diet level
med <- sort(tapply(bird$logMaxAbund, bird$Diet, median))
boxplot(logMaxAbund ~ factor(Diet, levels=names(med)), data=bird, col=c("white","lightblue1","skyblue1","skyblue3","skyblue4"))

# The best way to view the effect sizes graphically is to use plot.design()
plot.design(logMaxAbund ~ Diet, data=bird, ylab = expression("log"[10]*"(Maximum Abundance)"))

# Run an ANOVA using aov()
aov1 <- aov(logMaxAbund ~ Diet, data=bird)
summary(aov1) # Signficant Diet effect (p<0.05; at least one diet level differs from others)

# Or, run a linear model (MaxAbund as a function of Diet)
anov1 <- lm(logMaxAbund ~ Diet, data=bird)
anova(anov1) # Signficant Diet effect (p<0.05; at least one diet level differs from others)

# ANOVA (aov) and linear regression (lm) are the same thing. You can verify the results of aov() in
# terms of the linear regression that was carried out and view the parameter estimates (effect of each Diet level)
summary.lm(aov1)
summary(anov1)

# Plot for diagnostics
opar <- par(mfrow=c(2,2))
plot(anov1)
par(opar)
# Ideally the first plot should show similar scatter for each Diet level

# Test assumption of normality of residuals
shapiro.test(resid(anov1))
# Non-significant, therefore residuals are assumed to be normally distributed 

# Test assumption of homogeneity of variance
bartlett.test(logMaxAbund ~ Diet, data=bird)
# Non-significant, therefore variances are assumed to be equal 

# Back to summary results
summary(anov1) 
# note that R uses the Diet levels in alphabetic order and compares each consecutive level
# to the first "baseline" level. Had there been a "control" level, we could have assigned 
# this level as baseline. 

# But where does this difference lie? We can do a post-hoc test:
TukeyHSD(aov(anov1),ordered=T) 
# or equivalently
TukeyHSD(aov1,ordered=T) # The difference is only between Vertebrate and PlantInsect

# For situations where it is difficult to read the output, try:
Tukey <- TukeyHSD(aov1,ordered=T) 
Tukey$Diet[which(Tukey$Diet[,4] < 0.05),] 

#***************************************************************************************#

# Graphical illustration of ANOVA model using barplot()

sd <- tapply(bird$logMaxAbund, bird$Diet,sd) 
means <- tapply(bird$logMaxAbund, bird$Diet,mean)
n <- length(bird$logMaxAbund)
se <- 1.96*sd/sqrt(n)

bp <- barplot(means, col=c("white","lightblue1","skyblue1","skyblue3","skyblue4"), 
       ylab = expression("log"[10]*"(Maximum Abundance)"), xlab="Diet", ylim=c(0,1.8))

# Add vertical se bars
segments(bp, means - se, bp, means + se, lwd=2)
# and horizontal lines
segments(bp - 0.1, means - se, bp + 0.1, means - se, lwd=2)
segments(bp - 0.1, means + se, bp + 0.1, means + se, lwd=2)

#**********************************************************************************************#

#### Two-way ANOVA ####

#### Challenge 2 ####
# Try analyzing how log(MaxAbund) varies with Diet and Aquatic/Terrestrial
# (Answer below)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#

anov2 <- lm(logMaxAbund ~ Diet*Aquatic, data=bird)
opar <- par(mfrow=c(2,2))
plot(anov2) # Don't forget to verify diagnostics!
par(opar)

summary(anov2) 
anova(anov2)
# Is there any indication of an interaction between the covariates?

# To test the interaction, you can also compare nested models using anova()
anov3 <- lm(logMaxAbund ~ Diet + Aquatic, data=bird)
anova(anov3, anov2)
# Models with and without interaction are not significantly different (p > 0.05)
# Go with most parsimonous model: Drop interaction

# We can also view an interaction plot:
interaction.plot(bird$Diet, bird$Aquatic, bird$logMaxAbund, col="black", 
                 ylab = expression("log"[10]*"(Maximum Abundance)"), xlab="Diet")
# What do the gaps in the line for the Aquatic group mean? 
table(bird$Diet, bird$Aquatic)
# Does the plot suggest and interaction?

#### Challenge 3 ####

# Test significance of Aquatic factor by comparing nested models
# (answer below)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
anova(anov1,anov3) # Recall: anov1 is model with only Diet as a factor
# Aquatic factor is not significant (p > 0.05)

#**********************************************************************************************#

#### ANCOVA ####

# Running an ANCOVA in R is comparable to running a two-way ANOVA, e.g.: lm(Y ~ X*Z) 
# but instead of using two categorical variables (Diet and Aquatic), we will now use one
# categorical (Z) and one continuous (X).

# For example, using a build in dataset called CO2, where Y=uptake, X=conc and Z=Treatment
# The ANCOVA of Y ~ X*Z would be given by:
ancova.example <- lm(uptake ~ conc*Treatment, data=CO2)
anova(ancova.example)

# If you want to compare means across factors, you may want to use adjusted means
# which uses the equations given by the ANCOVA to estimate the mean of each level of the 
# factor (Z), corrected for the effect of the covariate (X)
install.packages("effects")
library(effects)
adj.means <- effect('Treatment', ancova.example)
plot(adj.means)

adj.means <- effect('conc*Treatment', ancova.example)
plot(adj.means)

#### Challenge 4 ####

# Run an ANCOVA to test the effect of Diet and Mass (as well as their interaction) on MaxAbund
# (answer below)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
ancov1 <- lm(logMaxAbund ~ logMass*Diet, data=bird)
summary(ancov1)
anova(ancov1)
# Given that the interaction between logMass & Diet is not signifcant, we expect slopes to either 
# be equal for each Diet group or not have sufficient observation per Diet group to apply the model.
# Note that Diet is likewise non-significant when logMass is included in the model

# We should first drop the interaction, and re-evaluate logMass and Diet
ancov2 <- lm(logMaxAbund ~ logMass + Diet, data=bird)
summary(ancov2)
anova(ancov2)

# Finally, compare nested models with and without Diet
anova(lm2, ancov2) 
# We should also drop Diet

# We failed to detect a significant Diet effect (concluded same slope and intercept across diet levels).
# It's still worth plotting the ANCOVA intercept and slopes (model ancov1 above) to explore 
# whether this conclusion is due to lack of power or whether the slopes are indeed parallel.

# Plot the ANCOVA intercept and slopes (model ancov1 above) using abline() and coef()
?abline
# 
# View coefficients
summary(ancov1)$coefficients 

# or
coef(ancov1)
# (Intercept) and logMass are the intercept and slope for the first Diet group.
# To obtain the intercepts and slopes of other Diet groups, we must add their estimated coefficients
# to the baseline group.


plot(logMaxAbund~logMass, data=bird, col=Diet, pch=19, ylab=expression("log"[10]*"(Maximum Abundance)"),xlab=expression("log"[10]*"(Mass)"))
abline(a=coef(ancov1)[1],b=coef(ancov1)[2], col="deepskyblue1")
abline(a=sum(coef(ancov1)[1]+coef(ancov1)[3]),b=sum(coef(ancov1)[2]+coef(ancov1)[7]), col="green2", lwd=2)
abline(a=sum(coef(ancov1)[1]+coef(ancov1)[4]),b=sum(coef(ancov1)[2]+coef(ancov1)[8]), col="orange1", lwd=2)
abline(a=sum(coef(ancov1)[1]+coef(ancov1)[5]),b=sum(coef(ancov1)[2]+coef(ancov1)[9]), col="lightsteelblue1", lwd=2)
abline(a=sum(coef(ancov1)[1]+coef(ancov1)[6]),b=sum(coef(ancov1)[2]+coef(ancov1)[10]), col="darkcyan", lwd=2)


#**********************************************************************************************#

#### Multiple Regression ####

# This dataset explores environmental variables that drive the abundance and presence/absence of 
# bird species. Here we look at one species (Dickcissel), which was chosen because it has less noise 
# than many other species

Dickcissel <- read.csv("Dickcissel.csv")
head(Dickcissel)

# Compare the relative importance of three variables (climate, productivity, and land cover) by running
# a multiple regression of abund as a function of clTma + NDVI + grass   
lm.mult <- lm(abund ~ clTma + NDVI + grass, data=Dickcissel)
summary(lm.mult)
opar <- par(mfrow=c(2,2))
plot(lm.mult,col=rgb(33,33,33,100,maxColorValue=225), pch=19)
opar <- par(mfrow=c(1,3),mar=c(4,4,1,2),mgp=c(2,.7,0))
plot(abund ~ clTma, data=Dickcissel, pch=19, col="orange")
plot(abund ~ NDVI, data=Dickcissel, pch=19, col="skyblue")
plot(abund ~ grass, data=Dickcissel, pch=19, col="green")
par(opar)

#**********************************************************************************************#

#### Challenge 5 ####

# Is a transformation needed for the variable "abund"?
# (answer below)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
#
hist(Dickcissel$abund, main="", xlab="Dickcissel abundance")
shapiro.test(Dickcissel$abund)
skewness(Dickcissel$abund)

summary(Dickcissel$abund) # numerous 0s! Must add a constant to log10() 

hist(log10(Dickcissel$abund+0.1), main="", xlab=expression("log"[10]*"(Dickcissel Abundance + 0.1)"))
shapiro.test(log10(Dickcissel$abund+0.1))
skewness(log10(Dickcissel$abund+0.1)) # still non-normal

#**********************************************************************************************#

#### Stepwise regression ####

# Run a model with everything except the Present variable
lm.full <- lm(abund ~ . - Present, data=Dickcissel) 
# note the period
summary(lm.full)

lm.step <- step(lm.full)
summary(lm.step)
?step

#**********************************************************************************************#

