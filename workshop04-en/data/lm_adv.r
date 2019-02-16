# Dataset: File names is "birdsdiet.csv" and "dickcissel.csv"
# Notes: Optional R script
#***************************************************************************************#

#### Loading and transform the data ####

bird<-read.csv("birdsdiet.csv")
bird$logMaxAbund <- log10(bird$MaxAbund)
bird$logMass <- log10(bird$Mass)

#### Linear Regression: Plotting CIs using polygon() ####

lm2 <- lm(logMaxAbund ~ logMass, data=bird)
I <- order(bird$logMass)
confit<-predict(lm2,interval="confidence")
fit <- confit[,1]
plusCI<-I(confit[,3])
minusCI<-I(confit[,2])
xx <- c(bird$logMass[I],rev(bird$logMass[I]))
yy <- c(plusCI[I],rev(minusCI[I]))
plot(xx,yy,type = "n", ylab = expression("log"[10]*"(Maximum Abundance)"), xlab = expression("log"[10]*"(Mass)"))
polygon(xx,yy,col=rgb(0,30,70,20,maxColorValue=225),border=rgb(0,30,70,20,maxColorValue=225))
lines(bird$logMass[I],fit[I],lwd=3) # same as abline(mod2)
points(bird$logMass[I],bird$logMaxAbund[I], pch=19, col=rgb(124,255,0,200,maxColorValue=255))


#***************************************************************************************#

# Visual comparison of all birds and terrestrial birds (lm2 vs lm3) using polygon()
par(mfrow=c(1,2))

# Model lm2 plot
plot(logMaxAbund ~ logMass, data=bird, main="All birds", ylab = expression("log"[10]*"(Maximum Abundance)"),
     xlab = expression("log"[10]*"(Mass)"), pch=19, col=rgb(124,255,0,200,maxColorValue=255))
polygon(xx,yy,col=rgb(0,30,70,20,maxColorValue=225),border=rgb(0,30,70,20,maxColorValue=225))
abline(lm2,lwd=2)

# Model lm3 plot
lm3 <- lm(logMaxAbund ~ logMass, data=bird, subset=bird$Aquatic == 0)
plot(logMaxAbund ~ logMass, data=bird, subset=!bird$Aquatic, main="Terrestrial birds",ylab = expression("log"[10]*"(Maximum Abundance)"),
     xlab = expression("log"[10]*"(Mass)"), pch=19, col=rgb(176,196,222,200,maxColorValue=225))
abline(lm3,lwd=2)
bird_t <- subset(bird,bird$Aquatic == 0)  # yet another way of subsetting
confit_lm3<-predict(lm3,interval="confidence")
I <- order(bird_t$logMass)
fit <- confit_lm3[,1]
plusCI<-I(confit_lm3[,3])
minusCI<-I(confit_lm3[,2])
xx_lm3 <- c(bird_t$logMass[I],rev(bird_t$logMass[I]))
yy_lm3 <- c(plusCI[I],rev(minusCI[I]))
polygon(xx_lm3,yy_lm3,col=rgb(0,30,70,20,maxColorValue=225),border=rgb(0,30,70,20,maxColorValue=225))

#***************************************************************************************#

#### Challenge 1 ####

# Visual comparison of all birds and passerine birds (lm2 vs lm4)
lm4 <- lm(logMaxAbund ~ logMass, data=bird, subset=bird$Passerine == 1)

par(mfrow=c(1,2))
plot(logMaxAbund ~ logMass, data=bird, main="All birds", ylab = expression("log"[10]*"(Maximum Abundance)"),
     xlab = expression("log"[10]*"(Mass)"), pch=19, col=rgb(124,255,0,150,maxColorValue=255))
polygon(xx,yy,col=rgb(0,30,70,20,maxColorValue=225),border=rgb(0,30,70,20,maxColorValue=225))
abline(lm2,lwd=2)

plot(logMaxAbund ~ logMass, subset=Passerine == 1, data=bird, main="Passerine birds",ylab = expression("log"[10]*"(Maximum Abundance)"),
     xlab = expression("log"[10]*"(Mass)"), pch=19, col=rgb(30,144,255,100,maxColorValue=255))
bird_p <- subset(bird,Passerine == 1)  # yet another way of subsetting
confit_lm4<-predict(lm4,interval="confidence")
I <- order(bird_p$logMass)
fit <- confit_lm4[,1]
plusCI<-I(confit_lm4[,3])
minusCI<-I(confit_lm4[,2])
xx_lm4 <- c(bird_p$logMass[I],rev(bird_p$logMass[I]))
yy_lm4 <- c(plusCI[I],rev(minusCI[I]))
polygon(xx_lm4,yy_lm4,col=rgb(0,30,70,20,maxColorValue=225),border=rgb(0,30,70,20,maxColorValue=225))
abline(lm4,lwd=2)

#***************************************************************************************#

#### ANOVA: Bartlett test and non-parametric tests ####

# Had we used the untransformed data, we would have violated the assumption of equal variance
# and failed to detect a significant difference due to Diet
bartlett.test(MaxAbund ~ Diet, data=bird)
anova(lm(MaxAbund ~ Diet, data=bird))

par(mfrow=c(1,1)); boxplot(MaxAbund ~ Diet, data=bird, col=rgb(30,144,255,100,maxColorValue=255))
# Options are to transform (e.g. logMaxAbund) or us a non-parametric test

kruskal.test(MaxAbund ~ Diet, data=bird)
# This converts the response values to ranks, and tests whether the ranks are distributed equally
# across Diets, as expected under the null hypothesis.

#***************************************************************************************#

#### ANOVA continued: Interpreting contrast outputs ####

# The default contrast in R is called the "contr.treatment" (each level is compared to the
# baseline level)

# The Intercept Estimate (here = 1.1539) is the baseline or control group and corresponds
# to the mean of the first (alphabetically) diet group (i.e. DietInsect)

# Notice that the Intercept Estimate + Estimates of each Diet return the mean for each Diet
tapply(bird$logMaxAbund, bird$Diet, mean)
anov1 <- lm(logMaxAbund ~ Diet, data=bird)
coef(anov1)
coef(anov1)[1] + coef(anov1)[2] # InsectVert
coef(anov1)[1] + coef(anov1)[3] # Plant
# etc.

# We can reorder multiple levels
med <- sort(tapply(bird$logMaxAbund, bird$Diet, median))
med
bird$Diet2 <- factor(bird$Diet, levels=names(med)) # reorder levels according to median
anov_rl <- lm(logMaxAbund ~ Diet2, data=bird)
summary(anov_rl) # note  the significance of each levels
anova(anov_rl)

# Or, we may want to relevel the baseline
bird$Diet2 <- relevel(bird$Diet, ref="Plant")
anov_rl <- lm(logMaxAbund ~ Diet2, data=bird)
summary(anov_rl) # note  the significance of each levels
anova(anov_rl)
# note that we are now comparing the "Plant" diet to all other diet levels

# To view how the levels differ
levels(bird$Diet)
levels(bird$Diet2)

# The default treatment contrast (comparison to one baseline) corresponds to a contrast of (4,-1,-1,-1,-1)

# Alternatively, we can compare different subsets of diets such as "Insect" and "InsectVert" to
# "PlantInsect" and "Vertebrate" (i.e. 0,1,1,-1,-1)

# Or just two diets such as "PlantInsect" with "Vertebrate" (i.e. 0,0,0,1,-1),
# Or "Insect" with "InsectVert" (i.e. 0,1,-1,0,0).

# To run the above comparison, we would manually change the contrast matrix as follows
contrasts(bird$Diet2) <- cbind(c(4,-1,-1,-1,-1), c(0,1,1,-1,-1), c(0,0,0,1,-1), c(0,1,-1,0,0))
contrasts(bird$Diet2)
anov_rl <- lm(logMaxAbund ~ Diet2, data=bird)
summary(anov_rl)
# This will produce your matrix of contrast coefficients
# The important restriction is that all columns must sum to zero, and that the product of
# any two columns must sum to zero (that all contrasts are orthogonal)

# Compare this to the default treatment contrast:
contr.treatment(5) # 5 because there are 5 diet levels

# An important thing to note at this point about the default contrast in R ("contr.treatment") is
# that it is NOT orthogonal

# Recall: To be orthogonal the following properties must be met
# 1. Coefficients sum to 0
sum(contrasts(bird$Diet)[,1])
# 2. Sum of the product of two columns sum to 0
sum(contrasts(bird$Diet)[,1]*contrasts(bird$Diet)[,2])
# Property 1 is not met with the Treatment contrast, therefore matrix is not orthogonal

# We may change the contrast altogether so that no longer comparing all levels to a given
# baseline (i.e. choose to change our contrast a priori)
options(contrasts=c("contr.helmert", "contr.poly"))
contrasts(bird$Diet)

# Here you can see that the contrasts are orthogonal (unlike the Treatment contrasts)
# Property 1
sum(contrasts(bird$Diet)[,1])
sum(contrasts(bird$Diet)[,2])
# etc.
# Property 2
sum(contrasts(bird$Diet)[,1]*contrasts(bird$Diet)[,2])
sum(contrasts(bird$Diet)[,1]*contrasts(bird$Diet)[,3])
# etc.

anov_ct <- lm(logMaxAbund ~ Diet, data=bird)
summary(anov_ct)
tapply(bird$logMaxAbund, bird$Diet, mean)
coef(anov_ct)[1] + coef(anov_ct)[2] # no longer equal to mean of InsectVert
# The Helmert contrasts will contrast the second level with the first, the third with the
# average of the first two, and so on.

# Disadvantages: Parameter estimates and their standard errors are much more difficult to
# understand in Helmert than in Treatment contrasts
# Advantages: Gives you proper orthogonal contrasts, and thus a clearer picture of which factor
# levels need to be retained

# Reset the contrast to Treatment if you are navigating between Workshop 4 and this Advanced Workshop 4
# Otherwise your model outputs will not be the same as those presented (on the Prezi slides)
options(contrasts=c("contr.treatment", "contr.poly"))

#***************************************************************************************#

#### Unbalanced ANOVA ####

# We will now examine the Sums of Squares (SSE) and the effect of variable order in unbalanced designs
# To do, we will need the "Anova" command (upper case A), which is part of the "car" package
# This Anova() will do Type II and III SSE
install.packages("car")
library(car)

# Birdsdiet data is actually unbalanced: Number of Aquatic and non-Aquatic is not equal
table(bird$Aquatic,bird$Diet)

unb.anov1 <- lm(logMaxAbund ~ Aquatic + Diet, data=bird)
unb.anov2 <- lm(logMaxAbund ~ Diet + Aquatic, data=bird)
anova(unb.anov1)
anova(unb.anov2)
# Note how the order of the explanatory variables changed the Sums of Squares values

# Now try type III Anova()
Anova(unb.anov1,type="III")
Anova(unb.anov2,type="III")
# What have you noticed?

# Alternatively, we could  have reset the contrasts to "contr.sum":
# options(contrasts=c("contr.sum", "contr.poly"))
# contrasts(bird$Diet)
# and anova(unb.anov1) would have given us the same results as Anova(unb.anov1, type="III")

#***************************************************************************************#

#### Multiple Regression: Multicollinearity of explanatory variables ####

# Load data
Dickcissel <- read.csv("Dickcissel.csv")

# Do a quick data exploration using the pairs() or cor() commands
# Note: Must remove categorical variables to run cor()
cor.matrix <- cor(Dickcissel[,-2]) # Keep all columns except the 2nd column
cor.matrix

# Just for fun, let's source a correlation plot function to visualize the correlation matrix
source('my.plotcorr.R')
source('col.plotcorr.R')
col.plotcorr(cor.matrix)

#### Polynomial regression ####
# We will now examine quadratic relationships between abundance and temperature
lm.linear <- lm(abund ~ clDD, data=Dickcissel)
lm.quad <- lm(abund ~ clDD + I(clDD^2), data=Dickcissel)
lm.cubic <- lm(abund ~ clDD + I(clDD^2) + I(clDD^3), data=Dickcissel)

# Compare the above models and determine which nested model we should keep.
# Run a summary of this model, report the regression formula, p-values and R2-adj
anova(lm.linear,lm.quad,lm.cubic) # list models in increasing complexity.
# We should take the lowest line that has a significant p-value. The cubic term can be dropped.

summary(lm.quad)
# Regression coefficients
summary(lm.quad)$coefficients[,1]
# Estimate p-values
summary(lm.quad)$coefficients[,4]
# R2-adj
summary(lm.quad)$adj.r.squared

#**********************************************************************************************#

#### Stepwise regression ####

# Run a model with everything except the Present variable
lm.full <- lm(abund ~ . - Present, data=Dickcissel)
# note the period
summary(lm.full)

lm.step <- step(lm.full)
summary(lm.step)

#**********************************************************************************************#

#***************************************************************************************#

#### Variance partitioning ####

# From the correlation plot, we saw that many variables selected were collinear
install.packages("HH")
library(HH)
?vif # a diagnostic of collinearity among explanatory variables
vif(clDD ~ clFD + clTmi + clTma + clP + grass, data=Dickcissel) # values exceeding 5 are considered evidence of collinearity

# Can use the function varpart() to partition the variance in abund with all land cover
# variables in one set and all climate variables in the other set (leaving out NDVI).
# Note: Collinear variables do not have to be removed prior to partitioning
install.packages("vegan")
library(vegan)
?varpar

part.lm = varpart(Dickcissel$abund, Dickcissel[,c("clDD", "clFD", "clTmi", "clTma", "clP")],
                  Dickcissel[,c("broadleaf", "conif", "grass", "crop", "urban", "wetland")])
part.lm

par(mfrow=c(1,1))
showvarparts(2)
plot(part.lm,digits=2, bg=rgb(30,144,255,100,maxColorValue=255), col=rgb(30,144,255,250,maxColorValue=255))

# Proportion of variance in abund explained by climate alone is given by X1|X2 (so 28.5%)
# Proportion explained by land cover alone is given by X2|X1 (so ~0%)
# Both combined is 2.4%

# Test significance of each fraction

# Climate set
out.1 = rda(Dickcissel$abund, Dickcissel[,c("clDD", "clFD", "clTmi", "clTma", "clP")],
            Dickcissel[,c("broadleaf", "conif", "grass", "crop", "urban", "wetland")])
anova(out.1, step=1000, perm.max=1000)

# Land cover set
out.2 = rda(Dickcissel$abund, Dickcissel[,c("broadleaf", "conif", "grass", "crop", "urban", "wetland")],
            Dickcissel[,c("clDD", "clFD", "clTmi", "clTma", "clP")])
anova(out.2, step=1000, perm.max=1000) # land cover fraction is non-significant once climate data is accounted for
