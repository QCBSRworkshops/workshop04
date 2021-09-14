##Section: 01-preparing-for-the-workshop.R 




##Section: 02-introduction.R 




##Section: 03-linear-models.R 

# Loading libraries and bird dataset
library(e1071)
library(MASS)
setwd("~/Desktop/...") # Don't forget to set your working directory (note: your directory will be different)
bird<-read.csv("birdsdiet.csv")

# Visualize the dataframe
names(bird)
str(bird)
head(bird)
summary(bird)
plot(bird)

lm1 <- lm(bird$MaxAbund ~ bird$Mass) # where Y ~ X means Y "as a function of" X>

opar <- par(mfrow=c(2,2)) # draws subsequent figures in a 2-by-2 panel
plot(lm1)
par(opar) # resets to 1-by-1 plot

# Plot Y ~ X and the regression line
plot(bird$MaxAbund ~ bird$Mass, pch=19, col="coral", ylab="Maximum Abundance",
     xlab="Mass")
abline(lm1, lwd=2)
?plot # For further details on plot() arguments
# see colours() for list of colours

# Is the data normally distributed?
hist(bird$MaxAbund,col="coral", main="Untransformed data",
     xlab="Maximum Abundance")
hist(bird$Mass, col="coral", main="Untransformed data", xlab="Mass")

# Test null hypothesis that the sample came from a normally distributed population
shapiro.test(bird$MaxAbund)
shapiro.test(bird$Mass)
# If p < 0.05, then distribution is not-normal
# if p > 0.05, then distribution is normal

skewness(bird$MaxAbund)
skewness(bird$Mass)
# where positive values indicate a left-skewed distribution, and negative value a right skew.

# Add log10() transformed variables to your dataframe
bird$logMaxAbund <- log10(bird$MaxAbund)
bird$logMass <- log10(bird$Mass)
names(bird) # to view the dataframe + new transformed variables

hist(bird$logMaxAbund,col="yellowgreen", main="Log transformed",
     xlab=expression("log"[10]*"(Maximum Abundance)"))
hist(bird$logMass,col="yellowgreen", main="Log transformed",
     xlab=expression("log"[10]*"(Mass)"))
shapiro.test(bird$logMaxAbund); skewness(bird$logMaxAbund)
shapiro.test(bird$logMass); skewness(bird$logMass)

# Re-run your analysis with the appropriate transformations
lm2 <- lm(bird$logMaxAbund ~ bird$logMass)

# Are there remaining problems with the diagnostics (heteroscedasticity, non-independence, high leverage)?
opar <- par(mfrow=c(2,2))
plot(lm2, pch=19, col="gray")
par(opar)

# Now we can look at the model coefficients and p-values
summary(lm2)

# You can also just call up the coefficients of the model
lm2$coef

# What else?
str(summary(lm2))
summary(lm2)$coefficients # where Std. Error is the standard error of each estimate
summary(lm2)$r.squared # Coefficient of determination
summary(lm2)$adj.r.squared # Adjusted coefficient of determination
summary(lm2)$sigma # Residual standard error (square root of Error Mean Square)
# etc…

# You can also check for yourself the equation for R2:
SSE = sum(resid(lm2)^2)
SST = sum((bird$logMaxAbund - mean(bird$logMaxAbund))^2)
R2 = 1 - ((SSE)/SST)
R2

plot(logMaxAbund ~ logMass, data=bird, pch=19, col="yellowgreen",
                   ylab = expression("log"[10]*"(Maximum Abundance)"), xlab = expression("log"[10]*"(Mass)"))
abline(lm2, lwd=2)

# You may also flag the previously identified high-leveraged points
points(bird$logMass[32], bird$logMaxAbund[32], pch=19, col="violet")
points(bird$logMass[21], bird$logMaxAbund[21], pch=19, col="violet")
points(bird$logMass[50], bird$logMaxAbund[50], pch=19, col="violet")

# We can also plot the confidence intervals
confit<-predict(lm2,interval="confidence")
points(bird$logMass,confit[,2])
points(bird$logMass,confit[,3])

# Recall that you can exclude objects using "!"
# We can analyze a subset of this data using this subset command in lm()
lm3 <- lm(logMaxAbund ~ logMass, data=bird, subset =! bird$Aquatic) # removing the Aquatic birds

# or equivalently
lm3 <- lm(logMaxAbund ~ logMass, data=bird, subset=bird$Aquatic == 0)

# Examine the model
opar <- par(mfrow=c(2,2))
plot(lm3, pch=19, col=rgb(33,33,33,100,maxColorValue=225))
summary(lm3)
par(opar)

# Compare the two datasets
opar <- par(mfrow=c(1,2))
plot(logMaxAbund ~ logMass, data=bird, main="All birds", ylab = expression("log"[10]*"(Maximum Abundance)"),
     xlab = expression("log"[10]*"(Mass)"))
abline(lm2,lwd=2)

plot(logMaxAbund ~ logMass, data=bird, subset=!bird$Aquatic, main="Terrestrial birds",
     ylab = expression("log"[10]*"(Maximum Abundance)"), xlab = expression("log"[10]*"(Mass)"))
abline(lm3,lwd=2)
opar(par)

lm4 <- lm(logMaxAbund ~ logMass, data=bird, subset=bird$Passerine == 1)

# Examine the diagnostic plots
opar <- par(mfrow=c(2,2))
plot(lm4)
summary(lm4)
par(opar)

# Compare variance explained by lm2, lm3 and lm4
str(summary(lm4)) # Recall: we want adj.r.squared
summary(lm4)$adj.r.squared # R2-adj = -0.02
summary(lm2)$adj.r.squared # R2-adj = 0.05
summary(lm3)$adj.r.squared # R2-adj = 0.25

# Visually compare the three models
opar <- par(mfrow=c(1,3))
plot(logMaxAbund ~ logMass, data=bird, main="All birds",
     ylab = expression("log"[10]*"(Maximum Abundance)"),
     xlab = expression("log"[10]*"(Mass)"), pch=19, col="yellowgreen")
abline(lm2,lwd=2)

plot(logMaxAbund ~ logMass, subset=Passerine == 1, data=bird, main="Passerine birds",
     ylab = expression("log"[10]*"(Maximum Abundance)"),
     xlab = expression("log"[10]*"(Mass)"), pch=19, col="violet")
abline(lm4,lwd=2)

plot(logMaxAbund ~ logMass, data=bird, subset=!bird$Aquatic, main="Terrestrial birds",
     ylab = expression("log"[10]*"(Maximum Abundance)"),
     xlab = expression("log"[10]*"(Mass)"), pch=19, col="skyblue")
abline(lm3,lwd=2)
par(opar)


##Section: 04-t-test-anova.R 

# T-test
boxplot(logMass ~ Aquatic, data=bird, ylab=expression("log"[10]*"(Bird Mass)"),
        names=c("Non-Aquatic","Aquatic"),
        col=c("yellowgreen","skyblue"))

# First, let's test the assumption of equal variance
# Note: we do not need to test the assumption of normally distributed data since
# we already log transformed the data above
tapply(bird$logMass,bird$Aquatic,var)
var.test(logMass~Aquatic,data=bird)

# We are now ready to run the t-test
ttest1 <- t.test(Mass~Aquatic, var.equal=TRUE, data=bird)

# or equivalently
ttest1 <- t.test(x=bird$logMass[bird$Aquatic==0], y=bird$logMass[bird$Aquatic==1], var.equal=TRUE)
ttest1

# Alternative T-test
uni.ttest1 <- t.test(logMass~Aquatic, var.equal=TRUE, data=bird, alternative="less")
uni.ttest1

ttest.lm1 <- lm(logMass ~ Aquatic, data=bird)
anova(ttest.lm1)

ttest1$statistic^2
anova(ttest.lm1)$F

# Default alphabetical order
boxplot(logMaxAbund ~ Diet, data=bird)

# Relevel factors
med <- sort(tapply(bird$logMaxAbund, bird$Diet, median))
boxplot(logMaxAbund ~ factor(Diet, levels=names(med)), data=bird, col=c("white","lightblue1",
           "skyblue1","skyblue3","skyblue4"))

plot.design(logMaxAbund ~ Diet, data=bird, ylab = expression("log"[10]*"(Maximum Abundance)"))

# Using aov()
aov1 <- aov(logMaxAbund ~ Diet, data=bird)
summary(aov1)

# Using lm()
anov1 <- lm(logMaxAbund ~ Diet, data=bird)
anova(anov1)

# Plot for diagnostics
opar <- par(mfrow=c(2,2))
plot(anov1)
par(opar)

# Test assumption of normality of residuals
shapiro.test(resid(anov1))

# Test assumption of homogeneity of variance
bartlett.test(logMaxAbund ~ Diet, data=bird)

aov1 <- aov(logMaxAbund ~ Diet, data=bird)

summary(aov1)

anov1 <- lm(logMaxAbund ~ Diet, data=bird)

anova(anov1)

# Where does the Diet difference lie?
TukeyHSD(aov(anov1),ordered=T)

# or equivalently
TukeyHSD(aov1,ordered=T)

# Graphical illustration of ANOVA model using barplot()

sd <- tapply(bird$logMaxAbund,list(bird$Diet),sd)
means <- tapply(bird$logMaxAbund,list(bird$Diet),mean)
n <- length(bird$logMaxAbund)
se <- 1.96*sd/sqrt(n)

bp <- barplot(means, col=c("white","lightblue1","skyblue1","skyblue3","skyblue4"),
       ylab = expression("log"[10]*"(Maximum Abundance)"), xlab="Diet", ylim=c(0,1.8))

# Add vertical se bars
segments(bp, means - se, bp, means + se, lwd=2)
# and horizontal lines
segments(bp - 0.1, means - se, bp + 0.1, means - se, lwd=2)
segments(bp - 0.1, means + se, bp + 0.1, means + se, lwd=2)

bird$Diet2 <- relevel(bird$Diet, ref="Plant")
anov_rl <- lm(logMaxAbund ~ Diet2, data=bird)
summary(anov_rl)
anova(anov_rl)

contrasts(bird$Diet2)

contrasts(bird$Diet2) <- cbind(c(4,-1,-1,-1,-1), c(0,1,1,-1,-1), c(0,0,0,1,-1), c(0,1,-1,0,0))

sum(contrasts(bird$Diet)[,1]) # first condition for column 1
sum(contrasts(bird$Diet)[,1]*contrasts(bird$Diet)[,2]) # second condition for column 1 and 2

anov2 <- lm(logMaxAbund ~ Diet*Aquatic, data=bird)
opar <- par(mfrow=c(2,2))
plot(anova2)
par(opar)
summary(anov2)
anova(anov2)

anov3 <- lm(logMaxAbund ~ Diet + Aquatic, data=bird)
anova(anov3, anov2)

interaction.plot(bird$Diet, bird$Aquatic, bird$logMaxAbund, col="black",
                 ylab = expression("log"[10]*"(Maximum Abundance)"), xlab="Diet")

table(bird$Diet, bird$Aquatic)

anova(anov1,anov3) # Recall: anov1 is model with only Diet as a factor

unb_anov1 <- lm(logMaxAbund ~ Aquatic + Diet, data=bird)
unb_anov2 <- lm(logMaxAbund ~ Diet + Aquatic, data=bird)
anova(unb_anov1)
anova(unb_anov2)

Anova(unb_anov1,type="III")


##Section: 05-ancova.R 

ancova.example <- lm(uptake ~ conc*Treatment, data=CO2)
anova(ancova.example)

install.packages("effects")
library(effects)
adj.means <- effect('Treatment', ancova.example)
plot(adj.means)

adj.means <- effect('conc*Treatment', ancova.example)
plot(adj.means)

# If you did the section on Contrasts above, you will need to reset the contrast to Treatment for ease of
# comparison using the ''options()'' function
# Otherwise, skip the first line of code below
options(contrasts=c("contr.treatment", "contr.poly"))
ancov1 <- lm(logMaxAbund ~ logMass*Diet, data=bird)
summary(ancov1)
anova(ancov1)

ancov2 <- lm(logMaxAbund ~ logMass + Diet, data=bird)

lm2 <- lm(logMaxAbund ~ logMass, data=bird)

coef(ancov1)


plot(logMaxAbund~logMass, data=bird, col=Diet, pch=19, ylab=expression("log"[10]*"(Maximum Abundance)"),
     xlab=expression("log"[10]*"(Mass)"))
abline(a=coef(ancov1)[1],b=coef(ancov1)[2], col="deepskyblue1")
abline(a=sum(coef(ancov1)[1]+coef(ancov1)[3]),b=sum(coef(ancov1)[2]+coef(ancov1)[7]),col="green2", lwd=2)
abline(a=sum(coef(ancov1)[1]+coef(ancov1)[4]),b=sum(coef(ancov1)[2]+coef(ancov1)[8]),col="orange1", lwd=2)
abline(a=sum(coef(ancov1)[1]+coef(ancov1)[5]),b=sum(coef(ancov1)[2]+coef(ancov1)[9]),col="lightsteelblue1",
       lwd=2)
abline(a=sum(coef(ancov1)[1]+coef(ancov1)[6]),b=sum(coef(ancov1)[2]+coef(ancov1)[10]),col="darkcyan", lwd=2)


##Section: 06-multiple-regression.R 

vif(clDD ~ clFD + clTmi + clTma + clP + grass, data=Dickcissel)

hist(Dickcissel$abund, main="", xlab="Dickcissel abundance")
shapiro.test(Dickcissel$abund)
skewness(Dickcissel$abund)
summary(Dickcissel$abund)

hist(log10(Dickcissel$abund+0.1), main="", xlab=expression("log"[10]*"(Dickcissel Abundance + 0.1)"))
shapiro.test(log10(Dickcissel$abund+0.1))
skewness(log10(Dickcissel$abund+0.1))

lm.mult <- lm(abund ~ clTma + NDVI + grass, data=Dickcissel)
summary(lm.mult)

plot(abund ~ clTma, data=Dickcissel, pch=19, col="orange")
plot(abund ~ NDVI, data=Dickcissel, pch=19, col="skyblue")
plot(abund ~ grass, data=Dickcissel, pch=19, col="green")

lm.linear <- lm(abund ~ clDD, data=Dickcissel)
lm.quad <- lm(abund ~ clDD + I(clDD^2), data=Dickcissel)
lm.cubic <- lm(abund ~ clDD + I(clDD^2) + I(clDD^3), data=Dickcissel)

anova(lm.linear,lm.quad,lm.cubic) # list models in increasing complexity
# We should take the lowest line that has a significant p-value

# Examine the summary
summary(lm.quad)
# Regression coefficients
summary(lm.quad)$coefficients[,1]
# Estimate p-values
summary(lm.quad)$coefficients[,4]
# R2-adj
summary(lm.quad)$adj.r.squared

lm.full <- lm(abund ~ . - Present, data=Dickcissel)
lm.step <- step(lm.full)
summary(lm.step)

part.lm = varpart(Dickcissel$abund, Dickcissel[,c("clDD", "clFD", "clTmi", "clTma", "clP")],
                  Dickcissel[,c("broadleaf", "conif", "grass", "crop", "urban", "wetland")])
part.lm

# Climate set
out.1 = rda(Dickcissel$abund, Dickcissel[,c("clDD", "clFD", "clTmi", "clTma", "clP")],
            Dickcissel[,c("broadleaf", "conif", "grass", "crop", "urban", "wetland")])
anova(out.1, step=1000, perm.max=1000)

# Land cover set
out.2 = rda(Dickcissel$abund, Dickcissel[,c("broadleaf", "conif", "grass", "crop", "urban", "wetland")],
        Dickcissel[,c("clDD", "clFD", "clTmi", "clTma", "clP")])
anova(out.2, step=1000, perm.max=1000)

showvarparts(2)
plot(part.lm,digits=2, bg=rgb(48,225,210,80,maxColorValue=225), col="turquoise4")


##Section: 07-final-considerations.R 




##Section: 08-references.R 




