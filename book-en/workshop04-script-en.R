##Section: 01-preparing-for-the-workshop.R 

###Notice ###
#                                                                            #
#This is an automatically generated script based on the code chunks from the #
#book for this workshop.                                                     #
#                                                                            #
#It is minimally annotated to allow participants to provide their comments:  #
#a practice that we highly encourage.                                        #
#                                                                            #
#Note that the solutions to the challenges are also included in this script. #
#When solving the challenges by yourself, attempt to not scroll and peek at  #
#the solutions.                                                              #
#                                                                            #
#Happy coding!                                                               #


# Install the required packages
install.packages("dplyr")
install.packages("vegan")
install.packages("e1071")
install.packages("MASS")
install.packages("car")
install.packages("effects")

# Load the required packages
library(dplyr)
library(vegan)
library(e1071)
library(MASS)
library(car)
library(effects)


##Section: 02-introduction.R 

# Import the "bidsdiet" dataset and save it in the "bird" object
bird <- read.csv("data/birdsdiet.csv", stringsAsFactors = TRUE)

# Explore the variables in the "bird" dataset
str(bird)

# Average maximum observed abundance
mean(bird$MaxAbund)

# Median of maximum observed abundance
median(bird$MaxAbund)

# Variance of the maximum observed abundance
var(bird$MaxAbund)

# Standard deviation of the maximum observed abundance
sd(bird$MaxAbund)

# plot the response in relation to the predictor
plot(bird$Mass, bird$MaxAbund)

plot(bird$Mass, bird$MaxAbund)
abline(coef = c(70, -.03),  lwd = 1.5, lty = 2, col = palette()[2])
abline(coef = c(20, -.005), lwd = 1.5, lty = 2, col = palette()[4])
abline(coef = c(200, -.1),  lwd = 1.5, lty = 2, col = palette()[6])


##Section: 03-linear-models.R 

# Linear regression of maximum abundance against mass
lm1 <- lm(MaxAbund ~ Mass, data = bird)

# Examination of the regression output
lm1

# Plot the four diagnostic plots
par(mfrow=c(2,2))
plot(lm1)

par(mfrow=c(2,2), mar = c(4,4,2,1.1), oma =c(0,0,0,0))
plot(lm1)

Diagnostic plot # 1 - Residuals vs Fitted
  set.seed(1234564)
  x <- rnorm(100,10,10)
  y <- 2*x+0 + rnorm(100)
  lm <- lm(y~x)
  plot(lm, which = 1)

Example of non-independence of residuals
par(mfrow=c(1,2))
set.seed(1234564)
x = rnorm(100,10,10)
y = (x)^2 + rnorm(length(x),0,30)
lm=lm(y~scale(x))
plot(lm,which = 1, main = "Nonlinear", col.main="red")
x = abs(rnorm(100,10,10))
y = (x) + rnorm(length(x), 0, x)
lm=lm(y~scale(x))
plot(lm,which = 1, main = "Heteroscedastic", col.main="red")

Diagnostic plot # 2 - Scale Location
set.seed(1234564)
x <- 1:100
y <- x + rnorm(100,sd=5)
lm=lm(y~x)
plot(lm,which = 3)

Example of non-equal dispersion of the residuals
set.seed(2)
x = abs(rnorm(100,10,10))
y = (x) + rnorm(length(x), 0, x)
lm=lm(y~scale(x))
plot(lm,which = 3)

Diagnostic plot # 3 - Normal QQ
set.seed(1234564)
x <- 1:100
y <- x + rnorm(100,sd=5)
lm=lm(y~x)
plot(lm, which = 2)

Example of non-normal distribution of the residuals
set.seed(2)
x = abs(rnorm(100,10,10))
y = (x) + rnorm(length(x), 0, x)
lm=lm(y~scale(x))
plot(lm, which = 2)

Diagnostic plot # 4 - Residuals vs Leverage
par(mfrow=c(3, 1), mar = c(4, 15, 1, 3), cex = 1.2)
set.seed(1234564)
x <- 1:20
y <- rnorm(x, x, 2)
lm0 <- lm(y ~ x)
# plot 1
plot(x, y, ylim = c(-4, 22), xlab = '', ylab = ''); abline(lm0, col = 2); points(11, -3, pch = 15)
# add 20, 10 point to the new lm
xx <- c(x, 11); yy <- c(y, -3)
abline(lm(yy ~ xx), col = 2, lty = 3)
text(-18, 10, srt=0, adj = 0, labels = '* No leverage \n* Low influence', xpd = TRUE, cex = 1.5)
# plot 2
plot(x, y, ylim = c(-4, 32), xlim = c(0, 31), xlab = '', ylab = ''); abline(lm0, col = 2); points(30, 30, pch = 15)
# add 20, 10 point to the new lm
xx <- c(x, 30); yy <- c(y, 30)
abline(lm(yy ~ xx), col = 2, lty = 3)
text(-31, 15, srt=0, adj = 0, labels = '* High leverage \n* No influence', xpd = TRUE, cex = 1.5)

# plot 3
plot(x, y, ylim = c(-4, 32), xlim = c(0, 31), xlab = '', ylab = ''); abline(lm0, col = 2); points(30, 15, pch = 15)
# add 20, 10 point to the new lm
xx <- c(x, 30); yy <- c(y, 15)
abline(lm(yy ~ xx), col = 2, lty = 3)
text(-31, 15, srt=0, adj = 0, labels = '* High leverage \n* High influence', xpd = TRUE, cex = 1.5)

Example of leverage
set.seed(1234564)
x = abs(rnorm(100,10,10))
y = (x) + rnorm(length(x), 0, x)
y[29] <- 100
lm=lm(y~scale(x))
plot(lm, which = 5, main = "High leverage and high influence")

Verify assumptions of `lm1`
par(mfrow=c(2,2), mar = c(4,4,2,1.1), oma =c(0,0,0,0))
plot(lm1)

# Plot linear model and observations
par(mfrow = c(1,2))
coef(lm1) # constant and slope
plot(MaxAbund ~ Mass, data=bird) # left plot
abline(lm1) # line defined by the model parameters
hist(residuals(lm1)) # plot on the right : distribution of residuals

# Test the normality of residuals
shapiro.test(residuals(lm1))

# Skewness test
library(e1071)
skewness(residuals(lm1))

# log-transform the variables
bird$logMaxAbund <- log10(bird$MaxAbund)
bird$logMass <- log10(bird$Mass)

# Linear model with transformed data
lm2 <- lm(logMaxAbund ~ logMass, data = bird)

# Diagnostic plots
par(mfrow=c(2,2), mar=c(3,4,1.15,1.2))
plot(lm2)

# Plot linear model and observations
par(mfrow = c(1,2))
coef(lm2) 
plot(logMaxAbund ~ logMass, data=bird)
abline(lm2) 
hist(residuals(lm2)) 

# Print fitted model parameters
summary(lm2)

# Vectors of residuals and predicted values
e <- residuals(lm2)
y <- fitted(lm2)
coefficients(lm2) # coefficients
summary(lm2)$coefficients # coefficients and T-tests
summary(lm2)$adj.r.squared # adjusted R squared

# Summary of the linear model with log-transformed data
summary(lm2)

# Linear model with terrestrial birds
lm3 <- lm(logMaxAbund~logMass, data=bird, subset=!bird$Aquatic)
# excludes waterfowls (!birdsAquatic == TRUE)
# or in an equivalent way:
# lm3 <- lm(logMaxAbund~logMass, data=bird, subset=bird$Aquatic == 0)

# Model output
lm3

par(mfrow=c(2,2), mar = c(4,4,2,1.1), oma =c(0,0,0,0))
plot(lm3)

summary(lm3)

# Fitting a linear model on passerine birds
lm4 <- lm(logMaxAbund ~ logMass, data=bird, subset=bird$Passerine == 1)
lm4

par(mfrow=c(2,2))
plot(lm4)

# Model assumptions
par(mfrow=c(2,2), mar = c(4,4,2,1.1), oma =c(0,0,0,0))
plot(lm4)

summary(lm4)


##Section: 04-t-test-anova.R 

# data and all requirements
library(car)
bird <- read.csv('data/birdsdiet.csv')

bird$logMass <- log(bird$Mass)
bird$logMaxAbund <- log(bird$MaxAbund)
bird$Diet <- as.factor(bird$Diet)

source('images/figAnova.R')

t.test(Y ~ X2, data= data, alternative = "two.sided",var.equal=TRUE)

boxplot(logMass ~ Aquatic, data=bird, ylab=expression("log"[10]*"(Bird Mass)"),
        names=c("Non-Aquatic","Aquatic"),
        col=c("yellowgreen","skyblue"))


# Assumption of equal variance
var.test(logMass~Aquatic,data=bird)

# We are now ready to run the t-test
ttest1 <- t.test(x=bird$logMass[bird$Aquatic==0], y=bird$logMass[bird$Aquatic==1], var.equal=TRUE)

# or equivalently
ttest1 <- t.test(logMass~Aquatic, var.equal=TRUE, data=bird)
ttest1


lm.t <- lm(logMass~Aquatic, data = bird)
anova(lm.t)


ttest1$statistic^2
anova(lm.t)$F

# Unilateral T-test
uni.ttest1 <- t.test(logMass~Aquatic, var.equal=TRUE, data=bird, alternative="less")

# Test de t en spécifiant l'argument "alternative"
uni.ttest1 <- t.test(logMass~Aquatic, var.equal=TRUE, data=bird, alternative="less")
uni.ttest1

# Test de t en spécifiant l'argument "alternative"
uni.ttest1 <- t.test(logMass~Aquatic, var.equal=TRUE, data=bird, alternative="greater")
uni.ttest1

# Default alphabetical order
boxplot(logMaxAbund ~ Diet, data=bird)

# Relevel factors
med <- sort(tapply(bird$logMaxAbund, bird$Diet, median))
boxplot(logMaxAbund ~ factor(Diet, levels=names(med)), data=bird, col=c("white","lightblue1",
           "skyblue1","skyblue3","skyblue4"))

plot.design(logMaxAbund ~ Diet, data=bird, ylab = expression("log"[10]*"(Maximum Abundance)"))

# Plot for diagnostics
aov1 <- aov(logMaxAbund ~ Diet, data=bird)
opar <- par(mfrow=c(2,2))
plot(aov1)
par(opar)
# Plot for diagnostics
aov1 <- aov(logMaxAbund ~ Diet, data=bird)
opar <- par(mfrow=c(2,2))
plot(aov1)
par(opar)

# Test assumption of normality of residuals
shapiro.test(resid(aov1))

# Test assumption of homogeneity of variance
#Bartlett's test
bartlett.test(logMaxAbund ~ Diet, data=bird)

#Levene's test
library(car)
leveneTest(logMaxAbund ~ Diet, data = bird)

# Using aov()
aov1 <- aov(logMaxAbund ~ Diet, data=bird)
summary(aov1)

# Using lm()
anov1 <- lm(logMaxAbund ~ Diet, data=bird)
anova(anov1)

aov1 <- aov(logMaxAbund ~ Diet, data=bird)

summary(aov1)

anov1 <- lm(logMaxAbund ~ Diet, data=bird)

anova(anov1)

aov1 <- aov(logMaxAbund ~ Diet, data=bird)
anov1 <- lm(logMaxAbund ~ Diet, data=bird)
summary(aov1)

# Where does the Diet difference lie?
TukeyHSD(aov(anov1),ordered=T)

# or equivalently
TukeyHSD(aov1,ordered=T)

# Cette commande est équivalente à la précédente :
TukeyHSD(aov1,ordered=T)

# Graphical illustration of ANOVA model using barplot()

sd <- tapply(bird$logMaxAbund,list(bird$Diet),sd)
means <- tapply(bird$logMaxAbund,list(bird$Diet),mean)
n <- length(bird$logMaxAbund)
se <- 1.96*sd/sqrt(n)

bp <- barplot(means, col=c("white","lightblue1","skyblue1","skyblue3","skyblue4"),
       ylab = expression("log"[10]*"(Maximum Abundance)"), xlab="Diet", ylim=c(0,5))

# Add vertical se bars
segments(bp, means - se, bp, means + se, lwd=2)
# and horizontal lines
segments(bp - 0.1, means - se, bp + 0.1, means - se, lwd=2)
segments(bp - 0.1, means + se, bp + 0.1, means + se, lwd=2)
#add a line at 0
abline(h=0)


tapply(bird$logMaxAbund, bird$Diet, mean)
coef(anov1)
coef(anov1)[1] + coef(anov1)[2] # InsectVert
coef(anov1)[1] + coef(anov1)[3] # Plant


summary(anov1)

bird$Diet2 <- relevel(bird$Diet, ref="Plant")
anov_rl <- lm(logMaxAbund ~ Diet2, data=bird)
summary(anov_rl)
anova(anov_rl)

med <- sort(tapply(bird$logMaxAbund, bird$Diet, median))
bird$Diet2 <- factor(bird$Diet, levels=names(med))
anov2 <- lm(logMaxAbund ~ Diet2,data = bird)
summary(anov2)
anova(anov2)

bird$Diet2 <- relevel(bird$Diet, ref="Plant")
contrasts(bird$Diet2)

sum(contrasts(bird$Diet2)[,1]) # first condition for column 1
sum(contrasts(bird$Diet2)[,1]*contrasts(bird$Diet2)[,2]) # second condition for column 1 and 2

options(contrasts=c("contr.helmert", "contr.poly"))
sum(contrasts(bird$Diet2)[,1]) 
sum(contrasts(bird$Diet2)[,1]*contrasts(bird$Diet2)[,2]) 

anov3 <- lm(logMaxAbund ~ Diet, data = bird)
summary(anov3)

contrasts(bird$Diet2) <- cbind(c(4,-1,-1,-1,-1), c(0,1,1,-1,-1), c(0,0,0,1,-1), c(0,1,-1,0,0))
contrasts(bird$Diet2)

sum(contrasts(bird$Diet2)[,1]) # first condition for column 1
sum(contrasts(bird$Diet2)[,1]*contrasts(bird$Diet2)[,2]) # second condition for column 1 and 2

aov <- lm(Y ~ X, data)

aov <- lm(Y ~ X1 * X2 * ..., data)

aov <- lm(Y ~ X1 + X2 + ..., data)

anov4 <- lm(logMaxAbund ~ Diet*Aquatic, data=bird)
opar <- par(mfrow=c(2,2))
plot(anov4)
par(opar)

anova(anov4)

anov5 <- lm(logMaxAbund ~ Diet + Aquatic, data=bird)
anova(anov5, anov4)

interaction.plot(bird$Diet, bird$Aquatic, bird$logMaxAbund, col="black",
                 ylab = expression("log"[10]*"(Maximum Abundance)"), xlab="Diet")

table(bird$Diet, bird$Aquatic)

anova(anov1,anov5) # Recall: anov1 is model with only Diet as a factor

table(bird$Aquatic)

unb_anov1 <- lm(logMaxAbund ~ Aquatic + Diet, data=bird)
unb_anov2 <- lm(logMaxAbund ~ Diet + Aquatic, data=bird)
anova(unb_anov1)
anova(unb_anov2)

library(car)
Anova(unb_anov1,type="III")
Anova(unb_anov2,type="III")


##Section: 05-ancova.R 

# data and all requirements
library(car)
library(effects)
bird <- read.csv('data/birdsdiet.csv')

bird$logMass <- log(bird$Mass)
bird$logMaxAbund <- log(bird$MaxAbund)
bird$Diet <- as.factor(bird$Diet)

functions
f1 <- function(x, a, b) {
  return(x*a+b)
}
# conf for plot
col = rgb(118, 143, 175, maxColorValue = 255)
x <- 1:20
par(mfrow = c(1, 3), mar = c(1, 1, 6.5, 4))
# plot 1
plot(x, f1(x, a=1.1,b=2), ylim = c(0, 60), type = 'l', lwd = 2.5, xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty='l', col = col)
lines(f1(x, a=1.1,b=17), lwd = 2.5, col = col)
lines(f1(x, a=0.6,b=22), lwd = 2.5, col = col)
lines(f1(x, a=1.1,b=40), lwd = 2.5, col = col)
mtext('One level of the factor\n has a different slope', side = 3, line = 2, cex = 1.5)
# plot 2
plot(x, f1(x, a=.5,b=2), ylim = c(0, 60), type = 'l', lwd = 2.5, xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty='l', col = col)
lines(f1(x, a=1.1,b=17), lwd = 2.5, col = col)
lines(f1(x, a=1.1,b=22), lwd = 2.5, col = col)
lines(f1(x, a=0.01,b=40), lwd = 2.5, col = col)
mtext('Many levels have\n different slopes', side = 3, line = 2, cex = 1.5)
# plot 3
plot(x, f1(x, a=1.1,b=2), ylim = c(0, 60), type = 'l', lwd = 2.5, xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty='l', col = col)
lines(f1(x, a=1.1,b=17), lwd = 2.5, col = col)
lines(f1(x, a=1.1,b=22), lwd = 2.5, col = col)
lines(f1(x, a=1.1,b=40), lwd = 2.5, col = col)
mtext('No interaction', side = 3, line = 2, cex = 1.5)

ancova.example <- lm(uptake ~ conc*Treatment, data=CO2)
anova(ancova.example)

install.packages("effects")
library(effects)


adj.means.ex <- effect('Treatment', ancova.example)
plot(adj.means.ex)

str(bird)

# If you did the section on Contrasts above, you will need to reset the contrast to Treatment for ease of
# comparison using the ''options()'' function
# Otherwise, skip the first line of code below
options(contrasts=c("contr.treatment", "contr.poly"))

#solution
ancov1 <- lm(logMaxAbund ~ logMass*Diet, data=bird)
anova(ancov1)

summary(ancov1)


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

Dickcissel = read.csv("data/dickcissel.csv")
str(Dickcissel)

# Multiple regression
lm.mult <- lm(abund ~ clTma + NDVI + grass, data = Dickcissel)
summary(lm.mult)

# Assumptions
par(mfrow = c(2, 2))
par(mfrow=c(2,2), mar = c(3.9,4,1.2,1.1), oma =c(0,0,0,0))
plot(lm.mult)

summary(lm.mult)$coefficients

# Plot the response vs predictor variables
par(mfrow=c(1,3), mar=c(4, 4, 0.5, 0.5), cex = 1)
plot(abund ~ clTma, data = Dickcissel)
plot(abund ~ NDVI,  data = Dickcissel)
plot(abund ~ grass, data = Dickcissel)


poly.reg=data.frame(Degree = 0:5,
                    Name = c("Constant","Linear","Quadratic",
                             "Cubic","Quartic","Quintic"),
                    Example = c("\\(3\\)",
                                "\\(x+9\\)",
                                "\\(x^2-x+4\\)",
                                "\\(x^3-x^2+5\\)",
                                "\\(6x^4-x^3+x-2\\)",
                                "\\(x^5-3x^3+x^2+8\\)"))
knitr::kable(poly.reg, format = "html", escape=FALSE)

lm.linear <- lm(abund ~ clDD, data = Dickcissel)
lm.quad   <- lm(abund ~ clDD + I(clDD^2), data = Dickcissel)
lm.cubic  <- lm(abund ~ clDD + I(clDD^2) + I(clDD^3), data = Dickcissel)


summary(lm.linear)
summary(lm.quad)
summary(lm.cubic)

mod <- lm(clDD ~ clFD + clTmi + clTma + clP + grass, data = Dickcissel)
car::vif(mod)

library(vegan)
part.lm = varpart(Dickcissel$abund, Dickcissel[,c("clDD","clFD","clTmi","clTma","clP")],
Dickcissel[ ,c("broadleaf","conif","grass","crop", "urban","wetland")])
part.lm

par(mar=rep(0.5,4))
showvarparts(2)

?showvarparts
# With two explanatory tables, the fractions
# explained uniquely by each of the two tables
# are ‘[a]’ and ‘[c]’, and their joint effect
# is ‘[b]’ following Borcard et al. (1992).

par(mar=rep(0.5,4))
plot(part.lm,
     digits = 2,
     bg = rgb(48,225,210,80,
              maxColorValue=225),
     col = "turquoise4")

out.1 = rda(Dickcissel$abund,
            Dickcissel[ ,c("clDD", "clFD","clTmi","clTma","clP")],
            Dickcissel[ ,c("broadleaf","conif","grass","crop", "urban","wetland")])

out.2 = rda(Dickcissel$abund,
            Dickcissel[ ,c("broadleaf","conif","grass","crop", "urban", "wetland")],
            Dickcissel[ ,c("clDD","clFD","clTmi", "clTma","clP")])


out.1 = rda(Dickcissel$abund,
            Dickcissel[ ,c("clDD", "clFD","clTmi","clTma","clP")],
            Dickcissel[ ,c("broadleaf","conif","grass","crop", "urban","wetland")])
out.2 = rda(Dickcissel$abund,
            Dickcissel[ ,c("broadleaf","conif","grass","crop", "urban", "wetland")],
            Dickcissel[ ,c("clDD","clFD","clTmi", "clTma","clP")])

# Climate
anova(out.1, step = 1000, perm.max = 1000)

# Land cover
anova(out.2, step = 1000, perm.max = 1000)


##Section: 07-final-considerations.R 




##Section: 08-references.R 




