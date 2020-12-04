## ----setup, echo = FALSE------------------------------------------------------
knitr::opts_chunk$set(
  comment = "#",
  collapse = TRUE,
  #cache = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.width=6, fig.height=6,
  fig.retina = 3,
  fig.align = 'center'
)
options(repos=structure(c(CRAN="http://cran.r-project.org")))


## ----install_pkgs, echo = FALSE, results = "asis"-----------------------------
cat(
  qcbsRworkshops::first_slides(4, c('dplyr', 'vegan', 'e1071', 'MASS', 'car','effect'), lang = "fr")
)


## ---- eval=TRUE, echo=FALSE---------------------------------------------------
bird <- read.csv("data/birdsdiet.csv", stringsAsFactors = TRUE)


## ---- eval = FALSE------------------------------------------------------------
## bird <- read.csv("birdsdiet.csv", stringsAsFactors = TRUE)


## -----------------------------------------------------------------------------
str(bird)


## -----------------------------------------------------------------------------
mean(bird$MaxAbund)


## -----------------------------------------------------------------------------
median(bird$MaxAbund)


## -----------------------------------------------------------------------------
var(bird$MaxAbund)


## -----------------------------------------------------------------------------
sd(bird$MaxAbund)


## -----------------------------------------------------------------------------
plot(bird$Mass, bird$MaxAbund)


## ---- eval=FALSE--------------------------------------------------------------
## plot(bird$Mass, bird$MaxAbund)


## ---- echo=FALSE--------------------------------------------------------------
plot(bird$Mass, bird$MaxAbund)
abline(coef = c(70, -.03),  lwd = 1.5, lty = 2, col = palette()[2])
abline(coef = c(20, -.005), lwd = 1.5, lty = 2, col = palette()[4])
abline(coef = c(200, -.1),  lwd = 1.5, lty = 2, col = palette()[6])


## ---- echo=FALSE, fig.height=4, fig.width=6-----------------------------------
x <- seq(-5, 5, length=100)
p <- dnorm(x, 0, 1)
plot(x, p, type = "l", lty = 2, lwd = 1.5,  xlab = "Residuals", ylab = "Probability", xaxt = "n")


## ---- eval=FALSE--------------------------------------------------------------
## y ~ 1 + x


## ---- eval=FALSE--------------------------------------------------------------
## y ~ x


## -----------------------------------------------------------------------------
plot(bird$Mass, bird$MaxAbund)


## ---- eval=FALSE--------------------------------------------------------------
## MaxAbund ~ Mass


## ----eval=TRUE----------------------------------------------------------------
lm1 <- lm(MaxAbund ~ Mass, data = bird)


## ---- eval=TRUE---------------------------------------------------------------
lm1


## ---- eval=FALSE, fig.height=6, fig.width=8-----------------------------------
## par(mfrow=c(2,2))
## plot(lm1)


## ---- eval=FALSE--------------------------------------------------------------
## par(mfrow=c(2,2)
## plot(lm1)


## ---- echo=FALSE, fig.height=4.75, fig.width=5.5------------------------------
par(mfrow=c(2,2), mar = c(4,4,2,1.1), oma =c(0,0,0,0))
plot(lm1)


## ---- echo = FALSE, fig.height=5, fig.width=6.5-------------------------------
  set.seed(1234564)
  x <- rnorm(100,10,10)
  y <- 2*x+0 + rnorm(100)
  lm <- lm(y~x)
  plot(lm, which = 1)


## ---- echo=FALSE, fig.height=4.5, fig.width=8.5, warning=FALSE----------------
par(mfrow=c(1,2))
set.seed(1234564)
x = rnorm(100,10,10)
y = (x)^2 + rnorm(length(x),0,30)
lm=lm(y~scale(x))
plot(lm,which = 1, main = "Non-linéaire", col.main="red")

x = abs(rnorm(100,10,10))
y = (x) + rnorm(length(x), 0, x)
lm=lm(y~scale(x))
plot(lm,which = 1, main = "Hétéroscédastique", col.main="red")


## ---- echo=FALSE, fig.height=4.5, fig.width=6, warning=FALSE------------------
set.seed(1234564)
x <- 1:100
y <- x + rnorm(100,sd=5)
lm=lm(y~x)
plot(lm,which = 3)


## ---- echo=FALSE, fig.height=4.5, fig.width=6, warning=FALSE------------------
set.seed(2)
x = abs(rnorm(100,10,10))
y = (x) + rnorm(length(x), 0, x)
lm=lm(y~scale(x))
plot(lm,which = 3)


## ---- echo=FALSE, fig.height=4.5, fig.width=6, warning=FALSE------------------
set.seed(1234564)
x <- 1:100
y <- x + rnorm(100,sd=5)
lm=lm(y~x)
plot(lm, which = 2)



## ---- echo=FALSE, fig.height=4.5, fig.width=6, warning=FALSE------------------
set.seed(2)
x = abs(rnorm(100,10,10))
y = (x) + rnorm(length(x), 0, x)
lm=lm(y~scale(x))
plot(lm, which = 2)


## ---- echo=FALSE, fig.height=8, fig.width=7.7, warning=FALSE------------------
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
text(-20, 10, srt=0, adj = 0, labels = "* Pas d'effet de levier \n* Faible influence
", xpd = TRUE, cex = 1.5)
# plot 2
plot(x, y, ylim = c(-4, 32), xlim = c(0, 31), xlab = '', ylab = ''); abline(lm0, col = 2); points(30, 30, pch = 15)
# add 20, 10 point to the new lm
xx <- c(x, 30); yy <- c(y, 30)
abline(lm(yy ~ xx), col = 2, lty = 3)
text(-33, 15, srt=0, adj = 0, labels = "* Effet de levier \n* Pas d'influence", xpd = TRUE, cex = 1.5)

# plot 3
plot(x, y, ylim = c(-4, 32), xlim = c(0, 31), xlab = '', ylab = ''); abline(lm0, col = 2); points(30, 15, pch = 15)
# add 20, 10 point to the new lm
xx <- c(x, 30); yy <- c(y, 15)
abline(lm(yy ~ xx), col = 2, lty = 3)
text(-33, 15, srt=0, adj = 0, labels = '* Effet de levier \n* Influence élevée', xpd = TRUE, cex = 1.5)


## ---- echo=FALSE, fig.height=4.5, fig.width=9, warning=FALSE------------------
par(mfrow=c(1,2))
set.seed(1234564)
x <- 1:100
y <- x + rnorm(100,sd=5)
lm=lm(y~x)
plot(lm, which = 5, main = "Aucune observation influente", col.main=palette()[3])

set.seed(1234564)
x = abs(rnorm(100,10,10))
y = (x) + rnorm(length(x), 0, x)
lm=lm(y~scale(x))
plot(lm, which = 5, main = "Effet de levier et faible influence",  col.main=palette()[3])


## ---- echo=FALSE, fig.height=5, fig.width=8, warning=FALSE--------------------
set.seed(1234564)
x = abs(rnorm(100,10,10))
y = (x) + rnorm(length(x), 0, x)
y[29] <- 100
lm=lm(y~scale(x))
plot(lm, which = 5, main = "Effet de levier et influence élevée")


## ---- eval=FALSE, fig.height=5.5, fig.width=7.5-------------------------------
## par(mfrow=c(2,2))
## plot(lm1)


## ---- echo=FALSE, fig.height=5.5, fig.width=7.5-------------------------------
par(mfrow=c(2,2), mar = c(4,4,2,1.1), oma =c(0,0,0,0))
plot(lm1)


## ---- fig.height=4, fig.width=11----------------------------------------------
par(mfrow = c(1,2))
coef(lm1) # constante et pente
plot(MaxAbund ~ Mass, data=bird) # graphique à gauche
abline(lm1) # ligne définie par les paramètres du modèle
hist(residuals(lm1)) # graphique à droite : distribution des résidus


## -----------------------------------------------------------------------------
shapiro.test(residuals(lm1))

library(e1071)
skewness(residuals(lm1))


## -----------------------------------------------------------------------------
bird$logMaxAbund <- log10(bird$MaxAbund)
bird$logMass <- log10(bird$Mass)


## -----------------------------------------------------------------------------
lm2 <- lm(logMaxAbund ~ logMass, data = bird)


## -----------------------------------------------------------------------------
lm2 <- lm(logMaxAbund ~ logMass, data = bird)

lm2


## ---- fig.height=5.5, fig.width=7.5-------------------------------------------
par(mfrow=c(2,2), mar=c(3,4,1.15,1.2))
plot(lm2)


## ---- fig.height=4, fig.width=11----------------------------------------------
par(mfrow = c(1,2))
coef(lm2) # constante et pente
plot(logMaxAbund ~ logMass, data=bird) # graphique à gauche
abline(lm2) # ligne définie par les paramètres du modèle
hist(residuals(lm2)) # graphique à droite : distribution des résidus


## ---- comment=""--------------------------------------------------------------
summary(lm2)


## -----------------------------------------------------------------------------
# Vecteurs de résidus et valeures prédites
e <- residuals(lm2)
y <- fitted(lm2)

coefficients(lm2) # coefficients
summary(lm2)$coefficients # coefficients avec test de t

summary(lm2)$adj.r.squared # R au carré ajusté


## ---- comment=""--------------------------------------------------------------
summary(lm2)


## -----------------------------------------------------------------------------
lm3 <- lm(logMaxAbund~logMass, data=bird, subset=!bird$Aquatic)
# exclut les oiseaux aquatiques (!birdsAquatic == TRUE)
# ou de façon équivalente :
# lm3 <- lm(logMaxAbund~logMass, data=bird, subset=bird$Aquatic == 0)

lm3


## ---- eval=FALSE, fig.height=5.5, fig.width=7.5-------------------------------
## par(mfrow=c(2,2))
## plot(lm3)


## ---- echo=FALSE, fig.height=5.5, fig.width=7.5-------------------------------
par(mfrow=c(2,2), mar = c(4,4,2,1.1), oma =c(0,0,0,0))
plot(lm3)


## ---- comment=""--------------------------------------------------------------
summary(lm3)


## -----------------------------------------------------------------------------
lm4 <- lm(logMaxAbund ~ logMass, data=bird, subset=bird$Passerine == 1)
lm4


## ---- eval=FALSE--------------------------------------------------------------
## par(mfrow=c(2,2))
## plot(lm4)


## ----fig.height=6, fig.width=8, echo=FALSE------------------------------------
par(mfrow=c(2,2), mar = c(4,4,2,1.1), oma =c(0,0,0,0))
plot(lm4)


## ---- comment=""--------------------------------------------------------------
summary(lm4)


## ---- echo = FALSE, fig.height=3, fig.width=6.5-------------------------------
source('script/figAnova.R')


## ----eval=TRUE,fig.height=5.2, fig.width=6.5----------------------------------
boxplot(logMass ~ Aquatic,
        data = bird, names = c("Non aquatique", "Aquatique"))


## -----------------------------------------------------------------------------
var.test(logMass ~ Aquatic, data = bird)


## -----------------------------------------------------------------------------
ttest1 <- t.test(logMass ~ Aquatic, var.equal = TRUE, data = bird)

# Or use lm()
ttest.lm1 <- lm(logMass ~ Aquatic, data=bird)


## -----------------------------------------------------------------------------
ttest1$statistic^2
anova(ttest.lm1)$`F value`
# réponse : F=60.3845 dans les deux cas


## -----------------------------------------------------------------------------
ttest1


## -----------------------------------------------------------------------------
# Unilateral t-test
uni.ttest1 <- t.test(logMass ~ Aquatic,
                     var.equal = TRUE,
                     data = bird,
                     alternative = "less")


## -----------------------------------------------------------------------------
uni.ttest1


## ----eval=F,echo=T------------------------------------------------------------
## # Unilateral t-test
## uni.ttest1 <- t.test(logMass ~ Aquatic,
##                      var.equal = TRUE,
##                      data = bird,
##                      alternative = "???")


## -----------------------------------------------------------------------------
# Unilateral t-test
uni.ttest1 <- t.test(logMass ~ Aquatic,
                     var.equal = TRUE,
                     data = bird,
                     alternative = "less")
uni.ttest1



## -----------------------------------------------------------------------------
str(bird)


## ---- fig.height=5, fig.width=7,echo=-1---------------------------------------
par(mar = c(4, 4, 0.5, 1))
boxplot(logMaxAbund ~ Diet, data = bird,
  ylab = expression("log"[10]*"(Abondance maximale)"), xlab = 'Régime alimentaire')


## ---- fig.height = 4.5, fig.width = 7, echo = -1------------------------------
par(mar = c(4, 4, .1, 1))
med <- sort(tapply(bird$logMaxAbund, bird$Diet, median))
boxplot(logMaxAbund ~ factor(Diet, levels = names(med)), data = bird,
        ylab = expression("log"[10]*"(Abondance maximale)"), xlab = 'Régime alimentaire')


## ---- fig.height=3.8, fig.width=6, echo=-1------------------------------------
par(mar = c(4,4,.4,1))
plot.design(logMaxAbund ~ Diet, data = bird,
  ylab = expression("log"[10]*"(Abondance maximale)"))


## -----------------------------------------------------------------------------
anov1 <- lm(logMaxAbund ~ Diet,
            data = bird)


## -----------------------------------------------------------------------------
aov1 <- aov(logMaxAbund ~ Diet,
            data = bird)


## -----------------------------------------------------------------------------
anova(anov1)

## -----------------------------------------------------------------------------
summary(aov1)


## -----------------------------------------------------------------------------
bartlett.test(logMaxAbund ~ Diet, data = bird)


## -----------------------------------------------------------------------------
library(car)
leveneTest(logMaxAbund ~ Diet, data = bird)


## -----------------------------------------------------------------------------
shapiro.test(resid(anov1))


## ----eval=FALSE---------------------------------------------------------------
## data$logY <- log10(data$Y)


## ----eval=FALSE---------------------------------------------------------------
## kruskal.test(Y~X, data)


## -----------------------------------------------------------------------------
summary(anov1)


## -----------------------------------------------------------------------------
summary.lm(aov1)


## -----------------------------------------------------------------------------
TukeyHSD(aov(anov1), ordered = TRUE)


## ---- fig.height=3, fig.width=7,echo=-1---------------------------------------
par(mar=c(3,3,0.5,0.5))
sd <- tapply(bird$logMaxAbund, bird$Diet, sd)
means <- tapply(bird$logMaxAbund, bird$Diet, mean)
n <- length(bird$logMaxAbund)
se <- 1.96*sd/sqrt(n)
bp <- barplot(means, ylim = c(0, max(bird$logMaxAbund) - 0.5))
epsilon = 0.1
segments(bp, means - se, bp, means + se, lwd=2) # barres verticales
segments(bp - epsilon, means - se, bp + epsilon, means - se, lwd = 2) # barres horizontales
segments(bp - epsilon, means + se, bp + epsilon, means + se, lwd = 2) # barres horizontales


## -----------------------------------------------------------------------------
anov2 <- lm(logMaxAbund ~ Diet*Aquatic, data = bird)
summary(anov2)


## -----------------------------------------------------------------------------
anov2 <- lm(logMaxAbund ~ Diet*Aquatic, data = bird)
anova(anov2)


## ----echo=FALSE,fig.height=3, fig.width=10------------------------------------
## functions
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
mtext('Un niveau du facteur\n a une pente différente', side = 3, line = 2, cex = 1.5)
# plot 2
plot(x, f1(x, a=.5,b=2), ylim = c(0, 60), type = 'l', lwd = 2.5, xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty='l', col = col)
lines(f1(x, a=1.1,b=17), lwd = 2.5, col = col)
lines(f1(x, a=1.1,b=22), lwd = 2.5, col = col)
lines(f1(x, a=0.01,b=40), lwd = 2.5, col = col)
mtext('Des nombreaux niveaux ont\n des pentes différentes', side = 3, line = 2, cex = 1.5)
# plot 3
plot(x, f1(x, a=1.1,b=2), ylim = c(0, 60), type = 'l', lwd = 2.5, xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty='l', col = col)
lines(f1(x, a=1.1,b=17), lwd = 2.5, col = col)
lines(f1(x, a=1.1,b=22), lwd = 2.5, col = col)
lines(f1(x, a=1.1,b=40), lwd = 2.5, col = col)
mtext("Pas d'interaction", side = 3, line = 2, cex = 1.5)


## ----eval=FALSE, warning=FALSE------------------------------------------------
## ancova.exemple <- lm(Y ~ X*Z, data=data) # X = quantitative; Z = qualitative
## library(effects)
## adj.means.ex <- effect('Z', ancova.exemple)
## plot(adj.means.ex)


## ----echo=FALSE, fig.height=3, fig.width=5------------------------------------
# plot to simulate effects::effect() plot
a = 40; b =20; sd = 8
par(mar=c(4,4,.5,1))
plot(c(a, b), ylim = c(10, 50), xlim = c(0.9, 2.1), xlab = 'factor Z', ylab = 'Y', xaxt = 'n')
lines(c(a, b), col = 4, pch = 1.5)
segments(c(1, 2), c(a-sd, b-sd), c(1,2), c(a+sd, b+sd), lwd = 1.5, col = 'orange')
segments(c(1, 2) - .04, c(a, b) + sd, c(1, 2) + .04, c(a, b) + sd, lwd = 1.5, col = 'orange')
segments(c(1, 2) - .04, c(a, b) - sd, c(1, 2) + .04, c(a, b) - sd, lwd = 1.5, col = 'orange')
points(c(1, 2), c(a, b), pch = 16, col = 4)
axis(1, at = c(1, 2), labels = FALSE) # add ticks
mtext(c('Level 1', 'Level 2'), 1, at = 1:2, line = 0.5) # add labels to ticks


## -----------------------------------------------------------------------------
str(bird)


## ----eval=FALSE---------------------------------------------------------------
## ancova.exemple <- lm(Y~X*Z, data=data)
## summary(ancova.exemple)


## ----eval=FALSE---------------------------------------------------------------
## ancova.exemple2 <- lm(Y~X+Z, data=data)
## summary(ancova.exemple2)


## -----------------------------------------------------------------------------
ancov1 <- lm(logMaxAbund ~ logMass*Diet,
             data = bird)
anova(ancov1)


## -----------------------------------------------------------------------------
ancov2 <- lm(logMaxAbund ~ logMass + Diet,
             data = bird)
anova(ancov2)


## ---- eval=TRUE---------------------------------------------------------------
Dickcissel = read.csv("data/dickcissel.csv")
str(Dickcissel)


## ---- fig.height=6, fig.width=7-----------------------------------------------
# select variables
var <- c('clTma', 'NDVI', 'grass', 'abund')
plot(Dickcissel[, var])


## ----eval=FALSE---------------------------------------------------------------
## lm.mult <- lm(abund ~ clTma + NDVI + grass, data = Dickcissel)
## summary(lm.mult)


## ----eval=FALSE,echo=-2-------------------------------------------------------
## par(mfrow = c(2, 2))
## par(mfrow=c(2,2), mar = c(3.9,4,1.2,1.1), oma =c(0,0,0,0))
## plot(lm.mult)


## -----------------------------------------------------------------------------
lm.mult <- lm(abund ~ clTma + NDVI + grass, data = Dickcissel)
summary(lm.mult)


## ---- fig.height=5.5, fig.width=8,echo=-2-------------------------------------
par(mfrow = c(2, 2))
par(mfrow=c(2,2), mar = c(3.9,4,1.2,1.1), oma =c(0,0,0,0))
plot(lm.mult)


## -----------------------------------------------------------------------------
summary(lm.mult)$coefficients


## ---- fig.height=3.5, fig.width=11,echo=-1------------------------------------
par(mfrow=c(1,3), mar=c(4, 4, 0.5, 0.5), cex = 1)
plot(abund ~ clTma, data = Dickcissel)
plot(abund ~ NDVI,  data = Dickcissel)
plot(abund ~ grass, data = Dickcissel)


## ----eval=FALSE---------------------------------------------------------------
## lm.full <- lm(abund ~ . - Present,
##               data = Dickcissel)
## lm.step <- step(lm.full)


## ----include=FALSE------------------------------------------------------------
lm.full <- lm(abund ~ . - Present,
              data = Dickcissel)
lm.step <- step(lm.full)


## ----eval=TRUE----------------------------------------------------------------
summary(lm.full)


## -----------------------------------------------------------------------------
summary(lm.step)


## -----------------------------------------------------------------------------
tapply(bird$logMaxAbund, bird$Diet, mean)
coef(anov1)
coef(anov1)[1] + coef(anov1)[2] # InsectVert
coef(anov1)[1] + coef(anov1)[3] # Plant


## ----eval=FALSE---------------------------------------------------------------
## bird$Diet2 <- relevel(bird$Diet, ref="Plant")
## anov2 <- lm(logMaxAbund ~ Diet2, data = bird)
## summary(anov2)
## anova(anov2)


## ----eval=FALSE---------------------------------------------------------------
## bird$Diet2 <- factor(bird$Diet, levels=names(med))
## anov2 <- lm(logMaxAbund ~ Diet2,
##             data = bird)
## summary(anov2)
## anova(anov2)


## -----------------------------------------------------------------------------
sum(contrasts(bird$Diet)[,1])
sum(contrasts(bird$Diet)[,1]*contrasts(bird$Diet)[,2])


## -----------------------------------------------------------------------------
options(contrasts=c("contr.helmert", "contr.poly"))
sum(contrasts(bird$Diet)[,1])
sum(contrasts(bird$Diet)[,1]*contrasts(bird$Diet)[,2])


## -----------------------------------------------------------------------------
anov3 <- lm(logMaxAbund ~ Diet, data = bird)
summary(anov3)


## -----------------------------------------------------------------------------
table(bird$Aquatic)


## -----------------------------------------------------------------------------
unb.anov1 <- lm(logMaxAbund ~ Aquatic + Diet, data = bird)
unb.anov2 <- lm(logMaxAbund ~ Diet + Aquatic, data = bird)


## -----------------------------------------------------------------------------
anova(unb.anov1)


## -----------------------------------------------------------------------------
anova(unb.anov2)


## -----------------------------------------------------------------------------
car::Anova(unb.anov1, type = "III")


## -----------------------------------------------------------------------------
car::Anova(unb.anov2, type = "III")


## ----echo=FALSE, warning=FALSE------------------------------------------------

poly.reg=data.frame(degre = 0:5,
                    Nom = c("Constante","Linéaire","Quadratique",
                             "Cubique","Quartique","Quintique"),
                    Example = c("\\(3\\)",
                                "\\(x+9\\)",
                                "\\(x^2-x+4\\)",
                                "\\(x^3-x^2+5\\)",
                                "\\(6x^4-x^3+x-2\\)",
                                "\\(x^5-3x^3+x^2+8\\)"))
knitr::kable(poly.reg, format = "html", escape=FALSE)


## ----echo=-c(4:6)-------------------------------------------------------------
lm.linear <- lm(abund ~ clDD, data = Dickcissel)
lm.quad   <- lm(abund ~ clDD + I(clDD^2), data = Dickcissel)
lm.cubic  <- lm(abund ~ clDD + I(clDD^2) + I(clDD^3), data = Dickcissel)
summ_lm.linear <- capture.output(summary(lm.linear))[c(9:12, 17, 18)]
summ_lm.quad <- capture.output(summary(lm.quad))[c(9:13, 18, 19)]
summ_lm.cubic <- capture.output(summary(lm.cubic))[c(9:14, 17, 18)]


## -----------------------------------------------------------------------------
print(summ_lm.linear)

## -----------------------------------------------------------------------------
print(summ_lm.quad)

## -----------------------------------------------------------------------------
print(summ_lm.cubic)


## ----warning=FALSE,message=FALSE----------------------------------------------
mod <- lm(clDD ~ clFD + clTmi + clTma + clP + grass, data = Dickcissel)
car::vif(mod)


## ----warning=FALSE,message=FALSE----------------------------------------------
library(vegan)
part.lm = varpart(Dickcissel$abund, Dickcissel[ ,c("clDD","clFD","clTmi","clTma","clP")],
                  Dickcissel[ ,c("broadleaf","conif","grass","crop", "urban","wetland")])
part.lm


## ----fig.height=3.2,echo=-1---------------------------------------------------
par(mar=rep(0.5,4))
showvarparts(2)


## ----eval=FALSE---------------------------------------------------------------
## ?showvarparts
## # With two explanatory tables, the fractions
## # explained uniquely by each of the two tables
## # are ‘[a]’ and ‘[c]’, and their joint effect
## # is ‘[b]’ following Borcard et al. (1992).


## ----fig.height=4,echo=-1-----------------------------------------------------
par(mar=rep(0.5,4))
plot(part.lm,
     digits = 2,
     bg = rgb(48,225,210,80,
              maxColorValue=225),
     col = "turquoise4")


## ----eval=FALSE---------------------------------------------------------------
## out.1 = rda(Dickcissel$abund,
##             Dickcissel[ ,c("clDD", "clFD","clTmi","clTma","clP")],
##             Dickcissel[ ,c("broadleaf","conif","grass","crop", "urban","wetland")])


## ----eval=FALSE---------------------------------------------------------------
## out.2 = rda(Dickcissel$abund,
##             Dickcissel[ ,c("broadleaf","conif","grass","crop", "urban", "wetland")],
##             Dickcissel[ ,c("clDD","clFD","clTmi", "clTma","clP")])
## 


## ----include=FALSE------------------------------------------------------------
out.1 = rda(Dickcissel$abund,
            Dickcissel[ ,c("clDD", "clFD","clTmi","clTma","clP")],
            Dickcissel[ ,c("broadleaf","conif","grass","crop", "urban","wetland")])
out.2 = rda(Dickcissel$abund,
            Dickcissel[ ,c("broadleaf","conif","grass","crop", "urban", "wetland")],
            Dickcissel[ ,c("clDD","clFD","clTmi", "clTma","clP")])


## -----------------------------------------------------------------------------
# Climat seul
anova(out.1, step = 1000, perm.max = 1000)


## -----------------------------------------------------------------------------
# Couverture du paysage seul
anova(out.2, step = 1000, perm.max = 1000)

