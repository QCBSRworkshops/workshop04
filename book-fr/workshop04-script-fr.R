##Section: 01-preparation-pour-l-atelier.R 

###Avis ###
#                                                                            #
#Ceci est un script généré automatiquement basé sur les morceaux de code du  #
#livre pour cet atelier.                                                     #
#                                                                            #
#Il est minimalement annoté pour permettre aux participants de fournir leurs #
#commentaires : une pratique que nous encourageons vivement.                 #
#                                                                            #
#Notez que les solutions aux défis sont également incluses dans ce script.   #
#Lorsque vous résolvez les défis par vous-méme, essayez de ne pas parcourir  #
#le code et de regarder les solutions.                                       #
#                                                                            #
#Bon codage !                                                               #


# Installez les librairies requises
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


##Section: 02-introduction-fr.R 

# Importez le jeu de données "bidsdiet" et le sauver dans l'objet "bird"
bird <- read.csv("data/birdsdiet.csv", stringsAsFactors = TRUE)

# Explorez les variables du jeu de données "bird"
str(bird)

# Moyenne de l'abondance maximale observée
mean(bird$MaxAbund)

# Médiane de l'abondance maximale observée
median(bird$MaxAbund)

# Variance de l'abondance maximale observée
var(bird$MaxAbund)

# Écart type de l'abondance maximale observée
sd(bird$MaxAbund)

# Tracer la réponse en fonction du prédicteur
plot(bird$Mass, bird$MaxAbund)

plot(bird$Mass, bird$MaxAbund)
abline(coef = c(70, -.03),  lwd = 1.5, lty = 2, col = palette()[2])
abline(coef = c(20, -.005), lwd = 1.5, lty = 2, col = palette()[4])
abline(coef = c(200, -.1),  lwd = 1.5, lty = 2, col = palette()[6])


##Section: 03-modele-lineaire.R 

# Régression linéaire de l'abondance maximale en fonction de la masse
lm1 <- lm(MaxAbund ~ Mass, data = bird)

# Examen de la sortie de la régression
lm1

# Tracer les quatres graphiques diagnostiques
par(mfrow=c(2,2))
plot(lm1)

par(mfrow=c(2,2), mar = c(4,4,2,1.1), oma =c(0,0,0,0))
plot(lm1)

Graphique #1 - Résidus vs valeurs prédites
  set.seed(1234564)
  x <- rnorm(100,10,10)
  y <- 2*x+0 + rnorm(100)
  lm <- lm(y~x)
  plot(lm, which = 1)

Exemple de distribution des points non-linéaire
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

Graphique #2 - Échelle localisée
set.seed(1234564)
x <- 1:100
y <- x + rnorm(100,sd=5)
lm=lm(y~x)
plot(lm,which = 3)

Exemple de distribution des points présentant une tendance marquée
set.seed(2)
x = abs(rnorm(100,10,10))
y = (x) + rnorm(length(x), 0, x)
lm=lm(y~scale(x))
plot(lm,which = 3)

Graphique #3 - Normal QQ
set.seed(1234564)
x <- 1:100
y <- x + rnorm(100,sd=5)
lm=lm(y~x)
plot(lm, which = 2)

# Exemple de distribution déviant de la normale
set.seed(2)
x = abs(rnorm(100,10,10))
y = (x) + rnorm(length(x), 0, x)
lm=lm(y~scale(x))
plot(lm, which = 2)

Graphique #4 - Résidus vs effet de levier
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

Graphique présentant l'influence des points et la distance de Cook
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

Exemple d'effet de levier
set.seed(1234564)
x = abs(rnorm(100,10,10))
y = (x) + rnorm(length(x), 0, x)
y[29] <- 100
lm=lm(y~scale(x))
plot(lm, which = 5, main = "Effet de levier et influence élevée")

# Vérification des conditions d'application de lm1
par(mfrow=c(2,2), mar = c(4,4,2,1.1), oma =c(0,0,0,0))
plot(lm1)

# Graphique du modèle et des observations
par(mfrow = c(1,2))
coef(lm1) # constante et pente
plot(MaxAbund ~ Mass, data=bird) # graphique à gauche
abline(lm1) # ligne définie par les paramètres du modèle
hist(residuals(lm1)) # graphique à droite : distribution des résidus

# Test de la normalité des résidus
shapiro.test(residuals(lm1))

# Test d'asymétrie
library(e1071)
skewness(residuals(lm1))

# Transformation logarithmique des données
bird$logMaxAbund <- log10(bird$MaxAbund)
bird$logMass <- log10(bird$Mass)

# Modèle linéaire sur les données transformées
lm2 <- lm(logMaxAbund ~ logMass, data = bird)

# Graphiques diagnostiques
par(mfrow=c(2,2), mar=c(3,4,1.15,1.2))
plot(lm2)

# Graphique du modèle et des observations
par(mfrow = c(1,2))
coef(lm2) # constantes et pente
plot(logMaxAbund ~ logMass, data=bird) # graphique à gauche
abline(lm2) # ligne définie par les paramètres du modèle
hist(residuals(lm2)) # graphique à droite : distribution des résidus

# Paramètres du modèle ajusté
summary(lm2)

# Vecteurs de résidus et valeurs prédites
e <- residuals(lm2)
y <- fitted(lm2)
coefficients(lm2) # coefficients
summary(lm2)$coefficients # coefficients avec test de t
summary(lm2)$adj.r.squared # R au carré ajusté

# Sommaire du modèle linéaire avec les données log-transformées
summary(lm2)

# Modèle linéaire avec oiseaux terrestres
lm3 <- lm(logMaxAbund~logMass, data=bird, subset=!bird$Aquatic)
# exclut les oiseaux aquatiques (!birdsAquatic == TRUE)
# ou de façon équivalente :
# lm3 <- lm(logMaxAbund~logMass, data=bird, subset=bird$Aquatic == 0)

# Sortie du modèle
lm3

par(mfrow=c(2,2))
plot(lm3)

par(mfrow=c(2,2), mar = c(4,4,2,1.1), oma =c(0,0,0,0))
plot(lm3)

summary(lm3)

# Modèle linéaire pour les passeraux
lm4 <- lm(logMaxAbund ~ logMass, data=bird, subset=bird$Passerine == 1)
lm4

par(mfrow=c(2,2))
plot(lm4)

# Conditions d'application du modèle
par(mfrow=c(2,2), mar = c(4,4,2,1.1), oma =c(0,0,0,0))
plot(lm4)

summary(lm4)


##Section: 04-test-t-anova.R 

# data and all requirements
library(car)
bird <- read.csv('data/birdsdiet.csv')

bird$logMass <- log(bird$Mass)
bird$logMaxAbund <- log(bird$MaxAbund)
bird$Diet <- as.factor(bird$Diet)

source('images/figAnova.R')

t.test(Y ~ X2, data= data, alternative = "two.sided")

# test de t
boxplot(logMass ~ Aquatic, data=bird, ylab=expression("log"[10]*"(Bird Mass)"),
        names=c("Non-Aquatic","Aquatic"),
        col=c("yellowgreen","skyblue"))


# Homogénéité de la variance
var.test(logMass~Aquatic,data=bird)


ttest1 <- t.test(x=bird$logMass[bird$Aquatic==0], y=bird$logMass[bird$Aquatic==1], var.equal=TRUE)

# ou l'équivalent
ttest1 <- t.test(logMass~Aquatic, var.equal=TRUE, data=bird)
ttest1


lm.t <- lm(logMass~Aquatic, data = bird)
anova(lm.t)


ttest1$statistic^2
anova(lm.t)$F

# Test de t en spécifiant l'argument "alternative"
uni.ttest1 <- t.test(logMass~Aquatic, var.equal=TRUE, data=bird, alternative="less")
uni.ttest1

# Test de t en spécifiant l'argument "alternative"
uni.ttest1 <- t.test(logMass~Aquatic, var.equal=TRUE, data=bird, alternative="less")
uni.ttest1

# Test de t en spécifiant l'argument "alternative"
uni.ttest1 <- t.test(logMass~Aquatic, var.equal=TRUE, data=bird, alternative="greater")
uni.ttest1

# Ordre alphabétique par défaut
boxplot(logMaxAbund ~ Diet, data=bird)

# Réorganiser l'ordre des facteurs
med <- sort(tapply(bird$logMaxAbund, bird$Diet, median))
boxplot(logMaxAbund ~ factor(Diet, levels=names(med)), data=bird, col=c("white","lightblue1",
           "skyblue1","skyblue3","skyblue4"))

plot.design(logMaxAbund ~ Diet, data=bird, ylab = expression("log"[10]*"(Maximum Abundance)"))

# Graphiques pour conditions d'application
aov1 <- aov(logMaxAbund ~ Diet, data=bird)
opar <- par(mfrow=c(2,2))
plot(aov1)
par(opar)
# Plot for diagnostics
aov1 <- aov(logMaxAbund ~ Diet, data=bird)
opar <- par(mfrow=c(2,2))
plot(aov1)
par(opar)

# Test de la supposition de la normalité des résidus
shapiro.test(resid(aov1))

# Test de la supposition de l'homogénéité de la variance
# Bartlett
bartlett.test(logMaxAbund ~ Diet, data=bird)

#Levene
library(car)
leveneTest(logMaxAbund ~ Diet, data = bird)

# En utilisant aov()
aov1 <- aov(logMaxAbund ~ Diet, data=bird)
summary(aov1)

# En utilisant lm()
anov1 <- lm(logMaxAbund ~ Diet, data=bird)
anova(anov1)

aov1 <- aov(logMaxAbund ~ Diet, data=bird)

summary(aov1)

anov1 <- lm(logMaxAbund ~ Diet, data=bird)

anova(anov1)

aov1 <- aov(logMaxAbund ~ Diet, data=bird)
anov1 <- lm(logMaxAbund ~ Diet, data=bird)
summary(aov1)

# À quel niveau se situe la différence de diète ?
TukeyHSD(aov(anov1),ordered=T)

# Cette commande est équivalente à la précédente :
TukeyHSD(aov1,ordered=T)

# Cette commande est équivalente à la précédente :
TukeyHSD(aov1,ordered=T)

# Visualisation d'un modèle d'ANOVA à l'aide de la fonction barplot()

sd <- tapply(bird$logMaxAbund,list(bird$Diet),sd)
means <- tapply(bird$logMaxAbund,list(bird$Diet),mean)
n <- length(bird$logMaxAbund)
se <- 1.96*sd/sqrt(n)

bp <- barplot(means, col=c("white","lightblue1","skyblue1","skyblue3","skyblue4"),
       ylab = expression("log"[10]*"(Maximum Abundance)"), xlab="Diet", ylim=c(0,5))


# Ajout des lignes verticales représentant les erreurs types
segments(bp, means - se, bp, means + se, lwd=2)
# et des lignes horizontales
segments(bp - 0.1, means - se, bp + 0.1, means - se, lwd=2)
segments(bp - 0.1, means + se, bp + 0.1, means + se, lwd=2)
#ajout d'une ligne à 0
abline(h=0)


tapply(bird$logMaxAbund, bird$Diet, mean)
coef(anov1)
coef(anov1)[1] + coef(anov1)[2] # InsectVert
coef(anov1)[1] + coef(anov1)[3] # Plant



summary(anov1)

bird$Diet2 <- relevel(bird$Diet, ref="Plant")
anov2 <- lm(logMaxAbund ~ Diet2, data=bird)
summary(anov2)
anova(anov2)

med <- sort(tapply(bird$logMaxAbund, bird$Diet, median))
bird$Diet2 <- factor(bird$Diet, levels=names(med))
anov2 <- lm(logMaxAbund ~ Diet2,data = bird)
summary(anov2)
anova(anov2)

bird$Diet2 <- relevel(bird$Diet, ref="Plant")
contrasts(bird$Diet2)

sum(contrasts(bird$Diet2)[,1]) # condition 1 pour colonne 1
sum(contrasts(bird$Diet2)[,1]*contrasts(bird$Diet2)[,2]) # condition 2 pour colonne 1 et 2

options(contrasts=c("contr.helmert", "contr.poly"))
sum(contrasts(bird$Diet2)[,1]) 
sum(contrasts(bird$Diet2)[,1]*contrasts(bird$Diet2)[,2]) 

anov3 <- lm(logMaxAbund ~ Diet, data = bird)
summary(anov3)

contrasts(bird$Diet2) <- cbind(c(4,-1,-1,-1,-1), c(0,1,1,-1,-1), c(0,0,0,1,-1), c(0,1,-1,0,0))
contrasts(bird$Diet2)

sum(contrasts(bird$Diet2)[,1]) # première condition pour la colonne 1
sum(contrasts(bird$Diet2)[,1]*contrasts(bird$Diet2)[,2]) # deuxième condition pour les colonnes 1 et 2

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

anova(anov1,anov5) # Rappel que anov3 est le modèle avec le facteur "Diet" seulement.

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
mtext('Un niveau du facteur\n a une pente différente', side = 3, line = 2, cex = 1.5)
# plot 2
plot(x, f1(x, a=.5,b=2), ylim = c(0, 60), type = 'l', lwd = 2.5, xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty='l', col = col)
lines(f1(x, a=1.1,b=17), lwd = 2.5, col = col)
lines(f1(x, a=1.1,b=22), lwd = 2.5, col = col)
lines(f1(x, a=0.01,b=40), lwd = 2.5, col = col)
mtext('Plusieurs niveaux\n ont une pente différente', side = 3, line = 2, cex = 1.5)
# plot 3
plot(x, f1(x, a=1.1,b=2), ylim = c(0, 60), type = 'l', lwd = 2.5, xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty='l', col = col)
lines(f1(x, a=1.1,b=17), lwd = 2.5, col = col)
lines(f1(x, a=1.1,b=22), lwd = 2.5, col = col)
lines(f1(x, a=1.1,b=40), lwd = 2.5, col = col)
mtext('Aucune intéraction', side = 3, line = 2, cex = 1.5)

ancova.example <- lm(uptake ~ conc*Treatment, data=CO2)
anova(ancova.example)

install.packages("effects")
library(effects)


adj.means.ex <- effect('Treatment', ancova.example)
plot(adj.means.ex)

str(bird)

# Si vous avez complété la section avancée sur les contrastes, vous devrez réinitialiser les contrastes
# à l'aide de la fonction ''options()''
# Si vous n'avez pas complété la section avancée sur les contrastes, ignorez la première ligne du script.
options(contrasts=c("contr.treatment", "contr.poly"))

#solution
ancov1 <- lm(logMaxAbund ~ logMass*Diet, data=bird)
anova(ancov1)

summary(ancov1)

ancov2 <- lm(logMaxAbund ~ logMass + Diet, data=bird)
anova(ancov2)

lm2 <- lm(logMaxAbund ~ logMass, data=bird)
summary(lm2)

coef(ancov1)


plot(logMaxAbund~logMass, data=bird, col=Diet, pch=19, ylab=expression("log"[10]*"(Maximum Abundance)"),
     xlab=expression("log"[10]*"(Mass)"))
abline(a=coef(ancov1)[1],b=coef(ancov1)[2], col="deepskyblue1")
abline(a=sum(coef(ancov1)[1]+coef(ancov1)[3]),b=sum(coef(ancov1)[2]+coef(ancov1)[7]),col="green2", lwd=2)
abline(a=sum(coef(ancov1)[1]+coef(ancov1)[4]),b=sum(coef(ancov1)[2]+coef(ancov1)[8]),col="orange1", lwd=2)
abline(a=sum(coef(ancov1)[1]+coef(ancov1)[5]),b=sum(coef(ancov1)[2]+coef(ancov1)[9]),col="lightsteelblue1",
       lwd=2)
abline(a=sum(coef(ancov1)[1]+coef(ancov1)[6]),b=sum(coef(ancov1)[2]+coef(ancov1)[10]),col="darkcyan", lwd=2)


##Section: 06-regression-multiple.R 

Dickcissel = read.csv("data/dickcissel.csv")
str(Dickcissel)

# Sélectionner lesvariables
var <- c('clTma', 'NDVI', 'grass', 'abund')

# Graphiques des relations entre variables
plot(Dickcissel[, var])

# Régression multiple
lm.mult <- lm(abund ~ clTma + NDVI + grass, data = Dickcissel)
summary(lm.mult)

# Conditions d'application
par(mfrow = c(2, 2))
par(mfrow=c(2,2), mar = c(3.9,4,1.2,1.1), oma =c(0,0,0,0))
plot(lm.mult)

summary(lm.mult)$coefficients

# Graphique de la varible réponse vs. les prédicteurs
par(mfrow=c(1,3), mar=c(4, 4, 0.5, 0.5), cex = 1)
plot(abund ~ clTma, data = Dickcissel)
plot(abund ~ NDVI,  data = Dickcissel)
plot(abund ~ grass, data = Dickcissel)


poly.reg=data.frame(Degré = 0:5,
                    Nom = c("Constante","Linéaire","Quadratique",
                             "Cubique","Quartique","Quintique"),
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

# Climat seul
anova(out.1, step = 1000, perm.max = 1000)

# Couverture du paysage seul
anova(out.2, step = 1000, perm.max = 1000)


##Section: 07-considerations-finales.R 




##Section: 08-references-fr.R 




